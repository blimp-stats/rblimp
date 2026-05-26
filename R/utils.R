#' @importFrom utils combn
NULL

#' Internal error function
#' Wrapper for `cli_abort` to not specify the call
#' @importFrom cli cli_abort
#' @noRd
throw_error <- function(message, ..., .envir = parent.frame(), .frame = .envir) {
    cli::cli_abort(message, ..., .envir = .envir, .frame = .frame, call = NULL)
}


#' Internal function for checking strings are identical with case insensitive
#' @noRd
is_equal <- function(a, b) {
    tolower(a) == tolower(b)
}

#' Marker for fixing a moderator at specific SIMPLE evaluated values
#'
#' Used inside [`simple_plot`] and [`jn_plot`] formulas to restrict the SIMPLE
#' rows to those whose moderator was evaluated at the given label(s).
#'
#' @param ... named arguments where each name is a moderator in the SIMPLE
#'   command and each value is a single label or a vector of labels
#'   (e.g., `"Q25"`, `"+1 SD"`, `"0"`, or a parameter name) to keep.
#' @returns Calling `at()` outside a formula raises an error; the function only
#'   has meaning when it appears in a `simple_plot()` or `jn_plot()` formula.
#' @seealso [`simple_plot`], [`jn_plot`], [`jn_map`], [`join`]
#' @examples
#' \dontrun{
#' # Pin `m2` at the SIMPLE label "0", leaving `m1` to color the lines
#' simple_plot(y ~ x | m1 + at(m2 = "0"), model)
#'
#' # Pin to a subset (vector of labels) -- `m2` still faces over those values
#' simple_plot(y ~ x | m1 + at(m2 = c("Q25", "Q75")), model)
#'
#' # Use inside `jn_plot()` the same way
#' jn_plot(y ~ x | m1 + at(m2 = "+1 SD"), model)
#'
#' # In a 4-way fit, pin one moderator and map the slope surface over the other two
#' jn_map(y ~ x | m1 + m2 + at(m3 = "0"), four_way_model)
#' }
#' @export
at <- function(...) {
    throw_error(c(
        "{.fn at} is only valid inside a formula passed to {.fn simple_plot} or {.fn jn_plot}.",
        "i" = "Example: {.code simple_plot(y ~ x | m1 + at(m2 = \"0\"), model)}"
    ))
}

#' Marker for bundling multiple SIMPLE moderators into one compound moderator
#'
#' Used inside [`simple_plot`] and [`jn_plot`] formulas to treat a set of
#' SIMPLE moderators as a single conceptual moderator. Each row's color/legend
#' label combines every component's `"name @ value"` so that, e.g., the dummy
#' codes of a nominal predictor render as one set of colored lines instead of
#' a sparse facet grid.
#'
#' Only the first bare term on the right-hand side of `|` may use `join(...)`
#' (that is, the color / x-axis position). Facet positions must remain plain
#' moderator names or `at(...)` calls.
#'
#' @param ... bare moderator names (at least two) to bundle together.
#' @returns Calling `join()` outside a formula raises an error; it only has
#'   meaning inside `simple_plot()` / `jn_plot()` formulas.
#' @seealso [`simple_plot`], [`jn_plot`], [`jn_map`], [`at`]
#' @examples
#' \dontrun{
#' # Nominal predictor with dummy codes -- bundle them as one moderator
#' simple_plot(y ~ x | join(group.1, group.2), model)
#'
#' # Combine with `at()` to pin an unrelated moderator
#' simple_plot(y ~ x | join(group.1, group.2) + at(z = "0"), model)
#'
#' # In a 4-way fit, collapse the two facet moderators so the bound
#' #   annotation goes back into each strip of the JN plot
#' jn_plot(y ~ x | m1 + join(m2, m3), four_way_model)
#' }
#' @export
join <- function(...) {
    throw_error(c(
        "{.fn join} is only valid inside a formula passed to {.fn simple_plot} or {.fn jn_plot}.",
        "i" = "Example: {.code simple_plot(y ~ x | join(mod.1, mod.2), model)}"
    ))
}

#' Numeric sort key for a single SIMPLE moderator value label.
#'
#' Maps `"Q25"` -> `0.25`, `"+1 SD"` / `"-1 SD"` -> `+1` / `-1`,
#' a plain number string -> its numeric value, otherwise `NA_real_`.
#' Used to lay out factor levels in their natural numeric order rather
#' than the order Blimp happened to emit them in.
#' @noRd
mod_value_sort_key <- function(label) {
    if (is.na(label) || !nzchar(label)) return(NA_real_)
    if (grepl("^Q[0-9.]+$", label))
        return(as.numeric(sub("^Q", "", label)) / 100)
    if (grepl("\\s*SD\\s*$", label)) {
        n_sd <- suppressWarnings(as.numeric(sub("\\s*SD\\s*$", "", label)))
        if (!is.na(n_sd)) return(n_sd)
    }
    n <- suppressWarnings(as.numeric(label))
    if (!is.na(n)) return(n)
    NA_real_
}

#' Order a vector of formatted moderator labels by the underlying numeric
#' value(s). Labels look like `"name @ value"` for plain moderators and
#' `"name @ value, name @ value"` for compound (`join()`) moderators.
#'
#' Strips any trailing bound annotation (after `\n`), splits a compound label
#' on `", "` so `join()`-style strings are sorted lexicographically across
#' their components, and falls back to insertion order for labels whose
#' value can't be parsed as numeric.
#' @noRd
order_by_mod_value <- function(labels) {
    if (length(labels) == 0) return(integer(0))
    raw_parts <- lapply(labels, function(lbl) {
        no_bound <- sub("\n.*$", "", as.character(lbl))
        parts <- strsplit(no_bound, ", ", fixed = TRUE)[[1]]
        vapply(parts, function(p) sub("^[^@]*@\\s*", "", p), character(1))
    })
    keys <- lapply(raw_parts, function(rv)
        vapply(rv, mod_value_sort_key, double(1)))
    max_len <- max(vapply(keys, length, integer(1)))
    if (max_len == 0) return(seq_along(labels))
    cols <- lapply(seq_len(max_len), function(j)
        vapply(keys, function(k) if (length(k) >= j) k[j] else NA_real_,
               double(1)))
    do.call(order, c(cols, list(na.last = TRUE)))
}

#' Resolve a formula moderator name against the SIMPLE moderator list.
#'
#' If the supplied name appears directly in `simple_mods`, it's returned as-is.
#' Otherwise, if the name is declared as nominal on the model (`model@syntax$nominal`),
#' the function looks in `simple_mods` for dummy-code columns of the form
#' `name.1`, `name.2`, ... and returns those as a character vector so the
#' caller can treat them as a compound moderator.
#' @noRd
resolve_mod_name <- function(name, simple_mods, model) {
    if (tolower(name) %in% tolower(simple_mods)) return(name)
    nominal_str <- model@syntax$nominal
    if (is.null(nominal_str) || !length(nominal_str) || !nzchar(nominal_str))
        return(name)
    nominal_vars <- strsplit(nominal_str, "[ ;]+")[[1]]
    nominal_vars <- nominal_vars[nzchar(nominal_vars)]
    if (!(tolower(name) %in% tolower(nominal_vars))) return(name)
    pattern <- paste0("^", tolower(name), "\\.[0-9]+$")
    matches <- simple_mods[grepl(pattern, tolower(simple_mods))]
    if (length(matches) > 0) return(matches)
    name
}

#' Parse the RHS of a simple_plot / jn_plot formula
#'
#' Walks the formula's right-hand side language tree to extract the focal
#' predictor, the bare moderator symbols (in order), and any held-constant
#' filter values supplied via `at(...)`. The resulting `bare_mods` are
#' returned as a character vector; the first is treated as the color/legend
#' moderator and any extras are facets. `at_filter` is a named list whose
#' names are moderator names and values are character vectors of allowed
#' SIMPLE labels.
#' @noRd
parse_plot_formula <- function(formula) {
    if (!inherits(formula, "formula") || length(formula) != 3)
        throw_error(c(
            "The {.arg formula} was not correctly specified.",
            "Must have the form: `outcome ~ focal | moderator`"
        ))

    out_lang <- formula[[2]]
    rhs      <- formula[[3]]

    if (!is.call(rhs) || !identical(rhs[[1]], as.name("|")) || length(rhs) != 3)
        throw_error(c(
            "The {.arg formula} was not correctly specified.",
            "Must have the form: `outcome ~ focal | moderator`"
        ))

    focal_lang <- rhs[[2]]
    mod_lang   <- rhs[[3]]

    # Walk `+` tree to collect terms
    walk_plus <- function(e) {
        if (is.call(e) && length(e) == 3 && identical(e[[1]], as.name("+"))) {
            c(walk_plus(e[[2]]), walk_plus(e[[3]]))
        } else list(e)
    }
    terms <- walk_plus(mod_lang)

    bare_mods      <- character()       # display names (per logical moderator)
    mod_components <- list()            # display name -> char vec of SIMPLE mod names
    at_filter      <- list()

    for (t in terms) {
        if (is.call(t) && identical(t[[1]], as.name("at"))) {
            args <- as.list(t)[-1]
            if (length(args) == 0 || is.null(names(args)) || any(!nzchar(names(args))))
                throw_error("{.fn at} requires named arguments, e.g. {.code at(m2 = \"0\")}.")
            for (nm in names(args)) {
                val <- tryCatch(eval(args[[nm]], envir = baseenv()),
                                error = function(e) {
                                    throw_error(c(
                                        "Could not evaluate the value for {.field {nm}} inside {.fn at}.",
                                        i = "Values must be literal strings, numbers, or `c(...)` of those."
                                    ))
                                })
                at_filter[[nm]] <- as.character(val)
            }
        } else if (is.call(t) && identical(t[[1]], as.name("join"))) {
            args <- as.list(t)[-1]
            if (length(args) < 2)
                throw_error("{.fn join} requires at least 2 moderators.")
            comps <- vapply(args, function(a) {
                if (!is.name(a) && !is.character(a))
                    throw_error("Arguments to {.fn join} must be bare moderator names.")
                as.character(a)
            }, character(1))
            label <- paste(comps, collapse = ", ")
            bare_mods <- c(bare_mods, label)
            mod_components[[label]] <- comps
        } else if (is.name(t) || is.character(t)) {
            nm <- as.character(t)
            bare_mods <- c(bare_mods, nm)
            mod_components[[nm]] <- nm
        } else {
            throw_error(c(
                "Unrecognized term on the right-hand side of {.code |}: {.code {deparse(t)}}",
                i = "Allowed: bare moderator names, {.fn at} calls, and {.fn join} calls."
            ))
        }
    }

    list(
        outcome        = as.character(out_lang),
        focal          = as.character(focal_lang),
        bare_mods      = bare_mods,
        mod_components = mod_components,
        at_filter      = at_filter
    )
}

#' Parse CSV header line respecting parentheses and quotes
#'
#' @description
#' Splits a CSV header line on commas, but treats commas inside parentheses
#' or quotes as part of the field name rather than delimiters. This handles
#' variable names like "yjt(x,0)" or quoted names like "variable(x,0)".
#'
#' @param header_line Character string containing the CSV header line
#' @return Character vector of field names
#' @noRd
parse_csv_header <- function(header_line) {
    if (length(header_line) == 0 || header_line == "") {
        return(character(0))
    }

    chars <- strsplit(header_line, "")[[1]]
    fields <- character()
    current_field <- character()
    paren_depth <- 0
    in_quotes <- FALSE

    for (char in chars) {
        if (char == '"' && (length(current_field) == 0 ||
                           current_field[length(current_field)] != "\\")) {
            in_quotes <- !in_quotes
            current_field <- c(current_field, char)
        } else if (!in_quotes && char == "(") {
            paren_depth <- paren_depth + 1
            current_field <- c(current_field, char)
        } else if (!in_quotes && char == ")") {
            paren_depth <- paren_depth - 1
            current_field <- c(current_field, char)
        } else if (char == "," && paren_depth == 0 && !in_quotes) {
            fields <- c(fields, paste(current_field, collapse = ""))
            current_field <- character()
        } else {
            current_field <- c(current_field, char)
        }
    }

    if (length(current_field) > 0) {
        fields <- c(fields, paste(current_field, collapse = ""))
    }

    # Remove surrounding quotes from fields if present
    fields <- gsub('^"(.*)"$', '\\1', fields)

    return(fields)
}

#' Calculate maximum parameter name width across multiple variables for alignment
#' 
#' @description
#' Calculates the maximum character width of parameter names across multiple 
#' variables to ensure consistent alignment in multivariate model output.
#' Applies the same parameter name cleaning logic as the summary method.
#' 
#' @details
#' This function processes parameter names for multiple variables and finds the
#' maximum width after applying standard cleaning operations:
#' - Remove variable prefixes and replace with standard spacing
#' - Clean special characters and standardize terminology  
#' - Add extra spacing for correlation/covariance parameters to align with main variables
#' 
#' Used internally to coordinate alignment between main variables and their
#' associated correlation models in multivariate output.
#'
#' @param object A blimp_obj containing model results
#' @param variables Character vector of variable names to process
#' @return Integer representing the maximum parameter name width in characters
#' @noRd
calculate_max_par_width <- function(object, variables) {
    # Extract outcome name information
    oname <- attr(object@iterations, "outcome_name")
    if (is.null(oname)) return(0)
    
    oname <- tolower(oname)
    max_width <- 0
    
    # Process each variable to find maximum parameter name width
    for (variable in variables) {
        # Find parameters for this variable
        sel <- which(oname == tolower(variable))
        if (length(sel) > 0) {
            est <- object@estimates[sel, , drop = FALSE]
            
            # Apply identical cleaning logic as summary method
            clean_names <- rownames(est)
            clean_names <- gsub(paste0(variable, ' '), '   ', clean_names)
            clean_names <- gsub(' ~', '', clean_names)
            clean_names <- gsub(' R2:', '', clean_names)
            clean_names <- gsub('\\(standardized\\)', '', clean_names)
            clean_names <- gsub('residual variance', 'Residual Var.', clean_names)
            clean_names <- gsub('residual SD', 'Residual SD', clean_names)
            
            # Add extra prefix spacing for correlation/covariance models
            # This ensures alignment with main variable parameters like "   Intercept"
            if (any(grepl("^(Cov|Cor)\\(", clean_names))) {
                clean_names <- paste0("   ", clean_names)
            }
            
            # Update maximum width across all variables
            var_max_width <- max(nchar(clean_names))
            max_width <- max(max_width, var_max_width)
        }
    }
    
    return(max_width)
}


#' Find Boundaries in a Binary Function
#'
#' Locates all boundaries where a function transitions between 0 and 1 using
#' a hybrid approach: coarse grid search for detection followed by binary
#' search refinement.
#'
#' @param f A function that takes a single numeric input and returns 0 or 1.
#' @param lower Numeric. Lower bound of the search interval.
#' @param upper Numeric. Upper bound of the search interval.
#' @param n_initial Integer. Number of points in the initial coarse grid search
#'   used to locate approximate boundary positions. Higher values improve
#'   detection of closely-spaced boundaries but increase computation time.
#'   Default is 100.
#' @param refine_tol Numeric. Tolerance for binary search refinement. The
#'   algorithm refines each boundary until the interval width is smaller than
#'   this value. Smaller values give higher precision but require more function
#'   evaluations. Default is 1e-12.
#' @param max_iter Integer. Maximum number of iterations for binary search
#'   refinement per boundary. Prevents infinite loops if tolerance cannot be
#'   achieved. Default is 1000.
#' @param adaptive Logical. If TRUE, performs additional refinement in regions
#'   where boundaries are detected to be closely spaced (within 1/1000 of the
#'   total range). This helps resolve boundaries that may be missed by the
#'   initial coarse grid. Default is TRUE.
#'
#' @return Numeric vector of boundary locations where the function transitions
#'   from 0 to 1 or 1 to 0, sorted in ascending order. Returns `numeric(0)` if
#'   no boundaries are found.
#'
#' @examples
#' # Simple step function
#' f <- function(x) as.numeric(x > 0.5)
#' find_boundaries(f, 0, 1)
#'
#' # Multiple boundaries
#' f <- function(x) as.numeric((x > 0.2 & x < 0.4) | (x > 0.6 & x < 0.8))
#' find_boundaries(f, 0, 1, n_initial = 200)
#' @noRd
find_boundaries <- function(
        f, lower, upper,
        n_initial = 100,
        refine_tol = 1e-12,
        max_iter = 1000,
        adaptive = TRUE) {

    # Cache for function evaluations
    cache <- new.env(hash = TRUE)
    call_count <- 0

    # Cached wrapper
    f_cached <- function(x) {
        key <- sprintf("%.15f", x)  # More precise key
        if (exists(key, envir = cache)) {
            return(get(key, envir = cache))
        }
        call_count <<- call_count + 1
        val <- f(x)
        assign(key, val, envir = cache)
        return(val)
    }

    # Stage 1: Initial grid
    x_values <- seq(lower, upper, length.out = n_initial)
    results <- sapply(x_values, f_cached)
    transitions <- which(diff(results) != 0)

    # No boundaries found
    if (length(transitions) == 0) return(numeric(0))

    # Stage 2: Binary search refinement
    boundaries <- vapply(transitions, function(idx) {
        binary_search_boundary(
            f_cached,
            x_values[idx],
            x_values[idx + 1],
            results[idx],
            results[idx + 1],
            tol = refine_tol,
            max_iter = max_iter
        )
    }, FUN.VALUE = numeric(1))

    # Stage 3: Adaptive refinement if needed
    if (adaptive && length(boundaries) > 1) {
        boundaries <- refine_close_boundaries(
            f_cached, boundaries, lower, upper, refine_tol, n_initial
        )
    }
    return(boundaries)
}

# Binary search with cached function
binary_search_boundary <- function(f_cached,
                                   lower, upper,
                                   f_lower, f_upper,
                                   tol = 1e-12, max_iter = 100) {

    iter <- 0
    while ((upper - lower) > tol && iter < max_iter) {
        mid <- (lower + upper) / 2
        f_mid <- f_cached(mid)

        if (f_mid == f_lower) {
            lower <- mid
            f_lower <- f_mid
        } else {
            upper <- mid
            f_upper <- f_mid
        }
        iter <- iter + 1
    }

    return((lower + upper) / 2)
}

# Adaptive refinement for closely-spaced boundaries
refine_close_boundaries <- function(f_cached, boundaries, lower, upper,
                                           tol, n_initial) {

    sorted_b <- sort(boundaries)
    min_spacing <- (upper - lower) / (n_initial * 2)

    # Find regions with closely spaced boundaries
    if (length(sorted_b) > 1) {
        spacing <- diff(sorted_b)
        close_pairs <- which(spacing < min_spacing)

        if (length(close_pairs) > 0) {
            # Process each region with close boundaries
            for (idx in close_pairs) {
                region_lower <- max(lower, sorted_b[idx] - min_spacing * 2)
                region_upper <- min(upper, sorted_b[idx + 1] + min_spacing * 2)

                # Finer grid in this region
                n_fine <- min(200, n_initial * 3)
                x_fine <- seq(region_lower, region_upper, length.out = n_fine)
                results_fine <- sapply(x_fine, f_cached)
                transitions_fine <- which(diff(results_fine) != 0)

                if (length(transitions_fine) > 0) {
                    # Refine boundaries in this region
                    new_boundaries <- vapply(transitions_fine, function(i) {
                        binary_search_boundary(
                            f_cached,
                            x_fine[i],
                            x_fine[i + 1],
                            results_fine[i],
                            results_fine[i + 1],
                            tol = tol,
                            max_iter = 100
                        )
                    }, FUN.VALUE = numeric(1))

                    # Replace old boundaries with refined ones
                    boundaries <- c(
                        boundaries[boundaries < region_lower | boundaries > region_upper],
                        new_boundaries
                    )
                }
            }
        }
    }
    return(sort(unique(round(boundaries, digits = -log10(tol)))))
}
