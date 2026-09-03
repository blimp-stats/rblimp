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
#' If `at()` is the only term after `|`, the moderators it names are also the
#' ones plotted, so `y ~ x | at(m = c("-1 SD", "+1 SD"))` shows just those two
#' values of `m` without having to name `m` twice.
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
#' # Show only some of a single moderator's values (no bare name needed)
#' simple_plot(y ~ x | at(m1 = c("Q25", "Q50", "Q75")), model)
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

#' Round a numeric range outward to the nearest "nice" step.
#'
#' Used by `simple_plot()`, `jn_plot()`, and `jn_map()` to convert a
#' trimmed (e.g. 1%/99% quantile) data range into tidy axis endpoints
#' without `pretty()`-style overshoot (`pretty(c(-4.6, 5.2))` returns
#' `c(-6, -4, ..., 6)`; this helper returns `c(-5, 6)`).
#'
#' Step = `10 ^ floor(log10(span / 2))`, so for a span of ~2-20 we round
#' to whole numbers; for ~0.2-2 we round to 0.1; for ~20-200 we round to
#' 10; etc. The low end is rounded down, the high end up. Dividing by 2
#' shifts the boundary so a span of 12 still rounds to integers instead
#' of jumping to multiples of 10.
#' @noRd
nice_outward <- function(r) {
    if (length(r) != 2 || any(!is.finite(r))) return(r)
    span <- diff(r)
    if (span <= 0) return(r)
    step <- 10 ^ floor(log10(span / 2))
    c(floor(r[1] / step) * step,
      ceiling(r[2] / step) * step)
}

#' Numeric sort key for a single SIMPLE moderator value label.
#'
#' Maps `"Q25"` -> `0.25`, `"+1 SD"` / `"-1 SD"` -> `+1` / `-1`,
#' `"Mean"` -> `0`, `"Mean +/- k SD"` -> `+/-k`, a plain number string ->
#' its numeric value, otherwise `NA_real_`. Used to lay out factor levels
#' in their natural numeric order rather than the order Blimp happened to
#' emit them in.
#' @noRd
mod_value_sort_key <- function(label) {
    if (is.na(label) || !nzchar(label)) return(NA_real_)
    lab <- trimws(label)
    # Expression values (`(...)` or `(...) sd`) get NA -- sort to end.
    if (grepl("^\\(.*\\)(?:\\s+sd)?$", lab, perl = TRUE)) return(NA_real_)
    if (grepl("^Q[0-9.]+$", lab))
        return(as.numeric(sub("^Q", "", lab)) / 100)
    mean_re <- "^Mean\\s*(?:([+-])\\s*([0-9.]+)\\s*SD)?$"
    if (grepl(mean_re, lab, perl = TRUE)) {
        parts <- regmatches(lab, regexec(mean_re, lab, perl = TRUE))[[1]]
        if (nzchar(parts[2]) && nzchar(parts[3]))
            return(as.numeric(paste0(parts[2], parts[3])))
        return(0)
    }
    if (grepl("\\s*SD\\s*$", lab)) {
        n_sd <- suppressWarnings(as.numeric(sub("\\s*SD\\s*$", "", lab)))
        if (!is.na(n_sd)) return(n_sd)
    }
    n <- suppressWarnings(as.numeric(lab))
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

    # The standard `outcome ~ focal | moderator` form always has a top-level `|`
    # on the RHS. Its absence marks the compound-parameter (PARAM) form, where
    # the focal is a parameter expression and the whole RHS names the
    # moderator(s). The focal label may be written unquoted (`b4 + b7*mod ~ mod`)
    # or quoted (`"b4 + b7*mod" ~ mod`); `deparse` recovers it either way, and
    # matching is whitespace/case-insensitive downstream.
    rhs_has_pipe <- is.call(rhs) && identical(rhs[[1]], as.name("|")) &&
        length(rhs) == 3

    if (!rhs_has_pipe) {
        label <- if (is.character(out_lang)) out_lang
                 else paste(deparse(out_lang), collapse = " ")
        parsed <- parse_mod_terms(rhs)
        return(list(
            is_param       = TRUE,
            outcome        = NA_character_,
            focal          = trimws(label),
            bare_mods      = parsed$bare_mods,
            mod_components = parsed$mod_components,
            at_filter      = parsed$at_filter
        ))
    }

    focal_lang <- rhs[[2]]
    mod_lang   <- rhs[[3]]

    parsed <- parse_mod_terms(mod_lang)
    list(
        is_param       = FALSE,
        outcome        = as.character(out_lang),
        focal          = as.character(focal_lang),
        bare_mods      = parsed$bare_mods,
        mod_components = parsed$mod_components,
        at_filter      = parsed$at_filter
    )
}

#' Walk the moderator side of a plot formula (the RHS of `|`, or the whole RHS
#' for a PARAM formula), collecting bare moderators, `join(...)` compounds, and
#' `at(...)` pins. Shared by both `parse_plot_formula` branches.
#' @noRd
parse_mod_terms <- function(mod_lang) {
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
                "Unrecognized moderator term: {.code {deparse(t)}}",
                i = "Allowed: bare moderator names, {.fn at} calls, and {.fn join} calls."
            ))
        }
    }

    # `at()` only ever filters values. When it is the whole moderator side,
    # there is no bare name saying which moderator to plot, so take the
    # filtered ones: `focal | at(m = c(...))` reads as `focal | m + at(m = c(...))`.
    if (length(bare_mods) == 0 && length(at_filter) > 0) {
        bare_mods <- names(at_filter)
        for (nm in bare_mods) mod_components[[nm]] <- nm
    }

    list(
        bare_mods      = bare_mods,
        mod_components = mod_components,
        at_filter      = at_filter
    )
}

#' Parse the "<m1> @ <v1>{ <continuation> }*" tail of a SIMPLE effect label
#' into aligned `mods` / `vals` character vectors.
#'
#' Continuations are separated from the previous clause by either `", "`
#' (Blimp's soft-wrap, emitted when the label exceeds ~29 chars) or plain
#' whitespace. Values may be multi-token (`"Mean + 1 SD"`, etc.).
#' @noRd
parse_effect_clauses <- function(rest) {
    # Boundary between two "<mod> @ <value>" clauses: either ", " or plain
    # whitespace, with a zero-width lookahead for the next "<name> @ " token.
    boundary_re <- "(?:,\\s+|\\s+)(?=[^\\s,@]+\\s+@\\s+)"
    pair_re     <- "^\\s*([^\\s,@]+)\\s+@\\s+(.*?)\\s*$"

    clauses <- strsplit(rest, boundary_re, perl = TRUE)[[1]]
    if (length(clauses) == 0)
        return(list(mods = character(0), vals = character(0)))

    mods <- character(length(clauses))
    vals <- character(length(clauses))
    for (i in seq_along(clauses)) {
        p <- regmatches(clauses[i], regexec(pair_re, clauses[i], perl = TRUE))[[1]]
        if (length(p) == 3) {
            mods[i] <- p[2]
            vals[i] <- p[3]
        }
    }

    keep <- nzchar(mods)
    list(mods = mods[keep], vals = vals[keep])
}

#' Parse a vector of Blimp SIMPLE column names into structured pieces.
#'
#' Per the SIMPLE-output spec, each column name (with the `INTER:`/`SLOPE:`
#' KIND prefix already stripped) has the form
#'   `"<outcome> ~ <focal> | <m1> @ <v1>{ <continuation> }*"`
#' where each continuation is `"<m> @ <v>"` separated from the previous clause
#' by either `", "` (Blimp's soft-wrap, emitted when the label exceeds ~29
#' chars) or plain whitespace. `<outcome>` may itself contain spaces; we split
#' on the FIRST `" ~ "`. Values may be multi-token (`"Mean + 1 SD"`, etc.).
#'
#' Returns a list with one entry per input column, each a list with fields
#' `outcome`, `predictor`, `mods` (character vector), `vals` (character vector
#' aligned with `mods`).
#' @noRd
parse_simple_colnames <- function(col_names) {

    blank_result <- function(outcome = character(0), focal = character(0)) {
        list(outcome = outcome, predictor = focal,
             mods = character(0), vals = character(0))
    }

    lapply(col_names, function(col_name) {
        tilde_idx <- regexpr(" ~ ", col_name, fixed = TRUE)
        if (tilde_idx < 0) return(blank_result())
        outcome <- substr(col_name, 1, tilde_idx - 1)
        effect  <- substr(col_name, tilde_idx + 3L, nchar(col_name))

        # Effect_label is "<focal> | <mod> @ <value>{ <continuation> }*";
        # split off the focal at the FIRST " | ".
        pipe_idx <- regexpr(" | ", effect, fixed = TRUE)
        if (pipe_idx < 0) return(blank_result(outcome))
        focal <- trimws(substr(effect, 1, pipe_idx - 1))
        rest  <- substr(effect, pipe_idx + 3L, nchar(effect))

        cl <- parse_effect_clauses(rest)
        list(outcome = outcome, predictor = focal,
             mods = cl$mods, vals = cl$vals)
    })
}

#' Parse a vector of Blimp SIMPLE `PARAM:` column names into structured pieces.
#'
#' Compound-parameter conditional effects are saved with a different header
#' shape than `INTER:`/`SLOPE:`: there is no `<outcome> ~` prefix. With the
#' `PARAM: ` KIND prefix already stripped, each name has the form
#'   `"<label> | <m1> @ <v1>{ <continuation> }*"`
#' where `<label>` is the focal expression the user wrote in SIMPLE (e.g.
#' `"b4 + b7*mod"`). Returns a list with one entry per column, each a list
#' with fields `label`, `mods` (character vector), and `vals` (aligned with
#' `mods`).
#' @noRd
parse_param_colnames <- function(col_names) {
    lapply(col_names, function(col_name) {
        pipe_idx <- regexpr(" | ", col_name, fixed = TRUE)
        if (pipe_idx < 0)
            return(list(label = trimws(col_name),
                        mods = character(0), vals = character(0)))
        label <- trimws(substr(col_name, 1, pipe_idx - 1))
        rest  <- substr(col_name, pipe_idx + 3L, nchar(col_name))
        cl <- parse_effect_clauses(rest)
        list(label = label, mods = cl$mods, vals = cl$vals)
    })
}

#' Normalize a compound-parameter focal label for matching: drop all
#' whitespace and lowercase, so `"b4 + b7*mod"` and `"b4+b7*mod"` compare equal.
#' @noRd
normalize_param_label <- function(x) {
    tolower(gsub("\\s+", "", x))
}

#' Convert a SIMPLE moderator value label to a numeric point on the
#' moderator axis (relative to `mu`).
#'
#' Per the SIMPLE-output spec value table, recognised forms (precedence-
#' ordered so later forms cannot collide with earlier ones):
#'   1. `"(expr)"`, `"(expr) sd"`  -- runtime expression of model parameters;
#'                                    not yet evaluated. Returns `NA` with a
#'                                    one-time warning.
#'   2. `"Q<NN>"`                  -- empirical quantile of `mod_data`.
#'   3. `"Mean"`, `"Mean +/- k SD"`-- mean (+/- k * sd) of `mod_data`.
#'   4. `"+k SD"`, `"-k SD"`       -- centered SD offset (equivalent to the
#'                                    Mean +/- k form above).
#'   5. Numeric literal            -- taken at face value.
#'   6. Parameter name             -- posterior mean across `iterations`.
#'
#' `mod_data` may be `NULL` when the moderator is not present in
#' `model@average_imp`; data-dependent branches then return `NA_real_`.
#' @noRd
mod_label_to_numeric <- function(label, mod_data, iterations, mu = 0) {
    lab <- trimws(label)
    if (grepl("^\\(.*\\)(?:\\s+sd)?$", lab, perl = TRUE)) {
        warn_expr_simple_value(lab)
        return(NA_real_)
    }
    if (grepl("^Q[0-9.]+$", lab)) {
        if (is.null(mod_data)) return(NA_real_)
        q <- as.numeric(sub("^Q", "", lab)) / 100
        return(unname(quantile(mod_data, q, na.rm = TRUE)) - mu)
    }
    mean_re <- "^Mean\\s*(?:([+-])\\s*([0-9.]+)\\s*SD)?$"
    if (grepl(mean_re, lab, perl = TRUE)) {
        if (is.null(mod_data)) return(NA_real_)
        parts <- regmatches(lab, regexec(mean_re, lab, perl = TRUE))[[1]]
        k <- if (nzchar(parts[2]) && nzchar(parts[3])) {
            as.numeric(paste0(parts[2], parts[3]))
        } else 0
        return(mean(mod_data, na.rm = TRUE) +
               k * sd(mod_data, na.rm = TRUE) - mu)
    }
    if (grepl("\\s*SD\\s*$", lab)) {
        n_sd <- suppressWarnings(as.numeric(sub("\\s*SD\\s*$", "", lab)))
        if (!is.na(n_sd)) {
            if (is.null(mod_data)) return(NA_real_)
            # Blimp evaluates `+/-k SD` at `mean(data) + k * sd(data)` in the
            # raw metric. For centered moderators `mu == mean(data)`, so the
            # offset cancels and we get `k * sd(data)` (the previous behavior).
            # For uncentered moderators `mu == 0`, this returns the raw
            # `mean(data) + k * sd(data)` that Blimp actually used.
            return(n_sd * sd(mod_data, na.rm = TRUE) +
                   (mean(mod_data, na.rm = TRUE) - mu))
        }
    }
    # Numeric literals and parameter-name values are already expressed in the
    # moderator's *model* metric (centered when the moderator is centered, raw
    # otherwise) -- the same metric as the plot axis -- so they are taken
    # as-is. This differs from the `Q..`/`SD` branches above, which rebuild a
    # raw value from `mod_data` and therefore subtract `mu`. (Subtracting `mu`
    # here double-centered them, e.g. Blimp's "@ 0" landed at `-mean(data)`.)
    nval <- suppressWarnings(as.numeric(lab))
    if (!is.na(nval)) return(nval)
    ind <- param_column_index(lab, iterations)
    if (length(ind) == 1) return(mean(iterations[[ind]]))
    NA_real_
}

#' Locate the `iterations` column holding a named model parameter.
#'
#' Parameters declared in Blimp's `PARAMETERS:` block are stored with a
#' `"Parameter:."` tag in `model@iterations` (and `"Parameter: "` in the
#' estimates row names), so a bare SIMPLE value label such as `"w_low"` never
#' matches the stored column name directly. Try the name as-is first, then
#' again with that tag stripped.
#'
#' @return The single matching column index, or `integer(0)` when the name is
#'   unknown or ambiguous.
#' @noRd
param_column_index <- function(label, iterations) {
    if (NROW(iterations) == 0) return(integer(0))
    cn <- colnames(iterations)
    if (is.null(cn)) return(integer(0))
    lab <- tolower(trimws(label))
    ind <- which(tolower(cn) == lab)
    if (length(ind) == 1) return(ind)
    bare <- trimws(sub("^parameter:[.[:space:]]*", "", tolower(cn)))
    ind <- which(bare == lab)
    if (length(ind) == 1) return(ind)
    integer(0)
}

#' One-time warning for expression-valued SIMPLE points.
#' @noRd
warn_expr_simple_value <- local({
    seen <- character(0)
    function(label) {
        if (label %in% seen) return(invisible())
        seen <<- c(seen, label)
        cli::cli_alert_warning(c(
            "Expression-valued SIMPLE points (e.g. {.code {label}}) cannot ",
            "yet be placed numerically on a JN axis; affected rows will be ",
            "dropped from the fit."
        ))
    }
})

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
