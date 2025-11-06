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
