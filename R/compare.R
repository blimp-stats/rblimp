## Compares Models
# Copyright Brian Keller 2022, all rights reserved


#' S4 class for Blimp model comparison results
#'
#' @description
#' Result object from comparing two Blimp models.
#'
#' @slot table Matrix of comparison results
#' @slot model0 The reference blimp_obj model
#' @slot model The comparison blimp_obj model
#' @slot greaterThan Logical indicating direction of comparison
#'
#' @export
setClass("blimp_cp", slots = list(table = "matrix", model0 = "blimp_obj", model = "blimp_obj", greaterThan = "logical"))

#' Show method for blimp_cp
#' @param object A `blimp_cp` object
#' @return No return value, called for the side effect of printing the
#'   comparison table to the console.
#' @export
setMethod(
    "show", "blimp_cp",
    function(object) {
        if (object@greaterThan) {
            cat("Proportion of parameter greater than:\n")
        } else {
            cat("Proportion of parameter less than:\n")
        }
        show(object@table)
    }
)

# Get Value
get_value <- function(x, use) {
    # Parse character
    if (is.character(use)) {
        if (use == "mean") {
            return(mean(x))
        } else if (use == "median") {
            return(median(x))
        } else {
            use <- suppressWarnings(as.numeric(use))
            if (is.na(use)) throw_error("{.arg use} is not a numeric value or 'mean' / 'median'")
        }
    }
    # Return Function
    else if (is.function(use)) {
        return(use(x))
    }
    # Return value
    if (use < 1) {
        return(quantile(x, probs = use))
    } else {
        return(quantile(x, probs = use / 100))
    }
}

# Wrapper to compare
model_compare_wrapper <- function(suffix, model, model0, use, greaterThan) {
    # Get names
    mname0 <- names(model0@iterations)
    mnames <- names(model@iterations)[endsWith(rownames(model@estimates), suffix)]
    row_names <- rownames(model@estimates)[endsWith(rownames(model@estimates), suffix)]
    matches <- mnames %in% mname0
    # Return output
    output <- list()
    count <- 1
    for (i in seq_along(mnames)) {
        if (matches[i]) {
            value <- get_value(model0@iterations[, mnames[i]], use)
            if (greaterThan) {
                output[[count]] <- c(mean(model@iterations[, mnames[i]] > value))
            } else {
                output[[count]] <- c(mean(model@iterations[, mnames[i]] < value))
            }
            names(output[[count]]) <- row_names[i]
            count <- count + 1
        }
    }
    return(output)
}

#' Compare two Blimp models
#'
#' @description
#' Compares two Bayesian models by calculating the proportion of posterior samples
#' where the comparison model's parameters exceed (or fall below) the reference model's
#' summary statistic. This is useful for model comparison and assessing incremental
#' variance explained (e.g., R-squared differences).
#'
#' @param model0 A `blimp_obj`. The baseline or simpler model used as the reference point.
#' @param model A `blimp_obj`. The comparison model (typically more complex) to evaluate.
#' @param use Summary statistic to use as the cutpoint from `model0`. Options:
#'   \itemize{
#'     \item Character: `"mean"` or `"median"`
#'     \item Numeric < 1: Quantile proportion (e.g., `0.5` for median)
#'     \item Numeric >= 1: Percentile (e.g., `50` for median)
#'     \item Function: Custom function applied to `model0` iterations
#'     \item List: Multiple summary statistics
#'   }
#' @param greaterThan Logical. If `TRUE` (default), calculates the proportion of `model`
#'   iterations greater than the `model0` cutpoint. If `FALSE`, calculates proportion less than.
#' @param suffixes Character vector of parameter name suffixes to compare. Defaults to
#'   all R-squared values (coefficients, random effects, residual variation).
#'
#' @return A `blimp_cp` object containing a matrix of comparison proportions.
#'
#' @details
#' The comparison works by:
#' \enumerate{
#'   \item Computing a summary statistic (e.g., mean) from `model0`'s posterior samples
#'   \item Calculating what proportion of `model`'s posterior samples exceed this value
#'   \item Reporting this proportion for each parameter matching the specified suffixes
#' }
#'
#' @note Due to R restrictions, lists of functions will not give useful printed names.
#'
#' @examplesIf has_blimp()
#' # Generate data
#' mydata <- rblimp_sim(
#'     c(
#'         'x1 ~ normal(0, 1)',
#'         'x2 ~ normal(0, 1)',
#'         'y ~ normal(10 + 0.5*x1 + 0.3*x2, 1)'
#'     ),
#'     n = 200,
#'     seed = 123
#' )
#'
#' # Fit baseline model (x1 only)
#' model0 <- rblimp(
#'     'y ~ x1',
#'     mydata,
#'     seed = 123,
#'     burn = 1000,
#'     iter = 1000
#' )
#'
#' # Fit comparison model (x1 + x2)
#' model1 <- rblimp(
#'     'y ~ x1 x2',
#'     mydata,
#'     seed = 123,
#'     burn = 1000,
#'     iter = 1000
#' )
#'
#' # Compare models - proportion of model1 R-squared > mean(model0 R-squared)
#' compare(model0, model1)
#'
#' @export
compare <- function(model0, model, use = "mean", greaterThan = TRUE, suffixes =
                 c(
                     "R2: Coefficients", "R2: Level-2 Random Intercepts",
                     "R2: Level-2 Random Slopes", "R2: Level-3 Random Slopes",
                     "R2: Level-3 Random Intercepts", "R2: Residual Variation",
                     "R2: Level-1 Residual Variation"
                 )) {

        # Check inputs
        if (!inherits(model0, "blimp_obj")) {
            throw_error("{.arg model0} must be a {.cls blimp_obj}")
        }
        if (!inherits(model, "blimp_obj")) {
            throw_error("{.arg model} must be a {.cls blimp_obj}")
        }
        # Loop over multiple uses if needed
        old_use_function_name <- deparse(substitute(use))
        use <- c(use)
        for (i in seq_along(use)) {
            tmp <- unlist(lapply(suffixes, model_compare_wrapper, model0 = model0, model = model, use = use[[i]], greaterThan = greaterThan))

            mat <- matrix(tmp, nrow = length(tmp))
            row.names(mat) <- names(tmp)

            # Get the column name
            tmp_use <- use[[i]]
            if (is.character(tmp_use)) {
                use_char <- tmp_use
            } else {
                use_char <- deparse(substitute(tmp_use))
                # Work around functions and edge cases
                if (length(use_char) > 1) {
                    use_char <- use_char[2]
                    if (length(use) == 1) {
                        use_char <- old_use_function_name
                    }
                }
            }
            if (use_char == "mean") {
                colnames(mat) <- "mean"
            } else if (use_char == "median") {
                colnames(mat) <- "median"
            } else {
                val <- suppressWarnings(as.numeric(use_char))
                if (is.na(val)) {
                    colnames(mat) <- use_char
                } else {
                    if (val < 1) {
                        colnames(mat) <- paste0(val * 100, "%")
                    } else {
                        colnames(mat) <- paste0(val, "%")
                    }
                }
            }
            if (i == 1) {
                output <- mat
            } else {
                output <- cbind(output, mat)
            }
        }
        # Convert to object
        output <- new("blimp_cp",
            table = output,
            model0 = model0,
            model = model,
            greaterThan = greaterThan
        )
        return(output)
}
