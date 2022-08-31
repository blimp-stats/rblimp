## Compares Models
# Not a true qq plot, but easier to understand
# Copyright Brian Keller 2022, all rights reserved
#' @export
setGeneric("compare", function(model0, model, use = "mean", greaterThan = TRUE, suffixes =
                                   c(
                                       "R2: Fixed Effects ", "R2: Level-2 Random Intercepts ",
                                       "R2: Level-2 Random Slopes ", "R2: Level-3 Random Slopes ",
                                       "R2: Level-3 Random Intercepts ", "R2: Residual Variation ",
                                       "R2: Level-1 Residual Variation "
                                   )) {
    stop(paste("Does not work with ", class(model)))
})

# Compare class
#' @export
setClass("blimp_cp", slots = list(table = "matrix", model0 = "blimp_obj", model = "blimp_obj", greaterThan = "logical"))

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
            if (is.na(use)) stop("use is not a numeric value or 'mean' / 'median'")
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

#' @export
setMethod(
    "compare", signature(model = "blimp_obj", model0 = "blimp_obj"),
    function(model0, model, use = "mean", greaterThan = TRUE, suffixes =
                 c(
                     "R2: Fixed Effects ", "R2: Level-2 Random Intercepts ",
                     "R2: Level-2 Random Slopes ", "R2: Level-3 Random Slopes ",
                     "R2: Level-3 Random Intercepts ", "R2: Residual Variation ",
                     "R2: Level-1 Residual Variation "
                 )) {

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
)
