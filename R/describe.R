# mcelreath  version of precis. Renamed to be described: see https://github.com/rmcelreath/rethinking

#' Format a data frame for printing
#' @description An internal helper function that rounds the numeric columns of a
#' data frame based on a named list of digits.
#' @param x A data frame or a list that can be coerced into a data frame.
#' @param digits A named numeric vector where names correspond to column names
#' in `x` and values specify the number of decimal places. A special name
#' `"default__"` can set a default rounding for all other numeric columns.
#' @return A formatted data frame with numeric columns rounded.
#' @noRd
format_show <- function(x, digits) {
    r <- as.data.frame(lapply(1:length(x), function(i) {
        if (!is.character(x[[i]])) {
            if (names(x)[i] %in% names(digits)) {
                round(x[[i]], digits[names(x)[i]])
            } else {
                round(x[[i]], digits["default__"])
            }
        } else {
            return(x[[i]])
        }
    }))
    names(r) <- names(x)
    rownames(r) <- rownames(x)
    return(r)
}

#' Concatenate strings
#' @description A lightweight wrapper for `paste(..., collapse = "", sep = "")`.
#' @param ... Objects to be converted to character vectors and concatenated.
#' @return A single character string.
#' @noRd
concat <- function(...) {
    paste(..., collapse = "", sep = "")
}

#' Compute standard errors from a model object
#' @description Extracts the standard errors by taking the square root of the
#' diagonal elements of a model's variance-covariance matrix.
#' @param model A fitted model object that has a `vcov` method.
#' @return A numeric vector of standard errors.
#' @noRd
se <- function(model) {
    sqrt(diag(vcov(model)))
}


#' Wickham's Histospark
#' @details
#' <https://github.com/hadley/precis/blob/master/R/histospark.R>
#' Copyright Hadley Wickham 2016
#' Released under GPL-3
#' @param x A numeric vector.
#' @param width The width of the sparkline in characters.
#' @return A character string representing the histogram.
#' @noRd
sparks <- c("\u2581", "\u2582", "\u2583", "\u2585", "\u2587")
histospark <- function(x, width = 10) {
    if (all(is.na(x))) {
        return(paste0(rep(" ", width), collapse = ""))
    }
    bins <- graphics::hist(x, breaks = width, plot = FALSE)
    factor <- cut(
        bins$counts / max(bins$counts),
        breaks = seq(0, 1, length = length(sparks) + 1),
        labels = sparks,
        include.lowest = TRUE
    )
    paste0(factor, collapse = "")
}

#' The 'describe' class for object summaries
#'
#' @description An S4 class that holds a summary data frame and formatting
#' information. It extends the `data.frame` class and is the return type for
#' the [describe()] function.
#'
#' @slot digits A numeric value indicating the number of digits to display.
#' @name describe-class
#' @rdname describe-class
#' @export
setClass("describe", slots = c(digits = "numeric"), contains = "data.frame")

#' Show method for 'describe' objects
#' @description Internal implementation for the `show` method.
#' @param object A `describe` class object.
#' @noRd
describe_show <- function(object) {
    # print( round( object@output , object@digits ) )
    r <- format_show(object, digits = c("default__" = object@digits, "n_eff" = 0))
    has_header <- !is.null(attr(object, "header"))
    if (has_header) {
        # show header
        cat(attr(object, "header"))
        cat("\n")
    }
    print(r)
}

#' @rdname describe-class
#' @param object An object of class `describe`.
#' @export
setMethod("show", "describe", function(object) describe_show(object))

#' Describe an object
#'
#' @description `describe` provides a flexible summary of various R objects,
#' such as data frames, lists of samples, or statistical models. It is an
#' alternative to `summary` and is inspired by `precis` from the `rethinking` package.
#'
#' @param object The object to describe.
#' @param depth An integer (`1`, `2`, or `3`) that controls the display of vector
#' and matrix parameters. `depth=1` (default) shows only scalar parameters.
#' `depth=2` shows scalars and vectors. `depth=3` shows all parameters.
#' @param pars An optional character vector of parameter names to include in the summary.
#' @param prob The probability mass for the credible interval (e.g., quantile interval).
#' @param digits The number of decimal places to display in the output.
#' @param sort An optional character string specifying a column name to sort the results by.
#' @param decreasing Logical. If `TRUE`, sorting is in decreasing order.
#' @param ... Additional arguments passed to specific methods.
#'
#' @return An object of class [describe-class], which is a data frame containing
#' summary statistics and attributes for printing.
#' @name describe
#' @aliases describe-methods
#' @export
setGeneric(
    "describe",
    function(object, depth = 1, pars, prob = 0.95, digits = 2, sort = NULL, decreasing = FALSE, ...) {
        new("describe", as.data.frame(object), digits = digits)
    }
)

#' Format describe output
#' @description An internal helper function to handle depth filtering and
#' sorting of the summary data frame produced by `describe` methods.
#' @param result The data frame of summary statistics.
#' @param depth Integer (`1`, `2`, or `3`) to control parameter display depth.
#' @param sort Column name to sort by, or `NULL`.
#' @param decreasing Logical sort order.
#' @return A formatted data frame.
#' @noRd
describe_format <- function(result, depth, sort, decreasing) {
    # deal with depth
    if (depth == 1) {
        hits <- regexpr("]", rownames(result), fixed = TRUE)
        hits_idx <- which(hits > -1)
        if (length(hits_idx) > 0) {
            result <- result[-hits_idx, ]
            message(paste(length(hits_idx), "vector or matrix parameters hidden. Use depth=2 to show them."))
        }
    }
    if (depth == 2) {
        hits <- regexpr(",", rownames(result), fixed = TRUE)
        hits_idx <- which(hits > -1)
        if (length(hits_idx) > 0) {
            result <- result[-hits_idx, ]
            message(paste(length(hits_idx), "matrix parameters hidden. Use depth=3 to show them."))
        }
    }

    # sort
    if (!is.null(sort)) {
        o <- order(result[, sort], decreasing = decreasing)
        result <- result[o, ]
    }

    # label Rhat with version
    rhat_col <- which(colnames(result) == "Rhat")
    if (!is.null(rhat_col)) {
        colnames(result)[rhat_col] <- "Rhat4"
    }

    return(result)
}

#' @rdname describe
#' @export
setMethod(
    "describe", "numeric",
    function(object, depth = 1, pars, prob = 0.95, digits = 2, sort = NULL, decreasing = FALSE, ...) {
        oname <- deparse(match.call()[[2]])
        df <- list()
        df[[oname]] <- object
        describe(df, prob = prob, ...)
    }
)

#' @rdname describe
#' @param hist Logical. If `TRUE` (and on a Unix-like OS), a unicode histogram
#' (`histospark`) is included in the output. Defaults to `TRUE`.
#' @export
setMethod(
    "describe", "data.frame",
    function(object, depth = 1, pars, prob = 0.50, digits = 2, sort = NULL, decreasing = FALSE, hist = TRUE, ...) {
        plo <- (1 - prob) / 2
        phi <- 1 - plo
        # replace any character or factor columns with NA numeric columns
        # histospark will detect all NA and return blank line
        for (i in 1:ncol(object)) {
            if (class(object[[i]]) %in% c("factor", "character")) {
                object[[i]] <- as.numeric(rep(NA, nrow(object)))
            }
        }
        if (hist == TRUE && (.Platform$OS.type == "unix")) {
            result <- data.frame(
                mean = apply(object, 2, mean, na.rm = TRUE),
                median = apply(object, 2, median, na.rm = TRUE),
                sd = apply(object, 2, sd, na.rm = TRUE),
                lo = apply(object, 2, quantile, na.rm = TRUE, probs = plo),
                hi = apply(object, 2, quantile, na.rm = TRUE, probs = phi),
                histogram = apply(object, 2, histospark),
                stringsAsFactors = FALSE
            )
        } else {
            # no unicode histogram
            result <- data.frame(
                mean = apply(object, 2, mean, na.rm = TRUE),
                median = apply(object, 2, median, na.rm = TRUE),
                sd = apply(object, 2, sd, na.rm = TRUE),
                lo = apply(object, 2, quantile, na.rm = TRUE, probs = plo),
                hi = apply(object, 2, quantile, na.rm = TRUE, probs = phi),
                stringsAsFactors = FALSE
            )
        }

        colnames(result)[4:5] <- paste(c(plo, phi) * 100, "%", sep = "")

        result <- describe_format(result, depth, sort, decreasing)

        has_source <- !is.null(attr(object, "source"))
        header_string <- concat("'data.frame': ", nrow(object), " obs. of ", ncol(object), " variables:")
        if (has_source) {
            header_string <- attr(object, "source")
        }
        attr(result, "header") <- header_string

        return(new("describe", result, digits = digits))
    }
)

#' @rdname describe
#' @export
setMethod(
    "describe", "list",
    function(object, depth = 1, pars, prob = 0.95, digits = 2, sort = NULL, decreasing = FALSE, hist = TRUE, ...) {
        # coerce to data frame and format row names for vectors/matrices to [] style
        result <- as.data.frame(object, stringsAsFactors = FALSE)
        # since data frame conversion vectorizes matrices, need to treat each variable
        for (i in 1:length(object)) {
            # check dimension and process names when > 1
            n <- length(dim(object[[i]]))
            if (n > 1) {
                dims <- dim(object[[i]])
                idx <- grep(concat("^", names(object)[i], "."), names(result))
                if (n == 2) {
                    # vector
                    new_names <- paste(names(object)[i], "[", 1:dims[2], "]", sep = "")
                    names(result)[idx] <- new_names
                } # 2
                if (n == 3) {
                    # matrix
                    new_names <- paste(names(object)[i], "[", rep(1:dims[2], each = dims[3]), ",", rep(1:dims[3], times = dims[2]), "]", sep = "")
                    names(result)[idx] <- new_names
                } # 3
            } # n>1
        } # i
        # hand off to data frame method
        if (!is.null(attr(object, "source"))) {
            attr(result, "source") <- attr(object, "source")
        }
        describe(result, depth, pars, prob, digits, sort, decreasing, hist = hist, ...)
    }
)

#' @rdname describe
#' @export
setMethod(
    "describe", "blimp_obj",
    function(object, depth = 1, pars, prob = 0.95, digits = 2, sort = NULL, decreasing = FALSE, hist = TRUE, ...) {
        tmp <- object@iterations
        names(tmp) <- rownames(object@estimates)
        plo <- (1 - prob) / 2
        phi <- 1 - plo
        # replace any character or factor columns with NA numeric columns
        # histospark will detect all NA and return blank line
        for (i in 1:ncol(tmp)) {
            if (class(tmp[[i]]) %in% c("factor", "character")) {
                tmp[[i]] <- as.numeric(rep(NA, nrow(tmp)))
            }
        }
        if (hist == TRUE) {
            result <- data.frame(
                mean = apply(tmp, 2, mean, na.rm = TRUE),
                median = apply(tmp, 2, median, na.rm = TRUE),
                sd = apply(tmp, 2, sd, na.rm = TRUE),
                lo = apply(tmp, 2, quantile, na.rm = TRUE, probs = plo),
                hi = apply(tmp, 2, quantile, na.rm = TRUE, probs = phi),
                histogram = apply(tmp, 2, histospark),
                stringsAsFactors = FALSE
            )
        } else {
            # no unicode histogram
            result <- data.frame(
                mean = apply(tmp, 2, mean, na.rm = TRUE),
                median = apply(tmp, 2, median, na.rm = TRUE),
                sd = apply(tmp, 2, sd, na.rm = TRUE),
                lo = apply(tmp, 2, quantile, na.rm = TRUE, probs = plo),
                hi = apply(tmp, 2, quantile, na.rm = TRUE, probs = phi),
                stringsAsFactors = FALSE
            )
        }

        colnames(result)[4:5] <- paste(c(plo, phi) * 100, "%", sep = "")

        result <- describe_format(result, depth, sort, decreasing)

        has_source <- !is.null(attr(tmp, "source"))
        header_string <- concat("'blimp_obj': ", ncol(tmp), " parameters summarized over ", nrow(tmp), " iterations:")
        if (has_source) {
            header_string <- attr(tmp, "source")
        }
        attr(result, "header") <- header_string

        return(new("describe", result, digits = digits))
    }
)
