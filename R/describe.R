# mcelreath  version of precis. Renamed to be described: see https://github.com/rmcelreath/rethinking


# format show
format_show <- function(x, digits) {
    r <- as.data.frame(lapply(1:length(x), function(i) {
        if (class(x[[i]]) != "character") {
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

# concat wrapper for paste
concat <- function(...) {
    paste(..., collapse = "", sep = "")
}

# Compute se
se <- function(model) {
    sqrt(diag(vcov(model)))
}


#' Wickham's Histospark
#' @details
#' <https://github.com/hadley/precis/blob/master/R/histospark.R>
#' Copyright Hadley Wickham 2016
#' Released under GPL-3
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
# describe class definition and show method
# setClass( "describe" , representation( output="data.frame" , digits="numeric" ) )
#' @export
setClass("describe", slots = c(digits = "numeric"), contains = "data.frame")

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

#' @export
setMethod("show", "describe", function(object) describe_show(object))

describe_plot <- function(x, y, pars, col.ci = "black", xlab = "Value", add = FALSE, xlim = NULL, labels = rownames(x)[1:n], ...) {
    if (!missing(pars)) {
        x <- x[pars, ]
    }
    n <- nrow(x)
    mu <- x[n:1, 1]
    left <- x[[3]][n:1]
    right <- x[[4]][n:1]
    set_nice_margins()
    labels <- labels[n:1]
    if (is.null(xlim)) xlim <- c(min(left), max(right))
    if (add == FALSE) {
        dotchart(mu, labels = labels, xlab = xlab, xlim = xlim, ...)
    } else {
        points(mu[n:1], n:1, ...)
    }
    for (i in 1:length(mu)) lines(c(left[i], right[i]), c(i, i), lwd = 2, col = col.ci)
    if (add == FALSE) abline(v = 0, lty = 1, col = col.alpha("black", 0.15))
}
#' @export
setMethod("plot", "describe", function(x, y, ...) describe_plot(x, y, ...))

# function to process a list of posterior samples from extract.samples into a summary table
# needed because as.data.frame borks the ordering of matrix parameters like varying effects
postlistdescribe <- function(post, prob = 0.95, spark = FALSE) {
    n_pars <- length(post)
    result <- data.frame(Mean = 0, StdDev = 0, lower = 0, upper = 0)
    if (spark != FALSE) {
        result <- data.frame(Mean = 0, StdDev = 0, Min = 0, Distribution = "", Max = 0)
        result$Distribution <- as.character(result$Distribution)
    }
    r <- 1
    for (k in 1:n_pars) {
        dims <- dim(post[[k]])
        if (length(dims) == 1) {
            # single parameter
            if (spark == FALSE) {
                hpd <- as.numeric(HPDI(post[[k]], prob = prob))
                result[r, ] <- c(mean(post[[k]]), sd(post[[k]]), hpd[1], hpd[2])
            } else {
                # histosparks in place of HPDI
                the_spark <- histospark(post[[k]], width = spark)
                result[r, 1:3] <- c(mean(post[[k]]), sd(post[[k]]), min(post[[k]]))
                result[r, 4] <- the_spark
                result[r, 5] <- max(post[[k]])
            }
            rownames(result)[r] <- names(post)[k]
            r <- r + 1
        }
        if (length(dims) == 2) {
            # vector of parameters
            # loop over
            for (i in 1:dims[2]) {
                if (spark == FALSE) {
                    hpd <- as.numeric(HPDI(post[[k]][, i], prob = prob))
                    result[r, ] <- c(mean(post[[k]][, i]), sd(post[[k]][, i]), hpd[1], hpd[2])
                } else {
                    the_spark <- histospark(post[[k]][, i], width = spark)
                    result[r, 1:3] <- c(mean(post[[k]][, i]), sd(post[[k]][, i]), min(post[[k]][, i]))
                    result[r, 4] <- the_spark
                    result[r, 5] <- max(post[[k]][, i])
                }
                rownames(result)[r] <- concat(names(post)[k], "[", i, "]")
                r <- r + 1
            }
        }
        if (length(dims) == 3) {
            # matrix of parameters
            for (i in 1:dims[2]) {
                for (j in 1:dims[3]) {
                    if (spark == FALSE) {
                        hpd <- as.numeric(HPDI(post[[k]][, i, j], prob = prob))
                        result[r, ] <- c(mean(post[[k]][, i, j]), sd(post[[k]][, i, j]), hpd[1], hpd[2])
                    } else {
                        the_spark <- histospark(post[[k]][, i, j], width = spark)
                        result[r, 1:3] <- c(mean(post[[k]][, i, j]), sd(post[[k]][, i, j]), min(post[[k]][, i, j]))
                        result[r, 4] <- the_spark
                        result[r, 5] <- max(post[[k]][, i, j])
                    }
                    rownames(result)[r] <- concat(names(post)[k], "[", i, ",", j, "]")
                    r <- r + 1
                }
            }
        }
    }
    if (spark == FALSE) {
        colnames(result)[3:4] <- c(paste("lower", prob), paste("upper", prob))
    }
    result
}
#' @export
setGeneric(
    "describe",
    function(object, depth = 1, pars, prob = 0.95, digits = 2, sort = NULL, decreasing = FALSE, ...) {
        new("describe", as.data.frame(object), digits = digits)
    }
)

# function that handles depth and sorting
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
#' @export
setMethod(
    "describe", "data.frame",
    function(object, depth = 1, pars, prob = 0.95, digits = 2, sort = NULL, decreasing = FALSE, hist = TRUE, ...) {
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



# template function for processing objects with coef and vcov methods
xdescribe_glm <- function(object, depth, pars, prob, digits, sort, decreasing, ...) {
    plo <- (1 - prob) / 2
    phi <- 1 - plo
    z <- qnorm(phi)
    result <- data.frame(
        mean = coef(object),
        sd = se(object),
        lo = coef(object) - z * se(object),
        hi = coef(object) + z * se(object)
    )
    colnames(result)[3:4] <- paste(c(plo, phi) * 100, "%", sep = "")

    result <- describe_format(result, depth, sort, decreasing)

    # obey pars list
    if (!missing(pars)) {
        # need to handle vector/matrix parameters
        # for each element in pars, add a copy with '[' on end
        # then use grep to white list any parameter that starts with element of pars
        pars_new <- paste(pars, "[", sep = "")
        pars <- c(pars, pars_new)
        keep_idx <- rep(FALSE, nrow(result))
        for (i in 1:length(pars)) {
            keep_idx <- keep_idx | startsWith(rownames(result), pars[i])
        }
        result <- result[keep_idx, ]
    }

    return(new("describe", result, digits = digits))
}
#' @export
setMethod(
    "describe", "glm",
    function(object, depth = 1, pars, prob = 0.95, digits = 2, sort = NULL, decreasing = FALSE, ...) {
        # just pass to glm method
        xdescribe_glm(object, depth, pars, prob, digits, sort, decreasing, ...)
    }
)
#' @export
setMethod(
    "describe", "lm",
    function(object, depth = 1, pars, prob = 0.95, digits = 2, sort = NULL, decreasing = FALSE, ...) {
        # just pass to glm method
        xdescribe_glm(object, depth, pars, prob, digits, sort, decreasing, ...)
    }
)

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
