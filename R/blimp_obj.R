## Blimp output object
# Copyright Brian Keller 2022, all rights reserved

# Set S3 class generated from blimp_source
setOldClass("blimp_out")

#' @exportS3Method
print.blimp_out <- function(x, ...) {
    cat(x, sep = "\n")
}

#' @export
`[.blimp_out` <- function(x, i, ...) {
    structure(
        as.character(x)[i, ...],
        class = "blimp_out",
        exitcode = attr(x, "exitcode")
    )
}

#' @export
setClass("blimp_obj", slots = list(
    call = "language", estimates = "matrix", burn = "list", iterations = "data.frame",
    psr = "data.frame", imputations = "list", average_imp = "data.frame",
    variance_imp = "data.frame", latent = "list", residuals = "list",
    predicted = "list", output = "blimp_out"
))

#' @export
setMethod(
    "as.data.frame", "blimp_obj",
    function(x, row.names = NULL, optional = FALSE, ...) {
        return(as.data.frame(x@iterations, row.names = NULL, optional = FALSE, ...))
    }
)

#' @export
setMethod(
    "as.matrix", "blimp_obj",
    function(x, ...) {
        return(as.matrix(x@iterations))
    }
)

#' @export
setMethod(
    "summary", "blimp_obj",
    function(object, ...) {
        return(object@estimates)
    }
)

#' @export
setMethod(
    "show", "blimp_obj",
    function(object) {
        print(object@estimates, max = length(object@estimates))
    }
)

# Need generics
setGeneric("psr", function(object) {
    stop(paste("Does not work with", class(object)))
})
setGeneric("algorithm", function(object) {
    stop(paste("Does not work with", class(object)))
})
setGeneric("datainfo", function(object) {
    stop(paste("Does not work with", class(object)))
})
setGeneric("modelinfo", function(object) {
    stop(paste("Does not work with", class(object)))
})
setGeneric("waldtest", function(object) {
    stop(paste("Does not work with", class(object)))
})
setGeneric("estimates", function(object) {
    stop(paste("Does not work with", class(object)))
})
setGeneric("output", function(object) {
    stop(paste("Does not work with", class(object)))
})


#' @export
setMethod(
    "psr", "blimp_obj",
    function(object) {
        if (length(object@output) == 1) stop("Output was not saved. Set print=FALSE.")
        if (length(object@output) == 1) stop("Output was not saved. Set print=FALSE.")
        strt <- which(object@output %in% "BURN-IN POTENTIAL SCALE REDUCTION (PSR) OUTPUT:")
        stop <- which(object@output %in% "DATA INFORMATION:")
        if (length(start) == 0) stop("Could not find PSR. Make sure it was requested.")
        if (length(stop) == 0) {
            stop <- which(object@output %in% "ANALYSIS MODEL ESTIMATES:")
        }
        if (length(stop) == 0) {
            stop <- which(object@output %in% "COVARIATE MODEL ESTIMATES:")
        }
        if (length(stop) == 0) stop("Could not find end of PSR.")
        if (strt >= stop) stop("Could not parse PSR.")
        return(object@output[strt:stop - 1])
    }
)

#' @export
setMethod(
    "algorithm", "blimp_obj",
    function(object) {
        if (length(object@output) == 1) stop("Output was not saved. Set print=FALSE.")
        if (length(object@output) == 1) stop("Output was not saved. Set print=FALSE.")
        strt <- which(object@output %in% "ALGORITHMIC OPTIONS SPECIFIED:")
        stop <- which(object@output %in% "BURN-IN POTENTIAL SCALE REDUCTION (PSR) OUTPUT:")
        if (length(start) == 0) stop("Could not find Algorithm options.")
        if (length(stop) == 0) stop("Could not find end of Algorithm options.")
        if (strt >= stop) stop("Could not parse Algorithm options.")
        return(object@output[strt:stop - 1])
    }
)

#' @export
setMethod(
    "datainfo", "blimp_obj",
    function(object) {
        if (length(object@output) == 1) stop("Output was not saved. Set print=FALSE.")
        if (length(object@output) == 1) stop("Output was not saved. Set print=FALSE.")
        strt <- which(object@output %in% "DATA INFORMATION:")
        stop <- which(object@output %in% "MODEL INFORMATION:")
        if (length(start) == 0) stop("Could not find Data Information.")
        if (length(stop) == 0) stop("Could not find end of Data Information.")
        if (strt >= stop) stop("Could not parse Data Information.")
        return(object@output[strt:stop - 1])
    }
)

#' @export
setMethod(
    "modelinfo", "blimp_obj",
    function(object) {
        if (length(object@output) == 1) stop("Output was not saved. Set print=FALSE.")
        if (length(object@output) == 1) stop("Output was not saved. Set print=FALSE.")
        strt <- which(object@output %in% "MODEL INFORMATION:")
        stop <- which(object@output %in% "WARNING MESSAGES:")
        if (length(start) == 0) stop("Could not find Model Information.")
        if (length(stop) == 0) stop("Could not find end of Model Information.")
        if (strt >= stop) stop("Could not parse Model Information.")
        return(object@output[strt:stop - 1])
    }
)

#' @export
setMethod(
    "waldtest", "blimp_obj",
    function(object) {
        if (length(object@output) == 1) stop("Output was not saved. Set print=FALSE.")
        if (length(object@output) == 1) stop("Output was not saved. Set print=FALSE.")
        strt <- which(object@output %in% "MODEL FIT:")
        stop <- which(object@output %in% "VARIABLE ORDER IN IMPUTED DATA:")
        if (length(start) == 0) stop("Could not find Model Fit.")
        if (length(stop) == 0) stop <- length(object@output)
        if (strt >= stop) stop("Could not parse Model Fit.")
        return(object@output[strt:stop - 1])
    }
)

#' @export
setMethod(
    "residuals", "blimp_obj",
    function(object, ...) {
        return(object@residuals)
    }
)

#' @export
setMethod(
    "predict", "blimp_obj",
    function(object, ...) {
        return(object@predicted)
    }
)


# Create Traceplot
create_traceplot <- function(x, colors = NULL, ...) {
    # Check colors
    if (is.null(colors)) {
        colors <- seq_along(x$data)
    } else {
        if (length(colors) != length(x$data)) {
            stop(paste0("colors must be length of number of chains (", length(x$data), ")"))
        }
    }
    for (i in seq_along(x$data)) {
        if (i == 1) {
            plot(x$data[[i]], type = "l", col = colors[i], xlab = "iteration", ylab = "value", main = x$name, ...)
        } else {
            lines(x$data[[i]], type = "l", col = colors[i])
        }
    }
}

#' @export
setMethod(
    "plot", "blimp_obj",
    function(x, y, colors = NULL, ...) {
        mydata <- do.call("rbind", x@burn)
        if (missing(y)) {
            out <- traceplot(x)
            for (i in seq_along(out)) {
                plot(out[[i]], colors = colors, ...)
                invisible(
                    readline(
                        prompt = paste0("  Hit <Return> to see next plot (", i, "/", length(out), ")")
                    )
                )
            }
            return(invisible(out))
        } else if (length(y) > 0 & is.numeric(y)) {
            return(plot(traceplot(x, y), colors = colors, ...))
        }
        stop("ERROR")
    }
)


#' @export
setMethod(
    "estimates", "blimp_obj",
    function(object) {
        if (length(object@output) == 1) stop("Output was not saved. Set print=FALSE.")
        if (length(object@output) == 1) stop("Output was not saved. Set print=FALSE.")
        strt <- which(object@output %in% "OUTCOME MODEL ESTIMATES:")
        if (length(start) == 0) {
            strt <- which(object@output %in% "PREDICTOR MODEL ESTIMATES:")
        }
        if (length(start) == 0) stop("Could not find Analysis model.")
        stop <- which(object@output %in% "MODEL FIT:")
        if (length(stop) == 0) {
            stop <- which(object@output %in% "VARIABLE ORDER IN IMPUTED DATA:")
        }
        if (length(stop) == 0) stop <- length(object@output)
        if (strt >= stop) stop("Could not parse Analysis Model output.")
        return(object@output[strt:stop - 1])
    }
)

#' @export
setMethod(
    "output", "blimp_obj",
    function(object) {
        return(object@output)
    }
)


#' @export
setGeneric("as.mitml", function(x) {
    if (!requireNamespace("mitml", quietly = TRUE)) {
        stop("Package \"mitml\" must be installed to use this function.")
    }
    return(mitml::as.mitml.list(x))
})

#' @export
setMethod(
    "as.mitml", "blimp_obj",
    function(x) {
        o <- x@imputations
        if (length(o) == 0) stop("No imputations were requested.")
        class(o) <- c("mitml.list", class(x@imputations))
        return(o)
    }
)


#' @export
setMethod(
    "with", "blimp_obj",
    function(data, expr, ...) {
        expr <- substitute(expr)
        pf <- parent.frame()
        out <- lapply(data@imputations, eval, expr = expr, enclos = pf)
        class(out) <- c("mitml.result", "list")
        return(out)
    }
)
