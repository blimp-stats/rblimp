## Blimp output object
# Copyright Brian Keller 2022, all rights reserved

# Set S3 class generated from blimp_source
setOldClass("blimp_out")
setOldClass("blimp_syntax")

#' @export
print.blimp_out <- function(x, ...) {
    cat(x, sep = "\n")
    invisible(x)
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
    variance_imp = "data.frame", waldtest = "data.frame", simple = "data.frame",
    syntax = "blimp_syntax", output = "blimp_out"
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

#' Internal validate `blimp_obj`
#' @noRd
is_blimp_obj <- function(x) {
    inherits(x, 'blimp_obj')
}

#' Internal validate `blimp_out`
#' @noRd
is_blimp_out <- function(x) {
    inherits(x, 'blimp_out')
}

#' Extract output (`blimp_out`) from `blimp_obj`
#' @export
output <- function(object) {
    if (!is_blimp_obj(object)) throw_error(
        "Object is not a {.cls blimp_obj}."
    )
    object@output
}

#' Extract POTENTIAL SCALE REDUCTION output from `blimp_obj` or `blimp_out`
#' @export
psr <- function(object) {
    if (is_blimp_obj(object)) output <- output(object)
    else if (is_blimp_out(object)) output <- object
    else throw_error(
        "Object is not a {.cls blimp_obj} or {.cls blimp_out}."
    )
    if (length(output) == 1) throw_error(c(
        "Output was not saved",
        "i" = "Set {.arg print = FALSE}.")
    )
    strt <- which(output %in% "BURN-IN POTENTIAL SCALE REDUCTION (PSR) OUTPUT:")
    stop <- which(output %in% "DATA INFORMATION:")
    if (length(strt) == 0) throw_error("Could not find PSR. Make sure it was requested.")
    if (length(stop) == 0) {
        stop <- which(output %in% "ANALYSIS MODEL ESTIMATES:")
    }
    if (length(stop) == 0) {
        stop <- which(output %in% "COVARIATE MODEL ESTIMATES:")
    }
    if (length(stop) == 0) throw_error("Could not find end of PSR.")
    if (strt >= stop) throw_error("Could not parse PSR.")
    return(output[strt:stop - 1])
}


#' Extract ALGORITHMIC OPTIONS output from `blimp_obj` or `blimp_out`
#' @export
algorithm <- function(object) {
    if (is_blimp_obj(object)) output <- output(object)
    else if (is_blimp_out(object)) output <- object
    else throw_error(
        "Object is not a {.cls blimp_obj} or {.cls blimp_out}."
    )
    if (length(output) == 1) throw_error(c(
        "Output was not saved",
        "i" = "Set {.arg print = FALSE}.")
    )
    strt <- which(output %in% "ALGORITHMIC OPTIONS SPECIFIED:")
    stop <- which(output %in% "BURN-IN POTENTIAL SCALE REDUCTION (PSR) OUTPUT:")
    if (length(strt) == 0) throw_error("Could not find Algorithm options.")
    if (length(stop) == 0) throw_error("Could not find end of Algorithm options.")
    if (strt >= stop) throw_error("Could not parse Algorithm options.")
    return(output[strt:stop - 1])
}

#' Extract DATA INFORMATION output from `blimp_obj` or `blimp_out`
#' @export
datainfo <- function(object) {
    if (is_blimp_obj(object)) output <- output(object)
    else if (is_blimp_out(object)) output <- object
    else throw_error(
        "Object is not a {.cls blimp_obj} or {.cls blimp_out}."
    )
    if (length(output) == 1) throw_error(c(
        "Output was not saved",
        "i" = "Set {.arg print = FALSE}.")
    )
    strt <- which(output %in% "DATA INFORMATION:")
    stop <- which(output %in% "MODEL INFORMATION:")
    if (length(strt) == 0) throw_error("Could not find Data Information.")
    if (length(stop) == 0) throw_error("Could not find end of Data Information.")
    if (strt >= stop) throw_error("Could not parse Data Information.")
    return(output[strt:stop - 1])
}

#' Extract MODEL INFORMATION from `blimp_obj` or `blimp_out`
#' @export
modelinfo <- function(object) {
    if (is_blimp_obj(object)) output <- output(object)
    else if (is_blimp_out(object)) output <- object
    else throw_error(
        "Object is not a {.cls blimp_obj} or {.cls blimp_out}."
    )
    if (length(output) == 1) throw_error(c(
        "Output was not saved",
        "i" = "Set {.arg print = FALSE}.")
    )
    strt <- which(output %in% "MODEL INFORMATION:")
    stop <- which(output %in% "WARNING MESSAGES:")
    if (length(strt) == 0) throw_error("Could not find Model Information.")
    if (length(stop) == 0) throw_error("Could not find end of Model Information.")
    if (strt >= stop) throw_error("Could not parse Model Information.")
    return(output[strt:stop - 1])
}

#' @export
modelfit <- function(object) {
    if (is_blimp_obj(object)) output <- output(object)
    else if (is_blimp_out(object)) output <- object
    else throw_error(
        "Object is not a {.cls blimp_obj} or {.cls blimp_out}."
    )
    if (length(output) == 1) throw_error(c(
        "Output was not saved",
        "i" = "Set {.arg print = FALSE}.")
    )
    strt <- which(output %in% "MODEL FIT:")
    stop <- which(output %in% "OUTCOME MODEL ESTIMATES:")
    if (length(strt) == 0) throw_error("Could not find MODEL FIT")
    if (length(stop) == 0) stop <- length(output)
    if (strt >= stop) throw_error("Could not parse MODEL FIT")
    return(output[strt:stop - 1])
}

#' Obtain MODEL ESTIMATES from `blimp_obj` or `blimp_out`
#' @export
estimates <- function(object) {
    if (is_blimp_obj(object)) output <- output(object)
    else if (is_blimp_out(object)) output <- object
    else throw_error(
        "Object is not a {.cls blimp_obj} or {.cls blimp_out}."
    )
    if (length(output) == 1) throw_error(c(
        "Output was not saved",
        "i" = "Set {.arg print = FALSE}.")
    )
    strt <- which(output %in% "OUTCOME MODEL ESTIMATES:")
    if (length(strt) == 0) strt <- which(output %in% "PREDICTOR MODEL ESTIMATES:")
    if (length(strt) == 0) throw_error("Could not find Analysis model.")
    stop <- which(output %in% "VARIABLE ORDER IN IMPUTED DATA:")
    if (length(stop) == 0) {
        stop <- which(output %in% "VARIABLE ORDER IN IMPUTED DATA:")
    }
    if (length(stop) == 0) stop <- length(output)
    if (strt >= stop) throw_error("Could not parse Analysis Model output.")
    return(output[strt:stop - 1])
}

#' Residuals scores from `blimp_obj`
#' @export
setMethod(
    "residuals", "blimp_obj",
    function(object, ...) {
        lapply(object@imputations, \(x) {
            x[, endsWith(names(x), ".residual") |
                  (endsWith(names(x), ".") & names(x) != "imp."), drop = F]
        })
    }
)

#' Residuals scores from `blimp_obj`
#' @export
setMethod(
    "resid", "blimp_obj",
    function(object, ...) {
        residuals(object, ...)
    }
)

#' Predicted scores from `blimp_obj`
#' @export
setMethod(
    "predict", "blimp_obj",
    function(object, ...) {
        lapply(object@imputations, \(x) {
            x[, endsWith(names(x), ".predicted") |
                  endsWith(names(x), ".probability"), drop = F]
        })
    }
)


#' Coerces a [`blimp_obj`] or `blimp_bygroup` to a `mitml.list`
#' @export
as.mitml <- function(object) {
    if (object |> inherits("blimp_bygroup")) {
        # Create un_split function
        un_split <- function (value, f, drop = FALSE) {
            x <- matrix(nrow = NROW(f), ncol = NCOL(value[[1L]])) |> data.frame()
            names(x) <- names(value[[1L]])
            split(x, f, drop = drop) <- value
            x
        }
        # Run on each imputation
        o <- lapply(seq_len(attr(object, "nimps")), \(i) {
            lapply(object, \(x) x@imputations[[i]]) |> un_split(attr(object, "group"))
        })
    } else if (!is_blimp_obj(object)) {
        throw_error("Object is not a {.cls blimp_obj}.")
    } else {
        o <- object@imputations
    }
    if (length(o) == 0) throw_error("No imputations were requested.")
    class(o) <- c("mitml.list", "list")
    return(o)
}


#' Fit Model across imputations with `mitml` package
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

