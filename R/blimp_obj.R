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

#' Summary method for blimp_obj
#'
#' @description
#' Provides formatted summary output for blimp_obj, optionally allowing
#' selection by variable name or block. Offers cleaner presentation than the
#' raw estimates matrix.
#'
#' @param object A [`blimp_obj`] object containing model results.
#' @param selector Optional character string specifying the variable name or block to extract.
#'   If missing, shows all estimates. Can be a variable name (e.g., "y1") or a block name (e.g., "between").
#' @param digits Integer specifying the number of decimal places for rounding. Default is 3.
#' @param ... Additional arguments (for S4 method compatibility).
#'
#' @return
#' If selector is provided: invisibly returns a matrix or list of estimates for the selected variable/block.
#' If no selector: returns the full estimates matrix with improved formatting.
#'
#' @export
setMethod(
    "summary", "blimp_obj",
    function(object, selector, digits = 3, ...) {

        # Extract parameter width from ellipsis (used for alignment in multivariate models)
        passed_par_width <- list(...)$.par_width

        # If no selector provided, show formatted output for all variables
        if (missing(selector)) {
            # Get model attributes for processing all variables
            oname <- attr(object@iterations, "outcome_name")
            if (is.null(oname)) {
                # Fallback to basic output if no outcome names
                niter <- nrow(object@iterations)
                nchain <- length(object@burn)
                cli::cli_h1('Model Summary')
                cli::cli_alert_info("Model fitted with {niter} iterations using {nchain} chains.")
                if (any(colnames(object@estimates) == "Estimate")) {
                    cli::cli_alert_info("Estimate column based on posterior median.")
                }
                cli::cli_h1('')
                return(object@estimates)
            }

            # Show formatted output for all unique variables
            # Include single variables and multivariate models, but exclude blocks
            all_variables <- unique(tolower(oname))

            # Get block information for filtering
            block_info <- attr(object@iterations, "block")
            if (!is.null(block_info)) {
                unique_variables <- all_variables[!all_variables %in% tolower(block_info)]
            } else {
                unique_variables <- all_variables
            }

            # Print header
            niter <- nrow(object@iterations)
            nchain <- length(object@burn)
            cli::cli_h1('Model Summary')
            cli::cli_alert_info("Model fitted with {niter} iterations using {nchain} chains.")
            if (any(colnames(object@estimates) == "Estimate")) {
                cli::cli_alert_info("Estimate column based on posterior median.")
            }
            cat("\n")

            # Process each variable with dividers
            result_list <- vector("list", length(unique_variables))
            names(result_list) <- unique_variables

            # Calculate maximum parameter name width for consistent alignment
            max_par_width <- if (!is.null(passed_par_width)) {
                passed_par_width  # Use passed width if available (from parent call)
            } else {
                calculate_max_par_width(object, unique_variables)  # Calculate for all variables
            }

            for (i in seq_along(unique_variables)) {
                var_name <- unique_variables[i]

                # Add newline between variables (except before first)
                if (i > 1) {
                    cat("\n")
                }

                # Get estimates for this variable
                result_list[[i]] <- summary(object, var_name, digits = digits, .header_level = 2, .par_width = max_par_width)
            }

            cli::cli_h1('')
            return(invisible(result_list))
        }

        # Input validation for selector
        if (length(selector) != 1) {
            throw_error("Argument {.arg selector} must be a single character string.")
        }

        if (!is.character(selector)) {
            throw_error("Argument {.arg selector} must be a character string.")
        }

        if (!is.numeric(digits) || length(digits) != 1 || digits < 0) {
            throw_error("Argument {.arg digits} must be a non-negative integer.")
        }

        # Extract header level from ellipsis (controls header hierarchy)
        header_level <- list(...)$.header_level %||% 1

        # Extract model attributes
        oname <- attr(object@iterations, "outcome_name")
        if (is.null(oname)) {
            throw_error("Model object does not contain outcome name information.")
        }
        oname <- tolower(oname)

        ptype <- attr(object@iterations, "parameter_type")
        if (is.null(ptype)) {
            throw_error("Model object does not contain parameter type information.")
        }

        block <- attr(object@iterations, "block")
        if (is.null(block)) {
            throw_error("Model object does not contain block information.")
        }
        block <- tolower(block)

        # Check if requesting block FIRST (before variable check)
        if (selector %in% block) {
            cli::cli_h1('Estimates Summary for {selector} block')

            # Get unique outcome names in this block
            unique_outcomes <- unique(oname[block %in% selector])

            if (length(unique_outcomes) == 0) {
                throw_error("No variables found in block {.val {selector}}.")
            }

            # Create named list and process each outcome
            result <- stats::setNames(vector("list", length(unique_outcomes)), unique_outcomes)
            for (i in seq_along(unique_outcomes)) {

                # Calculate maximum parameter name width for consistent alignment across block outcomes
                max_par_width <- if (!is.null(passed_par_width)) {
                    passed_par_width  # Use passed width if available
                } else {
                    calculate_max_par_width(object, unique_outcomes)  # Calculate for block outcomes
                }

                outcome <- unique_outcomes[i]

                # Add newline between variables (except before first)
                if (i > 1) {
                    cat("\n")
                }

                result[[outcome]] <- summary(object, outcome, digits = digits, .header_level = 2, .par_width = max_par_width)
            }

            cli::cli_h1('')
            return(invisible(result))
        }

        # Process as variable name
        variable <- tolower(selector)

        # Get parameter selection for the main variable
        sel <- which(oname == variable)

        # Check for associated correlation models early
        correlation_models <- unique(oname[grepl(paste0("\\b", variable, "\\b"), oname) &
                                          grepl(" ", oname) & oname != variable])

        # If we have correlation models and this is a direct call, handle specially
        if (length(correlation_models) > 0 && header_level == 1) {
            # Calculate max parameter name width across all related models
            all_models <- c(variable, correlation_models)

            # Calculate maximum parameter name width across all related models for consistent alignment
            max_par_width <- if (!is.null(passed_par_width)) {
                passed_par_width  # Use passed width if available
            } else {
                calculate_max_par_width(object, all_models)  # Calculate for main + correlation models
            }

            # Print main header with info once
            cli::cli_h1('Estimates Summary for {selector}')
            niter <- nrow(object@iterations)
            nchain <- length(object@burn)
            cli::cli_alert_info("Summaries based on {niter} iterations using {nchain} chains.")
            if (any(colnames(object@estimates) == "Estimate")) {
                cli::cli_alert_info("Estimate column based on posterior median.")
            }

            # Create result list
            result_list <- list()

            # Process main variable with level 2 header and shared parameter width
            if (length(sel) > 0) {
                result_list[[variable]] <- summary(object, selector, digits = digits, .header_level = 2, .par_width = max_par_width)
            }

            # Process correlation models with level 2 headers and shared parameter width
            for (corr_model in correlation_models) {
                cat("\n")
                result_list[[corr_model]] <- summary(object, corr_model, digits = digits, .header_level = 2, .par_width = max_par_width)
            }

            cli::cli_h1('')
            return(invisible(result_list))
        }

        if (length(sel) == 0) {
            available_vars <- unique(oname)
            available_blocks <- unique(block)
            available_vars <- available_vars[available_vars != '#parameter']
            throw_error(c(
                "Variable {.val {selector}} not found in model.",
                "i" = "Available variables: {.val {available_vars}}",
                "i" = "Available blocks: {.val {available_blocks}}"
            ))
        }

        # Subset estimates
        est <- object@estimates[sel, , drop = FALSE]

        # Clean up row names
        rownames(est) <- gsub(paste0(variable, ' '), '   ', rownames(est))
        rownames(est) <- gsub(' ~', '', rownames(est))
        rownames(est) <- gsub(' R2:', '', rownames(est))
        rownames(est) <- gsub('\\(standardized\\)', '', rownames(est))
        rownames(est) <- gsub('residual variance', 'Residual Var.', rownames(est))
        rownames(est) <- gsub('residual SD', 'Residual SD', rownames(est))

        # Add extra prefix spacing for correlation/covariance models to align with main variables
        # This ensures "Cov( x, z )" aligns with "   Intercept" from main models
        if (any(grepl("^(Cov|Cor)\\(", rownames(est)))) {
            rownames(est) <- paste0("   ", rownames(est))
        }

        # Determine parameter name width for consistent alignment
        # Use shared width in multivariate models, otherwise calculate locally
        if (!is.null(passed_par_width)) {
            nw <- passed_par_width  # Use shared width from multivariate calculation
        } else {
            nw <- max(nchar(rownames(est)))  # Calculate width for this model only
        }

        # Use paste0 to append spaces to row names until they match max width
        rname <- sapply(rownames(est), function(name) {
            current_length <- nchar(name)
            if (current_length < nw) {
                paste0(name, strrep(" ", nw - current_length))
            } else {
                name
            }
        }, USE.NAMES = FALSE)

        # Format values
        values <- est |>
            round(digits = digits) |>
            apply(2, format, width = max(nchar(colnames(est)), 4 + digits))

        # Check if values is one dim
        if (values |> dim() |> is.null()) {
            dim(values) <- c(1, length(values))
        }

        # Update selector name
        sel_name <- if (selector == '#_parameter') "Parameters" else selector

        # Print output header
        if (header_level == 1) {
            cli::cli_h1('Estimates Summary for {sel_name}')

            # Only show iteration/chain info for direct calls (level 1 headers)
            niter <- nrow(object@iterations)
            nchain <- length(object@burn)
            cli::cli_alert_info("Summaries based on {niter} iterations using {nchain} chains.")

            # Check if Estimate column is used
            if (any(colnames(est) == "Estimate")) {
                cli::cli_alert_info("Estimate column based on posterior median.")
            }
        } else {
            cli::cli_h2('Estimates Summary for {sel_name}')
        }

        # Print column headers
        cat("\n")
        cname <- colnames(object@estimates)

        # Format column names to align with values
        cname_formatted <- sapply(seq_along(cname), function(i) {
            format(
                cname[i],
                width = max(nchar(values[, i])),
                justify = 'right'
            )
        })

        # Print header row - use the actual row width (nw) not max(nchar(rname))
        cat(c(strrep(' ', nw), cname_formatted), fill = TRUE)

        # Print estimates by parameter type
        for (param_level in levels(ptype[sel])) {
            param_indices <- which(ptype[sel] == param_level)
            if (length(param_indices) > 0) {
                # Handle parameters
                if (param_level != 'Other' | selector != '#_parameter') {
                    cli::cli_h3(paste0(param_level, ':'))
                }
                for (j in param_indices) {
                    cat(paste0(rname[j], ' '))
                    cat(values[j, , drop = FALSE], fill = TRUE)
                }
            }
        }

        # End with empty header only for direct calls (not recursive calls)
        if (header_level == 1) {
            cli::cli_h1('')
        }

        # Return subset of estimates invisibly
        invisible(nw)
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
#' @noRd
#' @export
output <- function(object) {
    if (!is_blimp_obj(object)) throw_error(
        "Object is not a {.cls blimp_obj}."
    )
    object@output
}

#' Extract POTENTIAL SCALE REDUCTION output from `blimp_obj` or `blimp_out`
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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

#' Extract MODEL FIT from `blimp_obj` or `blimp_out`
#' @noRd
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
#' @noRd
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
                  (endsWith(names(x), ".") & names(x) != "imp."), drop = FALSE]
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
                  endsWith(names(x), ".probability"), drop = FALSE]
        })
    }
)


#' Coerces a [`blimp_obj`] or `blimp_bygroup` to a `mitml.list`
#' @param object [`blimp_obj`] or `blimp_bygroup` object
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


## Write blimp files
#' A function to write out blimp input and output from a model
#' @param object A [`blimp_obj`].
#' @param folder a location to a folder to write input and output
#' @examplesIf has_blimp()
#' # Generate Data
#' mydata <- data.frame(x = rnorm(1000), y = rnorm(1000))
#'
#' # Nonsensical model
#' mdl <- rblimp(
#'     c(
#'         'y <- x*2',
#'         'f1 <- 1'
#'     ),
#'     mydata,
#'     seed = 3927,
#'     nimps = 2,
#'     latent = ~ f1,
#'     center = cgm ~ x,
#'     fixed = ~ x
#' )
#'
#' # Write out input and output
#' \dontrun{
#' write.blimp(mdl, "folder_location")
#' }
#' @export
setGeneric("write.blimp", function(object, folder = "") {
    stop(paste("Does not work with ", class(object)))
})

setMethod("write.blimp", "blimp_syntax",
    function(object, folder = "") {
        fileConn <- base::file(file.path(folder))
        writeLines(as.character(object), fileConn)
        close(fileConn)
    }
)
setMethod("write.blimp", "blimp_out",
    function(object, folder = "") {
        fileConn <- base::file(file.path(folder))
        writeLines(as.character(object), fileConn)
        close(fileConn)
    }
)

#' @export
setMethod("write.blimp", "blimp_obj",
    function(object, folder = "") {
        nm <- deparse(substitute(object))
        write.blimp(object@syntax, file.path(folder, paste0(nm, ".imp")))
        write.blimp(object@output, file.path(folder, paste0(nm, ".blimp-out")))
    }
)
