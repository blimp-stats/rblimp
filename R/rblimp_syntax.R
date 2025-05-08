## Functions for parsing and generating blimp syntax
# Copyright Brian Keller 2024, all rights reserved


#' Generates a syntax 'imp' file for blimp based on rblimp
#' @rdname rblimp
#' @export
rblimp_syntax <- function(
        model,
        data,
        burn,
        iter,
        seed,
        thin,
        nimps,
        latent,
        randomeffect,
        parameters,
        clusterid,
        weight,
        ordinal,
        nominal,
        count,
        transform,
        fixed,
        center,
        chains,
        simple,
        waldtest,
        options,
        save,
        output) {

    # Check if data.frame
    if (!is.data.frame(data)) throw_error(
        "The {.arg data} must be a data.frame"
    )

    # Flatten model
    if (is.list(model)) {
        model <- rapply(model, paste0, collapse ='; ')
    }

    # Flatten waldtest
    if (!missing(waldtest) && is.list(waldtest)) {
        waldtest <- rapply(waldtest, paste0, collapse =';\n          ')
    }

    # Make sure it is a character vector
    if (!is.character(model)) throw_error(
        "The {.arg model} must be a character string."
    )

    # Parse latent list
    if (!missing(latent) && is.list(latent)) {
        names(latent) <- parse_names(latent)
        latent_list <- lapply(latent, parse_formula)
        latent <- NULL
        for (i in seq_along(latent_list)) {
            tmp <- paste(latent_list[[i]], collapse = " ")
            tmp_name <- names(latent_list[i])
            if (!is.null(tmp_name)) {
                if (tmp_name != "") tmp <- paste0(tmp_name, " = ", tmp)
            }
            latent <- paste0(latent, tmp, "; ")
        }
    }
    # Single formula
    else if (!missing(latent) && is.formula(latent)) {
        name <- parse_names(list(latent))
        form <- parse_formula(latent)
        if (!is.null(name)) {
            if (name != "") form <- paste0(name, " = ", form)
        }
        latent <- form
    }

    # Parse center list
    if (!missing(center) && (is.list(center) || is.vector(center))) {
        names(center) <- parse_names(center)
        center <- lapply(center, parse_formula)
        center_list <- c(
            # cwc
            if (!is.null(center$cwc)) {
                paste0("cwc = ", paste(center$cwc, collapse = " "))
            } else NULL,
            # cgm
            if (!is.null(center$cgm)) {
                paste0("cgm = ", paste(center$cgm, collapse = " "))
            } else NULL,
            # grandmean
            if (!is.null(center$grandmean)) {
                paste0("grandmean = ", paste(center$grandmean, collapse = " "))
            } else NULL,
            # groupmean
            if (!is.null(center$groupmean)) {
                paste0("groupmean = ", paste(center$groupmean, collapse = " "))
            } else NULL
        )
        if (!is.null(center_list)) {
            center <- NULL
            for (i in center_list) {
                center <- paste0(center, i, "; ")
            }
        } else {
            center <- unlist(center)
        }
    }
    # Single formula
    else if (!missing(center) && is.formula(center)) {
        name <- parse_names(list(center))
        form <- parse_formula(center)
        if (!is.null(name)) {
            if (name != "") form <- paste0(name, " = ", form)
        }
        center <- form
    }

    ## parse clusterid
    if (!missing(clusterid) && is.formula(clusterid)) {
        clusterid <- parse_formula(clusterid)
    }

    ## parse weight
    if (!missing(weight) && is.formula(weight)) {
        weight <- parse_formula(weight)
    }

    ## parse ordinal
    if (!missing(ordinal) && is.formula(ordinal)) {
        ordinal <- parse_formula(ordinal)
    }

    ## parse nominal
    if (!missing(nominal) && is.formula(nominal)) {
        nominal <- parse_formula(nominal)
    }

    ## parse count
    if (!missing(count) && is.formula(count)) {
        count <- parse_formula(count)
    }

    ## parse fixed
    if (!missing(fixed) && is.formula(fixed)) {
        fixed <- parse_formula(fixed)
    }

    # Return syntax file
    make_syntax(
        "data.csv", model,
        burn, seed, iter, names(data),
        thin, nimps, latent, randomeffect, clusterid, weight,
        ordinal, nominal, count, center, parameters, chains,
        simple, waldtest, options, output, save, transform, fixed
    )
}

#' parse a command to syntax
#' @noRd
parse_cmd <- function(y, collapse = " ", use.names = FALSE) {
    y <- format(y, scientific = FALSE, justify = "none")
    if (is.null(y)) {
        return("")
    }
    if (y[1] == "") {
        return("")
    }
    if (use.names) {
        y_names <- sapply(names(y), function(x) {
            if (x == "") {
                return(x)
            }
            return(paste0(x, ":"))
        })
        return(paste(y_names, y, collapse = collapse))
    }
    return(paste(y, collapse = collapse))
}

#' Parse formula to blimp syntax
#' @noRd
parse_formula <- function(x) {
    # Check for formula
    if (!is.call(x) && x[[1]] != quote(`~`)) {
        return(x)
    }
    return(paste(attr(terms(x), "term.labels"), collapse = " "))
}
#' Check for formula
#' @noRd
is.formula <- function(x) is.call(x) && x[[1]] == quote(`~`)

#' Get name from formula
#' @noRd
form_name <- function(x) {
    if (!is.formula(x)) {
        return("")
    }
    if (length(x) == 2) {
        return("")
    }
    return(as.character(x[[2]]))
}

#' Parse names
#' @noRd
parse_names <- function(x) {
    o <- vector("character", length(x))
    for (i in seq_along(x)) {
        o[i] <- if (is.null(names(x[i]))) {
            form_name(x[[i]])
        } else if (names(x[i]) == "") {
            form_name(x[[i]])
        } else {
            names(x[i])
        }
    }
    return(o)
}

#' Internal Blimp Syntax generation
#' @noRd
make_syntax <- function(datapath,
                        model,
                        burn,
                        seed,
                        iter,
                        variables,
                        thin,
                        nimps,
                        latent,
                        randomeffect,
                        clusterid,
                        weight,
                        ordinal,
                        nominal,
                        count,
                        centering,
                        parameters,
                        chains,
                        simple,
                        waldtest,
                        options,
                        output,
                        save,
                        transform,
                        fixed) {
    inputfile <- list()
    if (!missing(datapath)) inputfile$data   <- parse_cmd(datapath)
    if (!missing(variables)) {
        if (!is.null(variables)) inputfile$variables <- parse_cmd(variables)
    }
    if (!missing(transform)) inputfile$transform <- parse_cmd(transform, collapse = ";\n    ")
    if (!missing(ordinal))   inputfile$ordinal   <- parse_cmd(ordinal)
    if (!missing(nominal))   inputfile$nominal   <- parse_cmd(nominal)
    if (!missing(count))     inputfile$count     <- parse_cmd(count)
    if (!missing(fixed))     inputfile$fixed     <- parse_cmd(fixed)
    inputfile$missing <- parse_cmd("NA")
    if (!missing(clusterid)) inputfile$clusterid <- parse_cmd(clusterid)
    if (!missing(weight))    inputfile$weight    <- parse_cmd(weight)
    if (!missing(latent))   inputfile$latent <- parse_cmd(latent)
    if (!missing(randomeffect)) inputfile$randomeffect <- parse_cmd(randomeffect)
    if (!missing(centering)) inputfile$center    <- parse_cmd(centering)
    if (!missing(model))     inputfile$model     <- parse_cmd(model, collapse = ";\n    ", use.names = TRUE)
    if (!missing(parameters)) inputfile$parameters <- parse_cmd(parameters, collapse = ";\n    ")
    if (!missing(simple)) inputfile$simple <- parse_cmd(simple, collapse = ";\n    ")
    if (!missing(waldtest)) {
        if (length(waldtest) > 1) names(waldtest) <- c('', rep('WALDTEST', length(waldtest) - 1))
        inputfile$waldtest <- parse_cmd(waldtest, collapse = ";\n", use.names = T)
    }
    if (!missing(seed))     inputfile$seed   <- parse_cmd(seed)
    if (!missing(burn))     inputfile$burn   <- parse_cmd(burn)
    if (!missing(thin))     inputfile$thin   <- parse_cmd(thin)
    if (!missing(iter))     inputfile$iter   <- parse_cmd(iter)
    if (!missing(nimps))    inputfile$nimps  <- parse_cmd(nimps)
    if (!missing(chains)) inputfile$chains <- parse_cmd(chains)
    if (!missing(output)) inputfile$output <- parse_cmd(output)
    if (!missing(options)) inputfile$options <- parse_cmd(options)
    if (!missing(save)) inputfile$save <- parse_cmd(save, collapse = ";\n    ")
    ## Return file
    structure(
        inputfile,
        class = 'blimp_syntax'
    )
}

#' Convert `blimp_syntax` to character vector
#' @export
as.character.blimp_syntax <- function(x, ...) {
    # '
    paste0(
        paste0(
            '# Input generated by rblimp ',
            packageVersion('rblimp') |> paste0(collapse = '.'),
            ' on ', Sys.time() |> format(), '\n\n', coallpse =''
        ),
        paste0(
            toupper(names(x)),  ': ',
            as.character.default(x), ';',
            collapse = "\n"
        ),
        collapse = ''
    )
}

#' Print `blimp_syntax`
#' @export
print.blimp_syntax <- function(x, ...) {
    cat(as.character(x))
    invisible(x)
}


