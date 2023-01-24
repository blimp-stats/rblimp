## Functions for parsing and generating blimp syntax
# Copyright Brian Keller 2022, all rights reserved

# Adds a command
add_cmd <- function(cmd, y, collapse = " ", use.names = FALSE) {
    y <- format(y, scientific = FALSE)
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
        return(paste0(cmd, paste(y_names, y, collapse = collapse)))
    }
    return(paste0(cmd, paste(y, collapse = collapse)))
}

# Parse formula to blimp syntax
parse_formula <- function(x) {
    # Check for formula
    if (!is.call(x) && x[[1]] != quote(`~`)) {
        return(x)
    }
    return(paste(attr(terms(x), "term.labels"), collapse = " "))
}
## Check for formula
is.formula <- function(x) is.call(x) && x[[1]] == quote(`~`)

## Get name from formula
form_name <- function(x) {
    if (!is.formula(x)) {
        return("")
    }
    if (length(x) == 2) {
        return("")
    }
    return(as.character(x[[2]]))
}

## Parse names
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

## Blimp Syntax generation
blimp_syntax <- function(datapath,
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
                         ordinal,
                         nominal,
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
    if (!missing(datapath)) inputfile[[length(inputfile) + 1]] <- add_cmd("DATA: ", datapath)
    if (!missing(burn)) inputfile[[length(inputfile) + 1]] <- add_cmd("BURN: ", burn)
    if (!missing(thin)) inputfile[[length(inputfile) + 1]] <- add_cmd("THIN: ", thin)
    if (!missing(seed)) inputfile[[length(inputfile) + 1]] <- add_cmd("SEED: ", seed)
    if (!missing(iter)) inputfile[[length(inputfile) + 1]] <- add_cmd("ITER: ", iter)
    if (!missing(nimps)) inputfile[[length(inputfile) + 1]] <- add_cmd("NIMPS: ", nimps)
    if (!missing(latent)) inputfile[[length(inputfile) + 1]] <- add_cmd("LATENT: ", latent)
    if (!missing(randomeffect)) inputfile[[length(inputfile) + 1]] <- add_cmd("RANDOMEFFECT: ", randomeffect)
    inputfile[[length(inputfile) + 1]] <- add_cmd("MISSING: ", "NA")
    if (!missing(clusterid)) inputfile[[length(inputfile) + 1]] <- add_cmd("CLUSTERID: ", clusterid)
    if (!missing(ordinal)) inputfile[[length(inputfile) + 1]] <- add_cmd("ORDINAL: ", ordinal)
    if (!missing(nominal)) inputfile[[length(inputfile) + 1]] <- add_cmd("NOMINAL: ", nominal)
    if (!missing(model)) inputfile[[length(inputfile) + 1]] <- add_cmd("MODEL: ", model, ";\n    ", use.names = rblimp.env$beta)
    if (!missing(parameters)) inputfile[[length(inputfile) + 1]] <- add_cmd("PARAMETERS: ", parameters, ";\n    ")
    if (!missing(centering)) inputfile[[length(inputfile) + 1]] <- add_cmd("CENTER: ", centering)
    if (!missing(chains)) inputfile[[length(inputfile) + 1]] <- add_cmd("CHAINS: ", chains)
    if (!missing(fixed)) inputfile[[length(inputfile) + 1]] <- add_cmd("FIXED: ", fixed)
    if (!missing(simple)) inputfile[[length(inputfile) + 1]] <- add_cmd("SIMPLE: ", simple, ";\n    ")
    if (!missing(waldtest)) inputfile[[length(inputfile) + 1]] <- add_cmd("WALDTEST: ", waldtest, ";\n    ")
    if (!missing(options)) inputfile[[length(inputfile) + 1]] <- add_cmd("OPTIONS: ", options)
    if (!missing(output)) inputfile[[length(inputfile) + 1]] <- add_cmd("OUTPUT: ", output)
    if (!missing(transform)) inputfile[[length(inputfile) + 1]] <- add_cmd("TRANSFORM: ", transform, ";\n    ")
    if (!missing(save)) inputfile[[length(inputfile) + 1]] <- add_cmd("SAVE: ", save, ";\n    ")
    ## Return file
    return(paste0(inputfile, collapse = ";\n"))
}
