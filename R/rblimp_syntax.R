## Functions for parsing and generating blimp syntax
# Copyright Brian Keller 2024, all rights reserved


#' Generates a syntax 'imp' file for blimp based on rblimp
#' @param save Specify SAVE command for blimp syntax
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
        filter,
        fixed,
        center,
        chains,
        simple,
        waldtest,
        options,
        save,
        output) {

    # Check if data is simulation or data.frame
    is_simulation <- inherits(data, "blimp_simulate")

    if (!is_simulation && !is.data.frame(data)) {
        throw_error("The {.arg data} must be a data.frame or a blimp_simulate object")
    }

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

    # Build syntax file
    if (is_simulation) {
        variables_list <- data$variables
    } else {
        variables_list <- names(data)
    }

    syntax <- make_syntax(
        "data.csv", model,
        burn, seed, iter, variables_list,
        thin, nimps, latent, randomeffect, clusterid, weight,
        ordinal, nominal, count, center, parameters, chains,
        simple, waldtest, options, output, save, transform, filter,
        fixed
    )

    # If simulation, replace DATA with SIMULATE and reorder
    if (is_simulation) {
        syntax$data <- NULL
        syntax$simulate <- data$simulate

        # Reorder: SIMULATE must be first
        # Get all names except simulate
        other_names <- setdiff(names(syntax), "simulate")
        # Reorder with simulate first, preserving class
        syntax <- structure(
            syntax[c("simulate", other_names)],
            class = class(syntax)
        )
    }

    return(syntax)
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
                        filter,
                        fixed) {
    inputfile <- list()
    if (!missing(datapath)) inputfile$data   <- parse_cmd(datapath)
    if (!missing(variables)) {
        if (!is.null(variables)) inputfile$variables <- parse_cmd(variables)
    }
    if (!missing(transform)) inputfile$transform <- parse_cmd(transform, collapse = ";\n    ")
    if (!missing(filter))    inputfile$filter    <- parse_cmd(filter)
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
        inputfile$waldtest <- parse_cmd(waldtest, collapse = ";\n", use.names = TRUE)
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

#' Normalize Blimp syntax value - add semicolons intelligently
#' @noRd
normalize_syntax_value <- function(value) {
    # Handle multi-line values
    if (grepl("\n", value)) {
        lines <- strsplit(value, "\n")[[1]]

        # Filter empty lines
        lines <- lines[trimws(lines) != ""]

        # Find minimum indentation level (to preserve relative nesting)
        min_indent <- Inf
        for (line in lines) {
            # Convert tabs to spaces
            line <- gsub("\t", "    ", line)
            # Count leading spaces
            leading <- nchar(line) - nchar(trimws(line, which = "left"))
            if (leading < min_indent && trimws(line) != "") {
                min_indent <- leading
            }
        }
        if (min_indent == Inf) min_indent <- 0

        normalized <- character(length(lines))
        for (j in seq_along(lines)) {
            line <- lines[j]

            # Convert tabs to 4 spaces for consistency
            line <- gsub("\t", "    ", line)

            # Get relative indentation (beyond minimum)
            leading <- nchar(line) - nchar(trimws(line, which = "left"))
            relative_indent <- leading - min_indent
            content <- trimws(line)

            # Base indentation (4 spaces) + relative indentation
            full_indent <- paste0("    ", strrep(" ", relative_indent))

            # Add semicolon to last line if it doesn't have one
            if (j == length(lines) && !endsWith(content, ";")) {
                normalized[j] <- paste0(full_indent, content, ";")
            } else {
                normalized[j] <- paste0(full_indent, content)
            }
        }

        return(paste(normalized, collapse = "\n"))
    }

    # Single-line value - return as is
    return(value)
}


#' Convert `blimp_syntax` to character vector
#' @param x A `blimp_syntax` object
#' @param ... Additional arguments (unused)
#' @importFrom utils packageVersion
#' @export
as.character.blimp_syntax <- function(x, ...) {
    # Normalize and format each command
    commands <- character(length(x))
    for (i in seq_along(x)) {
        cmd_name <- toupper(names(x)[i])
        value <- as.character.default(x[[i]])

        # Normalize the value
        value <- normalize_syntax_value(value)

        # Multi-line commands: put command name on its own line
        if (grepl("\n", value)) {
            commands[i] <- paste0(cmd_name, ':\n', value)
        } else {
            # Single-line commands: check if value already ends with semicolon
            if (endsWith(trimws(value), ";")) {
                commands[i] <- paste0(cmd_name, ': ', value)
            } else {
                commands[i] <- paste0(cmd_name, ': ', value, ';')
            }
        }
    }

    paste0(
        paste0(
            '# Input generated by rblimp ',
            utils::packageVersion('rblimp') |> paste0(collapse = '.'),
            ' on ', Sys.time() |> format(), '\n\n', coallpse =''
        ),
        paste(commands, collapse = "\n"),
        collapse = ''
    )
}

#' Print `blimp_syntax`
#' @param x A `blimp_syntax` object
#' @param ... Additional arguments (unused)
#' @export
print.blimp_syntax <- function(x, ...) {
    cat(as.character(x))
    invisible(x)
}


