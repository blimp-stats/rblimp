## Runs blimp
# Copyright Brian Keller 2024, all rights reserved

#' Wrapper functions to provide Blimp functions in R
#' @description
#' `rblimp` will generate the input, run the script, and load most the saved data into an R object. `rblimp_fcs`
#' is used to specify the FCS command in Blimp. `rblimp_syntax` will generate the Blimp syntax file only.
#' @param model a character string or vector/list of character strings. Specifies Blimp's MODEL command. See details.
#' @param data a [`data.frame`]. The data set used by Blimp
#' @param burn an integer. The number of burn-in iterations to be run
#' @param iter an integer. The number of post burn-in iterations to be run
#' @param seed a positive integer. The seeding value for Blimp's pseudo random number generator
#' @param thin an integer. The thinning interval for imputations only.
#' @param nimps an integer. The number of imputations to save.
#' @param latent a character string, formula, or vector/list of character strings. Specifies Blimp's LATENT command. See details.
#' @param randomeffect a character string or vector/list of character strings. Specifies Blimp's RANDOMEFFECTS command.
#' @param parameters a character string or vector/list of character strings. Specifies Blimp's MODEL command. See details.
#' @param clusterid a character string, formula, or vector/list of character strings. Specifies Blimp's CLUSTERID command. See details.
#' @param weight a character string, formula, or vector/list of character strings. Specifies Blimp's WEIGHT command. See details.
#' @param ordinal a character string, formula, or vector/list of character strings. Specifies Blimp's ORDINAL command. See details.
#' @param nominal a character string, formula, or vector/list of character strings. Specifies Blimp's NOMINAL command. See details.
#' @param count a character string, formula, or vector/list of character strings. Specifies Blimp's COUNT command. See details.
#' @param center a character string, formula, or vector/list of character strings. Specifies Blimp's CENTER command. See details.
#' @param chains an integer, character string, or vector/list of character strings. Specifies Blimp's CHAINS command.
#' @param simple  a character string or vector/list of character strings. Specifies Blimp's SIMPLE command. See details.
#' @param waldtest  a character string or vector/list of character strings. Specifies Blimp's WALDTEST command. See details.
#' @param options a character string or vector/list of character strings. Specifies Blimp's OPTIONS command.
#' @param transform a character string or vector/list of character strings. Specifies Blimp's TRANSFORM command.
#' @param filter a character string. Specifies Blimp's FILTER command.
#' @param fixed a character string, formula, or vector/list of character strings. Specifies Blimp's FIXED command. See details.
#' @param output a character string or vector/list of character strings. Specifies Blimp's OUTPUT command
#' @param tmpfolder a character string. If specified `rblimp` will use the given
#' file path as a temporary directory instead of creating one with [`tempdir`]
#' @param add_save a single logical value or a list of logical values.
#' If `TRUE` then saveLatent, saveResidual, and savePredicted will be included in OPTIONS command.
#' Otherwise, it will be coerced to a list. The elements of the list should labeled `latent`, `residual`, and `predicted`
#' each containing a single logical value that can be used to toggle on or off them individually.
#' Missing elements will be defaulted to `TRUE`.
#' @param print_output The type of output printed to the console.
#' `'iteration'` or logical `TRUE` is only iteration history, `'none'` or logical `FALSE`
#' suppresses all output to console, and `'all'` prints all output to console.
#' @param nopowershell Windows only. Uses cmd.exe with some limited functions (instead of powershell).
#' @details
#' The above functions require knowledge of specifying Blimp commands. Blimp's syntax commands are
#' documented in the \href{https://docs.google.com/document/d/1D3MS79CakuX9mVVvGH13B5nRd9XLttp69oGsvrIRK64/edit?usp=sharing}{Blimp User Manual}
#'
#' By default, these commands can be inputted as character strings that will be used to generate the syntax.
#' For multiple lined commands, you can wrap multiple strings into a character [`vector`] or a [`list`].
#' The appropriate semicolons will be specified, so they are not required in any character strings.
#' If specifying a character [`vector`] or a [`list`] for the `model`, providing names to each element
#' will be used as blocks in Blimp's model syntax. Similarly, specifying named elements can be used
#' for the `center` command to specify if you would like centering within a cluster or
#' grand mean centering. This also works for the `latent` input when requesting latent
#' variables at a specific cluster identifier.
#' See the \href{https://docs.google.com/document/d/1D3MS79CakuX9mVVvGH13B5nRd9XLttp69oGsvrIRK64/edit?usp=sharing}{Blimp User Manual}
#' for more details about types of centering and specifying latent variables.
#'
#' In addition, R's formula syntax can be used to specify lists of variables
#' in place of character strings. For example, specifying the CLUSTERID command with
#' the variable `id` can be specified as ` ~ id` as opposed to a character string.
#' Similarly, `+` can be used to specify multiple variables. For example, to specify
#' two variables as ordinal the formula would be: `~ x1 + x2`. Finally, this can also be
#' used to specify centering and latent variables. For example, to center `x1` and `x2`
#' within cluster we can specify: `cwc ~ x1 + x2`.
#'
#' Running `rblimp` will also check if blimp is up to date.
#' See details in [`rblimp_source`] for more information.
#' @returns [`blimp_obj`]
#' @importFrom graphics abline axis dotchart hist lines points
#' @importFrom methods new show
#' @importFrom stats coef median model.matrix ppoints pt qnorm quantile resid sd start vcov
#' @importFrom utils read.csv read.table write.csv
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
#' # View results
#' summary(mdl)
#' @export
rblimp <- function(model,
                   data,
                   burn = 10000,
                   iter = 10000,
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
                   center,
                   chains,
                   simple,
                   waldtest,
                   options,
                   transform,
                   filter,
                   fixed,
                   output,
                   tmpfolder,
                   add_save = TRUE,
                   print_output = TRUE,
                   nopowershell = FALSE) {

    # Check inputs
    if (length(burn) != 1 || !is.numeric(burn) || burn < 0) throw_error(
        "{.arg burn} must be a positive numeric value"
    )
    if (length(iter) != 1 || !is.numeric(iter) || iter < 0) throw_error(
        "{.arg iter} must be a positive numeric value"
    )
    if (!missing(nimps)) {
        if (length(nimps) != 1 || !is.numeric(nimps) || nimps < 0) throw_error(
            "{.arg nimps} must be a positive numeric value"
        )
    }

    # Check output
    if (!is.logical(print_output)) {
        if (print_output != "all" & print_output != "none" & print_output != "iteration") {
            throw_error("Unrecognized {.arg output} choice: {print_output}")
        }
    } else {
        if (print_output) {
            print_output <- "iteration"
        } else {
            print_output <- "none"
        }
    }

    # Get temp folder if needed
    if (missing(tmpfolder)) {
        tmpfolder <- tempfile()
        if (!dir.create(tmpfolder)) throw_error(
            "Was unable to create temporary directory."
        )
    }

    # Check if data.frame
    if (!is.data.frame(data)) throw_error(
        "The {.arg data} must be a data.frame"
    )
    # Convert to data frame if a tibble
    if ("tbl_df" %in% class(data)) {
        cli::cli_alert_warning("Converting data to `data.farme`")
        data <- as.data.frame(data)
    }

    # Get attributes loop over and convert to numeric
    att_list <- vector('list', NCOL(data))
    for (i in seq_along(att_list)) {
        if (!is.numeric(data[, i])) {
            att_list[[i]] <- attributes(data[,i])
            data[, i] <- as.numeric(data[, i])
        }
    }

    # Write data to temp folder
    write.csv(data, file.path(tmpfolder, "data.csv"), row.names = FALSE, quote = FALSE)

    # Create saveCommand
    saveCmd <- vector('list', 5L)
    saveCmd[[1]] <- "estimates = estimates.csv"
    saveCmd[[2]] <- "iterations = iter.csv"
    saveCmd[[3]] <- "psr = psr.csv"
    saveCmd[[4]] <- "avgimp = avgimp.csv"
    saveCmd[[5]] <- "varimp = varimp.csv"
    if (!missing(nimps)) saveCmd[[length(saveCmd) + 1]] <- "stacked = imps.csv"
    if (!missing(waldtest)) saveCmd[[length(saveCmd) + 1]] <- "waldtest = waldtest.csv"
    if (!missing(simple)) saveCmd[[length(saveCmd) + 1]] <- "simple = simple.csv"

    ## append to options
    if (is.logical(add_save) && length(add_save) == 1) {
        if (add_save) {
            if (missing(options)) options <- NULL
            options <- c(options, "savepredicted savelatent saveresidual")
        }
    }
    else {
        add_save <- as.list(add_save)
        if (!is.null(add_save$predicted)) {
            if (add_save$predicted) {
                if (missing(options)) options <- NULL
                options <- c(options, "savepredicted")
            }
        } else {
            if (missing(options)) options <- NULL
            options <- c(options, "savepredicted")
        }
        if (!is.null(add_save$latent)) {
            if (add_save$latent) {
                if (missing(options)) options <- NULL
                options <- c(options, "savelatent")
            }
        } else {
            if (missing(options)) options <- NULL
            options <- c(options, "savelatent")
        }
        if (!is.null(add_save$residual)) {
            if (add_save$residual) {
                if (missing(options)) options <- NULL
                options <- c(options, "saveresidual")
            }
        } else {
            if (missing(options)) options <- NULL
            options <- c(options, "saveresidual")
        }
    }

    # Write input file
    imp_file <- rblimp_syntax(
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
        saveCmd,
        output
    )

    # Change file path
    imp_file$data <- file.path(tmpfolder, "data.csv")
    # Remove variables
    imp_file$variables <- NULL

    # Write imp file
    fileConn <- file(file.path(tmpfolder, "input.imp"))
    writeLines(as.character(imp_file), fileConn)
    close(fileConn)

    # Run File
    result <- rblimp_source(
        file.path(tmpfolder, "input.imp"),
        plots = TRUE,
        print_output,
        nopowershell
    )
    exitcode <- attr(result, "exitcode")

    # Check exit code
    if (length(exitcode) == 1) {
        if (exitcode == "1") {
            if (missing(tmpfolder)) unlink(tmpfolder)
            throw_error("Blimp had an error. Check output.")
        }
    }

    # Read parameter labels
    lab2 <- lab <- read.table(file.path(tmpfolder, "plots", "labels.dat"))[-1, ]
    # Set outcome, parameter, and block type
    oname <- lab$V1
    ptype <- lab$V2
    block <- lab$V5
    # Handle predictor models
    lab$V3[startsWith(ptype, "Level-")] |>
        sapply(\(x) {
            if(grepl('Var\\(|(Residual Var\\.)', x)) return('Variance')
            else if (grepl('~', x)) return('Beta')
            else if (grepl('Threshold', x)) return('Threshold')
            else if (startsWith(x, 'Grand Mean#')) return('Grand Mean')
            return ('Beta')
        }) -> ptype[startsWith(ptype, "Level-")]
    ptype[startsWith(lab$V3, 'Grand Mean#')] <- 'Grand Mean'

    # parse labels file and create figure titles
    lab$V2[lab$V3 == "Intercept" & lab$V2 != "Odds Ratio"] <- "intercept"
    lab$V2[lab$V2 == "Beta" | grepl("Level-", lab$V2, fixed = TRUE) & (lab$V3 != "Residual Var.")] <- "regressed on"
    lab$V2[lab$V2 == "Standardized Beta" | grepl("Level-", lab$V2, fixed = TRUE) & (lab$V3 != "Residual Var.")] <- "regressed on (standardized)"
    lab$V2[lab$V2 == "Variance" & lab$V3 == "L2 Intercept (i)"] <- "level-2 intercept variance"
    lab$V2[lab$V2 == "Variance" & lab$V3 == "L3 Intercept (i)"] <- "level-3 intercept variance"
    lab$V2[lab$V2 == "Standard Deviation" & lab$V3 == "L2 Intercept (i)"] <- "level-2 intercept SD"
    lab$V2[lab$V2 == "Standard Deviation" & lab$V3 == "L3 Intercept (i)"] <- "level-3 intercept SD"
    lab$V2[grepl("L2 ", lab$V3, fixed = TRUE) & endsWith(lab$V3, ", Intercept") & lab$V2 == "Variance"] <- "level-2 intercept covariance with"
    lab$V2[grepl("L3 ", lab$V3, fixed = TRUE) & endsWith(lab$V3, ", Intercept") & lab$V2 == "Variance"] <- "level-3 intercept covariance with"
    lab$V2[grepl(",", lab$V3, fixed = TRUE) & grepl("L2 ", lab$V3, fixed = TRUE) & lab$V2 == "Variance"] <- "level-2 covariance between"
    lab$V2[grepl(",", lab$V3, fixed = TRUE) & grepl("L3 ", lab$V3, fixed = TRUE) & lab$V2 == "Variance"] <- "level-3 covariance between"
    lab$V2[grepl("L2 ", lab$V3, fixed = TRUE) & lab$V2 == "Variance"] <- "level-2 slope variance of"
    lab$V2[grepl("L3 ", lab$V3, fixed = TRUE) & lab$V2 == "Variance"] <- "level-3 slope variance of"
    lab$V2[grepl("L2 ", lab$V3, fixed = TRUE) & endsWith(lab$V3, ", Intercept") & lab$V2 == "Standard Deviation"] <- "level-2 intercept correlation with"
    lab$V2[grepl("L3 ", lab$V3, fixed = TRUE) & endsWith(lab$V3, ", Intercept") & lab$V2 == "Standard Deviation"] <- "level-3 intercept correlation with"
    lab$V2[grepl(",", lab$V3, fixed = TRUE) & grepl("L2 ", lab$V3, fixed = TRUE) & lab$V2 == "Standard Deviation"] <- "level-2 correlation between"
    lab$V2[grepl(",", lab$V3, fixed = TRUE) & grepl("L3 ", lab$V3, fixed = TRUE) & lab$V2 == "Standard Deviation"] <- "level-3 correlation between"
    lab$V2[grepl("L2 ", lab$V3, fixed = TRUE) & lab$V2 == "Standard Deviation"] <- "level-2 slope SD of"
    lab$V2[grepl("L3 ", lab$V3, fixed = TRUE) & lab$V2 == "Standard Deviation"] <- "level-3 slope SD of"
    lab$V2[lab$V2 == "Level-1" & lab$V3 == "Residual Var."] <- "level-1 residual variance"
    lab$V2[lab$V2 == "Level-2" & lab$V3 == "Residual Var."] <- "level-2 residual variance"
    lab$V2[lab$V2 == "Level-3" & lab$V3 == "Residual Var."] <- "level-3 residual variance"
    lab$V2[lab$V2 == "Variance" & lab$V3 == "Residual Var."] <- "residual variance"
    lab$V2[lab$V2 == "Standard Deviation" & lab$V3 == "Residual SD"] <- "residual SD"
    r2sel <- lab$V2 == "R2"
    lab$V2[r2sel] <- paste("R2:", lab$V3[r2sel])
    lab$V3[r2sel] <- ""
    lab$V3 <- gsub("\\|", "dummy code", lab$V3)
    delete <- c(
        "Grand Mean", "Variance", "Residual Var.", "Tau", "L2 Intercept (i)",
        "L3 Intercept (i)", "L2 (i),", "L3 (i),", "L2: ", "L3: ", "L2", "L3",
        ", Intercept", "Intercept", "Residual SD"
    )
    for (i in seq_along(delete)) lab$V3 <- gsub(delete[i], "", lab$V3, fixed = TRUE)
    # Deal with odds ratio
    lab$V3[lab$V2 == "Odds Ratio" & lab$V3 == ""] <- "intercept"
    lab$V2[lab$V2 == "Odds Ratio"] <- "regressed on (odds ratio)"
    lab$V2 <- tolower(lab$V2)

    # Parse multivariate models
    cov_sel <- lab$V2 == "variance" & startsWith(lab$V3, "Cov(")
    if (any(cov_sel)) {
        lab$V1[cov_sel] <- sapply(strsplit(lab$V1[cov_sel], " "), \(x) paste0(x, collapse = '.'))
        lab$V2[cov_sel] <- "covariance"
        lab$V3[cov_sel] <- ""
    }

    cor_sel <- lab$V2 == "correlations" & startsWith(lab$V3, "Cor(")
    if (any(cor_sel)) {
        lab$V1[cor_sel] <- sapply(strsplit(lab$V1[cor_sel], " "), \(x) paste0(x, collapse = '.'))
        lab$V3[cor_sel] <- ""
    }

    # Parse parameters
    param_select <- lab$V1 == lab$V2 & lab$V2 == lab$V3
    lab$V1[param_select] <- "Parameter:"
    lab$V3[param_select] <- ""

    # Trim white space
    lab$V3 <- trimws(lab$V3)

    # Set up lab names
    lab_names <- vector("character", nrow(lab))
    for (i in seq_along(lab_names)) {
        lab_names[i] <- paste(lab[i, 1], lab[i, 2], lab[i, 3], sep = ".")
    }
    # Remove trailing period
    lab_names[endsWith(lab_names, ".")] <- gsub(
        ".{1}$", "", lab_names[endsWith(lab_names, ".")]
    )

    # Row Name parsing
    lab2$V2[lab2$V2 == "Beta" & lab2$V3 == "Intercept"] <- "~ Intercept"
    lab2$V2[lab2$V2 == "Beta" | grepl("Level-", lab2$V2, fixed = TRUE) & (lab2$V3 != "Residual Var.")] <- "~"
    lab2$V3[lab2$V2 == "Standardized Beta" | grepl("Level-", lab2$V2, fixed = TRUE) & (lab2$V3 != "Residual Var.")] <-
        paste0(lab2$V3[lab2$V2 == "Standardized Beta" | grepl("Level-", lab2$V2, fixed = TRUE) & (lab2$V3 != "Residual Var.")], " (standardized)")
    lab2$V2[lab2$V2 == "Standardized Beta" | grepl("Level-", lab2$V2, fixed = TRUE) & (lab2$V3 != "Residual Var.")] <- "~"
    lab2$V2[lab2$V2 == "Variance" & lab2$V3 == "L2 Intercept (i)"] <- "level-2 intercept variance"
    lab2$V2[lab2$V2 == "Variance" & lab2$V3 == "L3 Intercept (i)"] <- "level-3 intercept variance"
    lab2$V2[lab2$V2 == "Standard Deviation" & lab2$V3 == "L2 Intercept (i)"] <- "level-2 intercept SD"
    lab2$V2[lab2$V2 == "Standard Deviation" & lab2$V3 == "L3 Intercept (i)"] <- "level-3 intercept SD"
    lab2$V2[grepl(",", lab2$V3, fixed = TRUE) & grepl("L2 ", lab2$V3, fixed = TRUE) & lab2$V2 == "Variance"] <- "level-2 covariance between"
    lab2$V2[grepl(",", lab2$V3, fixed = TRUE) & grepl("L3 ", lab2$V3, fixed = TRUE) & lab2$V2 == "Variance"] <- "level-3 covariance between"
    lab2$V2[grepl("L2 ", lab2$V3, fixed = TRUE) & endsWith(lab2$V3, ", Intercept") & lab2$V2 == "Variance"] <- "level-2 intercept covariance with"
    lab2$V2[grepl("L3 ", lab2$V3, fixed = TRUE) & endsWith(lab2$V3, ", Intercept") & lab2$V2 == "Variance"] <- "level-3 intercept covariance with"
    lab2$V2[grepl("L2 ", lab2$V3, fixed = TRUE) & lab2$V2 == "Variance"] <- "level-2 slope variance of"
    lab2$V2[grepl("L3 ", lab2$V3, fixed = TRUE) & lab2$V2 == "Variance"] <- "level-3 slope variance of"
    lab2$V2[grepl(",", lab2$V3, fixed = TRUE) & grepl("L2 ", lab2$V3, fixed = TRUE) & lab2$V2 == "Standard Deviation"] <- "level-2 correlation between"
    lab2$V2[grepl(",", lab2$V3, fixed = TRUE) & grepl("L3 ", lab2$V3, fixed = TRUE) & lab2$V2 == "Standard Deviation"] <- "level-3 correlation between"
    lab2$V2[grepl("L2 ", lab2$V3, fixed = TRUE) & endsWith(lab2$V3, ", Intercept") & lab2$V2 == "Standard Deviation"] <- "level-2 intercept correlation with"
    lab2$V2[grepl("L3 ", lab2$V3, fixed = TRUE) & endsWith(lab2$V3, ", Intercept") & lab2$V2 == "Standard Deviation"] <- "level-3 intercept correlation with"
    lab2$V2[grepl("L2 ", lab2$V3, fixed = TRUE) & lab2$V2 == "Standard Deviation"] <- "level-2 slope SD of"
    lab2$V2[grepl("L3 ", lab2$V3, fixed = TRUE) & lab2$V2 == "Standard Deviation"] <- "level-3 slope SD of"
    lab2$V2[lab2$V2 == "Level-1" & lab2$V3 == "Residual Var."] <- "level-1 residual variance"
    lab2$V2[lab2$V2 == "Level-2" & lab2$V3 == "Residual Var."] <- "level-2 residual variance"
    lab2$V2[lab2$V2 == "Level-3" & lab2$V3 == "Residual Var."] <- "level-3 residual variance"
    lab2$V2[lab2$V2 == "Variance" & lab2$V3 == "Residual Var."] <- "residual variance"
    lab2$V2[lab2$V2 == "Standard Deviation" & lab2$V3 == "Residual SD"] <- "residual SD"
    r2sel <- lab2$V2 == "R2"
    lab2$V2[r2sel] <- paste("R2:", lab2$V3[r2sel])
    lab2$V3[r2sel] <- ""
    lab2$V3 <- gsub("\\|", "dummy code", lab2$V3)
    delete <- c(
        "Grand Mean", "Variance", "Residual Var.", "Tau", "L2 Intercept (i)",
        "L3 Intercept (i)", "L2 (i),", "L3 (i),", "L2: ", "L3: ",
        "L2", "L3",", Intercept", "Intercept", "Residual SD"
    )
    for (i in seq_along(delete)) lab2$V3 <- gsub(delete[i], "", lab2$V3, fixed = TRUE)

    # Deal with odds ratio
    lab2$V3[lab2$V2 == "Odds Ratio" & lab2$V3 == ""] <- "Intercept"
    lab2$V3[lab2$V2 == "Odds Ratio"] <- paste0(lab2$V3[lab2$V2 == "Odds Ratio"], " (odds ratio)")
    lab2$V2[lab2$V2 == "Odds Ratio"] <- "~"

    # Parse multivariate models
    cov_sel <- lab2$V2 == "Variance" & startsWith(lab2$V3, "Cov(")
    if (any(cov_sel)) {
        lab2$V2[cov_sel] <- sapply(strsplit(lab2$V1[cov_sel], " "), \(x) paste0(x, collapse = ', '))
        lab2$V1[cov_sel] <- "Cov("
        lab2$V3[cov_sel] <- ")"
    }

    cor_sel <- lab2$V2 == "Correlations" & startsWith(lab2$V3, "Cor(")
    if (any(cor_sel)) {
        lab2$V2[cor_sel] <- sapply(strsplit(lab2$V1[cor_sel], " "), \(x) paste0(x, collapse = ', '))
        lab2$V1[cor_sel] <- "Cor("
        lab2$V3[cor_sel] <- ")"
    }

    # Parse parameters
    param_select <- lab2$V1 == lab2$V2 & lab2$V2 == lab2$V3
    lab2$V1[param_select] <- "Parameter:"
    lab2$V3[param_select] <- ""

    # Trim white space
    lab2$V3 <- trimws(lab2$V3)

    # Set up lab row names
    lab_row_names <- vector("character", nrow(lab2))
    for (i in seq_along(lab_row_names)) {
        lab_row_names[i] <- paste(lab2[i, 1], lab2[i, 2], lab2[i, 3])
    }

    # Read data in
    output <- list()
    output$estimates <- as.matrix(read.csv(file.path(tmpfolder, "estimates.csv"), header = TRUE))
    rownames(output$estimates) <- trimws(lab_row_names)
    colnames(output$estimates) <- gsub('^X', '', colnames(output$estimates))
    colnames(output$estimates) <- gsub('\\.$', '%', colnames(output$estimates))


    output$iterations <- structure(
        read.csv(file.path(tmpfolder, "iter.csv"), header = FALSE),
        parameter_type = ptype
    )
    names(output$iterations) <- lab_names
    # Set up outcome_name
    attr(output$iterations, 'outcome_name') <- oname
    # Set up outcome_name
    attr(output$iterations, 'block_name') <- block
    # Set up parameter_type
    attr(output$iterations, 'parameter_type') <- ptype |> sapply(\(x) {
        switch(
            x,
            Variance = 1,
            Correlations = 1,
            Beta = 2,
            `Grand Mean` = 2,
            `Standardized Beta` = 3,
            `Odds Ratio` = 3,
            R2 = 4,
            Threshold = 5,
            6
        )
    }) |> factor(seq_len(6), labels = c(
        'Var/Cov/Cor',
        'Coefficient',
        'Standardized',
        'Rsquare',
        'Threshold',
        'Other'
    ))

    output$psr <- read.csv(file.path(tmpfolder, "psr.csv"), header = FALSE)
    names(output$psr) <- lab_names

    output$burn <- list()

    files <- list.files(file.path(tmpfolder, "plots"), pattern = "*.csv")
    for (i in seq_along(files)) {
        if (files[i] == "labels.dat") next
        index <- as.numeric(gsub("[A-z \\.\\(\\)]", "", files[i]))
        output$burn[[index]] <- cbind(index, read.table(file.path(tmpfolder, "plots", files[i]), sep = ","))
        names(output$burn[[index]]) <- c("chain", "iteration", lab_names)
    }

    names(output$psr) <- lab_names

    if (file.exists(file.path(tmpfolder, "imps.csv"))) {
        tmp <- read.csv(file.path(tmpfolder, "imps.csv"), header = TRUE)
        # Loop over and add attributes back in
        for (i in seq_along(att_list)) {
            if (!is.null(att_list[[i]])) {
                attributes(tmp[, i + 1]) <-  att_list[[i]]
            }
        }
        # Split imputations
        output$imputations <- split(tmp[, -1, drop = FALSE], tmp[, 1])
    } else {
        output$imputations <- list()
    }

    # Waldtest
    if (file.exists(file.path(tmpfolder, "waldtest.csv"))) {
        output$waldtest <- read.csv(file.path(tmpfolder, "waldtest.csv"), header = TRUE)
    } else {
        output$waldtest <- data.frame(
            test_number = numeric(),
            df = numeric(),
            statistic = numeric(),
            probability = numeric()
        )
    }

    # Simple
    if (file.exists(file.path(tmpfolder, "simple.csv"))) {
        output$simple <- read.csv(file.path(tmpfolder, "simple.csv"), header = TRUE)
        file.path(tmpfolder, "simple.csv") |> readLines(1)  |>
            strsplit('\\",') |> unlist() |>
            gsub('\"', '', x = _) -> names(output$simple)
    } else {
        output$simple <- data.frame()
    }

    # Get average imputation
    if (file.exists(file.path(tmpfolder, "avgimp.csv"))) {
        output$average_imp <- read.csv(file.path(tmpfolder, "avgimp.csv"), header = TRUE)
    } else {
        output$average_imp <- data.frame()
    }
    # Get variance of imputation
    if (file.exists(file.path(tmpfolder, "varimp.csv"))) {
        output$variance_imp <- read.csv(file.path(tmpfolder, "varimp.csv"), header = TRUE)
    } else {
        output$variance_imp <- data.frame()
    }

    # Delete temp files
    if (missing(tmpfolder)) unlink(tmpfolder)

    # Return output
    return(
        new("blimp_obj",
            call = match.call(), estimates = output$estimates, burn = output$burn,
            iterations = output$iterations, psr = output$psr,
            imputations = output$imputations, average_imp = output$average_imp,
            variance_imp = output$variance_imp, waldtest = output$waldtest,
            simple = output$simple, syntax = imp_file, output = result
        )
    )
}
