## Runs blimp
# Copyright Brian Keller 2022, all rights reserved

#' @importFrom graphics abline axis dotchart hist lines points
#' @importFrom methods new show
#' @importFrom stats coef median model.matrix ppoints pt qnorm quantile resid sd start vcov
#' @importFrom utils read.csv read.table write.csv


#' @export
rblimp <- function(model,
                   data,
                   burn = 1000,
                   iter = 1000,
                   seed,
                   thin,
                   nimps,
                   latent,
                   randomeffect,
                   parameters,
                   clusterid,
                   ordinal,
                   nominal,
                   center,
                   chains,
                   simple,
                   waldtest,
                   options,
                   transform,
                   fixed,
                   output,
                   tmpfolder,
                   print_output = TRUE,
                   nopowershell = FALSE) {

    # TODO Check burn, iter (remove defaults)

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

    # Write data to temp folder
    write.csv(data, file.path(tmpfolder, "data.csv"), row.names = F, quote = F)

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
        ordinal,
        nominal,
        transform,
        fixed,
        center,
        chains,
        simple,
        waldtest,
        options,
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

    # Parse output to create header
    if (!missing(output)) {
        tmp <- tolower(output) # Convert to lower case
        tmp <- do.call("c", strsplit(tmp, ";|\\s")) # Split by space  or semicolon

        if ("default" %in% tmp) {
            output_header <- c("Median", "StdDev", "2.5%", "97.5%", "PSR", "N_Eff")
        } else if ("default_median" %in% tmp) {
            output_header <- c("Median", "MAD SD", "2.5%", "97.5%", "PSR", "N_Eff")
        } else if ("default_mean" %in% tmp) {
            output_header <- c("Mean", "StdDev", "2.5%", "97.5%", "PSR", "N_Eff")
        } else {
            output_header <- character()
            for (s in tmp) {
                if (s == "") {
                    next
                } # skip empty
                else if (s == "mean") {
                    col_name <- "Mean"
                } else if (s == "median") {
                    col_name <- "Median"
                } else if (s == "madsd" | s == "mad_sd" | s == "mad") {
                    col_name <- "MAD SD"
                } else if (s == "stddev" | s == "sd") {
                    col_name <- "StdDev"
                } else if (s == "q95" | s == "quant95") {
                    col_name <- c("2.5%", "97.5%")
                } else if (s == "q50" | s == "quant50") {
                    col_name <- c("25%", "75%")
                } else if (s == "quantiles" | s == "quantile" | s == "quant") {
                    col_name <- c("2.5%", "25%", "50%", "75%", "97.5%")
                } else if (s == "psr") {
                    col_name <- "PSR"
                } else if (s == "n_eff" | s == "ess" | s == "neff") {
                    col_name <- "N_Eff"
                } else if (s == "mcmcse" | s == "mcmc_se") {
                    col_name <- "MCMC_SE"
                } else {
                    stop(paste("Could not pasre OUTPUT command:", output))
                }
                output_header <- c(output_header, col_name)
            }
        }
    } else {
        output_header <- c("Median", "StdDev", "2.5%", "97.5%", "PSR", "N_Eff")
    }

    # Read parameter labels
    # parse labels file and create figure titles
    lab2 <- lab <- read.table(file.path(tmpfolder, "plots", "labels.dat"))[-1, ]
    lab$V2[lab$V3 == "Intercept" & lab$V2 != "Odds Ratio"] <- "intercept"
    lab$V2[lab$V2 == "Beta" | grepl("Level-", lab$V2, fixed = T) & (lab$V3 != "Residual Var.")] <- "regressed on"
    lab$V2[lab$V2 == "Standardized Beta" | grepl("Level-", lab$V2, fixed = T) & (lab$V3 != "Residual Var.")] <- "regressed on (standardized)"
    lab$V2[lab$V2 == "Variance" & lab$V3 == "L2 Intercept (i)"] <- "level-2 intercept variance"
    lab$V2[lab$V2 == "Variance" & lab$V3 == "L3 Intercept (i)"] <- "level-3 intercept variance"
    lab$V2[grepl("L2 ", lab$V3, fixed = T) & endsWith(lab$V3, ", Intercept")] <- "level-2 intercept covariance with"
    lab$V2[grepl("L3 ", lab$V3, fixed = T) & endsWith(lab$V3, ", Intercept")] <- "level-3 intercept covariance with"
    lab$V2[grepl(",", lab$V3, fixed = T) & grepl("L2 ", lab$V3, fixed = T) & lab$V2 == "Variance"] <- "level-2 covariance between"
    lab$V2[grepl(",", lab$V3, fixed = T) & grepl("L3 ", lab$V3, fixed = T) & lab$V2 == "Variance"] <- "level-3 covariance between"
    lab$V2[grepl("L2 ", lab$V3, fixed = T) & lab$V2 == "Variance"] <- "level-2 slope variance of"
    lab$V2[grepl("L3 ", lab$V3, fixed = T) & lab$V2 == "Variance"] <- "level-3 slope variance of"
    lab$V2[lab$V2 == "Level-1" & lab$V3 == "Residual Var."] <- "level-1 residual variance"
    lab$V2[lab$V2 == "Level-2" & lab$V3 == "Residual Var."] <- "level-2 residual variance"
    lab$V2[lab$V2 == "Level-3" & lab$V3 == "Residual Var."] <- "level-3 residual variance"
    lab$V2[lab$V2 == "Variance" & lab$V3 == "Residual Var."] <- "residual variance"
    r2sel <- lab$V2 == "R2"
    lab$V2[r2sel] <- paste("R2:", lab$V3[r2sel])
    lab$V3[r2sel] <- ""
    lab$V3 <- gsub("\\|", "dummy code", lab$V3)
    delete <- c("Grand Mean", "Variance", "Residual Var.", "Tau", "L2 Intercept (i)", "L3 Intercept (i)", "L2 (i),", "L3 (i),", "L2: ", "L3: ", "L2", "L3", ", Intercept", "Intercept")
    for (i in seq_along(delete)) lab$V3 <- gsub(delete[i], "", lab$V3, fixed = T)
    # Deal with odds ratio
    lab$V3[lab$V2 == "Odds Ratio" & lab$V3 == ""] <- "intercept"
    lab$V2[lab$V2 == "Odds Ratio"] <- "regressed on (odds ratio)"
    lab$V2 <- tolower(lab$V2)
    # Fix all to lower
    lab_names <- vector("character", nrow(lab))
    # Parse parameters
    param_select <- lab$V1 == lab$V2 & lab$V2 == lab$V3
    lab$V1[param_select] <- "Parameter:"
    lab$V3[param_select] <- ""
    for (i in seq_along(lab_names)) {
        lab_names[i] <- paste(lab[i, 1], lab[i, 2], lab[i, 3], sep = ".")
    }

    # Row Name parsing
    lab2$V2[lab2$V2 == "Beta" & lab2$V3 == "Intercept"] <- "~ Intercept"
    lab2$V2[lab2$V2 == "Beta" | grepl("Level-", lab2$V2, fixed = T) & (lab2$V3 != "Residual Var.")] <- "~"
    lab2$V3[lab2$V2 == "Standardized Beta" | grepl("Level-", lab2$V2, fixed = T) & (lab2$V3 != "Residual Var.")] <-
        paste0(lab2$V3[lab2$V2 == "Standardized Beta" | grepl("Level-", lab2$V2, fixed = T) & (lab2$V3 != "Residual Var.")], " (standardized)")
    lab2$V2[lab2$V2 == "Standardized Beta" | grepl("Level-", lab2$V2, fixed = T) & (lab2$V3 != "Residual Var.")] <- "~"
    lab2$V2[lab2$V2 == "Variance" & lab2$V3 == "L2 Intercept (i)"] <- "level-2 intercept variance"
    lab2$V2[lab2$V2 == "Variance" & lab2$V3 == "L3 Intercept (i)"] <- "level-3 intercept variance"
    lab2$V2[grepl(",", lab2$V3, fixed = T) & grepl("L2 ", lab2$V3, fixed = T) & lab2$V2 == "Variance"] <- "level-2 covariance between"
    lab2$V2[grepl(",", lab2$V3, fixed = T) & grepl("L3 ", lab2$V3, fixed = T) & lab2$V2 == "Variance"] <- "level-3 covariance between"
    lab2$V2[grepl("L2 ", lab2$V3, fixed = T) & endsWith(lab2$V3, ", Intercept")] <- "level-2 intercept covariance with"
    lab2$V2[grepl("L3 ", lab2$V3, fixed = T) & endsWith(lab2$V3, ", Intercept")] <- "level-3 intercept covariance with"
    lab2$V2[grepl("L2 ", lab2$V3, fixed = T) & lab2$V2 == "Variance"] <- "level-2 slope variance of"
    lab2$V2[grepl("L3 ", lab2$V3, fixed = T) & lab2$V2 == "Variance"] <- "level-3 slope variance of"
    lab2$V2[lab2$V2 == "Level-1" & lab2$V3 == "Residual Var."] <- "level-1 residual variance"
    lab2$V2[lab2$V2 == "Level-2" & lab2$V3 == "Residual Var."] <- "level-2 residual variance"
    lab2$V2[lab2$V2 == "Level-3" & lab2$V3 == "Residual Var."] <- "level-3 residual variance"
    lab2$V2[lab2$V2 == "Variance" & lab2$V3 == "Residual Var."] <- "residual variance"
    r2sel <- lab2$V2 == "R2"
    lab2$V2[r2sel] <- paste("R2:", lab2$V3[r2sel])
    lab2$V3[r2sel] <- ""
    lab2$V3 <- gsub("\\|", "dummy code", lab2$V3)
    delete <- c("Grand Mean", "Variance", "Residual Var.", "Tau", "L2 Intercept (i)", "L3 Intercept (i)", "L2 (i),", "L3 (i),", "L2: ", "L3: ", "L2", "L3", ", Intercept", "Intercept")
    for (i in seq_along(delete)) lab2$V3 <- gsub(delete[i], "", lab2$V3, fixed = T)

    # Deal with odds ratio
    lab2$V3[lab2$V2 == "Odds Ratio" & lab2$V3 == ""] <- "Intercept"
    lab2$V3[lab2$V2 == "Odds Ratio"] <- paste0(lab2$V3[lab2$V2 == "Odds Ratio"], " (odds ratio)")
    lab2$V2[lab2$V2 == "Odds Ratio"] <- "~"

    lab_row_names <- vector("character", nrow(lab2))
    # Parse parameters
    param_select <- lab2$V1 == lab2$V2 & lab2$V2 == lab2$V3
    lab2$V1[param_select] <- "Parameter:"
    lab2$V3[param_select] <- ""
    for (i in seq_along(lab_row_names)) {
        lab_row_names[i] <- paste(lab2[i, 1], lab2[i, 2], lab2[i, 3])
    }

    # Read data in
    output <- list()
    output$estimates <- as.matrix(read.csv(file.path(tmpfolder, "estimates.csv"), header = F))
    colnames(output$estimates) <- output_header
    rownames(output$estimates) <- trimws(lab_row_names)
    output$iterations <- read.csv(file.path(tmpfolder, "iter.csv"), header = F)
    names(output$iterations) <- lab_names
    output$psr <- read.csv(file.path(tmpfolder, "psr.csv"), header = F)
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
        tmp <- read.csv(file.path(tmpfolder, "imps.csv"), header = T)
        output$imputations <- split(tmp[, seq_len(ncol(data)) + 1, drop = F], tmp[, 1])
        output$predicted <- split(tmp[, endsWith(names(tmp), ".predicted") | endsWith(names(tmp), ".probability"), drop = F], tmp[, 1])
        output$residuals <- split(tmp[, endsWith(names(tmp), ".residual") |
                                          (endsWith(names(tmp), ".") & names(tmp) != "imp."), drop = F], tmp[, 1])
        output$latent <- split(tmp[, endsWith(names(tmp), ".latent"), drop = F], tmp[, 1])
    } else {
        output$predicted <- list()
        output$imputations <- list()
        output$latent <- list()
        output$residuals <- list()
    }

    # Get average imputation
    if (file.exists(file.path(tmpfolder, "avgimp.csv"))) {
        output$average_imp <- read.csv(file.path(tmpfolder, "avgimp.csv"), header = T)
    } else {
        output$average_imp <- data.frame()
    }
    # Get variance of imputation
    if (file.exists(file.path(tmpfolder, "varimp.csv"))) {
        output$variance_imp <- read.csv(file.path(tmpfolder, "varimp.csv"), header = T)
    } else {
        output$variance_imp <- data.frame()
    }

    # Delete temp files
    if (missing(tmpfolder)) unlink(tmpfolder)

    # Return output
    return(
        new("blimp_obj",
            call = match.call(), estimates = output$estimates, burn = output$burn, iterations = output$iterations,
            psr = output$psr, imputations = output$imputations, average_imp = output$average_imp,
            variance_imp = output$variance_imp, latent = output$latent, residuals = output$residuals,
            predicted = output$predicted, output = result
        )
    )
}
