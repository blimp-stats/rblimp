# Wrapper for fcs
#' @rdname rblimp
#' @param variables list of variables to be included in FCS procedure
#' @export
rblimp_fcs <- function(variables,
                       data,
                       burn = 10000,
                       iter = 10000,
                       seed,
                       thin,
                       nimps,
                       clusterid,
                       ordinal,
                       nominal,
                       chains,
                       options,
                       transform,
                       fixed,
                       output,
                       tmpfolder,
                       add_save = TRUE,
                       print_output = TRUE,
                       nopowershell = FALSE) {

    return(rblimp(
        model = paste0(";FCS: ", paste(variables, collapse = " "), ";"),
        data = data,
        burn = burn,
        iter = iter,
        seed = seed,
        thin = thin,
        nimps = nimps,
        clusterid = clusterid,
        ordinal = ordinal,
        nominal = nominal,
        chains = chains,
        options = options,
        transform = transform,
        tmpfolder = tmpfolder,
        fixed = fixed,
        output = output,
        print_output = print_output,
        nopowershell = nopowershell
    ))
}
