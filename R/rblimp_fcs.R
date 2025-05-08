# Wrapper for fcs
#' @rdname rblimp
#' @export
rblimp_fcs <- function(variables,
                       data,
                       burn,
                       iter,
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
                       save_add = TRUE,
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
