# Wrapper for fcs
#' @export
rblimp_fcs <- function(variables,
                       data,
                       burn = 1000,
                       iter = 1000,
                       seed,
                       thin,
                       nimps,
                       clusterid,
                       ordinal,
                       nominal,
                       chains,
                       options,
                       transform,
                       tmpfolder,
                       fixed,
                       output,
                       print_output = TRUE,
                       nopowershell = FALSE) {
    # TODO Remove default `burn` and `iter`

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
