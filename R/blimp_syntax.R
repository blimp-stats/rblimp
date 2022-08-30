## Functions for parsing and generating blimp syntax
# Copyright Brian Keller 2022, all rights reserved

# Adds a command
add_cmd <- function(cmd,x,y, collapse = ' ') {
    if(is.null(y)) return (x)
    if(y[1] == '') return (x)
    return(paste0(x,cmd,paste(y,collapse=collapse),";\n"))
}

# Parse formula to blimp syntax
parse_formula <- function(x) {
    # Check for formula
    if (!is.call(x) && x[[1]] != quote(`~`)) return(x)
    return(paste(attr(terms(x) , "term.labels"), collapse = ' '))

}
## Check for formula
is.formula <- function(x) is.call(x) && x[[1]] == quote(`~`)

## Get name from formula
form_name <- function(x) {
    if (!is.formula(x)) return('')
    if (length(x) == 2) return('')
    return(as.character(x[[2]]))
}

## Parse names
parse_names <- function(x) {
    o <- vector('character', length(x))
    for(i in seq_along(x)) {
        o[i] <- if(is.null(names(x[i]))) {
            form_name(x[[i]])
        }
        else if (names(x[i]) == '') {
            form_name(x[[i]])
        } else {
            names(x[i])
        }
    }
    return(o)
}

## Blimp Syntax generation
blimp_syntax <- function(
        datapath,
        model,
        burn,
        seed,
        iter,
        variables,
        thin,
        nimps,
        latent,
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

    if (!missing(datapath))   inputfile <- add_cmd('data: ','', datapath)
    if (!missing(burn))       inputfile <- add_cmd('burn: ',inputfile, burn)
    if (!missing(thin))       inputfile <- add_cmd('thin: ',inputfile, thin)
    if (!missing(seed))       inputfile <- add_cmd('seed: ',inputfile, seed)
    if (!missing(iter))       inputfile <- add_cmd('iter: ',inputfile, iter)
    if (!missing(nimps))      inputfile <- add_cmd('nimps: ',inputfile, nimps)
    if (!missing(latent))     inputfile <- add_cmd('latent: ',inputfile, latent)
                              inputfile <- add_cmd('missing: ',inputfile, 'NA')
    if (!missing(clusterid))  inputfile <- add_cmd('clusterid: ',inputfile, clusterid)
    if (!missing(ordinal))    inputfile <- add_cmd('ordinal: ',inputfile, ordinal)
    if (!missing(nominal))    inputfile <- add_cmd('nominal: ',inputfile, nominal)
    if (!missing(model))      inputfile <- add_cmd('model: ',inputfile, model, ';')
    if (!missing(parameters)) inputfile <- add_cmd('parameters: ',inputfile, parameters, ';')
    if (!missing(centering))  inputfile <- add_cmd('center: ',inputfile, centering)
    if (!missing(chains))     inputfile <- add_cmd('chains: ',inputfile, chains)
    if (!missing(fixed))      inputfile <- add_cmd('fixed: ',inputfile, fixed)
    if (!missing(simple))     inputfile <- add_cmd('simple: ',inputfile, simple,';')
    if (!missing(waldtest))   inputfile <- add_cmd('waldtest: ',inputfile, waldtest, ';')
    if (!missing(options))    inputfile <- add_cmd('options: ',inputfile, options)
    if (!missing(output))     inputfile <- add_cmd('output: ', inputfile, output)
    if (!missing(transform))  inputfile <- add_cmd('transform: ',inputfile, transform,';')
    if (!missing(save))       inputfile <- add_cmd('save: ',inputfile, save, ';')
    ## Return file
    return(inputfile)

}
