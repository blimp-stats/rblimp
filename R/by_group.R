# Copyright Brian Keller 2024, all rights reserved

# Set bygroup class
setOldClass("blimp_bygroup")

#' Wrapper function to run multiple instances of [`rblimp`] based on grouping variable
#' @description
#' `rblimp` will generate the input, run the script, and load most the saved data into an R object. `rblimp_fcs`
#' is used to specify the FCS command in Blimp. `rblimp_syntax` will generate the Blimp syntax file only.
#' @param expr a call to [`rblimp`] or [`rblimp_fcs`].
#' @param group a character vector or index pointing to grouping variable in data set
#' @details
#' Separates by grouping to run blimp command on each individual sub data set.
#' @returns a list of all blimp runs by group with class of `blimp_bygroup`
#' @seealso [`as.mitml`]
#' @examples
#' # Generate Data
#' mydata <- data.frame(
#'     x = rnorm(1000),
#'     y = rnorm(1000),
#'     group = rbinom(1000, 1, .5)
#' )
#'
#' # Nonsensical model
#' mdl <- rblimp_fcs(
#'     c('x', 'y'),
#'     mydata,
#'     seed = 3927,
#'     nimps = 2,
#' ) |> by_group('group')
#'
#' # Analyze each imputation
#' results <- with(mdl, lm(y ~ x))
#' # use `mitml` package to pool results
#'
#' @export
by_group <- function(expr, group) {
    func_call <- substitute(expr)
    fn <- func_call[[1]]
    func_call <- match.call(eval(fn), func_call)
    dataset <- eval(func_call$data, parent.frame())
    if (NROW(group) == 1)  group <- dataset[,group]
    split(dataset, group) |> lapply(\(x) {
        func_call$data <- x
        eval(func_call)
    }) |> structure(
        class = 'blimp_bygroup',
        group = group,
        nimps = func_call$nimps
    )
}

