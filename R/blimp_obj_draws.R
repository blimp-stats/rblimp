## Conversion of blimp_obj to 3-D array, posterior draws formats
# Copyright Brian Keller 2022, all rights reserved

#' Reshape a chain-stacked data.frame of MCMC draws into a 3-D numeric array
#' with dimensions `[iteration, chain, parameter]`.
#' @noRd
chains_to_array_internal <- function(iterations_df, n_chains, param_names) {
    if (nrow(iterations_df) %% n_chains != 0L) {
        throw_error(c(
            "Row count ({nrow(iterations_df)}) is not divisible by chain count ({n_chains}).",
            "i" = "This indicates inconsistent 'Blimp' output; please report as a bug."
        ))
    }

    n_iter  <- nrow(iterations_df) / n_chains
    n_param <- ncol(iterations_df)

    # NOTE: Assumes chain-stacked row ordering from Blimp's iter.csv — rows
    # 1..n_iter are chain 1, (n_iter+1)..2*n_iter are chain 2, etc. R's
    # column-major `dim<-` on the underlying matrix yields
    # arr[i, c, p] = df[(c-1)*n_iter + i, p]. If Blimp ever switches to
    # interleaved output, swap the reshape for:
    #   aperm(array(mat, dim = c(n_chains, n_iter, n_param)), c(2, 1, 3))
    mat <- as.matrix(iterations_df)
    dim(mat) <- c(n_iter, n_chains, n_param)
    dimnames(mat) <- list(
        iterations = NULL,
        chains     = as.character(seq_len(n_chains)),
        parameters = param_names
    )
    mat
}

#' Stack per-chain burn-in data.frames into one chain-stacked data.frame.
#' @noRd
burn_to_df_internal <- function(burn_list) {
    row_counts <- vapply(burn_list, function(df) {
        if (is.null(df)) return(NA_integer_)
        nrow(df)
    }, integer(1))
    if (any(is.na(row_counts))) {
        throw_error("One or more chains are missing warmup data.")
    }
    if (length(unique(row_counts)) != 1L) {
        throw_error(c(
            "Inconsistent warmup row counts across chains: {row_counts}.",
            "i" = "This indicates malformed 'Blimp' output."
        ))
    }

    do.call(rbind, lapply(burn_list, function(df) df[, -(1:2), drop = FALSE]))
}


#' Convert blimp_obj to a 3-D MCMC array
#'
#' @description
#' Reshapes the MCMC draws in a `blimp_obj` into a 3-D numeric array with
#' dimensions `[iteration, chain, parameter]`. This is the format consumed
#' directly by `bayesplot::mcmc_*` functions and by [posterior::as_draws_array()].
#'
#' @details
#' The `warmup` argument controls whether burn-in draws are included:
#' \describe{
#'   \item{`"exclude"`}{(default) returns only post-burn sampling draws.}
#'   \item{`"include"`}{returns warmup draws stacked on top of sampling
#'     draws within each chain. The returned array carries an
#'     `"n_warmup"` attribute giving the number of warmup iterations, so
#'     it can be piped directly into
#'     `bayesplot::mcmc_trace(arr, n_warmup = attr(arr, "n_warmup"))`.}
#'   \item{`"only"`}{returns only warmup draws.}
#' }
#'
#' @param x A `blimp_obj` object.
#' @param warmup One of `"exclude"`, `"include"`, or `"only"`. Default
#'   `"exclude"`.
#' @param ... Additional arguments (unused).
#' @return A numeric 3-D array with dimensions `[iteration, chain, parameter]`,
#'   named `dimnames`, and (when `warmup = "include"`) an `"n_warmup"`
#'   attribute.
#' @seealso [as_draws_array.blimp_obj()] for the [posterior::draws_array]
#'   equivalent.
#' @examplesIf has_blimp()
#' # Generate Data with `rblimp_sim`
#' mydata <- rblimp_sim(
#'     c(
#'         'f ~ normal(0, 1)',
#'         'x1:x5 ~ normal(f, 1)',
#'         'y ~ normal(10 + 0.3*f, 1 - .3^2)'
#'     ),
#'     n = 500,
#'     seed = 19723,
#'     variables = c('y', 'x1:x5')
#' )
#'
#' # Fit model
#' model <- rblimp(
#'     list(structure = 'y ~ f', measurement = 'f -> x1:x5'),
#'     mydata,
#'     chains = 4,
#'     seed = 3927,
#'     latent = ~ f
#' )
#'
#' # Post-burn only (default)
#' arr <- as.array(model)
#' dim(arr)
#' dimnames(arr)$parameters
#'
#' # Include warmup, feed to bayesplot with its n_warmup marker
#' \dontrun{
#' if (requireNamespace("bayesplot", quietly = TRUE)) {
#'     arr_full <- as.array(model, warmup = "include")
#'     bayesplot::mcmc_trace(arr_full, n_warmup = attr(arr_full, "n_warmup"))
#' }
#' }
#' @export
setMethod(
    "as.array", "blimp_obj",
    function(x, warmup = c("exclude", "include", "only"), ...) {
        warmup <- match.arg(warmup)

        n_chains <- length(x@burn)
        param_names <- rownames(x@estimates)

        if (warmup == "exclude") {
            return(chains_to_array_internal(x@iterations, n_chains, param_names))
        }

        burn_df  <- burn_to_df_internal(x@burn)
        burn_arr <- chains_to_array_internal(burn_df, n_chains, param_names)

        if (warmup == "only") {
            return(burn_arr)
        }

        # warmup == "include": stack warmup on top of sampling within each chain
        samp_arr <- chains_to_array_internal(x@iterations, n_chains, param_names)

        n_warmup <- dim(burn_arr)[1]
        n_sample <- dim(samp_arr)[1]
        n_param  <- dim(samp_arr)[3]

        combined <- array(
            NA_real_,
            dim = c(n_warmup + n_sample, n_chains, n_param),
            dimnames = list(
                iterations = NULL,
                chains     = dimnames(samp_arr)$chains,
                parameters = dimnames(samp_arr)$parameters
            )
        )
        combined[seq_len(n_warmup), , ] <- burn_arr
        combined[n_warmup + seq_len(n_sample), , ] <- samp_arr
        attr(combined, "n_warmup") <- n_warmup
        combined
    }
)


#' Convert blimp_obj to a posterior draws object
#'
#' @description
#' S3 methods for the [posterior::as_draws()] family of generics, allowing
#' `blimp_obj` to be used anywhere a [posterior::draws] object is expected.
#' This is the gateway to the **posterior** and **bayesplot** ecosystems:
#' once converted, the usual `summarise_draws()`, `mcmc_trace()`,
#' `mcmc_areas()`, etc., all work.
#'
#' The **posterior** package must be installed to use these methods. It is
#' listed in `Suggests`, so install it explicitly with
#' `install.packages("posterior")` if needed.
#'
#' @details
#' All three methods accept a `warmup` argument with the same semantics as
#' [as.array,blimp_obj-method]. Because `draws_array` objects do not carry
#' a separate warmup/sampling distinction, the `"n_warmup"` attribute set by
#' `as.array(x, warmup = "include")` is dropped during conversion; users who
#' need the warmup marker should convert via `as.array()` and pass the
#' result to `bayesplot::mcmc_trace()` directly.
#'
#' @param x A `blimp_obj` object.
#' @param warmup One of `"exclude"`, `"include"`, or `"only"`. Default
#'   `"exclude"`.
#' @param ... Additional arguments (unused).
#' @return An object of class [posterior::draws_array] (for `as_draws` and
#'   `as_draws_array`) or [posterior::draws_df] (for `as_draws_df`).
#' @name as_draws.blimp_obj
#' @examplesIf has_blimp() && requireNamespace("posterior", quietly = TRUE)
#' mydata <- rblimp_sim(
#'     c(
#'         'f ~ normal(0, 1)',
#'         'x1:x5 ~ normal(f, 1)',
#'         'y ~ normal(10 + 0.3*f, 1 - .3^2)'
#'     ),
#'     n = 500,
#'     seed = 19723,
#'     variables = c('y', 'x1:x5')
#' )
#' model <- rblimp(
#'     list(structure = 'y ~ f', measurement = 'f -> x1:x5'),
#'     mydata,
#'     chains = 4,
#'     seed = 3927,
#'     latent = ~ f
#' )
#'
#' draws <- posterior::as_draws_array(model)
#' posterior::summarise_draws(draws)
#'
#' \dontrun{
#' if (requireNamespace("bayesplot", quietly = TRUE)) {
#'     bayesplot::mcmc_trace(draws)
#'     bayesplot::mcmc_areas(draws)
#' }
#' }
NULL

#' @rdname as_draws.blimp_obj
#' @exportS3Method posterior::as_draws
as_draws.blimp_obj <- function(x, warmup = c("exclude", "include", "only"), ...) {
    as_draws_array.blimp_obj(x, warmup = warmup, ...)
}

#' @rdname as_draws.blimp_obj
#' @exportS3Method posterior::as_draws_array
as_draws_array.blimp_obj <- function(x, warmup = c("exclude", "include", "only"), ...) {
    arr <- as.array(x, warmup = match.arg(warmup))
    attr(arr, "n_warmup") <- NULL  # not carried by draws_array
    posterior::as_draws_array(arr)
}

#' @rdname as_draws.blimp_obj
#' @exportS3Method posterior::as_draws_df
as_draws_df.blimp_obj <- function(x, warmup = c("exclude", "include", "only"), ...) {
    posterior::as_draws_df(as_draws_array.blimp_obj(x, warmup = match.arg(warmup)))
}
