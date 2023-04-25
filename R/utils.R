#' Internal error function
#' Wrapper for `cli_abort` to not specify the call
#' @importFrom cli cli_abort
#' @noRd
throw_error <- function(message, ..., .envir = parent.frame(), .frame = .envir) {
    cli::cli_abort(message, ..., .envir = .envir, .frame = .frame, call = NULL)
}
