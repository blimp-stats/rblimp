# General Imports

#' Inform interactive users if Blimp is unavailable.
#' (No prompt here - the actual install offer happens just-in-time when the
#'  user calls rblimp(), rblimp_source(), etc. See detect_blimp(prompt = TRUE).)
#' @noRd
.onAttach <- function(libname, pkgname) {
    if (interactive() && !has_blimp()) {
        packageStartupMessage(cli::format_message(c(
            "i" = "rblimp requires Blimp, but no installation was detected.",
            "i" = "Run {.fn install_blimp} to install Blimp now, or you will be prompted the first time you run a model.",
            "i" = "Alternatively, set the Blimp location via {.fn set_blimp} or the {.envvar R_BLIMP} environment variable."
        )))
    }
}
