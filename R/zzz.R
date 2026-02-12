# General Imports

#' Inform interactive users if Blimp is unavailable
#' @noRd
.onAttach <- function(libname, pkgname) {
    if (interactive() && !has_blimp()) {
        cli::cli_warn(c(
            "Unable to automatically detect Blimp installation.",
            "x" = "This package requires Blimp to be separately installed.",
            "i" = "You can set Blimp location via `set_blimp()`",
            "i" = "Or you can set Blimp location via `R_BLIMP` enviornment variable"
        ))
    }
}
