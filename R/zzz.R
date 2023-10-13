# General Imports

# Check if Blimp is detected
.onLoad <- function(libname, pkgname) {
    tryCatch(
        detect_blimp(),
        error = function(e) {
            cli::cli_warn(c(
                "Unable to automatically detect Blimp installation.",
                "x" = "This package requires Blimp to be separately installed.",
                "i" = "You can set Blimp location via {.fun set_blimp}",
                "i" = "Or you can  set Blimp location via `R_BLIMP` enviornment variable"
            ))
        }
    )
}
