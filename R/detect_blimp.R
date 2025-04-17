
#' Internal function to find blimp on macOS
#' @noRd
detect_blimp_macos <- function(exec) {
    ## Try to find executable
    output <- suppressWarnings(system(paste("which", exec), intern = T, ignore.stderr = T)[1])
    if (length(output) != 0) {
        if (file.exists(output)) {
            return(output)
        }
    }

    ## Otherwise search default locations

    # Global App dir
    output <- paste0("/Applications/Blimp/", exec)
    if (file.exists(output)) {
        return(output)
    }

    # User App Dir
    output <- paste0("~", output)
    if (file.exists(output)) {
        return(output)
    }

    # Error out
    throw_error(c(
        "Unable to find blimp executable.",
        "i" = "Make sure blimp is installed."
    ))
}

#' Internal function to find blimp on generic linux
#' @noRd
detect_blimp_linux <- function(exec) {
    ## Try to find executable
    output <- suppressWarnings(system(paste("which", exec), intern = T, ignore.stderr = T)[1])
    if (length(output) != 0) {
        if (file.exists(output)) {
            return(output)
        }
    }

    # Error out
    throw_error(c(
        "Unable to find blimp executable.",
        "i" = "Make sure blimp is installed and in PATH."
    ))
}

#' Internal function to find blimp on windows
#' @noRd
detect_blimp_windows <- function(exec) {
    ## Try to find executable
    output <- suppressWarnings(system(paste("where", exec), intern = T, ignore.stderr = T))
    if (length(output) != 0) {
        if (file.exists(output)) {
            return(output)
        }
    }

    ## Otherwise search default locations

    #  Program Files in C directory
    output <- paste0("C:\\Program Files\\Blimp\\", exec)
    if (file.exists(output)) {
        return(output)
    }

    # Error outs
    throw_error(c(
        "Unable to find blimp executable.",
        "i" = "Make sure blimp is installed."
    ))
}

# rblimp env to store exec position location
rblimp.env <- new.env(parent = emptyenv())
rblimp.env$beta <- FALSE # Default no beta

#' Internal function to set beta version of blimp
#' @noRd
set_blimp_beta <- function() {
    rblimp.env$beta <- TRUE
    cli::cli_alert_warning("Setting beta does not persist on exit.")
}

#' Set Blimp Executable Location
#' @description
#' This function can set the Blimp executable location if it cannot be autodetected.
#' @param exec a character string for the Blimp executable's location
#' @param beta a logical value. If true [`rblimp::rblimp`] will use the beta version which must be installed
#' @details
#' By default [`rblimp::rblimp`] tries to determine the location of Blimp's computational engine based on
#' standard operating system installation locations. This function is useful for non standard installations
#' or when wanting to use a beta version of blimp's computational engine (which must be installed via the updater).
#' @returns
#' `TRUE` if the executable is successfully set; otherwise, it produces an error.
#' @examples
#' \dontrun{
#'     # Use blimp executable at `filepath` location
#'     set_blimp('filepath')
#'
#'     # Use default blimp location but beta build
#'     set_blimp(beta = TRUE)
#' }
#' @export
set_blimp <- function(exec, beta = FALSE) {
    if (missing(exec) || is.null(exec)) {
        if (beta) set_blimp_beta()
        else  rblimp.env$exec <- NULL
        return(invisible(TRUE))
    }
    # Check if file exists and error it it doesn't
    if (!file.exists(exec)) throw_error(
        "Unable to find supplied executable location."
    )
    else if (dir.exists(exec)) throw_error(
        "The supplied location is a directory, not an executable."
    )

    # Set exec and return true
    cli::cli_alert_warning("Setting executable does not persist on exit.")
    rblimp.env$exec <- exec
    invisible(TRUE)
}



#' Produce Blimp location Conditional on Operating System
#' @description
#' This function is called when running Blimp and can be used to determine what location
#' [`rblimp::rblimp`] uses for the Blimp executable.
#' @details
#' [`rblimp::rblimp`] will first use any location set with the [`rblimp::set_blimp`] function. Next, it will
#' check if `R_BLIMP` is set in the environment variables. Finally, it will fall back to the default
#' Blimp install location based on the operating system.
#' @seealso [`rblimp::set_blimp`] to set blimp location
#' @returns
#' A character string of blimp's executable location.
#' @export
detect_blimp <- function() {
    # Return any set executable
    if (!is.null(rblimp.env$exec)) {
        return(rblimp.env$exec)
    }

    ## Check ENV variables
    env_r_blimp <- Sys.getenv("R_BLIMP", unset = NA)
    if (!is.na(env_r_blimp)) {
        if (file.exists(env_r_blimp) & !dir.exists(env_r_blimp)) {
            return(env_r_blimp)
        }
    }

    # Otherwise try to find
    user_os <- tolower(R.Version()$os)
    exec <- if(rblimp.env$beta) "blimp-beta" else "blimp"
    if (grepl("darwin", user_os)) {
        return(detect_blimp_macos(exec))
    } else if (grepl("linux", user_os)) {
        return(detect_blimp_linux(exec))
    } else if (
        grepl("windows", user_os) || grepl("mingw32", user_os)
    ) {
        return(detect_blimp_windows(paste0(exec, ".exe")))
    }
    throw_error("Unable to detect Operating System.")
}
