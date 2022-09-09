

# Find blimp on macOS
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
    stop("Unable to find blimp executable.\n  Make sure blimp is installed.")
}

# Find blimp on generic linux
detect_blimp_linux <- function(exec) {

    ## Try to find executable
    output <- suppressWarnings(system(paste("which", exec), intern = T, ignore.stderr = T)[1])
    if (length(output) != 0) {
        if (file.exists(output)) {
            return(output)
        }
    }

    # Error out
    stop("Unable to find blimp executable.\n  Make sure blimp is installed and in PATH.")
}

# Find blimp on windows
detect_blimp_windows <- function(exec) {

    ## Try to find executable
    output <- suppressWarnings(system(paste("where", exec), intern = T, ignore.stderr = T)[1])
    if (length(output) != 0) {
        if (file.exists(output)) {
            return(output)
        }
    }

    ## Otherwise search default locations

    #  Program Files in C directory
    output <- paste0("C:\\Program Files\\Blimp", exec)
    if (file.exists(output)) {
        return(output)
    }

    # Error out
    stop("Unable to find blimp executable.\n  Make sure blimp is installed.")
}

# rblimp env to store exec position location
rblimp.env <- new.env(parent = emptyenv())
rblimp.env$beta <- FALSE # Default no beta

set_blimp_beta <- function(beta, ...) {
    if (!missing(beta)) {
        rblimp.env$beta <- beta
        warning("Setting beta does not persist on exit.")
        return(invisible(TRUE))
    }
    return(invisible(FALSE))
}

# Set Blimp Executable
#' @export
set_blimp <- function(exec, ...) {
    if (missing(exec) || is.null(exec)) {
        if (!set_blimp_beta(...)) {
            rblimp.env$exec <- NULL
        }
        return(invisible(TRUE))
    }
    if (file.exists(exec)) {
        warning("Setting executable does not persist on exit.")
        rblimp.env$exec <- exec
        return(invisible(TRUE))
    }

    stop("Unable to find supplied executable location.")
}



# Detect blimp generic
#' @export
detect_blimp <- function() {
    # Return any set executable
    if (!is.null(rblimp.env$exec)) {
        return(rblimp.env$exec)
    }

    ## Check ENV variables
    env_r_blimp <- Sys.getenv("R_BLIMP", unset = NA)
    if (!is.na(env_r_blimp)) {
        if (file.exists(env_r_blimp)) {
            return(env_r_blimp)
        }
    }

    # Otherwise try to find
    user_os <- tolower(R.Version()$os)
    exec <- ifelse(rblimp.env$beta, "blimp-beta", "blimp")
    if (grepl("darwin", user_os)) {
        return(detect_blimp_macos(exec))
    } else if (grepl("linux", user_os)) {
        return(detect_blimp_linux(exec))
    } else if (
        grepl("windows", user_os) || grepl("minwg32", user_os)
    ) {
        return(detect_blimp_windows(paste0(exec, ".exe")))
    }
    stop("Unable to detect Operating System.")
}
