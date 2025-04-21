# Copyright Brian Keller 2025, all rights reserved

#' Internal function to find get blimp updater location
#' @noRd
detect_blimp_updater <- function(user_os) {

    # Check ENV variables
    env_r_blimp_up <- Sys.getenv("R_BLIMP_UPDATER", unset = NA)

    # Check for
    if (!is.na(env_r_blimp_up)) {
        if (!file.exists(env_r_blimp_up)) {
            env_r_blimp_up <- NA
        }
    }

    if (grepl("darwin", user_os)) {
        if (is.na(env_r_blimp_up)) {
            return(paste0(dirname(detect_blimp()), "/blimp-updater.app/Contents/MacOS/blimp-updater"))
        } else {
            return(paste0(env_r_blimp_up, "/Contents/MacOS/blimp-updater"))
        }
    } else if (grepl("linux", user_os)) {
        throw_error("Linux does not have a Blimp Updater")
    } else if (
        grepl("windows", user_os) || grepl("mingw32", user_os)
    ) {
        if (is.na(env_r_blimp_up)) {
            filep <- gsub("/", "\\\\", dirname(detect_blimp()), fixed = TRUE)
            return(paste0(filep,"\\blimp-updater.exe"))
        } else {
            return(env_r_blimp_up)
        }
    }
}



#' Internal function to get Blimp's version
#' @noRd
blimp_version <- function() {
    # Establish blimp exec
    blimp_path <- detect_blimp()

    # Preprocess path for non-unix (assumes windows)
    if (.Platform$OS.type != "unix") blimp_path <- paste0('"', blimp_path, '"')

    # Run command and capture output
    v <- suppressWarnings(tryCatch(
        system(paste(blimp_path, "--changelog"), intern = TRUE)[3],
        error = \(e) NA
    ))

    # Return version
    substr(v, 9, nchar(v)) |> strsplit('\\.') |> unlist()
}

#' Internal function to check Blimp update version
#' @noRd
update_version <- function() {
    # Get changelog from url
    u <- url('https://blimp-stats.github.io/Blimp%20Base%20(Required)-changelog.txt')

    # Read lines
    v <- suppressWarnings(
        tryCatch(readLines(u, 8, warn = FALSE)[8], error = \(e) NA)
    )
    # Close connection
    close(u)

    # Return version
    substr(v, 9, nchar(v)) |> strsplit('\\.') |> unlist()
}

#' Internal command to check if blimp is up to date
#' @noRd
has_blimp_update <- function() {
    (blimp_version() < update_version()) |> any()
}

#' Runs the Blimp Updater in order to Update Blimp
#' @details
#' If the session is not an interactive session then it will not open the updater.
#' @examplesIf has_blimp()
#' # Open Blimp Updater
#' update_blimp()
#' @export
update_blimp <- function() {

    ## Get User's OS
    user_os <- tolower(R.Version()$os)

    ## Exit if it is non interactive
    if (interactive() == FALSE) return()

    ## Exit for linux
    if (grepl("linux", user_os)) throw_error(
        x = "Linux does not have an updater.",
        i = "Manually download an update at {.url https://www.appliedmissingdata.com/blimp}"
    )

    ## Get Filepath
    filep <- detect_blimp_updater(user_os)

    ## Check if it exists
    if(!file.exists(filep)) {
        throw_error(c(
            "Unable to automatically detect Blimp Updater",
            "i" = "Manually download an update at {.url https://www.appliedmissingdata.com/blimp}",
            "i" = "Or you can set Blimp Updater location via `R_BLIMP_UPDATER` enviornment variable"
        ))
    }

    ## Check for updates
    if (grepl("darwin", user_os)) {
        system(paste0(filep, ' --start-updater'), ignore.stdout = TRUE, ignore.stderr = TRUE, wait = FALSE)
    } else if (
        grepl("windows", user_os) || grepl("mingw32", user_os)
    ) {
        system(paste0('"',filep, '" --start-updater'), ignore.stdout = TRUE, ignore.stderr = TRUE, wait = FALSE)
    }
    return()
}
