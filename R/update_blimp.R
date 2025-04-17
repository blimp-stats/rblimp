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



#' Check for Update of Blimp
#' @description
#' Check for Blimp updates
#' @details
#' This function is called on loading the package.
#' If the session is interactive then it will not check for an update.
#'
#' Checking update on load can be disabled by setting options `check_blimp_update` to `FALSE`. See example below.
#' @examples
#' # Disable Update checking on load
#' options(check_blimp_update = FALSE)
#' @export
update_blimp <- function() {

    ## Get User's OS
    user_os <- tolower(R.Version()$os)

    ## Exit if it is non interactive
    if (interactive() == FALSE) return()

    ## Exit for linux
    if (grepl("linux", user_os)) return()

    ## Get Filepath
    filep <- detect_blimp_updater(user_os)

    ## Check if it exists
    if(!file.exists(filep)) {
        cli::cli_warn(c(
            "Unable to automatically detect Blimp Updater",
            "i" = "Manually check for update at {.url https://www.appliedmissingdata.com/blimp}",
            "i" = "Or you can set Blimp Updater location via `R_BLIMP_UPDATER` enviornment variable"
        ))
        return()
    }

    ## Check for updates
    if (grepl("darwin", user_os)) {
        o <- suppressWarnings(system(
            paste0(filep, ' --checkupdates'),
            ignore.stdout = TRUE
        ))
        if (o == 1) {
            cat('  Blimp has an update!\n  Would you like to open up the updater?\n')
            x <- readline('(Yes/No): ')
            if (tolower(x) == 'yes' || tolower(x) == 'y') {
                system(paste0(filep, ' --start-updater'), ignore.stdout = TRUE, ignore.stderr = TRUE, wait = FALSE)
            }
        }
    } else if (
        grepl("windows", user_os) || grepl("mingw32", user_os)
    ) {
        o <- suppressWarnings(system(
            paste0('"',filep, '" --checkupdates'),
            inter = TRUE, ignore.stdout = FALSE, ignore.stderr = TRUE
        ))
        if (any(grepl("update name=", o, fixed = TRUE))) {
            cat('  Blimp has an update!\n  Would you like to open up the updater?\n')
            x <- readline('(Yes/No): ')
            if (tolower(x) == 'yes' || tolower(x) == 'y') {
                system(paste0('"',filep, '" --start-updater'), ignore.stdout = TRUE, ignore.stderr = TRUE, wait = FALSE)
            }
        }
    }
}
