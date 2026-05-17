# Copyright Brian Keller 2026, all rights reserved

#' Internal function to find Blimp updater location (system installs only)
#' @noRd
detect_blimp_updater <- function(user_os) {

    # Check ENV variables
    env_r_blimp_up <- Sys.getenv("R_BLIMP_UPDATER", unset = NA)
    if (!is.na(env_r_blimp_up)) {
        if (!file.exists(env_r_blimp_up)) {
            env_r_blimp_up <- NA
        }
    }

    if (grepl("darwin", user_os)) {
        if (is.na(env_r_blimp_up)) {
            return(paste0(dirname(detect_blimp(prompt = FALSE)),
                          "/blimp-updater.app/Contents/MacOS/blimp-updater"))
        } else {
            return(paste0(env_r_blimp_up, "/Contents/MacOS/blimp-updater"))
        }
    } else if (grepl("linux", user_os)) {
        throw_error("Linux does not have a Blimp Updater")
    } else if (
        grepl("windows", user_os) || grepl("mingw32", user_os)
    ) {
        if (is.na(env_r_blimp_up)) {
            filep <- gsub("/", "\\\\",
                          dirname(detect_blimp(prompt = FALSE)),
                          fixed = TRUE)
            return(paste0(filep,"\\blimp-updater.exe"))
        } else {
            return(env_r_blimp_up)
        }
    }
}

#' Internal: parse a "X.Y.Z" version string into numeric components
#' @noRd
parse_version_string <- function(v) {
    if (is.null(v) || is.na(v) || !nzchar(v)) return(NA_integer_)
    parts <- strsplit(v, "\\.")[[1]]
    suppressWarnings(as.integer(parts))
}

#' Internal: query installed Blimp's version via --changelog
#' @noRd
blimp_version <- function() {
    blimp_path <- tryCatch(detect_blimp(prompt = FALSE),
                           error = function(e) NA_character_)
    if (is.na(blimp_path)) return(NA_integer_)

    # Preprocess path for non-unix (assumes windows)
    if (.Platform$OS.type != "unix") blimp_path <- paste0('"', blimp_path, '"')

    v <- suppressWarnings(tryCatch(
        system(paste(blimp_path, "--changelog"), intern = TRUE)[3],
        error = \(e) NA_character_
    ))

    if (is.na(v)) return(NA_integer_)
    parse_version_string(substr(v, 9, nchar(v)))
}

#' Internal: latest Blimp version from the manifest
#' Falls back to NA on any network or parse error.
#' @noRd
update_version <- function() {
    manifest <- tryCatch(fetch_blimp_manifest(quiet = TRUE),
                         error = function(e) NULL)
    if (is.null(manifest)) return(NA_integer_)
    parse_version_string(manifest$version)
}

#' Internal: returns TRUE only if both versions parse AND installed < latest
#' @noRd
has_blimp_update <- function() {
    if (interactive() == FALSE) return(invisible(FALSE))
    cur <- blimp_version()
    new <- update_version()
    if (anyNA(cur) || anyNA(new)) return(FALSE)
    any(cur < new)
}

#' Update the installed Blimp engine
#' @description
#' If Blimp was installed via [`install_blimp`], the latest version is
#' re-downloaded into the managed directory. If Blimp was installed via the
#' system installer, the Blimp Updater application is launched (back-compat).
#' @details
#' If the session is not interactive, the updater is not opened.
#'
#' For managed installs, the manifest is checked first and the engine is
#' re-downloaded only if a newer version exists.
#' @section Privacy:
#' Calling `update_blimp()` on a managed install contacts
#' `updates.blimpstats.com` over HTTPS to check for a newer version of
#' Blimp, and downloads from it if one is available. Information may be
#' collected as described in the privacy policy.
#'
#' See privacy policy: <https://www.blimpstats.com/privacy>.
#' @returns Logical indicating whether an update was started.
#' @examplesIf has_blimp()
#' # Update Blimp
#' \dontrun{update_blimp()}
#' @seealso [`install_blimp`]
#' @export
update_blimp <- function() {

    ## Get User's OS
    user_os <- tolower(R.Version()$os)

    ## Exit if it is non interactive
    if (interactive() == FALSE) return(invisible(FALSE))

    ## Managed install path: check for a newer version, then re-download.
    if (!is.null(detect_managed_blimp())) {
        if (!has_blimp_update()) {
            cli::cli_alert_success("Blimp is already up to date.")
            return(invisible(FALSE))
        }
        install_blimp(force = TRUE)
        return(invisible(TRUE))
    }

    ## Exit for linux (no system updater)
    if (grepl("linux", user_os)) throw_error(
        x = "Linux does not have an updater.",
        i = c(
            "Use {.fn install_blimp} to install a managed copy that rblimp can update,",
            "or manually download an update at {.url https://www.appliedmissingdata.com/blimp}."
        )
    )

    ## Get Filepath of system updater
    filep <- detect_blimp_updater(user_os)

    ## Check if it exists
    if(!file.exists(filep)) {
        throw_error(c(
            "Unable to automatically detect Blimp Updater",
            "i" = "Manually download an update at {.url https://www.appliedmissingdata.com/blimp}",
            "i" = "Or you can set Blimp Updater location via the {.envvar R_BLIMP_UPDATER} environment variable",
            "i" = "Or install a managed copy with {.fn install_blimp} that rblimp can update directly."
        ))
    }

    ## Open the system updater
    if (grepl("darwin", user_os)) {
        system(paste0(filep, ' --start-updater'),
               ignore.stdout = TRUE, ignore.stderr = TRUE, wait = FALSE)
    } else if (
        grepl("windows", user_os) || grepl("mingw32", user_os)
    ) {
        system(paste0('"', filep, '" --start-updater'),
               ignore.stdout = TRUE, ignore.stderr = TRUE, wait = FALSE)
    }
    return(invisible(TRUE))
}
