# Copyright Brian Keller 2026, all rights reserved

# Manifest endpoint (override via R_BLIMP_MANIFEST_URL for staging/testing).
.blimp_manifest_url_default <- "https://updates.blimpstats.com/v1/blimp_base.json"
.blimp_privacy_url <- "https://www.blimpstats.com/privacy"
.blimp_manifest_schema <- 1L

#' Internal: resolve manifest URL
#' @noRd
blimp_manifest_url <- function() {
    Sys.getenv("R_BLIMP_MANIFEST_URL", unset = .blimp_manifest_url_default)
}

#' Internal: run `expr` with HTTPUserAgent temporarily set to
#' "rblimp/<version> (<sysname>)" - e.g. "rblimp/1.2.0 (Darwin)" - so the
#' Blimp update server can distinguish rblimp traffic from anonymous
#' libcurl/R clients and break out by OS. Restored on exit even if `expr`
#' errors.
#' @noRd
with_rblimp_ua <- function(expr) {
    old <- getOption("HTTPUserAgent")
    on.exit(options(HTTPUserAgent = old), add = TRUE)
    ua <- sprintf("rblimp/%s (%s)",
                  utils::packageVersion("rblimp"),
                  Sys.info()[["sysname"]])   # "Darwin" / "Linux" / "Windows"
    options(HTTPUserAgent = ua)
    expr
}

#' Internal: minimal JSON parser for the Blimp manifest
#'
#' Hand-rolled to avoid an extra package dependency. Handles the JSON subset
#' used by the manifest schema: objects, strings (with backslash escapes),
#' integers, true/false/null, arrays. Not a general-purpose parser.
#'
#' Note: does not handle \\uXXXX Unicode escapes. The current manifest schema
#' only uses ASCII strings (URLs, hex sha256, version digits, paths); add
#' Unicode-escape support if that changes.
#' @noRd
parse_blimp_json <- function(text) {
    s <- new.env(parent = emptyenv())
    s$text <- paste(text, collapse = "\n")
    s$n <- nchar(s$text)
    s$pos <- 1L

    peek <- function() substr(s$text, s$pos, s$pos)
    advance <- function(k = 1L) { s$pos <- s$pos + k; invisible() }

    skip_ws <- function() {
        while (s$pos <= s$n) {
            ch <- peek()
            if (!(ch == " " || ch == "\t" || ch == "\n" || ch == "\r")) break
            advance()
        }
    }

    # Note: messages here use cli-style `{var}` for variable interpolation and
    # `{{` / `}}` to escape literal braces. throw_error -> cli::cli_abort which
    # parses the message as a glue template.
    parse_string <- function() {
        skip_ws()
        if (peek() != '"') throw_error("Expected double-quote at position {s$pos}")
        advance()
        out <- character()
        while (s$pos <= s$n) {
            ch <- peek()
            if (ch == '"') { advance(); return(paste(out, collapse = "")) }
            if (ch == "\\") {
                advance()
                esc <- peek()
                ch <- switch(esc,
                    '"' = '"', "\\" = "\\", "/" = "/",
                    n = "\n", t = "\t", r = "\r", b = "\b", f = "\f",
                    throw_error("Unsupported escape \\\\{esc} at position {s$pos}")
                )
            }
            out <- c(out, ch)
            advance()
        }
        throw_error("Unterminated string")
    }

    parse_number <- function() {
        skip_ws()
        start <- s$pos
        if (peek() == "-") advance()
        while (s$pos <= s$n) {
            ch <- peek()
            if (!grepl("[0-9.eE+-]", ch)) break
            advance()
        }
        as.numeric(substr(s$text, start, s$pos - 1L))
    }

    parse_literal <- function(lit, value) {
        if (substr(s$text, s$pos, s$pos + nchar(lit) - 1L) != lit) {
            throw_error("Expected '{lit}' at position {s$pos}")
        }
        advance(nchar(lit))
        value
    }

    parse_value <- function() {
        skip_ws()
        ch <- peek()
        if (ch == '"') return(parse_string())
        if (ch == "{") return(parse_object())
        if (ch == "[") return(parse_array())
        if (grepl("[0-9-]", ch)) return(parse_number())
        if (ch == "t") return(parse_literal("true", TRUE))
        if (ch == "f") return(parse_literal("false", FALSE))
        if (ch == "n") return(parse_literal("null", NULL))
        throw_error("Unexpected character '{ch}' at position {s$pos}")
    }

    parse_object <- function() {
        skip_ws()
        if (peek() != "{") throw_error("Expected '{{' at position {s$pos}")
        advance()
        out <- list()
        skip_ws()
        if (peek() == "}") { advance(); return(out) }
        repeat {
            key <- parse_string()
            skip_ws()
            if (peek() != ":") throw_error("Expected ':' at position {s$pos}")
            advance()
            out[[key]] <- parse_value()
            skip_ws()
            ch <- peek()
            if (ch == ",") { advance(); skip_ws(); next }
            if (ch == "}") { advance(); return(out) }
            throw_error("Expected ',' or '}}' at position {s$pos}")
        }
    }

    parse_array <- function() {
        skip_ws()
        if (peek() != "[") throw_error("Expected '[' at position {s$pos}")
        advance()
        out <- list()
        skip_ws()
        if (peek() == "]") { advance(); return(out) }
        repeat {
            out[[length(out) + 1L]] <- parse_value()
            skip_ws()
            ch <- peek()
            if (ch == ",") { advance(); skip_ws(); next }
            if (ch == "]") { advance(); return(out) }
            throw_error("Expected ',' or ']' at position {s$pos}")
        }
    }

    parse_value()
}

#' Internal: detect Linux distro variant from /etc/os-release
#' Returns "rhel8" for RHEL/CentOS/Rocky/Alma 8, else "ubuntu" (generic).
#' @noRd
detect_linux_variant <- function() {
    if (!file.exists("/etc/os-release")) return("ubuntu")
    lines <- tryCatch(readLines("/etc/os-release", warn = FALSE),
                      error = function(e) character())
    info <- list()
    for (line in lines) {
        m <- regmatches(line, regexec("^([A-Za-z_]+)=(.*)$", line))[[1]]
        if (length(m) == 3L) {
            val <- m[3]
            val <- gsub('^"|"$', "", val)
            info[[m[2]]] <- val
        }
    }
    id <- tolower(info[["ID"]] %||% "")
    version_id <- info[["VERSION_ID"]] %||% ""
    if (id %in% c("rhel", "centos", "rocky", "almalinux") &&
        startsWith(version_id, "8")) {
        return("rhel8")
    }
    "ubuntu"
}

# Null-coalescing helper
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

#' Internal: resolve the manifest platform key for the current system
#' @noRd
blimp_platform_key <- function(linux_variant = NULL) {
    sysname <- Sys.info()[["sysname"]]
    if (sysname == "Darwin") return("macOS")
    if (.Platform$OS.type == "windows") return("windows_x64")
    if (sysname == "Linux") {
        v <- linux_variant %||% detect_linux_variant()
        if (v == "rhel8") return("linux_x64_rhel8")
        return("linux_x64")
    }
    throw_error("Unsupported operating system: {.val {sysname}}.")
}

#' Internal: default managed install directory per OS
#' macOS / Linux: ~/.blimp/ (hidden, tool-managed convention)
#' Windows: %LOCALAPPDATA%/Blimp/ (LocalAppData is not roamed - right place for
#'   multi-MB binaries that shouldn't sync across machines)
#' @noRd
blimp_default_dir <- function() {
    if (.Platform$OS.type == "windows") {
        local_appdata <- Sys.getenv("LOCALAPPDATA")
        # Avoid paths with spaces or non-ASCII (tinytex pattern)
        if (nzchar(local_appdata) && !grepl("[ ]", local_appdata) &&
            !grepl("[^\x01-\x7f]", local_appdata)) {
            return(file.path(local_appdata, "Blimp"))
        }
        progdata <- Sys.getenv("ProgramData")
        if (nzchar(progdata)) return(file.path(progdata, "Blimp"))
        return(file.path(Sys.getenv("HOME"), "Blimp"))
    }
    file.path(Sys.getenv("HOME"), ".blimp")
}

#' Internal: resolve the managed Blimp install root directory.
#' Honors R_BLIMP_HOME if set; otherwise uses blimp_default_dir().
#' @noRd
blimp_root <- function() {
    env <- Sys.getenv("R_BLIMP_HOME", unset = NA)
    if (!is.na(env) && nzchar(env)) return(env)
    blimp_default_dir()
}

#' Internal: detect an existing system (non-managed) Blimp install
#' Returns the executable path or NULL.
#' @noRd
detect_system_blimp <- function() {
    user_os <- tolower(R.Version()$os)
    exec <- if (isTRUE(rblimp.env$beta)) "blimp-beta" else "blimp"
    sys_paths <- character()
    if (grepl("darwin", user_os)) {
        sys_paths <- c(
            paste0("/Applications/Blimp/", exec),
            path.expand(paste0("~/Applications/Blimp/", exec))
        )
    } else if (grepl("windows", user_os) || grepl("mingw32", user_os)) {
        sys_paths <- paste0("C:\\Program Files\\Blimp\\", exec, ".exe")
    }
    found <- sys_paths[file.exists(sys_paths)]
    if (length(found)) return(found[1])
    NULL
}

#' Internal: locate the managed Blimp executable
#' Returns path string or NULL.
#' @noRd
detect_managed_blimp <- function() {
    dir <- blimp_root()
    if (!dir.exists(dir)) return(NULL)
    find_blimp_exec(dir)
}

#' Internal: search an extracted install tree for the blimp executable
#' @noRd
find_blimp_exec <- function(dir) {
    is_win <- .Platform$OS.type == "windows"
    exec_name <- if (isTRUE(rblimp.env$beta)) "blimp-beta" else "blimp"
    if (is_win) exec_name <- paste0(exec_name, ".exe")
    matches <- list.files(
        dir, pattern = paste0("^", exec_name, "$"),
        recursive = TRUE, full.names = TRUE, ignore.case = TRUE
    )
    if (length(matches) == 0L) return(NULL)
    matches[which.min(nchar(matches))]
}

#' Internal: fetch and validate the manifest
#' One automatic retry after 1s per the manifest spec.
#' Shows the one-time privacy notice before the first request, since this is
#' the first time rblimp talks to the update server.
#' @noRd
fetch_blimp_manifest <- function(quiet = FALSE) {
    show_privacy_notice()

    url <- blimp_manifest_url()
    tmp <- tempfile(fileext = ".json")
    on.exit(unlink(tmp), add = TRUE)

    attempt_download <- function() {
        status <- tryCatch(
            with_rblimp_ua(suppressWarnings(utils::download.file(
                url, tmp, mode = "wb",
                quiet = quiet, cacheOK = FALSE
            ))),
            error = function(e) 1L
        )
        isTRUE(status == 0L) && file.exists(tmp) && file.info(tmp)$size > 0L
    }

    if (!attempt_download()) {
        Sys.sleep(1)
        if (!attempt_download()) {
            throw_error(c(
                "Couldn't reach {.url updates.blimpstats.com}.",
                "i" = "Check your connection and try again."
            ))
        }
    }

    text <- readLines(tmp, warn = FALSE, encoding = "UTF-8")
    manifest <- tryCatch(parse_blimp_json(text), error = function(e) {
        throw_error(c(
            "Failed to parse Blimp manifest.",
            "x" = conditionMessage(e)
        ))
    })

    schema <- manifest$schema
    supported <- .blimp_manifest_schema
    if (!isTRUE(schema == supported)) {
        throw_error(c(
            "Please update rblimp; this Blimp release uses a newer manifest format.",
            "i" = "Manifest schema: {schema}, supported: {supported}"
        ))
    }

    manifest
}

#' Internal: show one-time privacy notice
#' Suppressed in non-interactive sessions (CI etc.) - the sentinel only gets
#' created when an interactive user has actually seen the message.
#' Not gated by `quiet =` arguments: this is a compliance disclosure that
#' should fire on the first interactive call regardless of progress-noise
#' preferences.
#' @noRd
show_privacy_notice <- function() {
    if (!interactive()) return(invisible())
    sentinel_dir <- tools::R_user_dir("rblimp", "config")
    sentinel <- file.path(sentinel_dir, "privacy_acknowledged")
    if (file.exists(sentinel)) return(invisible())

    url <- .blimp_privacy_url
    cli::cli_alert_info(
        "rblimp will contact {.url updates.blimpstats.com} for version and download data."
    )
    cli::cli_alert_info("See privacy policy: {.url {url}}")

    dir.create(sentinel_dir, recursive = TRUE, showWarnings = FALSE)
    tryCatch(file.create(sentinel), error = function(e) NULL,
             warning = function(w) NULL)
    invisible()
}

#' Internal: extract archive into install dir
#' .7z via system tar (libarchive-backed on macOS / Windows 10+);
#' .tar.gz via utils::untar().
#' @noRd
extract_blimp_archive <- function(archive_file, dir) {
    if (grepl("\\.7z$", archive_file, ignore.case = TRUE)) {
        tar_cmd <- if (.Platform$OS.type == "windows") "tar.exe" else "tar"
        res <- suppressWarnings(tryCatch(
            system2(tar_cmd, c("-xf", shQuote(archive_file),
                               "-C", shQuote(dir)),
                    stdout = FALSE, stderr = FALSE),
            error = function(e) 127L
        ))
        if (!isTRUE(res == 0L)) {
            throw_error(c(
                "Failed to extract 7z archive.",
                "i" = "rblimp uses the system {.command tar} (libarchive-backed) to extract .7z files.",
                "i" = "Requires macOS, or Windows 10 build 17063+ (April 2018).",
                "i" = "On older Windows, install 7-Zip and ensure {.command tar} is on PATH, or use the Blimp system installer."
            ))
        }
        return(invisible())
    }
    # tar.gz path (Linux)
    utils::untar(archive_file, exdir = dir)
}


#' Internal: confirm installation interactively, with a privacy disclosure.
#' Calls `show_privacy_notice()` so the user sees what installing means before
#' they answer (and so the sentinel is marked, preventing the same notice
#' from repeating during the subsequent install).
#' @noRd
confirm_install <- function() {
    if (!interactive()) return(FALSE)
    cli::cli_alert_info("Blimp is not installed.")
    show_privacy_notice()
    cli::cli_alert("")
    cli::cli_alert("Install Blimp now?")
    cli::cli_alert("  {.strong yes}  - download and install via {.fn install_blimp}")
    cli::cli_alert("  {.strong no}   - skip; install later or set the path manually")
    ans <- readline("Selection (yes/no): ")
    isTRUE(tolower(trimws(ans)) %in% c("y", "yes"))
}

#' Install the Blimp computational engine
#' @description
#' Downloads and installs Blimp into a user-writable directory managed by rblimp.
#' This is an alternative to installing Blimp via the system installer from
#' <https://www.appliedmissingdata.com/blimp>.
#' @param dir Install directory. Defaults to a hidden, user-writable
#'   per-OS location (`~/.blimp/` on macOS and Linux,
#'   `%LOCALAPPDATA%/Blimp/` on Windows), overridable via the `R_BLIMP_HOME`
#'   environment variable.
#' @param force Logical; overwrite an existing managed install, or proceed
#'   even when a system installation of Blimp is detected. Default `FALSE`.
#'   When both managed and system installations exist, the managed install
#'   takes precedence in [`detect_blimp`].
#' @param quiet Logical; suppress progress messages. Default `FALSE`.
#' @param linux_variant Optional character; `"ubuntu"` or `"rhel8"`. When `NULL`
#'   (default), the variant is auto-detected from `/etc/os-release`. Ignored on
#'   macOS and Windows.
#' @details
#' The install directory location can be overridden globally via the
#' `R_BLIMP_HOME` environment variable.
#'
#' Set `R_BLIMP_PREVENT_INSTALL=true` to disable [`install_blimp`] (intended for
#' locked-down environments).
#'
#' Non-interactive R sessions require `force = TRUE`.
#' @section Privacy:
#' Calling `install_blimp()` contacts `updates.blimpstats.com` over HTTPS
#' to fetch the Blimp engine. Information may be collected as described
#' in the privacy policy.
#'
#' See privacy policy: <https://www.blimpstats.com/privacy>.
#' @returns
#' Path to the installed `blimp` executable, invisibly.
#' @examplesIf interactive()
#' \dontrun{
#' # Install Blimp to the default managed location
#' install_blimp()
#'
#' # Install to a custom directory
#' install_blimp(dir = "~/blimp-managed")
#'
#' # Force install on a Linux RHEL 8 system without auto-detection
#' install_blimp(linux_variant = "rhel8")
#' }
#' @seealso [`uninstall_blimp`], [`update_blimp`]
#' @export
install_blimp <- function(dir = NULL, force = FALSE, quiet = FALSE,
                          linux_variant = NULL) {

    if (tolower(Sys.getenv("R_BLIMP_PREVENT_INSTALL", "")) == "true") {
        throw_error(c(
            "Install blocked by environment variable.",
            "i" = "Unset {.envvar R_BLIMP_PREVENT_INSTALL} to enable installation."
        ))
    }

    if (!interactive() && !force) {
        throw_error(c(
            "{.fn install_blimp} can only run in interactive sessions.",
            "i" = "Pass {.code force = TRUE} to override."
        ))
    }

    if (!is.null(linux_variant) &&
        !linux_variant %in% c("ubuntu", "rhel8")) {
        throw_error(
            "{.arg linux_variant} must be {.val ubuntu} or {.val rhel8}."
        )
    }

    sys_path <- detect_system_blimp()
    if (!is.null(sys_path) && !force) {
        throw_error(c(
            "A system installation of Blimp was found at:",
            "i" = sys_path,
            "i" = "Uninstall it first, or pass {.code force = TRUE} to install the managed version alongside it.",
            "i" = "When both exist, the managed install takes precedence in {.fn detect_blimp}."
        ))
    }

    if (is.null(dir)) dir <- blimp_root()

    if (dir.exists(dir) &&
        length(list.files(dir, all.files = TRUE, no.. = TRUE)) > 0L &&
        !force) {
        throw_error(c(
            "Blimp is already installed at {.path {dir}}.",
            "i" = "Pass {.code force = TRUE} to reinstall."
        ))
    }

    if (!quiet) cli::cli_alert_info("Fetching Blimp manifest...")
    manifest <- fetch_blimp_manifest(quiet = quiet)

    platform <- blimp_platform_key(linux_variant)
    asset <- manifest$platforms[[platform]]
    if (is.null(asset) || is.null(asset$url)) {
        throw_error(c(
            "Blimp Base isn't available for your platform on this release.",
            "i" = "Platform key: {.val {platform}}, Version: {.val {manifest$version}}"
        ))
    }


    archive_ext <- if (grepl("\\.7z$", asset$url, ignore.case = TRUE)) {
        ".7z"
    } else {
        ".tar.gz"
    }
    archive_file <- tempfile(fileext = archive_ext)
    on.exit(unlink(archive_file), add = TRUE)

    if (!quiet) {
        cli::cli_alert_info(
            "Downloading Blimp {manifest$version} ({.val {platform}})..."
        )
    }

    dl_status <- tryCatch(
        with_rblimp_ua(suppressWarnings(utils::download.file(
            asset$url, archive_file, mode = "wb", quiet = quiet
        ))),
        error = function(e) 1L
    )
    if (!isTRUE(dl_status == 0L) || !file.exists(archive_file)) {
        throw_error("Download of Blimp Base failed - try again.")
    }

    if (!is.null(asset$size_bytes)) {
        actual <- file.info(archive_file)$size
        expected <- as.numeric(asset$size_bytes)
        if (!isTRUE(actual == expected)) {
            throw_error(c(
                "Downloaded file size does not match manifest.",
                "i" = "Expected {expected} bytes, got {actual} bytes.",
                "i" = "Try again; if this persists, please report it."
            ))
        }
    }

    if (dir.exists(dir) && force) {
        unlink(dir, recursive = TRUE, force = TRUE)
    }
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)

    if (!quiet) cli::cli_alert_info("Extracting archive...")
    extract_blimp_archive(archive_file, dir)

    exec <- find_blimp_exec(dir)
    if (is.null(exec)) {
        throw_error(c(
            "Could not locate the Blimp executable after extraction.",
            "i" = "Install dir: {.path {dir}}"
        ))
    }

    # Ensure executable bit is set on Unix
    if (.Platform$OS.type == "unix") {
        suppressWarnings(Sys.chmod(exec, mode = "0755"))
    }

    writeLines(manifest$version, file.path(dir, ".blimp_version"))

    if (!quiet) {
        cli::cli_alert_success(
            "Blimp {manifest$version} installed to {.path {dir}}."
        )
    }

    invisible(exec)
}

#' Remove the managed Blimp installation
#' @description
#' Deletes the managed install directory created by [`install_blimp`]. Does
#' not affect system installations of Blimp.
#' @param quiet Logical; suppress messages and skip the interactive
#'   confirmation prompt. Default `FALSE`.
#' @returns
#' `TRUE` if anything was removed, `FALSE` otherwise (invisibly).
#' @examplesIf interactive()
#' \dontrun{uninstall_blimp()}
#' @seealso [`install_blimp`]
#' @export
uninstall_blimp <- function(quiet = FALSE) {
    dir <- blimp_root()
    if (!dir.exists(dir)) {
        if (!quiet) {
            cli::cli_alert_info(
                "No managed Blimp install found at {.path {dir}}."
            )
        }
        return(invisible(FALSE))
    }
    if (interactive() && !quiet) {
        ans <- readline(paste0("Remove ", dir, "? (Yes/No): "))
        if (!isTRUE(tolower(ans) %in% c("y", "yes"))) {
            cli::cli_alert_info("Cancelled.")
            return(invisible(FALSE))
        }
    }
    unlink(dir, recursive = TRUE, force = TRUE)
    if (!quiet) cli::cli_alert_success("Removed {.path {dir}}.")
    invisible(TRUE)
}
