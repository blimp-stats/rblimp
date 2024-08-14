#' Runs an 'imp' file with Blimp
#' @description
#' This function runs 'imp' file with Blimp and captures the output only.
#' @param file a character string to the 'imp' file's location
#' @param plots a logical. Setting to `TRUE` will generate the data required for plotting by Blimp
#' @param output The type of output printed to the console.
#' `'iteration'` or logical `TRUE` is only iteration history, `'none'` or logical `FALSE`
#' suppresses all output to console, and `'all'` prints all output to console.
#' @param nopowershell Windows only. Uses cmd.exe with some limited functions (instead of powershell).
#' @returns a [`rblimp::blimp_out`] object
#' @export
rblimp_source <- function(
        file,
        plots = FALSE,
        output = TRUE,
        nopowershell = FALSE) {

    # Establish blimp exec
    blimp_path <- detect_blimp()

    # Preprocess path for non-unix (assumes windows)
    if (.Platform$OS.type != "unix") {
        blimp_path <- if (output == "all") {
            paste0("'", blimp_path, "'")
        } else {
            paste0('"', blimp_path, '"')
        }
    }

    # Set up exit code
    exitcode <- 0

    # Get dir from file
    folder <- dirname(file)

    # Make plot dir
    if (plots) dir.create(file.path(folder, "plots"), showWarnings = F)

    # Make Command
    cmd <- if (plots) {
        paste(blimp_path, file, "--traceplot", file.path(folder, "plots"), "--truncate 100")
    } else {
        paste(blimp_path, file, "--truncate 100")
    }

    # Run command
    if (.Platform$OS.type == "unix") {
        if (output == "all") {
            exitcode <- system(paste0(cmd, "| tee ", file.path(folder, "output.blimp-out")))
        } else {
            exitcode <- system(paste(cmd, "--output", file.path(folder, "output.blimp-out")),
                               ignore.stdout = output == "none"
            )
        }
        result <- readLines(file.path(folder, "output.blimp-out"))
    } else {
        if (nopowershell) {
            result <- shell(cmd, intern = output == "none")
        } else {
            if (output == "all") {
                exitcode <- system(paste0('powershell -command \" & ', cmd, " | tee '", file.path(folder, "output.blimp-out"), '\'\"'))
                result <- readLines(file.path(folder, "output.blimp-out"), skipNul = TRUE)[-1] # Remove NULL
                result <- result[c(1, seq(0, length(result), 2))]
            } else {
                exitcode <- shell(paste(cmd, "--output", file.path(folder, "output.blimp-out")),
                                  ignore.stdout = output == "none"
                )
                result <- readLines(file.path(folder, "output.blimp-out"))
            }
        }
    }

    # Return output
    return(
        structure(
            result,
            class = "blimp_out",
            exitcode = exitcode
        )
    )
}
