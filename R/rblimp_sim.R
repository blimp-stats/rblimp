## Simulate data using Blimp
# Copyright Brian Keller 2025, all rights reserved

#' Simulate data using Blimp
#'
#' @description
#' Generate simulated datasets using Blimp's SIMULATE command.
#' Supports both single-level and multilevel data generation.
#'
#' @param model Character string or vector specifying data generation equations.
#'   Can be specified as a vector: `c("x = normal(0, 1)", "y = normal(10, 1)")`
#'   OR as a single string with newlines/semicolons: `"x = normal(0, 1); y = normal(10, 1);"`
#'   For multilevel: use named list with arbitrary level names (e.g., `list(schools = ..., students = ...)`).
#'   The sample size determines the level hierarchy (largest = level-1, smallest = highest level).
#' @param n Sample size. For single-level: integer (e.g., `1000`).
#'   For multilevel: named list with sample sizes (e.g., `list(students = 1000, schools = 100)`).
#'   Names must match the names used in `model` list. Sample sizes must be unique.
#' @param seed Random seed for reproducibility.
#' @param define Character vector or string of parameter definitions (optional).
#'   Can be `c("b0 = 10", "b1 = 0.5")` OR `"b0 = 10; b1 = 0.5;"`.
#' @param variables Formula or character string specifying which variables to save (optional).
#'   If not specified, all variables from the model are saved.
#' @param tmpfolder Character string. Temporary folder path. If not specified, creates one with [`tempdir`].
#' @param nopowershell Windows only. Uses cmd.exe instead of powershell.
#'
#' @returns A data.frame with simulated data. The returned object has two attributes:
#'   \describe{
#'     \item{syntax}{The Blimp syntax used for simulation (blimp_syntax object)}
#'     \item{output}{The raw Blimp output (blimp_out object)}
#'   }
#'   Access these with `attr(result, "syntax")` and `attr(result, "output")`.
#'
#' @examplesIf has_blimp()
#' # Simple regression: y = 10 + 0.5*x + error
#' dat <- rblimp_sim(
#'   model = c(
#'     "x = normal(0, 1)",
#'     "y = normal(10 + x*0.5, 1)"
#'   ),
#'   n = 1000,
#'   seed = 10972
#' )
#'
#' # Same thing with full quoted syntax
#' dat <- rblimp_sim(
#'   model = "x = normal(0, 1);
#'            y = normal(10 + x*0.5, 1);",
#'   n = 1000,
#'   seed = 10972
#' )
#'
#' # With parameter definitions
#' dat <- rblimp_sim(
#'   model = c(
#'     "x = normal(0, 1)",
#'     "y = normal(b0 + b1*x, s2e)"
#'   ),
#'   n = 1000,
#'   define = c("b0 = 10", "b1 = 0.5", "s2e = 1"),
#'   seed = 10972
#' )
#'
#' # Multilevel model
#' dat <- rblimp_sim(
#'   model = list(
#'     schools = c(
#'       "z = normal(0, 1)",
#'       "u0 = normal(10 + z*-0.5, 1.0)"
#'     ),
#'     students = c(
#'       "x = normal(0, 1)",
#'       "y = normal(u0 + x*0.5, 1)"
#'     )
#'   ),
#'   n = list(students = 1000, schools = 100),
#'   seed = 198723
#' )
#'
#' # Access syntax and output
#' syntax <- attr(dat, "syntax")
#' print(syntax)
#'
#' output <- attr(dat, "output")
#' print(output)
#'
#' @seealso [`SIMULATE()`] for creating a simulation specification to use with [`rblimp()`]
#'
#' @export
rblimp_sim <- function(
    model,
    n,
    seed,
    define = NULL,
    variables = NULL,
    tmpfolder,
    nopowershell = FALSE
) {

    # Validate inputs
    if (missing(model)) throw_error("{.arg model} is required")
    if (missing(n)) throw_error("{.arg n} is required")
    if (missing(seed)) throw_error("{.arg seed} is required for reproducibility")

    # Create temp folder if needed
    if (missing(tmpfolder)) {
        tmpfolder <- tempfile()
        if (!dir.create(tmpfolder)) {
            throw_error("Unable to create temporary directory")
        }
        cleanup <- TRUE
    } else {
        cleanup <- FALSE
    }

    # Generate syntax
    sim_syntax_obj <- rblimp_sim_syntax(
        model = model,
        n = n,
        seed = seed,
        define = define,
        variables = variables
    )

    # Write syntax file
    fileConn <- file(file.path(tmpfolder, "simulate.imp"))
    writeLines(as.character(sim_syntax_obj), fileConn)
    close(fileConn)

    # Run Blimp
    result <- rblimp_source(
        file.path(tmpfolder, "simulate.imp"),
        plots = FALSE,
        output = TRUE,
        nopowershell = nopowershell
    )

    # Check exit code
    exitcode <- attr(result, "exitcode")
    if (length(exitcode) == 1 && exitcode == "1") {
        # Capture syntax and output for error message
        syntax_text <- as.character(sim_syntax_obj)
        output_text <- paste(result, collapse = "\n")

        if (cleanup) unlink(tmpfolder, recursive = TRUE)
        throw_error("Blimp simulation failed.")
    }

    # Read simulated data
    sim_data <- read.csv(
        file.path(tmpfolder, "generated_data.csv"),
        header = TRUE
    )

    # Cleanup temp folder
    if (cleanup) unlink(tmpfolder, recursive = TRUE)

    # Add syntax and output as attributes
    attr(sim_data, "syntax") <- sim_syntax_obj
    attr(sim_data, "output") <- result

    return(sim_data)
}


#' Create a SIMULATE specification for use in rblimp()
#'
#' @description
#' Creates a simulation specification that can be passed to `rblimp()` as the `data` argument.
#' Instead of reading existing data, `rblimp()` will use Blimp's SIMULATE command to generate
#' data and then fit the specified model to it.
#'
#' @param model Character string or vector specifying data generation equations.
#'   Same format as [`rblimp_sim()`].
#' @param n Sample size. For single-level: integer. For multilevel: named list.
#' @param define Character vector or string of parameter definitions (optional).
#' @param variables Formula or character string specifying which variables to save (optional).
#'
#' @returns A `blimp_simulate` object (subclass of `blimp_syntax`) that can be passed to
#'   `rblimp()` as the `data` argument.
#'
#' @examplesIf has_blimp()
#' # Create simulation specification
#' sim_spec <- SIMULATE(
#'   model = c(
#'     "x = normal(0, 1)",
#'     "y = normal(10 + x*0.5, 1)"
#'   ),
#'   n = 1000
#' )
#'
#' # View the specification
#' print(sim_spec)
#'
#' # Use in rblimp to fit a model to simulated data
#' mdl <- rblimp(
#'   model = "y ~ x",
#'   data = sim_spec,
#'   seed = 123,
#'   burn = 5000,
#'   iter = 5000
#' )
#'
#' summary(mdl)
#'
#' @seealso [`rblimp_sim()`] for directly generating simulated data without fitting a model
#'
#' @export
SIMULATE <- function(
    model,
    n,
    define = NULL,
    variables = NULL
) {

    # Validate inputs
    if (missing(model)) throw_error("{.arg model} is required")
    if (missing(n)) throw_error("{.arg n} is required")

    # Build the simulate block (same as sim_syntax does)
    simulate_block <- build_simulate_block(n, model, define)

    # Parse formula inputs
    if (!missing(variables) && !is.null(variables) && is.formula(variables)) {
        variables <- parse_formula(variables)
    }

    # Build syntax structure
    inputfile <- list()
    inputfile$simulate <- paste(simulate_block, collapse = "\n")

    if (!is.null(variables)) {
        inputfile$variables <- parse_cmd(variables)
    }

    # Return as blimp_syntax subclass (no seed, no save)
    structure(
        inputfile,
        class = c("blimp_simulate", "blimp_syntax")
    )
}


#' Generate Blimp syntax for simulation (syntax-only version)
#'
#' @description
#' Generates Blimp simulation syntax without running the simulation.
#' Useful for inspecting the syntax before running or saving to a file.
#'
#' @rdname rblimp_sim
#' @noRd
rblimp_sim_syntax <- function(
    model,
    n,
    seed,
    define = NULL,
    variables = NULL
) {

    # Parse formula inputs
    if (!missing(variables) && !is.null(variables) && is.formula(variables)) {
        variables <- parse_formula(variables)
    }

    # Build SIMULATE block
    simulate_block <- build_simulate_block(n, model, define)

    # Build syntax
    make_sim_syntax(
        simulate = simulate_block,
        variables = variables,
        seed = seed
    )
}


#' Normalize input - handle both vector and full string syntax
#' @noRd
normalize_input <- function(x) {
    if (is.null(x)) return(NULL)

    if (length(x) == 1) {
        lines <- strsplit(x, ";|\n")[[1]]
        lines <- trimws(lines)
        lines <- lines[lines != ""]
        return(lines)
    } else {
        return(trimws(x))
    }
}


#' Build SIMULATE block from inputs
#' @noRd
build_simulate_block <- function(n, model, define) {

    result <- character()

    if (is.numeric(n) && length(n) == 1) {
        result <- c(result, paste0("n = ", n))

        if (!is.null(define)) {
            define_lines <- normalize_input(define)
            result <- c(result, "define:")
            result <- c(result, paste0("    ", define_lines))
        }

        model_lines <- normalize_input(model)
        result <- c(result, "n:")
        result <- c(result, paste0("    ", model_lines))

    } else if (is.list(n)) {
        n_values <- unlist(n)
        if (any(duplicated(n_values))) {
            dup_vals <- unique(n_values[duplicated(n_values)])
            throw_error(
                "All sample sizes in {.arg n} must be unique to determine level hierarchy. ",
                "Found duplicate sample sizes: {paste(dup_vals, collapse = ', ')}"
            )
        }

        n_sorted <- sort(n_values, decreasing = TRUE)
        level_names <- names(n_sorted)

        for (i in seq_along(level_names)) {
            level_name <- level_names[i]
            result <- c(result, paste0(level_name, "(", i, ") = ", n_sorted[i]))
        }

        if (!is.null(define)) {
            define_lines <- normalize_input(define)
            result <- c(result, "define:")
            result <- c(result, paste0("    ", define_lines))
        }

        # Model blocks - higher levels (smaller n) come first, so reverse order
        for (i in rev(seq_along(level_names))) {
            level_name <- level_names[i]
            if (!is.null(model[[level_name]])) {
                model_lines <- normalize_input(model[[level_name]])
                result <- c(result, paste0(level_name, ":"))
                result <- c(result, paste0("    ", model_lines))
            }
        }

    } else {
        throw_error("{.arg n} must be a single integer or a named list")
    }

    return(result)
}


#' Make simulation syntax
#' @noRd
make_sim_syntax <- function(simulate, variables, seed) {

    inputfile <- list()

    # Format SIMULATE block - as.character.blimp_syntax will normalize semicolons and handle formatting
    inputfile$simulate <- paste(simulate, collapse = "\n")

    if (!is.null(variables)) {
        inputfile$variables <- parse_cmd(variables)
    }

    inputfile$seed <- parse_cmd(seed)

    inputfile$save <- "dataset = generated_data.csv"

    structure(
        inputfile,
        class = 'blimp_syntax'
    )
}
