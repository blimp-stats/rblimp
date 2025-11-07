
# Copyright Brian Keller 2025, all rights reserved


#' Internal function to Filter if significant or Not
#' @noRd
set_group <- function(x){ with(rle(x), {
    unlist(lapply(seq_along(lengths), \(i) rep(i, lengths[i])))
})}


#' Function to generate a Johnson-Neyman Plot of Conditional Slope based on a function to produce the conditional effect.
#' @description
#' Generates a Johnson-Neyman Plot using a function to produce the conditional effect
#' @param func a [`function`] used to compute the conditional effect on moderator.
#' @param xrange a [`numeric`] of length two with the min and max of the x-axis
#' @param ci a value between 0 and 1 specifying the credible interval size
#' @param ... values passed to internal boundary search algorithm. See Details below.
#' @returns a [`ggplot2::ggplot`] plot. The bounding values are saved in the attribute 'bounds'.
#' @details
#' To change colors use ggplot2's scale system. Both fill and color are used. See
#' [`ggplot2::aes_colour_fill_alpha`] for more information about setting a manual set of colors.
#'
#' For `...`, the arguments are passed to the internal boundary search algorithm.
#' This algorithm uses an initial grid search to locate boundaries based on the range
#' and then a binary search to refine the estimates.
#' The following arguments are available:
#' \describe{
#'   \item{n_initial}{Number of points in the initial coarse grid search used to
#'     locate approximate boundary positions. Higher values improve detection of
#'     closely-spaced boundaries but increase computation time. Default is 1000.}
#'   \item{refine_tol}{Tolerance for binary search refinement. The algorithm
#'     refines each boundary until the interval width is smaller than this value.
#'     Smaller values give higher precision but require more function evaluations.
#'     Default is 1e-12.}
#'   \item{max_iter}{Maximum number of iterations for binary search refinement
#'     per boundary. Prevents infinite loops if tolerance cannot be achieved.
#'     Default is 100.}
#'   \item{adaptive}{Logical indicating whether to perform additional refinement
#'     in regions where boundaries are detected to be closely spaced. When TRUE,
#'     uses a finer grid to resolve boundaries that may be missed by the initial
#'     coarse grid. Default is TRUE.}
#' }
#' @examplesIf has_blimp()
#' # Generate Data
#' mydata <- rblimp_sim(
#'     c(
#'         'x1 ~ normal(0, 1)',
#'         'x2 ~ normal(0, 1)',
#'         'm ~ normal(0, 1)',
#'         'y ~ normal(10 + 0.5*x1 + 0.5*x2 + m + 0.2*x1*x2 + 0.3*x2*m + 0.1*x1*m + 0.7*x1*x2*m, 1)'
#'     ),
#'     n = 100,
#'     seed = 981273
#' )
#'
#' # Run Rblimp
#' m1 <- rblimp(
#'     'y ~ x1 x2 m x1*x2 x1*m x2*m x1*x2*m',
#'     mydata,
#'     center = ~ m,
#'     seed = 10972,
#'     burn = 1000,
#'     iter = 1000
#' )
#'
#' # Get parameter values
#' params <- m1 |> as.matrix()
#'
#' # Generate Plot
#' (
#'     jn_plot_func(
#'         compute_condeff(params[,6], params[,9]),
#'         xrange = c(-3, 3)
#'     )
#'     + ggplot2::labs(
#'         title = 'Johnson-Neyman Plot for `x1` * `x2` Moderated by `x2`',
#'         subtitle = 'Red area represents 0 within 95% interval',
#'         y = 'y ~ x1 * x2',
#'         x = 'm'
#'     )
#'     + ggplot2::theme_minimal()
#' )
#' @seealso [compute_condeff()]
#' @import ggplot2
#' @export
jn_plot_func <- function(func, xrange, ci = 0.95, ...) {

    # Check inputs
    if (is.function(func) == FALSE) throw_error(
        "The {.arg func} must be a function"
    )

    if (ci >= 1.0 | ci <= 0.0) throw_error(
        "The {.arg ci} must be between 0 and 1"
    )

    if (!is.numeric(xrange) || !is.vector(xrange) || is.list(xrange) || length(xrange) != 2) {
        throw_error("{.arg {xrange}} must be a numeric vector of length 2")
    }

    if (any(is.na(xrange))) throw_error(
        "{.arg {xrange}} cannot contain NA or NaN values"
    )

    if (any(is.infinite(xrange))) throw_error(
        "{.arg {xrange}} cannot contain Inf values"
    )

    if (xrange[1] >= xrange[2]) throw_error(
        "{.arg {xrange}}[1] ({xrange[1]}) must be < {.arg {xrange}}[2] ({xrange[2]})"
    )


    # Handle probabilities
    ci <- (1 - ci) / 2
    probs <- c(ci, 1 - ci)

    # Create function
    f <- function(m) {
        # Check if 0 is within the interval (product will be negative)
        apply(func(m, quantile, probs = probs), 2, prod) >= 0
    }

    # Obtain boundaries
    boundaries <- find_boundaries(f, xrange[1], xrange[2], ...)
    bound_y <- sapply(boundaries, func, quantile, probs = probs)

    # Suppress R CMD check NOTEs about ggplot2 NSE
    x <- y <- NULL
    # Create plot
    p <- (
        ggplot()
        # Set 0 value line
        + geom_hline(yintercept = 0)
        # Create Ribbon
        + stat_function(
            fun = f,
            aes(
                # Draw ribbon along lower
                ymin = func(after_stat(x), quantile, probs = probs[1]),
                # Draw ribbon along upper
                ymax = func(after_stat(x), quantile, probs = probs[2]),
                # Set color based on 0 being in the interval
                fill = after_stat(y), group = set_group(after_stat(y))
            ),
            # Draws a ribbon transparency and
            geom = 'ribbon', alpha = 0.25, n = 1000
        )
        # Line for 2.5%
        + geom_function(
            fun = func,
            args = list(quantile, probs = probs[1]),
            color = 'black', linetype = 'dashed'
        )
        # Line for 97.5%
        + geom_function(
            fun = func,
            args = list(quantile, probs = probs[2]),
            color = 'black', linetype = 'dashed'
        )
        # Line for Median
        + geom_function(
            fun = func,
            args = list(median),
            color = 'black'
        )
    )

    if (NROW(bound_y) == 2) {
        p <- (
            p
            # Add boundary lines
            + geom_segment(
                aes(
                    x = boundaries,
                    xend = boundaries,
                    y = bound_y[1,],
                    yend = bound_y[2,],
                ),
                color = 'black', alpha = 0.50
            )
        )
    }

    p <- (
        p
        # Set range
        + xlim(xrange)
        # Set fill values
        + scale_fill_manual(guide = 'none', values = c(`FALSE` = '#ca0020', `TRUE` = '#0571b0'))
    )

    # Return plot
    return(
        structure(
            p,
            boundaries = boundaries
        )
    )
}

#' Convenience Function for computing conditional effects for [`jn_plot_func`]
#' @param value1 The base value
#' @param value2 The value to change as a function of moderator
#' @returns a [`function`]
#' @seealso [jn_plot_func()]
#' @examplesIf has_blimp()
#' # Generate Data
#' mydata <- rblimp_sim(
#'     c(
#'         'x1 ~ normal(0, 1)',
#'         'x2 ~ normal(0, 1)',
#'         'm ~ normal(0, 1)',
#'         'y ~ normal(10 + 0.5*x1 + 0.5*x2 + m + 0.2*x1*x2 + 0.3*x2*m + 0.1*x1*m + 0.7*x1*x2*m, 1)'
#'     ),
#'     n = 100,
#'     seed = 981273
#' )
#'
#' # Run Rblimp
#' m1 <- rblimp(
#'     'y ~ x1 x2 m x1*x2 x1*m x2*m x1*x2*m',
#'     mydata,
#'     center = ~ m,
#'     seed = 10972,
#'     burn = 1000,
#'     iter = 1000
#' )
#'
#' # Get parameter values
#' params <- m1 |> as.matrix()
#'
#' # Generate Plot
#' (
#'     jn_plot_func(
#'         compute_condeff(params[,6], params[,9]),
#'         xrange = c(-3, 3)
#'     )
#'     + ggplot2::labs(
#'         title = 'Johnson-Neyman Plot for `x1` * `x2` Moderated by `x2`',
#'         subtitle = 'Red area represents 0 within 95% interval',
#'         y = 'y ~ x1 * x2',
#'         x = 'm'
#'     )
#'     + ggplot2::theme_minimal()
#' )
#' @export
compute_condeff <- function(value1, value2) {
    force(value1); force(value2)
    function(m, func, ...) {
        o <- sapply(m, \(x) value1 + value2 * x, simplify = TRUE)
        apply(o, 2, func, ...)
    }
}

#' Function to generate a Johnson-Neyman Plot of Conditional Slope with [`rblimp`]
#' @description
#' Generates a Johnson-Neyman Plot based on the posterior summaries from the output of [`rblimp`].
#' @param formula an object of class [`formula`] to specify simple effect to plot.
#' The formula must have the following form: `outcome ~ focal | moderator`. See Details below for nominal moderators.
#' @param model an [`blimp_obj`].
#' @param ci a value between 0 and 1 specifying the credible interval size
#' @param ... passed bounds search algorithm. See [`jn_plot_func`] for details.
#' @returns a [`ggplot2::ggplot`] plot. The bounding values are saved in the attribute 'bounds'.
#' @details
#' To change colors use ggplot2's scale system. Both fill and color are used. See
#' [`ggplot2::aes_colour_fill_alpha`] for more information about setting a manual set of colors.
#'
#' @seealso [jn_plot_func()]
#' @examplesIf has_blimp()
#' # Generate Data
#' mydata <- rblimp_sim(
#'     c(
#'         'x ~ normal(0, 1)',
#'         'm ~ normal(0, 1)',
#'         'y ~ normal(10 + 0.5*x + m + 0.2*x*m, 1)'
#'     ),
#'     n = 100,
#'     seed = 981273
#' )
#'
#' # Run Rblimp
#' m1 <- rblimp(
#'     'y ~ x m x*m',
#'     mydata,
#'     center = ~ m,
#'     simple = 'x | m',
#'     seed = 10972,
#'     burn = 1000,
#'     iter = 1000
#' )
#'
#' # Generate Plot
#' jn_plot(y ~ x | m, m1)
#' @import ggplot2
#' @importFrom methods is
#' @export
jn_plot <- function(formula, model, ci = 0.95, ...) {

    # Extract Characters
    f <- formula |> as.character() |>
        gsub('`', '', x = _) |> gsub(' \\+ ', ' ', x = _)

    # Check inputs
    if (length(f) != 3) throw_error(c(
        "The {.arg formula} was not correctly specified.",
        "Must have the form: `outcome ~ focal | moderator`"
    ))

    if (!is(model, 'blimp_obj')) throw_error(
        "{.arg model} is not a `blimp_obj`"
    )

    # Obtain variable names
    tmp <- strsplit(f[3], ' \\| ')[[1]]
    if (length(tmp) != 2) throw_error(c(
        "The {.arg formula} was not correctly specified.",
        "Must have the form: `outcome ~ focal | moderator`"
    ))
    out <- f[2]
    pre <- tmp[1]
    mod <- tmp[2]

    # Check if it is centered
    pre_is_cent <- if (is.null(model@syntax$center)) FALSE else{
        (tolower(pre) |> sub("\\s*\\[[^]]*\\]$", "", x = _)) %in% (
            model@syntax$center |> strsplit(' ') |> unlist() |> tolower() |>
                gsub(';', '', x = _)
        )
    }
    mod_is_cent <- if (is.null(model@syntax$center)) FALSE else{
        (tolower(mod) |> sub("\\s*\\[[^]]*\\]$", "", x = _)) %in% (
            model@syntax$center |> strsplit(' ') |> unlist() |> tolower() |>
                gsub(';', '', x = _)
        )
    }

    ## Obtain m_range
    ind <- (model@average_imp |> names() |> tolower()) == tolower(mod)

    # Check if it is found. If not check latent variables.
    if (sum(ind) != 1) {
        ind <- (model@average_imp |> names() |> tolower()) == tolower(paste0(mod, ".latent"))
    }
    # If that isn't found crash out
    if (sum(ind) != 1)  throw_error(
        "Cannot find moderator in imputed data"
    )
    m <- if (mod_is_cent) mean(model@average_imp[,ind]) else 0.0
    m_range <- (model@average_imp[,ind] - m) |> pretty() |> range()

    ## Find indicators for two slopes
    # get parameter names and outcome model names
    pnames <- model@estimates |> row.names() |> tolower()
    bx_sel <- which(pnames == tolower(paste0(out, " ~ ", pre)))
    bxm_s1 <- which(pnames == tolower(paste0(out, " ~ ", pre, "*", mod)))
    bxm_s2 <- which(pnames == tolower(paste0(out, " ~ ", mod, "*", pre)))

    ## Error handling
    if (length(bx_sel) == 0) {
        pname <- paste0(out, " ~ ", pre)
        throw_error(c(
            x = "Cannot find required parameter `{pname}`",
            i = "Ensure that it is one of the row names in `summary(model)`"
        ))
    }
    if (length(bxm_s1) == 0 && length(bxm_s2) == 0) {
        pname <- paste0(out, " ~ ", pre, "*", mod)
        throw_error(c(
            x = "Cannot find required parameter `{pname}`",
            i = "Ensure that it is one of the row names in `summary(model)`"
        ))
    }
    if ((length(bxm_s1) == 1 && length(bxm_s2) == 1) ||
        length(bxm_s1) > 1 ||  length(bxm_s2) > 1) {
        throw_error(c(
            i = "Multiple interactions were found.",
            x = "Cannot parse model"
        ))
    }

    ## Obtain iterations
    iter <- model |> as.matrix()

    # Create plot
    plt <- jn_plot_func(
        compute_condeff(
            iter[, bx_sel],
            iter[, c(bxm_s1, bxm_s2)]
        ),
        xrange = m_range,
        ci = ci
    )

    # Get boundaries and create subtitle
    bounds <- attr(plt, 'boundaries')
    subt <- if (length(bounds) == 1L) {
        paste0("\nBound: ",  paste(sprintf("%.3g", bounds), collapse = ", "))
    } else if (length(bounds) > 1L) {
        paste0("\nBounds: ",  paste(sprintf("%.3g", bounds), collapse = ", "))
    } else NULL

    # Return plot
    return(
        structure(
            # Compute plot based on function
            plt
            # Set labels
            + labs(
                title = "Johnson-Neyman Plot of Conditional Slope",
                subtitle = paste0(
                    "Red area represents 0 within 95% interval",
                    subt
                ),
                y = paste(out, "~", if (pre_is_cent) paste("Centered", pre) else pre),
                x = if (mod_is_cent) paste("Centered", mod) else mod
            ),
            bounds = bounds
        )
    )
}
