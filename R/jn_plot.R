
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
#' @returns a [`ggplot2::ggplot`] plot
#' @details
#' To change colors use ggplot2's scale system. Both fill and color are used. See
#' [`ggplot2::aes_colour_fill_alpha`] for more information about setting a manual set of colors.
#' @examplesIf has_blimp()
#' # set seed
#' set.seed(981273)
#'
#' # Generate Data
#' mydata <- data.frame(
#'     x1 = rnorm(100),
#'     x2 = rnorm(100),
#'     m = rnorm(100)
#' )
#' mydata$y <- with(
#'     mydata,
#'     rnorm(100,
#'           10 + x1*0.5 + x2*0.5 + m + .2*x1*x2 + .3*x2*m + .1*x1*m + .7*x1*x2*m,
#'           1
#'     )
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
jn_plot_func <- function(func, xrange, ci = 0.95) {

    # Check inputs
    if (is.function(func) == FALSE) throw_error(
        "The {.arg func} must be a function"
    )

    if (ci >= 1.0 | ci <= 0.0) throw_error(
        "The {.arg ci} must be between 0 and 1"
    )

    # Handle probabilities
    ci <- (1 - ci) / 2
    probs <- c(ci, 1 - ci)

    # Return plot
    return(
        ggplot()
        # Set 0 value line
        + geom_hline(yintercept = 0)
        # Create Ribbon
        + stat_function(
            fun = \(m) {
                # Check if 0 is within the interval (product will be negative)
                apply(func(m, quantile, probs = probs), 2, prod) >= 0
            },
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
        # Set range
        + xlim(xrange)
        # Set fill values
        + scale_fill_manual(guide = 'none', values = c('#ca0020', '#0571b0'))
    )
}

#' Convenience Function for computing conditional effects for [`jn_plot_func`]
#' @param value1 The base value
#' @param value2 The value to change as a function of moderator
#' @returns a [`function`]
#' @seealso [jn_plot_func()]
#' @examplesIf has_blimp()
#' # set seed
#' set.seed(981273)
#'
#' # Generate Data
#' mydata <- data.frame(
#'     x1 = rnorm(100),
#'     x2 = rnorm(100),
#'     m = rnorm(100)
#' )
#' mydata$y <- with(
#'     mydata,
#'     rnorm(100,
#'           10 + x1*0.5 + x2*0.5 + m + .2*x1*x2 + .3*x2*m + .1*x1*m + .7*x1*x2*m,
#'           1
#'     )
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
        o <- sapply(m, \(x) value1 + value2 * x, simplify = T)
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
#' @returns a [`ggplot2::ggplot`] plot
#' @details
#' To change colors use ggplot2's scale system. Both fill and color are used. See
#' [`ggplot2::aes_colour_fill_alpha`] for more information about setting a manual set of colors.
#'
#' @seealso [jn_plot_func()]
#' @examplesIf has_blimp()
#' # set seed
#' set.seed(981273)
#'
#' # Generate Data
#' mydata <- data.frame(
#'     x = rnorm(100),
#'     m = rnorm(100)
#' )
#' mydata$y <- with(mydata, rnorm(100, 10 + x*0.5 + m + .2*x*m, 1))
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
jn_plot <- function(formula, model, ci = 0.95) {

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
        tolower(pre) %in% (
            model@syntax$center |> strsplit(' ') |> unlist() |> tolower()
        )
    }
    mod_is_cent <- if (is.null(model@syntax$center)) FALSE else{
        tolower(mod) %in% (
            model@syntax$center |> strsplit(' ') |> unlist() |> tolower()
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
    m <- if (pre_is_cent) mean(model@average_imp[,ind]) else 0.0
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

    # Return plot
    return(
        # Compute plot based on function
        jn_plot_func(
            compute_condeff(
                iter[, bx_sel],
                iter[, c(bxm_s1, bxm_s2)]
            ),
            xrange = m_range,
            ci = ci
        )
        # Set labels
        + labs(
            title = "Johnson-Neyman Plot of Conditional Slope",
            subtitle = "Red area represents 0 within 95% interval",
            y = paste(out, "~", pre),
            x = if (mod_is_cent) paste("Centered", mod) else mod
        )
    )
}
