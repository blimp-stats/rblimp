# Copyright Brian Keller 2025, all rights reserved

#' Function to generate conditional regression equation plots (i.e., simple effects) with [`rblimp`] and SIMPLE command
#' @description
#' Generates a conditional effect plots based based on the posterior summaries from the output of [`rblimp`].
#' @param model an [`blimp_obj`]. The object must have a SIMPLE command output saved.
#' @param variable the name of the outcome for which to create a plot
#' @param nsigma the number of standard deviations to produce credible bounds
#' @param point_col the color of the points in the plot
#' @param horz_line the color of the horiztonal zero line
#' @param col1 the color of the loess mean line
#' @param col2 the color of the loess credible bound lines
#' @param linewidth the linewidth value for the loess lines and its bounds.
#' @param ... arguments passed to [`loess`] call used to loess lines.
#' @returns a [`ggplot2::ggplot`] plot
#' @details
#' All colors are passed into `ggplot2`. See [`ggplot2::aes_colour_fill_alpha`] for details on changing colors.
#'
#' @examples
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
#'     'y ~ x m',
#'     mydata,
#'     nimps = 10,
#'     seed = 10972,
#'     burn = 1000,
#'     iter = 1000
#' )
#'
#' # Generate Plot
#' residual_plot(m1, 'y') + ggplot2::theme_minimal()
#' @import ggplot2
#' @importFrom grDevices xy.coords
#' @importFrom stats loess terms var
#' @export
residual_plot <- function(
        model, variable, nsigma = 1,
        point_col = 'black', horz_line = 'black',
        col1 = '#0571b0', col2 = '#ca0020',
        linewidth = 1.1,
        ...
) {

    # Check inputs
    if (nsigma <= 0) throw_error(
        "The {.arg nsigma} must be positive"
    )
    if (class(model) != 'blimp_obj') throw_error(
        "{.arg model} is not a `blimp_obj`"
    )
    if (length(model@imputations) == 0) throw_error(c(
        "{.arg model} does not contain imputations.",
        "i" = "Set {.arg nimps} to a positive number and rerun {.cli rblimp}."
    ))
    if (is.character(variable) == FALSE) throw_error(
        "{.arg variable} must be of class `character`"
    )
    if (length(variable) != 1) throw_error(
        "{.arg variable} must be of length one."
    )

    # Obtain Predicted and Residual scores
    pscores <- predict(model)
    rscores <- residuals(model)

    if (length(pscores) == 0) throw_error(
        "{.arg model} does not contain imputed predicted scores"
    )
    if (length(rscores) == 0) throw_error(
        "{.arg model} does not contain imputed residual scores"
    )

    # Check variable is in predicted and residuals
    # NOTE assumes that first has same names as all
    if ((paste0(variable, '.predicted') %in% names(pscores[[1]])) == FALSE) {
        throw_error(
            "{.arg model} does not contain imputed predicted scores for `{variable}`"
        )
    }
    if ((paste0(variable, '.residual') %in% names(rscores[[1]])) == FALSE) {
        throw_error(
            "{.arg model} does not contain imputed residual scores for `{variable}`"
        )
    }
    # predicted name
    predi_name <- paste0(variable, '.predicted')
    resid_name <- paste0(variable, '.residual')

    # Perform imputations
    o <- mapply(
        \(x, y) {
            # Specific for this model
            coord_data <- xy.coords(x, y)
            x <- coord_data$x; y <- coord_data$y; x0 <- sort(x);
            mod <- loess(y ~ x, ...)
            pred <- predict(mod, data.frame(x = x0), se = T)
            yfit <- pred$fit
            var <- pred$se.fit^2
            list(x = x, y = y, yfit = yfit, var = var)
        },
        x = lapply(pscores, \(.) .[, predi_name]),
        y = lapply(rscores, \(.) .[, resid_name]),
        SIMPLIFY = F
    )

    # Pool
    vars <- sapply(o, \(.) .$var)
    varW <- rowMeans(vars)
    varB <- (1 / (NCOL(vars) - 1))*apply(vars, 1, var)
    sd <- sqrt(varW + varB + varB/NCOL(vars))

    yfit <- rowMeans(sapply(o, \(.) .$yfit))
    x <- rowMeans(sapply(o, \(.) .$x))
    y <- rowMeans(sapply(o, \(.) .$y))
    x0 <- sort(x)

    # Return Plot information
    return(
        ggplot2::ggplot()
        + ggplot2::geom_point(ggplot2::aes(x, y), alpha = .25, color = point_col)
        + ggplot2::geom_hline(yintercept = 0, color = horz_line)
        + ggplot2::geom_line(ggplot2::aes(x0, yfit), color = col1, linewidth = linewidth)
        + ggplot2::geom_line(ggplot2::aes(x0, yfit + nsigma * sd), color = col2, linewidth = linewidth)
        + ggplot2::geom_line(ggplot2::aes(x0, yfit - nsigma * sd), color = col2, linewidth = linewidth)
        + xlab(predi_name) + ylab(resid_name)
        + ggplot2::labs(
            title = paste('Residuals v. Predicted Values for', variable),
            subtitle = paste('Averaged over', length(pscores), 'imputations'),
            x = predi_name, y = resid_name
        )
    )
}
