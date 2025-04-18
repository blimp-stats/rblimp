# Copyright Brian Keller 2025, all rights reserved

#' Internal function for obtaining data set with one parameter
#' @param model an [`blimp_obj`]
#' @param parameter selection used to obtain data
#' @noRd
make_plot_data <- function(model, parameter) {

    # Check it is length 1
    if (length(parameter) != 1) throw_error("Unable to Determine Parameter for: {parameter}")

    # Get parameter number
    param_num <- if (is.numeric(parameter)) {
        parameter
    } else if (is.character(parameter)) {
        # Check estimate names
        c(
            grep(tolower(parameter), tolower(rownames(model@estimates)), fixed = TRUE),
            grep(tolower(parameter), tolower(names(model@iterations)), fixed = TRUE)
        )
    } else throw_error("Unable to Determine Parameter for: {parameter}")

    # Make sure that param_num is length 1
    if (length(param_num) != 1) throw_error("Unable to Determine Parameter for: {parameter}")

    # Create plot data
    plot_data <- data.frame(Parameter = model@iterations[, param_num])
    plot_data$`Parameter Type` <- attr(model@iterations, "parameter_type")[param_num]
    plot_data$param_num <- param_num
    plot_data$param_nam <- rownames(model@estimates)[param_num]
    plot_data$Outcome   <- attr(model@iterations, "outcome_name")[param_num]

    # Return data
    return(plot_data)
}

#' Internal function for creating facet labeller function
#' @param pnames a vector of parameter names
#' @param quants a matrix of quantiles
#' @importFrom ggplot2 as_labeller
#' @noRd
make_labeller <- function(pnames, quants) {
    force(pnames)
    force(quants)
    ggplot2::as_labeller(
        function(value) {
            i <- as.integer(value)
            paste0(
                pnames[i], "\n",
                sprintf("Estimate = %.3f  95%% CI = [%.3f, %.3f]", quants[i,2], quants[i,1], quants[i,3])
            )
        }
    )
}

#' Function to generate graph posterior density plots for parameters
#' @description
#' Generates [`ggplot2::ggplot`] plots using [`ggplot2::geom_density`] based on the output from [`rblimp`]
#' @param model an [`blimp_obj`].
#' @param selector a name of a variable, a name of a parameter, a number of a parameter,
#' or a combination of any of them. If left empty, a plot of all parameters will be returned. See Examples.
#' @param ... arguments passed to internally called [`ggplot2::facet_wrap`] when used.
#' @returns a [`ggplot2::ggplot`] plot
#' @details
#' To change colors use ggplot2's scale system. Both fill and color are used. See
#' [`ggplot2::scale_manual`] for more information about setting a manual set of colors.
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
#'     c(
#'         'y ~ x m',
#'         'x ~~ m'
#'     ),
#'     mydata,
#'     seed = 10972,
#'     burn = 1000,
#'     iter = 1000
#' )
#'
#' # Generate plot of all parameters with `y`
#' posterior_plot(m1, 'y') + ggplot2::theme_minimal()
#'
#' # Generate plot of all parameters
#' posterior_plot(m1) + ggplot2::theme_minimal()
#'
#' # Generate plot of all parameters for `y` and `x`
#' posterior_plot(m1, c('x', 'y')) + ggplot2::theme_minimal()
#' # Generate Plot of Parameter 5
#' posterior_plot(m1, 5) + ggplot2::theme_minimal()
#'
#' # Generate plot of `x residual variance`
#' posterior_plot(m1, 'x residual variance') + ggplot2::theme_minimal()
#'
#' # Generate plot of Parameters 7 and 9
#' posterior_plot(m1, c(7, 9)) + ggplot2::theme_minimal()
#' @import ggplot2
#' @importFrom stats quantile
#' @export
posterior_plot <- function(
        model, selector,
        ...
) {

    # Check model
    if (class(model) != 'blimp_obj') throw_error(
        "{.arg model} is not a `blimp_obj`"
    )

    # Set missing selector to NULL
    if (missing(selector)) selector <- NULL

    # Create empty list
    feature_list <- list()

    # Get outcome names
    oname <- attr(model@iterations, "outcome_name")

    # Determine type of plot
    # Produce all plots
    if (length(selector) == 0) {
        selector <- NROW(model@estimates) |> seq_len()
        # Add to feature list
        feature_list <- list(ggplot2::ggtitle("Posterior Distributions for All Parameters"))
    }
    # Handle length 1
    if (length(selector) == 1) {

        # Check if variable or parameter
        o <- grep(paste0('\\b', selector, '\\b'), oname, ignore.case = TRUE)
        # If variable name return all parameters and new title
        if (length(o) > 0) {
            return(
                posterior_plot(model, o, ...)
                + ggplot2::ggtitle(
                    paste0("Posterior Distributions for ", selector, " Model Parameters"),
                )
            )
        } else {
            plot_data <- make_plot_data(model, selector)
            quant <- quantile(plot_data$Parameter, probs = c(0.025, 0.500, 0.975))
            feature_list <- list(
                ggplot2::ggtitle(
                    paste0("Posterior Distribution for ", plot_data$param_nam[1]),
                    sprintf("Estimate = %.3f  95%% CI = [%.3f, %.3f]", quant[2], quant[1], quant[3])
                )
            )
        }
    }
    # Otherwise handle multiple
    else {
        # Check if variable or parameter
        olist <- lapply(selector, \(x) {
            grep(paste0('\\b', x, '\\b'), oname, ignore.case = TRUE) |> as.list()
        })
        for (i in seq_along(olist)) {
            if (length(olist[[i]]) == 0) olist[[i]] <- selector[i]
        }
        selector <- Reduce(c, olist) # update selector

        # Get plot data
        plot_data <- selector |> lapply(make_plot_data, model = model) |>
            do.call(rbind, args = _)

        # Build function for facet labels
        make_label <- make_labeller(
            rownames(model@estimates),
            model@iterations |>
                apply(2, quantile, probs = c(0.025, 0.500, 0.975)) |> t()
        )
        # Create feature list
        if (length(feature_list) == 0) {
            feature_list <- list(
                ggplot2::ggtitle("Posterior Distributions for Selected Parameters"),
                ggplot2::facet_wrap(
                    ~ as.factor(param_num), scales = "free", labeller = make_label,
                    ...
                )
            )
        }
        # otherwise only add wrap because already has title
        else {
            feature_list <- c(
                feature_list,
                list(ggplot2::facet_wrap(
                    ~ as.factor(param_num), scales = "free", labeller = make_label,
                    ...
                ))
            )
        }
    }

    # Return Plot information
    return(
        ggplot2::ggplot(plot_data, ggplot2::aes(Parameter))
        + ggplot2::geom_density(
            ggplot2::aes(color = `Parameter Type`, fill = `Parameter Type`), alpha = .25
        )
        + ggplot2::stat_summary(
            ggplot2::aes(xintercept = ggplot2::after_stat(x), y = 0), fun = quantile, geom = "vline",
            fun.args = list(probs = c(0.025, 0.500, 0.975)),
            orientation = "y"
        )
        + ylab('Density') + feature_list
    )
}
