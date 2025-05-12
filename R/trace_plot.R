# Copyright Brian Keller 2025, all rights reserved

#' Internal function to compute psrf
#' @param x matrix of parameter values by chain
#' @param split_chain Compute split chain or not
#' @noRd
psrf <- function(x, split_chain = TRUE) {
    # Coerce x to matrix
    x <- as.matrix(x)

    # Set up params
    n <- ifelse(split_chain, floor(NROW(x) / 2), NROW(x))
    m <- ifelse(split_chain, NCOL(x) * 2, NCOL(x))
    # If not enough draws error
    if (n < 2) stop("Not enough draws")

    # Create matrix of entire parameters
    chaindat <- do.call("cbind", apply(x, 2, function(x) {
        if (split_chain) {
            return(cbind(x[1:n], x[(n + 1):(2 * n)]))
        } else {
            return(x[1:n])
        }
    }, simplify = F))

    # Compute PSR
    t_j <- colMeans(chaindat)
    t_bar <- mean(t_j)

    # Compute between
    B <- (1 / (m - 1)) * sum((t_j - t_bar)^2)

    # Compute within
    W <- sum((sweep(chaindat, 2, t_j)^2) / (n - 1))
    W <- ((n - 1) / n) * (W / m)

    # Compute psr
    return(sqrt((W + B) / W))
}

#' Internal function to compute psrf based on last half
#' @param x matrix of parametr values by chain
#' @param split_chain Compute split chain or not
#' @noRd
psrf_lhalf <- function(x, split_chain = TRUE) {
    # Coerce x to matrix
    x <- as.matrix(x)
    return(psrf(x[(floor(NROW(x) / 2) + 1):NROW(x), ], split_chain))
}


#' Internal function for obtaining data set with one parameter for traceplots
#' @param model an [`blimp_obj`]
#' @param parameter selection used to obtain data
#' @noRd
make_traceplot_data <- function(model, parameter) {

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

    # Get list of data
    plot_data_list <- model@burn |>
        lapply(\(x) data.frame(
            Chain = x[, 'chain'],
            Iteration = x[, 'iteration'],
            Parameter = x[, param_num + 2]
        ))

    # Calculate psrf
    psrf_val <- plot_data_list |> lapply(\(x) x[, 'Parameter']) |>
        do.call(cbind, args = _) |> psrf_lhalf()

    # Create plot data
    plot_data <- do.call(rbind, plot_data_list)
    plot_data$`Parameter Type` <- attr(model@iterations, "parameter_type")[param_num]
    plot_data$param_num <- param_num
    plot_data$param_nam <- rownames(model@estimates)[param_num]
    plot_data$Outcome   <- attr(model@iterations, "outcome_name")[param_num]
    plot_data$Chain     <- as.factor(plot_data$Chain) # Make factor



    # Return data
    return(
        structure(
            plot_data,
            psrf = psrf_val
        )
    )

}

#' Internal function for creating facet labeller function for traceplot
#' @param pnames a vector of parameter names
#' @param psrf_val a vector of psrf values
#' @importFrom ggplot2 as_labeller
#' @noRd
make_labeller_traceplot <- function(pnames, psrf_val) {
    force(pnames)
    force(psrf_val)
    ggplot2::as_labeller(
        function(value) {
            i <- as.integer(value)
            paste0(
                pnames[i], "\n",
                sprintf("PSRF = %.3f", psrf_val[i])
            )
        }
    )
}

#' Function to generate trace plots of the burn-in iterations
#' @description
#' Generates [`ggplot2::ggplot`] plots using [`ggplot2::geom_line`] based on the output from [`rblimp`]
#' @param model an [`blimp_obj`].
#' @param selector a name of a variable, a name of a parameter, a number of a parameter,
#' or a combination of any of them. If left empty, a plot of all parameters will be returned. See Examples.
#' @param ... arguments passed to internally called [`ggplot2::facet_wrap`] when used.
#' @returns a [`ggplot2::ggplot`] plot
#' @details
#' To change colors use ggplot2's scale system. Both fill and color are used. See
#' [`ggplot2::aes_colour_fill_alpha`] for more information about setting a manual set of colors.
#'
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
#' trace_plot(m1, 'y') + ggplot2::theme_minimal()
#'
#' # Generate plot of all parameters with `y`
#' # Add limits to only graph first 250 iterations
#' trace_plot(m1, 'y') + ggplot2::xlim(0, 250) + ggplot2::theme_minimal()
#'
#' # Generate plot of all parameters
#' trace_plot(m1) + ggplot2::theme_minimal()
#'
#' # Generate plot of all parameters for `y` and `x`
#' trace_plot(m1, c('x', 'y')) + ggplot2::theme_minimal()
#' # Generate Plot of Parameter 5
#' trace_plot(m1, 5) + ggplot2::theme_minimal()
#'
#' # Generate plot of `x residual variance`
#' trace_plot(m1, 'x residual variance') + ggplot2::theme_minimal()
#'
#' # Generate plot of Parameters 7 and 9
#' trace_plot(m1, c(7, 9)) + ggplot2::theme_minimal()
#' @import ggplot2
#' @importFrom stats quantile
#' @importFrom methods is
#' @export
trace_plot <- function(
        model, selector,
        ...
) {

    # Check model
    if (!is(model, 'blimp_obj')) throw_error(
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
        feature_list <- list(ggplot2::ggtitle("Trace Plots for All Parameters"))
    }
    # Handle length 1
    if (length(selector) == 1) {

        # Check if variable or parameter
        o <- grep(paste0('\\b', selector, '\\b'), oname, ignore.case = TRUE)
        # If variable name return all parameters and new title
        if (length(o) > 0) {
            return(
                trace_plot(model, o, ...)
                + ggplot2::ggtitle(
                    paste0("Trace Plots for ", selector, " Model Parameters"),
                )
            )
        } else {
            plot_data <- make_traceplot_data(model, selector)
            feature_list <- list(
                ggplot2::ggtitle(
                    paste0("Trace Plot for ", plot_data$param_nam[1]),
                    sprintf("PSRF = %.3f", attr(plot_data, 'psrf'))
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
        plot_data <- selector |> lapply(make_traceplot_data, model = model)

        # Get psrf values
        psrf_val <- sapply(plot_data, attr, 'psrf')

        # Combine into data set
        plot_data <- do.call(rbind, args = plot_data)

        # Build function for facet labels
        make_label <- make_labeller_traceplot(
            rownames(model@estimates),
            psrf_val
        )
        # Create feature list
        if (length(feature_list) == 0) {
            feature_list <- list(
                ggplot2::ggtitle("Trace Plots for Selected Parameters"),
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
        ggplot2::ggplot(plot_data, ggplot2::aes(Iteration, Parameter, color = Chain))
        + ggplot2::geom_line()
        + feature_list
    )
}
