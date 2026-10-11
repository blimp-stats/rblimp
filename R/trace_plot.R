# Copyright Brian Keller 2025, all rights reserved

#' Internal function to compute Rhat
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
    if (n < 2) throw_error("Not enough draws")

    # Create matrix of entire parameters
    chaindat <- do.call("cbind", apply(x, 2, function(x) {
        if (split_chain) {
            return(cbind(x[1:n], x[(n + 1):(2 * n)]))
        } else {
            return(x[1:n])
        }
    }, simplify = FALSE))

    # Rhat: sqrt((B/W + n - 1) / n)
    var_between <- n * var(colMeans(chaindat))
    var_within  <- mean(apply(chaindat, 2, var))

    # Return NA for constant draws
    if (var_within < .Machine$double.eps) return(NA)

    return(sqrt((var_between / var_within + n - 1) / n))
}

#' Internal function to rank-normalize and compute Rhat (Vehtari et al., 2021)
#' @param x matrix of parameter values (rows=iterations, cols=chains)
#' @noRd
rhat <- function(x) {
    x <- as.matrix(x)

    # Split each chain in half
    n <- floor(NROW(x) / 2)
    x_split <- do.call("cbind", lapply(seq_len(NCOL(x)), function(i) {
        cbind(x[1:n, i], x[(n + 1):(2 * n), i])
    }))

    # Rank-normalize the split chains (Blom transform + inverse normal)
    S <- length(x_split)
    r <- rank(as.vector(x_split), ties.method = "average")
    z_bulk <- matrix(qnorm((r - 3/8) / (S + 1/4)), nrow = n)

    # Fold around median, split, rank-normalize
    x_fold <- abs(x - median(x))
    x_fold_split <- do.call("cbind", lapply(seq_len(NCOL(x_fold)), function(i) {
        cbind(x_fold[1:n, i], x_fold[(n + 1):(2 * n), i])
    }))
    r_f <- rank(as.vector(x_fold_split), ties.method = "average")
    z_tail <- matrix(qnorm((r_f - 3/8) / (S + 1/4)), nrow = n)

    # Compute basic Rhat on each using psrf (already split, so split_chain = FALSE)
    rhat_bulk <- psrf(z_bulk, split_chain = FALSE)
    rhat_tail <- psrf(z_tail, split_chain = FALSE)

    if (is.na(rhat_bulk) && is.na(rhat_tail)) return(NA)
    return(max(rhat_bulk, rhat_tail, na.rm = TRUE))
}

#' Internal function to compute Rhat based on last half
#' @param x matrix of parameter values by chain
#' @noRd
rhat_lhalf <- function(x) {
    x <- as.matrix(x)
    return(rhat(x[(floor(NROW(x) / 2) + 1):NROW(x), , drop = FALSE]))
}

#' Internal function to compute psrf based on last half
#' @param x matrix of parameter values by chain
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
        sterm <- tolower(trimws(parameter))
        union(
            which(tolower(rownames(model@estimates)) == sterm),
            which(tolower(names(model@iterations)) == sterm)
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

    # Calculate Rhat
    psrf_val <- plot_data_list |> lapply(\(x) x[, 'Parameter']) |>
        do.call(cbind, args = _) |> rhat_lhalf()

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
make_labeller_traceplot <- function(pnames, psrf_val, param_nums) {
    force(pnames)
    force(psrf_val)
    force(param_nums)
    ggplot2::as_labeller(
        function(value) {
            i <- as.integer(value)
            j <- match(i, param_nums)
            paste0(
                pnames[i], "\n",
                sprintf("Rhat = %.3f", psrf_val[j])
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

    # Titles, and a facet labeller when there is one panel per parameter
    title <- subtitle <- labeller <- NULL

    # Get outcome names
    oname <- attr(model@iterations, "outcome_name")

    # Determine type of plot
    # Produce all plots
    if (length(selector) == 0) {
        selector <- NROW(model@estimates) |> seq_len()
        title <- "Trace Plots for All Parameters"
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
            title <- paste0("Trace Plot for ", plot_data$param_nam[1])
            subtitle <- sprintf("Rhat = %.3f", attr(plot_data, 'psrf'))
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
        labeller <- make_labeller_traceplot(
            rownames(model@estimates),
            psrf_val,
            selector
        )
        # A title for all parameters is kept
        if (is.null(title)) title <- "Trace Plots for Selected Parameters"
    }

    draw_trace(list(data = plot_data, title = title, subtitle = subtitle, labeller = labeller), ...)
}

#' Internal function to draw the trace plot from its plot data `d`
#' The saved plot keeps this function's environment, so it is given only what it draws.
#' `...` goes to [`ggplot2::facet_wrap`].
#' @noRd
draw_trace <- function(d, ...) {
    # Evaluate `...` now, so the plot does not keep the caller's environment
    list(...)
    # Suppress R CMD check NOTEs about ggplot2 NSE
    Iteration <- Parameter <- Chain <- NULL
    ggplot2::ggplot(d$data, ggplot2::aes(Iteration, Parameter, color = Chain)) + list(
        ggplot2::geom_line(),
        if (is.null(d$subtitle)) ggplot2::ggtitle(d$title) else ggplot2::ggtitle(d$title, d$subtitle),
        if (!is.null(d$labeller)) ggplot2::facet_wrap(~ as.factor(param_num), scales = "free", labeller = d$labeller, ...)
    )
}
