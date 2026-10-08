# Copyright Brian Keller 2026, all rights reserved

#' Predicted probabilities across a predictor for an outcome in a [`blimp_obj`]
#' @description
#' Plots the predicted probability of each category of an ordinal or nominal outcome against
#' a predictor, using every saved imputation.
#' @param model a [`blimp_obj`] with saved imputations.
#' @param variable a character string naming an ordinal or nominal outcome.
#' @param x a character string naming one predictor of `variable`.
#' @returns a [`ggplot2::ggplot`] plot
#' @details
#' Every saved category probability is plotted: all categories for an ordinal outcome and the
#' non-reference categories for a nominal outcome. Points stack all imputations. For a
#' continuous predictor, each category gets a loess curve fit within each imputation and
#' averaged; for a predictor declared ORDINAL, NOMINAL, or COUNT, each category gets its mean
#' probability at each level.
#'
#' Categories are distinguished by colour, point shape, and line type, so the plot also reads
#' in greyscale. These come from the theme's discrete palettes (`palette.colour.discrete`,
#' `palette.shape.discrete`, and `palette.linetype.discrete`); without shape or line type
#' palettes, a fixed set of seven of each is used. Point transparency and the category-mean
#' markers are set with [`blimp_style`].
#' @seealso [`residual_plot`], [`diagnostic_plots`], [`blimp_style`]
#' @examplesIf has_blimp()
#' # Generate Data
#' mydata <- rblimp_sim(
#'     c(
#'         'x ~ normal(0, 1)',
#'         'ystar ~ normal(0.7*x, 1)'
#'     ),
#'     n = 200,
#'     seed = 981273
#' )
#' mydata$y <- cut(mydata$ystar, c(-Inf, -0.5, 0.5, Inf), labels = FALSE)
#'
#' # Run Rblimp
#' m1 <- rblimp('y ~ x', mydata, ordinal = 'y', nimps = 10, seed = 10972, burn = 1000, iter = 1000)
#'
#' # Predicted probability across x
#' probability_plot(m1, 'y', 'x')
#' @import ggplot2
#' @export
probability_plot <- function(model, variable, x) {

    # Check inputs
    check_diagnostic_model(model)
    check_variable_arg(variable, "diagnostic_plots")
    outcome <- diag_outcome(model, variable)
    if (!is.null(outcome$category_of)) {
        variable <- outcome$category_of
        outcome <- diag_outcome(model, variable)
    }
    if (outcome$level != 1L || !outcome$type %in% c("ordinal", "nominal")) throw_error(
        "Predicted probabilities are plotted only for ordinal and nominal outcomes."
    )
    preds <- diag_outcome_predictors(model, outcome)
    allowed <- c(preds$level1, preds$level2)
    if (missing(x) || !is.character(x) || length(x) != 1) throw_error(c(
        "{.arg x} must be a single predictor name.",
        "i" = "Predictors of {.val {variable}}: {.val {names(allowed)}}."
    ))
    col <- resolve_predictor(x, allowed)
    if (is.na(col)) throw_error(c(
        "{.val {x}} is not a predictor of {.val {variable}}.",
        "i" = "Predictors of {.val {variable}}: {.val {names(allowed)}}."
    ))
    is_discrete <- names(allowed)[match(col, allowed)] %in% diag_discrete_vars(model)
    imp_list <- lapply(model@imputations, as.data.frame)

    # Category probability columns
    rx <- paste0("^", gsub(".", "\\.", variable, fixed = TRUE), "\\.([0-9]+)\\.probability$")
    prob_cols <- grep(rx, names(imp_list[[1]]), value = TRUE)
    if (!length(prob_cols)) throw_error("No saved probabilities for {.val {variable}}.")
    ks <- as.integer(sub(rx, "\\1", prob_cols))
    prob_cols <- prob_cols[order(ks)]
    cats <- as.character(sort(ks))
    K <- length(cats)
    single <- K == 1L

    long_df <- do.call(rbind, lapply(imp_list, \(d) do.call(rbind, lapply(seq_len(K), \(i) {
        z <- data.frame(x = d[[col]], prob = d[[prob_cols[i]]], category = factor(cats[i], levels = cats))
        z[is.finite(z$x) & is.finite(z$prob), , drop = FALSE]
    }))))
    M <- length(imp_list)

    p <- ggplot2::ggplot()
    p <- if (single) {
        p + geom_point_mi(data = long_df, ggplot2::aes(x = x, y = prob), n_imps = M)
    } else {
        p + geom_point_mi(data = long_df, ggplot2::aes(x = x, y = prob, colour = category, shape = category), n_imps = M)
    }

    if (is_discrete) {
        # Category means at each level, within each imputation then averaged
        cm <- do.call(rbind, lapply(imp_list, \(d) do.call(rbind, lapply(seq_len(K), \(i) {
            z <- data.frame(x = d[[col]], prob = d[[prob_cols[i]]], category = factor(cats[i], levels = cats))
            z <- z[is.finite(z$x) & is.finite(z$prob), , drop = FALSE]
            if (nrow(z)) stats::aggregate(prob ~ x + category, data = z, FUN = mean)
        }))))
        cell <- stats::aggregate(prob ~ x + category, data = cm, FUN = mean)
        p <- if (single) {
            p + ggplot2::geom_line(data = cell, ggplot2::aes(x = x, y = prob, colour = ggplot2::from_theme(accent),
                    linewidth = ggplot2::from_theme(linewidth * curve_line_ratio))) +
                summary_points(ggplot2::geom_point(data = cell, ggplot2::aes(x = x, y = prob, colour = ggplot2::from_theme(accent))))
        } else {
            p + ggplot2::geom_line(data = cell, ggplot2::aes(x = x, y = prob, colour = category, linetype = category,
                    linewidth = ggplot2::from_theme(linewidth * curve_line_ratio))) +
                summary_points(ggplot2::geom_point(data = cell, ggplot2::aes(x = x, y = prob, colour = category, shape = category)))
        }
        p <- p + ggplot2::scale_x_continuous(breaks = integer_breaks)
    } else {
        # Fit-then-pool loess for each category
        curves <- do.call(rbind, lapply(seq_len(K), \(i) {
            cv <- pooled_loess(lapply(imp_list, \(d) data.frame(x = d[[col]], y = d[[prob_cols[i]]])))
            if (nrow(cv)) cv$category <- factor(cats[i], levels = cats)
            cv
        }))
        if (!is.null(curves) && nrow(curves)) {
            p <- if (single) {
                p + ggplot2::geom_line(data = curves, ggplot2::aes(x = x, y = y, colour = ggplot2::from_theme(accent),
                        linewidth = ggplot2::from_theme(linewidth * curve_line_ratio)))
            } else {
                p + ggplot2::geom_line(data = curves, ggplot2::aes(x = x, y = y, colour = category, linetype = category,
                        linewidth = ggplot2::from_theme(linewidth * curve_line_ratio)))
            }
        }
    }

    p <- p + ggplot2::labs(
        title = sprintf("Predicted Probability vs. Predictor: %s ~ %s", variable, x),
        x = var_label(col), y = "Predicted Probability"
    )
    if (single) return(p)
    legend_title <- paste(variable, "Category")
    # Shapes and line types: the theme's discrete palettes, or a fixed set of seven
    p +
        ggplot2::discrete_scale(
            "shape", palette = NULL, fallback.palette = \(n) c(16, 17, 15, 18, 4, 8, 3)[seq_len(n)]
        ) +
        ggplot2::discrete_scale(
            "linetype", palette = NULL,
            fallback.palette = \(n) c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "1F")[seq_len(n)]
        ) +
        ggplot2::labs(colour = legend_title, shape = legend_title, linetype = legend_title)
}
