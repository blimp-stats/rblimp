# Copyright Brian Keller 2026, all rights reserved

#' Observed and imputed distribution of a variable in a [`blimp_obj`]
#' @description
#' Plots the distribution of one variable across every saved imputation: observed values as
#' filled bars, imputed values as outlined bars, and a density curve for each completed dataset.
#' @param model a [`blimp_obj`] with saved imputations.
#' @param variable a character string naming one saved variable: a variable in the model,
#'   a model-implied quantity (e.g., `"y.residual"`, `"f.latent"`, `"y.1.probability"`,
#'   `"y.yjt"`), or a level-2 quantity (e.g., `"y[school]"`, `"x.mean[school]"`).
#' @param bins an integer. The target number of histogram bins for continuous variables; it also
#'   sets the smoothness of the density curves.
#'   Integer-valued variables always use a bin width of 1.
#' @param discrete_max_unique an integer. A variable with this many or fewer distinct values
#'   in a completed dataset is treated as discrete (no density curves or Q-Q plot, integer
#'   axis ticks) even if it was not declared ORDINAL, NOMINAL, or COUNT. Declared variables
#'   and factor, character, and logical columns always are; categories are labeled by name.
#' @returns a [`ggplot2::ggplot`] plot
#' @details
#' Variables are shown in one of three ways:
#' - **Observed** (no missing values): filled bars.
#' - **Mixed** (some values imputed): filled bars for observed values, outlined bars for
#'   imputed values from every imputation, and the percent imputed in the title.
#' - **Model-implied** (residuals, latent variables, probabilities, and level-2 quantities):
#'   outlined bars. Level-2 quantities are reduced to one value per cluster first.
#'
#' One density curve is drawn for each completed dataset (the observed values plus one
#' imputation's values), so the spread of the curves shows how much the imputations differ.
#' A variable with no imputed values gets one curve. The bandwidth equals the histogram's bin
#' width, so the curves and the bars show the same level of detail; `bins` changes both. The
#' curves are omitted for declared or effectively discrete variables. The vertical
#' (density) axis is not drawn; add `ggplot2::scale_y_continuous()`,
#' `ggplot2::guides(y = "axis")`, and `ggplot2::labs(y = "Density")` to show it.
#'
#' Colors come from the plot theme: the observed-value bars use the theme's geom fill and the
#' imputed-value bars the `data_color` setting of [`blimp_style`], or the first two colors of
#' the discrete fill and colour palettes when these are not set; the density curves use the
#' theme's accent. See [`ggplot2::element_geom`]. A fill or colour scale added to the plot
#' (e.g., `scale_fill_manual(values = c(Observed = "grey70"))`) takes precedence. The
#' transparency of the observed-value bars and of the density curves is set with [`blimp_style`].
#' @seealso [`qq_plot`], [`distribution_plots`], [`residual_plot`], [`blimp_style`]
#' @examplesIf has_blimp()
#' # Generate Data
#' mydata <- rblimp_sim(
#'     c(
#'         'x ~ normal(0, 1)',
#'         'y ~ normal(10 + 0.5*x, 1)'
#'     ),
#'     n = 200,
#'     seed = 981273
#' )
#'
#' # Make some values of x missing
#' set.seed(19723)
#' mydata$x[sample(200, 50)] <- NA
#'
#' # Run Rblimp
#' m1 <- rblimp('y ~ x', mydata, nimps = 10, seed = 10972, burn = 1000, iter = 1000)
#'
#' # Observed vs imputed values of x
#' distribution_plot(m1, 'x')
#'
#' # Distribution of the residuals
#' distribution_plot(m1, 'y.residual') + ggplot2::theme_minimal()
#' @import ggplot2
#' @export
distribution_plot <- function(model, variable, bins = 30, discrete_max_unique = 5L) {

    # Check inputs
    check_diagnostic_model(model)
    check_variable_arg(variable, "distribution_plots")
    check_max_unique(discrete_max_unique)
    vals <- distribution_values(model, variable)
    raw <- model@imputations[[1]][[variable]]
    force_integer <- is_discrete(raw, variable %in% diag_discrete_vars(model), discrete_max_unique)

    # Title
    title_var <- var_label(variable)
    title <- if (vals$kind == "mixed") {
        sprintf("Observed vs. Imputed Distributions: %s (%.1f%% imputed)", title_var, vals$pct_imp)
    } else {
        sprintf("Distribution of %s", title_var)
    }

    # Bins
    all_vals <- c(vals$observed, vals$imputed)
    is_int <- all(abs(all_vals - round(all_vals)) < 1e-6)
    binwidth <- if (is_int) 1 else diff(range(all_vals)) / bins
    if (!is.finite(binwidth) || binwidth <= 0) binwidth <- 1
    boundary <- if (is_int) floor(min(all_vals)) - 0.5 else floor(min(all_vals) / binwidth) * binwidth

    # Bars: observed values filled, imputed values outlined
    df <- data.frame(
        value = all_vals,
        type = factor(rep(c("Observed", "Imputed"), c(length(vals$observed), length(vals$imputed))),
                      levels = c("Observed", "Imputed"))
    )
    # Each bar layer gets its own legend key; palette positions are fixed by both levels.
    # The theme's geom fill (observed) and `blimp.data.color` (imputed) override the palette when set.
    lv <- c("Observed", "Imputed")
    p <- ggplot2::ggplot(df, ggplot2::aes(x = value))
    if (length(vals$observed)) p <- p + themed(
        theme_color_override(ggplot2::geom_histogram(
            data = df[df$type == "Observed", , drop = FALSE],
            ggplot2::aes(y = ggplot2::after_stat(density), fill = type),
            colour = NA, binwidth = binwidth, boundary = boundary
        ), "fill", \(theme) ggplot2::calc_element("geom", theme)$fill, level = 1),
        alpha = list(area.alpha = 0.3)
    )
    if (length(vals$imputed)) p <- p + theme_color_override(ggplot2::geom_histogram(
        data = df[df$type == "Imputed", , drop = FALSE],
        ggplot2::aes(y = ggplot2::after_stat(density), colour = type),
        fill = NA, binwidth = binwidth, boundary = boundary
    ), "colour", \(theme) blimp_setting(theme, "data.color"), level = 2)
    p <- p +
        ggplot2::scale_fill_discrete(NULL, limits = lv, breaks = "Observed") +
        ggplot2::scale_colour_discrete(NULL, limits = lv, breaks = "Imputed")

    # One density curve per completed dataset (observed values plus that imputation's
    # values). The bandwidth is the bin width, so the curves and the bars show the same
    # level of detail at any M
    completed <- Filter(\(x) length(x) >= 2, vals$completed)
    if (!force_integer && length(completed)) {
        dens <- do.call(rbind, lapply(seq_along(completed), \(i) data.frame(imp = i, value = completed[[i]])))
        p <- p +
            themed(
                ggplot2::geom_line(
                    data = dens, stat = "density",
                    ggplot2::aes(x = value, group = imp, colour = ggplot2::from_theme(accent), linetype = "Combined"),
                    bw = binwidth, inherit.aes = FALSE,
                    show.legend = c(linetype = TRUE, colour = FALSE)
                ),
                alpha = list(curve.alpha = 1 / length(completed)^(1 / 3))
            ) +
            ggplot2::scale_linetype_manual(NULL, values = c(Combined = "solid"))
    }

    p <- p + if (vals$kind == "mixed") {
        ggplot2::guides(
            fill = ggplot2::guide_legend(order = 1), colour = ggplot2::guide_legend(order = 2),
            linetype = ggplot2::guide_legend(order = 3, override.aes = list(alpha = 1))
        )
    } else {
        ggplot2::guides(fill = "none", colour = "none", linetype = "none")
    }
    if (force_integer) p <- p + discrete_x_scale(column_levels(model, variable))
    # Density values are not read off the plot, so the y axis is not drawn
    p +
        ggplot2::scale_y_continuous(breaks = NULL) +
        ggplot2::guides(y = "none") +
        ggplot2::labs(title = title, x = title_var, y = NULL)
}

#' Normal Q-Q plot of a variable in a [`blimp_obj`]
#' @description
#' Plots the standardized values of one variable against standard normal quantiles, one
#' set of points per completed dataset, overlaid.
#' @inheritParams distribution_plot
#' @returns a [`ggplot2::ggplot`] plot
#' @details
#' Each completed dataset (the observed values plus one imputation's imputed values; level-2
#' quantities reduced to one value per cluster) is sorted and plotted against the normal
#' quantiles for its size, so every case counts once in each dataset. Points are matched by
#' rank, not by case. Values are standardized by their mean and the pooled within-imputation
#' standard deviation, and plotted on equal axes with a 45-degree reference line. A variable
#' with no imputed values gives one set of points.
#'
#' A Q-Q plot is not drawn for predicted probabilities (bounded in \[0, 1\]), for the residuals
#' of ordinal, nominal, or count outcomes (latent-response residuals), or for discrete
#' variables; requesting one gives an error explaining why.
#'
#' Point transparency and the reference line are set with [`blimp_style`].
#' @seealso [`distribution_plot`], [`distribution_plots`], [`blimp_style`]
#' @examplesIf has_blimp()
#' # Generate Data
#' mydata <- rblimp_sim(
#'     c(
#'         'x ~ normal(0, 1)',
#'         'y ~ normal(10 + 0.5*x, 1)'
#'     ),
#'     n = 200,
#'     seed = 981273
#' )
#'
#' # Run Rblimp
#' m1 <- rblimp('y ~ x', mydata, nimps = 10, seed = 10972, burn = 1000, iter = 1000)
#'
#' # Normality of the residuals
#' qq_plot(m1, 'y.residual')
#' @import ggplot2
#' @export
qq_plot <- function(model, variable, discrete_max_unique = 5L) {

    # Check inputs
    check_diagnostic_model(model)
    check_variable_arg(variable, "distribution_plots")
    check_max_unique(discrete_max_unique)
    reason <- qq_omitted(model, variable)
    if (!is.null(reason)) throw_error(c("A normal Q-Q plot is not drawn for {.val {variable}}.", "i" = reason))
    vals <- distribution_values(model, variable)
    v <- c(vals$observed, vals$imputed)
    declared <- variable %in% diag_discrete_vars(model)
    if (is_discrete(model@imputations[[1]][[variable]], declared, discrete_max_unique)) throw_error(c(
        "A normal Q-Q plot is not drawn for {.val {variable}}.",
        "i" = "It is discrete; a normal reference would mislead."
    ))
    if (length(v) < 3 || stats::sd(v) == 0) throw_error("Too few distinct values of {.val {variable}}.")

    # One Q-Q per completed dataset: its sorted values against the normal quantiles for its
    # size, standardized by the mean and the pooled within-imputation SD
    completed <- Filter(\(x) length(x) >= 3, vals$completed)
    center <- mean(unlist(completed))
    scale <- sqrt(mean(vapply(completed, stats::var, 0)))
    if (!is.finite(scale) || scale == 0) throw_error("Too few distinct values of {.val {variable}}.")
    df <- do.call(rbind, lapply(completed, \(x) {
        qq <- stats::qqnorm((x - center) / scale, plot.it = FALSE)
        data.frame(theoretical = qq$x, sample = qq$y)
    }))
    lim <- max(abs(c(df$theoretical, df$sample)))

    ggplot2::ggplot(df, ggplot2::aes(x = theoretical, y = sample)) +
        themed(
            ggplot2::geom_abline(
                data = data.frame(slope = 1, intercept = 0),
                ggplot2::aes(slope = slope, intercept = intercept, linewidth = ggplot2::from_theme(linewidth * ref_line_ratio)),
                linetype = "dashed", inherit.aes = FALSE
            ),
            colour = list(reference.color = "grey50")
        ) +
        geom_point_mi(n_imps = length(completed)) +
        ggplot2::coord_fixed(ratio = 1, xlim = c(-lim, lim), ylim = c(-lim, lim)) +
        ggplot2::labs(
            title = sprintf("Normal Q-Q: %s", var_label(variable)),
            x = "Standard Normal Quantiles",
            y = paste("Standardized", var_label(variable))
        )
}

#' Internal function giving why a Q-Q plot is not drawn (NULL when it is)
#' @noRd
qq_omitted <- function(model, variable) {
    if (grepl("\\.probability$", variable)) {
        return("Predicted probabilities are bounded in [0, 1], so a normal Q-Q bends by construction.")
    }
    if (grepl("\\.residual$", variable)) {
        base <- sub("\\.residual$", "", variable)
        parent <- sub("\\.[0-9]+$", "", base)
        if (base %in% diag_discrete_vars(model) || parent %in% diag_discrete_vars(model, "nominal")) {
            return("Residuals of ordinal, nominal, and count outcomes are latent-response residuals, not normal-scale ones.")
        }
    }
    NULL
}

#' Internal function to extract observed and imputed values of one variable
#' @noRd
distribution_values <- function(model, variable) {
    cols <- names(model@imputations[[1]])
    if (!variable %in% cols) throw_error(c(
        "{.val {variable}} is not a saved variable.",
        "i" = "Use {.code names(model)} to list the saved variables."
    ))
    lv <- column_levels(model, variable)

    # Level-2 quantities: one value per cluster within each imputation
    if (grepl("\\[.+\\]$", variable)) {
        cl <- sub("^.+\\[(.+)\\]$", "\\1", variable)
        completed <- lapply(model@imputations, \(d) {
            x <- as_codes(d[[variable]], lv)
            if (cl %in% names(d)) x[!duplicated(d[[cl]])] else x
        })
        return(finite_values("implied", numeric(0), unlist(completed, use.names = FALSE), variable, completed))
    }

    # Model-implied quantities: every value varies across imputations
    if (grepl("\\.(residual|latent|probability|yjt)$", variable)) {
        completed <- lapply(model@imputations, \(d) as_codes(d[[variable]], lv))
        return(finite_values("implied", numeric(0), unlist(completed, use.names = FALSE), variable, completed))
    }

    # Data variables: Blimp flags imputed cases with a nonzero imputation variance
    if (NROW(model@variance_imp) == 0 || !variable %in% names(model@variance_imp)) throw_error(
        "Cannot determine which values of {.val {variable}} were imputed."
    )
    vimp <- model@variance_imp[[variable]]
    miss <- is.finite(vimp) & vimp != 0
    completed <- lapply(model@imputations, \(d) as_codes(d[[variable]], lv))
    obs <- completed[[1]][!miss]
    imp <- unlist(lapply(completed, \(x) x[miss]), use.names = FALSE)
    out <- finite_values(if (any(miss)) "mixed" else "observed", obs, imp, variable,
                         if (any(miss)) completed else completed[1])
    out$pct_imp <- 100 * mean(miss)
    out
}

#' Internal function to keep finite numeric values
#' `completed` holds the values in each completed dataset (one when nothing is imputed).
#' @noRd
finite_values <- function(kind, observed, imputed, variable, completed) {
    if (!is.numeric(observed) && length(observed) || !is.numeric(imputed) && length(imputed)) {
        throw_error("{.val {variable}} is not numeric.")
    }
    observed <- observed[is.finite(observed)]
    imputed <- imputed[is.finite(imputed)]
    if (!length(observed) && !length(imputed)) throw_error("{.val {variable}} has no finite values.")
    completed <- lapply(completed, \(x) x[is.finite(x)])
    list(kind = kind, observed = observed, imputed = imputed, completed = completed, pct_imp = NA_real_)
}
