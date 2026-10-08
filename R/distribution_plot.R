# Copyright Brian Keller 2026, all rights reserved

#' Observed and imputed distribution of a variable in a [`blimp_obj`]
#' @description
#' Plots the distribution of one variable across every saved imputation: observed values as
#' filled bars, imputed values as outlined bars, and a density curve for the combined values.
#' @param model a [`blimp_obj`] with saved imputations.
#' @param variable a character string naming one saved variable: a variable in the model,
#'   a model-implied quantity (e.g., `"y.residual"`, `"f.latent"`, `"y.1.probability"`,
#'   `"y.yjt"`), or a level-2 quantity (e.g., `"y[school]"`, `"x.mean[school]"`).
#' @param bins an integer. The target number of histogram bins for continuous variables; it also
#'   sets the smoothness of the density curve.
#'   Integer-valued variables always use a bin width of 1.
#' @param discrete_max_unique an integer. A variable with this many or fewer distinct values
#'   is treated as discrete (no density curve, integer axis ticks) even if it was not declared
#'   ORDINAL, NOMINAL, or COUNT.
#' @returns a [`ggplot2::ggplot`] plot
#' @details
#' Variables are shown in one of three ways:
#' - **Observed** (no missing values): filled bars.
#' - **Mixed** (some values imputed): filled bars for observed values, outlined bars for
#'   imputed values from every imputation, and the percent imputed in the title.
#' - **Model-implied** (residuals, latent variables, probabilities, and level-2 quantities):
#'   outlined bars. Level-2 quantities are reduced to one value per cluster first.
#'
#' The density curve weights each imputed value by \eqn{1/M} so that cases with missing values
#' count once. Its bandwidth equals the histogram's bin width, so the curve and the bars show
#' the same level of detail; `bins` changes both. It is omitted for declared or effectively
#' discrete variables. The vertical
#' (density) axis is not drawn; add `ggplot2::scale_y_continuous()`,
#' `ggplot2::guides(y = "axis")`, and `ggplot2::labs(y = "Density")` to show it.
#'
#' Colors come from the plot theme: the observed-value bars use the theme's geom fill and the
#' imputed-value bars the `data_color` setting of [`blimp_style`], or the first two colors of
#' the discrete fill and colour palettes when these are not set; the density curve uses the
#' theme's accent. See [`ggplot2::element_geom`]. A fill or colour scale added to the plot
#' (e.g., `scale_fill_manual(values = c(Observed = "grey70"))`) takes precedence. The
#' transparency of the observed-value bars is set with [`blimp_style`].
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
    vals <- distribution_values(model, variable)
    force_integer <- variable %in% diag_discrete_vars(model) ||
        is_effectively_discrete(c(vals$observed, vals$imputed), discrete_max_unique)

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

    # Density curve over all cases; imputed values weighted 1/M. Its bandwidth is the
    # bin width, so the curve and the bars show the same level of detail at any M
    if (!force_integer && length(all_vals) >= 2) {
        n_imps <- length(model@imputations)
        dens <- data.frame(
            value = all_vals,
            weight = c(rep(1, length(vals$observed)), rep(1 / n_imps, length(vals$imputed)))
        )
        dens$weight <- dens$weight / sum(dens$weight)
        p <- p +
            ggplot2::geom_density(
                data = dens,
                ggplot2::aes(x = value, weight = weight, colour = ggplot2::from_theme(accent), linetype = "Combined",
                             linewidth = ggplot2::from_theme(linewidth * curve_line_ratio)),
                fill = NA, bw = binwidth, inherit.aes = FALSE, key_glyph = "path",
                show.legend = c(linetype = TRUE, colour = FALSE, fill = FALSE)
            ) +
            ggplot2::scale_linetype_manual(NULL, values = c(Combined = "solid"))
    }

    p <- p + if (vals$kind == "mixed") {
        ggplot2::guides(
            fill = ggplot2::guide_legend(order = 1), colour = ggplot2::guide_legend(order = 2),
            linetype = ggplot2::guide_legend(order = 3)
        )
    } else {
        ggplot2::guides(fill = "none", colour = "none", linetype = "none")
    }
    if (force_integer) p <- p + ggplot2::scale_x_continuous(breaks = integer_breaks)
    # Density values are not read off the plot, so the y axis is not drawn
    p +
        ggplot2::scale_y_continuous(breaks = NULL) +
        ggplot2::guides(y = "none") +
        ggplot2::labs(title = title, x = title_var, y = NULL)
}

#' Normal Q-Q plot of a variable in a [`blimp_obj`]
#' @description
#' Plots the standardized values of one variable, pooled across every saved imputation,
#' against standard normal quantiles.
#' @inheritParams distribution_plot
#' @returns a [`ggplot2::ggplot`] plot
#' @details
#' The values are those shown by [`distribution_plot`] (level-2 quantities reduced to one value
#' per cluster), standardized, and plotted on equal axes with a 45-degree reference line.
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
    reason <- qq_omitted(model, variable)
    if (!is.null(reason)) throw_error(c("A normal Q-Q plot is not drawn for {.val {variable}}.", "i" = reason))
    vals <- distribution_values(model, variable)
    v <- c(vals$observed, vals$imputed)
    if (is_effectively_discrete(v, discrete_max_unique) || variable %in% diag_discrete_vars(model)) throw_error(c(
        "A normal Q-Q plot is not drawn for {.val {variable}}.",
        "i" = "It is discrete; a normal reference would mislead."
    ))
    if (length(v) < 3 || stats::sd(v) == 0) throw_error("Too few distinct values of {.val {variable}}.")

    z <- (v - mean(v)) / stats::sd(v)
    qq <- stats::qqnorm(z, plot.it = FALSE)
    df <- data.frame(theoretical = qq$x, sample = qq$y)
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
        geom_point_mi(n_imps = if (vals$kind == "observed") 1 else length(model@imputations)) +
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

    # Level-2 quantities: one value per cluster within each imputation
    if (grepl("\\[.+\\]$", variable)) {
        cl <- sub("^.+\\[(.+)\\]$", "\\1", variable)
        v <- unlist(lapply(model@imputations, \(d) {
            x <- d[[variable]]
            if (cl %in% names(d)) x[!duplicated(d[[cl]])] else x
        }), use.names = FALSE)
        return(finite_values("implied", numeric(0), v, variable))
    }

    # Model-implied quantities: every value varies across imputations
    if (grepl("\\.(residual|latent|probability|yjt)$", variable)) {
        v <- unlist(lapply(model@imputations, \(d) d[[variable]]), use.names = FALSE)
        return(finite_values("implied", numeric(0), v, variable))
    }

    # Data variables: Blimp flags imputed cases with a nonzero imputation variance
    if (NROW(model@variance_imp) == 0 || !variable %in% names(model@variance_imp)) throw_error(
        "Cannot determine which values of {.val {variable}} were imputed."
    )
    vimp <- model@variance_imp[[variable]]
    miss <- is.finite(vimp) & vimp != 0
    obs <- model@imputations[[1]][[variable]][!miss]
    imp <- unlist(lapply(model@imputations, \(d) d[[variable]][miss]), use.names = FALSE)
    out <- finite_values(if (any(miss)) "mixed" else "observed", obs, imp, variable)
    out$pct_imp <- 100 * mean(miss)
    out
}

#' Internal function to keep finite numeric values
#' @noRd
finite_values <- function(kind, observed, imputed, variable) {
    if (!is.numeric(observed) && length(observed) || !is.numeric(imputed) && length(imputed)) {
        throw_error("{.val {variable}} is not numeric.")
    }
    observed <- observed[is.finite(observed)]
    imputed <- imputed[is.finite(imputed)]
    if (!length(observed) && !length(imputed)) throw_error("{.val {variable}} has no finite values.")
    list(kind = kind, observed = observed, imputed = imputed, pct_imp = NA_real_)
}
