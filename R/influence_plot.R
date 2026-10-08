# Copyright Brian Keller 2026, all rights reserved

#' Leverage and Cook's distance plots for an outcome in a [`blimp_obj`]
#' @description
#' Index plots of the leverage and Cook's distance values Blimp computes for an outcome.
#' The model must be fit with `influence = TRUE` (see [`rblimp`]).
#' @param model a [`blimp_obj`] fit with `influence = TRUE`.
#' @param variable a character string naming one outcome.
#' @param measure which values to plot: `"leverage"`, `"cooks"`, or both (the default), which
#'   are drawn as two panels.
#' @param mark_imputed a logical value. If `TRUE`, cases whose outcome value was imputed are
#'   drawn with a different point shape. Change the shapes with [`ggplot2::scale_shape_manual`].
#' @param max_labels an integer. The maximum number of cases labeled in each panel.
#' @returns a [`ggplot2::ggplot`] plot
#' @details
#' Blimp computes leverage and Cook's distance at every post burn-in iteration, from the
#' outcome's fixed-effects design as fitted, and averages them per case. Cook's distance uses
#' the pooled residual variance, so it is on the same scale across iterations. See
#' [`influence`][influence,blimp_obj-method] for the values.
#'
#' Values are available for single-level continuous outcomes (including `yjt()` outcomes and
#' outcomes with a variance model) and ordinal probit outcomes.
#'
#' Reference lines are drawn at \eqn{2p/n} and \eqn{3p/n} for leverage, where \eqn{p} is the
#' number of columns in the design (the sum of the leverages), and at \eqn{4/n} for Cook's
#' distance. Cases above \eqn{3p/n} or \eqn{4/n} are labeled.
#'
#' For a case whose outcome was imputed, Cook's distance reflects the uncertainty in its
#' imputed value: it is the case's expected influence given its leverage.
#'
#' Point transparency, reference lines, and labels are set with [`blimp_style`].
#' @seealso [`residual_plot`], [`diagnostic_plots`], [`influence`][influence,blimp_obj-method],
#'   [`blimp_style`]
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
#' \dontrun{
#' # Requires a version of Blimp that supports influence output
#' m1 <- rblimp(
#'     'y ~ x m x*m',
#'     mydata,
#'     nimps = 10,
#'     seed = 10972,
#'     burn = 1000,
#'     iter = 1000,
#'     influence = TRUE
#' )
#'
#' # Leverage and Cook's distance
#' influence_plot(m1, 'y')
#'
#' # Only Cook's distance
#' influence_plot(m1, 'y', measure = 'cooks')
#' }
#' @import ggplot2
#' @export
influence_plot <- function(
        model, variable, measure = c("leverage", "cooks"),
        mark_imputed = TRUE, max_labels = 10
) {

    # Check inputs
    if (!is(model, 'blimp_obj')) throw_error("{.arg model} is not a `blimp_obj`")
    check_variable_arg(variable, "diagnostic_plots")
    measure <- match.arg(measure, several.ok = TRUE)
    if (!is.logical(mark_imputed) || length(mark_imputed) != 1 || is.na(mark_imputed)) throw_error(
        "{.arg mark_imputed} must be a single logical value"
    )
    inf <- get_influence(model)
    lev_col <- paste0(variable, ".leverage")
    if (!lev_col %in% names(inf)) {
        eligible <- sub("\\.leverage$", "", grep("\\.leverage$", names(inf), value = TRUE))
        throw_error(c(
            "No influence values for {.val {variable}}.",
            "i" = "Values are computed for single-level continuous and ordinal probit outcomes.",
            "i" = "Outcomes with values: {.val {eligible}}."
        ))
    }

    # Long data, one panel per measure
    lev <- inf[[lev_col]]
    n <- length(lev)
    p <- sum(lev, na.rm = TRUE)
    labels <- c(leverage = "Leverage", cooks = "Cook's D")
    df <- do.call(rbind, lapply(measure, \(m) data.frame(
        case = seq_len(n), value = inf[[paste0(variable, ".", m)]],
        measure = factor(labels[[m]], levels = labels[measure])
    )))
    df$label <- df$case
    df <- df[is.finite(df$value), , drop = FALSE]

    # Mark cases with an imputed outcome
    vimp <- if (NROW(model@variance_imp)) model@variance_imp[[variable]] else NULL
    imputed <- if (is.null(vimp)) rep(FALSE, n) else is.finite(vimp) & vimp != 0
    marked <- mark_imputed && any(imputed)
    df$outcome_status <- factor(ifelse(imputed[df$case], "Imputed", "Observed"), levels = c("Observed", "Imputed"))

    # Reference lines
    refs <- rbind(
        data.frame(measure = "Leverage", yintercept = c(2, 3) * p / n, label = c("2p/n", "3p/n")),
        data.frame(measure = "Cook's D", yintercept = 4 / n, label = "4/n")
    )
    refs <- refs[refs$measure %in% labels[measure], , drop = FALSE]
    refs$measure <- factor(refs$measure, levels = labels[measure])

    plt <- ggplot2::ggplot(df, ggplot2::aes(x = case, y = value)) +
        ref_hline(refs, dashed = TRUE) +
        themed(
            ggplot2::geom_text(
                data = refs, ggplot2::aes(x = -Inf, y = yintercept, label = label),
                hjust = -0.1, vjust = -0.3, inherit.aes = FALSE
            ),
            colour = list(label.color = "black")
        )
    plt <- if (marked) {
        plt + geom_point_mi(ggplot2::aes(shape = outcome_status)) +
            ggplot2::labs(shape = "Outcome")
    } else {
        plt + geom_point_mi()
    }

    # Label the most extreme cases in each panel
    cuts <- c(Leverage = 3 * p / n, "Cook's D" = 4 / n)
    for (m in labels[measure]) {
        plt <- plt + outlier_labels(df[df$measure == m, , drop = FALSE], "value", cuts[[m]], max_labels)
    }

    if (length(measure) > 1) plt <- plt + ggplot2::facet_wrap(~measure, ncol = 1, scales = "free_y")
    title <- if (length(measure) > 1) "Influence" else if (measure == "leverage") "Leverage" else "Cook's Distance"
    plt + ggplot2::labs(
        title = sprintf("%s: %s", title, variable),
        x = "Record Number",
        y = if (length(measure) == 1) labels[[measure]] else NULL
    )
}
