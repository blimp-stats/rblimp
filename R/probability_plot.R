# Copyright Brian Keller 2026, all rights reserved

#' Predicted probabilities across a predictor for an outcome in a [`blimp_obj`]
#' @description
#' Plots the predicted probability of each category of an ordinal or nominal outcome against
#' a predictor, using every saved imputation, optionally with one line per group and with the
#' observed proportions for comparison.
#' @param formula an object of class [`formula`] of the form `outcome ~ x` or
#'   `outcome ~ x | group`. `outcome` is an ordinal or nominal outcome, `x` is one of its
#'   predictors, and `group` is a discrete variable that splits the lines, as the moderator does
#'   in [`simple_plot`].
#' @param model a [`blimp_obj`] with saved imputations.
#' @param observed logical. If `TRUE`, adds the observed proportion of each category, so the
#'   model's predictions can be compared with the data. See Details.
#' @param discrete_max_unique an integer. A predictor with this many or fewer distinct values
#'   in a completed dataset gets category means instead of loess curves, even if it was not
#'   declared ORDINAL, NOMINAL, or COUNT. Declared predictors, the TIMEID variable, and factor, character, and logical columns always do.
#'   The same rule decides whether `group` is discrete.
#' @returns a [`ggplot2::ggplot`] plot
#' @details
#' Every saved category probability is plotted: all categories for an ordinal outcome and the
#' non-reference categories for a nominal outcome. An outcome with two categories, including
#' the missingness indicator a DROPOUT command creates, shows only the probability of its higher
#' category. Points stack all imputations. For a continuous predictor, each category gets a
#' loess curve fit within each imputation and averaged; for a discrete predictor (see
#' `discrete_max_unique`), each category gets its mean probability at each level. Factor and
#' character predictors are labeled by category name. With curves, the subtitle gives the
#' number of imputations averaged, as in [`residual_plot`].
#'
#' With `group`, the lines are split by the group's values, and the group takes over the colour,
#' point shape, and line type. If the outcome also has more than one category plotted, each
#' category gets its own panel.
#'
#' With `observed = TRUE`, the plot compares the model with the data. Markers show the
#' observed proportion of each category: at each level of a discrete predictor, or in bins of
#' a continuous predictor (about the square root of the number of cases, with equal counts, as
#' in the binned residuals of [`residual_plot`]). The predicted probabilities are drawn as lines
#' only, and the individual points are left out. Only cases whose outcome was observed are used,
#' for both the markers and the lines, so the two describe the same cases.
#'
#' Categories (or groups) are distinguished by colour, point shape, and line type, so the plot
#' also reads in greyscale. These come from the theme's discrete palettes
#' (`palette.colour.discrete`, `palette.shape.discrete`, and `palette.linetype.discrete`);
#' without shape or line type palettes, a fixed set of seven of each is used. Point
#' transparency and the category-mean and observed-proportion markers are set with
#' [`blimp_style`].
#' @seealso [`residual_plot`], [`diagnostic_plots`], [`blimp_style`], [`simple_plot`]
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
#' probability_plot(y ~ x, m1)
#'
#' # Compared with the observed proportions
#' probability_plot(y ~ x, m1, observed = TRUE)
#' @import ggplot2
#' @export
probability_plot <- function(formula, model, observed = FALSE, discrete_max_unique = 10L) {
    if (is(formula, "blimp_obj")) throw_error(c(
        "The {.arg formula} comes first.",
        "i" = "Use {.code probability_plot(outcome ~ x, model)}."
    ))
    f <- parse_probability_formula(formula)
    build_probability(model, f$variable, f$x, f$group, observed, discrete_max_unique)
}

#' Internal function to read a `probability_plot()` formula
#' One rule: `outcome ~ x`, with an optional `| group`.
#' @noRd
parse_probability_formula <- function(formula) {
    bad <- \() throw_error(
        "{.arg formula} must have the form {.code outcome ~ x} or {.code outcome ~ x | group}."
    )
    if (!inherits(formula, "formula") || length(formula) != 3L) bad()
    lhs <- formula[[2]]
    rhs <- formula[[3]]
    group <- NULL
    if (is.call(rhs) && identical(rhs[[1]], as.name("|"))) {
        group <- rhs[[3]]
        rhs <- rhs[[2]]
    }
    if (!is.name(lhs) || !is.name(rhs) || (!is.null(group) && !is.name(group))) bad()
    list(
        variable = as.character(lhs), x = as.character(rhs),
        group = if (!is.null(group)) as.character(group)
    )
}

#' Internal function to build the predicted probability plot from names
#' `probability_plot()` calls it after reading its formula; [diagnostic_plots()] calls it
#' directly.
#' @noRd
build_probability <- function(model, variable, x, group = NULL, observed = FALSE, discrete_max_unique = 10L) {

    # Check inputs
    check_diagnostic_model(model)
    check_variable_arg(variable, "diagnostic_plots")
    check_max_unique(discrete_max_unique)
    if (!isTRUE(observed) && !isFALSE(observed)) throw_error("{.arg observed} must be TRUE or FALSE.")
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
    declared <- diag_discrete_vars(model)
    raw <- model@imputations[[1]][[col]]
    discrete <- is_discrete(raw, names(allowed)[match(col, allowed)] %in% declared, discrete_max_unique)
    imp_list <- imputation_frames(model)

    # Group: any discrete column, named as a predictor or as saved
    gcol <- NULL
    if (!is.null(group)) {
        gcol <- resolve_predictor(group, allowed)
        if (is.na(gcol) && group %in% names(imp_list[[1]])) gcol <- group
        if (is.na(gcol)) throw_error("{.val {group}} is not a column in the imputations.")
        if (gcol == col) throw_error("The group must differ from {.val {x}}.")
        if (!is_discrete(model@imputations[[1]][[gcol]], group %in% declared, discrete_max_unique)) throw_error(c(
            "The group {.val {group}} must be discrete.",
            "i" = "It has more than {discrete_max_unique} distinct values; see {.arg discrete_max_unique}."
        ))
        gvals <- category_values(lapply(imp_list, \(d) d[[gcol]]))
        glevels <- column_levels(model, gcol)
        glabels <- if (length(glevels)) glevels[gvals] else as.character(gvals)
    }

    # Category probability columns; an outcome with two categories shows only the higher one
    rx <- paste0("^", gsub(".", "\\.", variable, fixed = TRUE), "\\.([0-9]+)\\.probability$")
    ks <- sort(as.integer(sub(rx, "\\1", grep(rx, names(imp_list[[1]]), value = TRUE))))
    if (!length(ks)) throw_error("No saved probabilities for {.val {variable}}.")
    if (outcome$type == "ordinal" && length(ks) == 2L) ks <- ks[2]
    prob_cols <- paste0(variable, ".", ks, ".probability")
    cats <- as.character(ks)
    K <- length(ks)

    # Rows: with observed proportions, only cases whose outcome was observed
    n <- nrow(imp_list[[1]])
    keep <- if (observed) outcome_status(model, outcome, n) == "Observed" else rep(TRUE, n)
    if (!any(keep)) throw_error("{.val {variable}} has no observed values.")

    # One long data frame per imputation: x, group, category, predicted and observed
    long_list <- lapply(seq_along(imp_list), \(m) {
        d <- imp_list[[m]][keep, , drop = FALSE]
        g <- if (is.null(gcol)) rep(0, nrow(d)) else d[[gcol]]
        do.call(rbind, lapply(seq_len(K), \(i) {
            z <- data.frame(
                imp = m, x = d[[col]], g = g, k = i, prob = d[[prob_cols[i]]],
                obs = as.numeric(d[[variable]] == ks[i])
            )
            ok <- is.finite(z$x) & is.finite(z$prob) & is.finite(z$obs)
            if (!is.null(gcol)) ok <- ok & z$g %in% gvals
            z[ok, , drop = FALSE]
        }))
    })
    M <- length(imp_list)

    # Series (colour, shape, line type) and panel for each row
    label <- \(df) {
        if (!nrow(df)) return(df)
        df$series <- if (is.null(gcol)) factor(cats[df$k], levels = cats) else factor(df$g, levels = gvals, labels = glabels)
        df$panel <- factor(sprintf("%s.%s", variable, cats[df$k]), levels = sprintf("%s.%s", variable, cats))
        df
    }

    cell <- curves <- obs <- subtitle <- NULL
    if (discrete) {
        # Means at each level, within each imputation then averaged
        cell_means <- \(value) {
            long <- do.call(rbind, long_list)
            per_imp <- stats::aggregate(long[value], by = long[c("imp", "x", "g", "k")], FUN = mean)
            out <- stats::aggregate(per_imp[value], by = per_imp[c("x", "g", "k")], FUN = mean)
            names(out)[names(out) == value] <- "y"
            label(out)
        }
        cell <- cell_means("prob")
        if (observed) obs <- cell_means("obs")
    } else {
        # Fit-then-pool loess of the predictions and pooled bins of the observed proportions
        combos <- unique(do.call(rbind, lapply(long_list, \(z) z[c("k", "g")])))
        pick <- \(kk, gg, value) lapply(long_list, \(z) {
            s <- z[z$k == kk & z$g == gg, , drop = FALSE]
            data.frame(x = s$x, y = s[[value]])
        })
        cvs <- lapply(seq_len(nrow(combos)), \(r) {
            cv <- pooled_loess(pick(combos$k[r], combos$g[r], "prob"))
            if (nrow(cv)) {
                cv$k <- combos$k[r]
                cv$g <- combos$g[r]
            }
            cv
        })
        curves <- label(do.call(rbind, lapply(cvs, \(cv) if (nrow(cv)) cv[c("x", "y", "k", "g")])) %||%
            data.frame(x = numeric(0), y = numeric(0)))
        subtitle <- curve_subtitle(cvs)
        if (observed) {
            obs <- do.call(rbind, lapply(seq_len(nrow(combos)), \(r) {
                bs <- pooled_bins(pick(combos$k[r], combos$g[r], "obs"))
                if (!is.null(bs)) {
                    bs$k <- combos$k[r]
                    bs$g <- combos$g[r]
                }
                bs
            }))
            obs <- if (is.null(obs)) NULL else label(obs)
        }
    }
    if (observed) {
        note <- paste0(
            "Markers show observed proportions",
            if (!all(keep)) sprintf(" (%d cases with observed %s)", sum(keep), variable)
        )
        subtitle <- paste(c(subtitle, note), collapse = "\n")
    }

    draw_probability(list(
        points = if (!observed) label(do.call(rbind, long_list)[c("x", "prob", "k", "g")]),
        n_imps = M, single = is.null(gcol) && K == 1L, facet = !is.null(gcol) && K > 1L,
        discrete = discrete, observed = observed, cell = cell, curves = curves, obs = obs,
        levels = if (discrete) column_levels(model, col),
        categories = if (discrete) category_values(lapply(imp_list, \(d) d[[col]])),
        title = sprintf(
            "%s vs. Predictor: %s ~ %s%s",
            if (observed) "Observed and Predicted Probability" else "Predicted Probability",
            variable, x, if (is.null(group)) "" else paste(" |", group)
        ),
        subtitle = subtitle,
        x_label = var_label(col),
        y_label = if (observed) "Probability" else "Predicted Probability",
        legend_title = if (is.null(group)) paste(variable, "Category") else group
    ))
}

#' Internal function to draw the predicted probability plot from its plot data `d`
#' The saved plot keeps this function's environment, so it is given only what it draws.
#' @noRd
draw_probability <- function(d) {
    single <- d$single
    lines <- if (d$discrete) d$cell else d$curves
    # Markers: the observed proportions, or else the predicted means at each level
    markers <- if (d$observed) d$obs else if (d$discrete) d$cell
    # Marked so that scale_imputed() on a diagnostic_plots() list leaves it unchanged
    as_diagnostic_plot(ggplot2::ggplot() + list(
        if (!is.null(d$points)) {
            if (single) {
                geom_point_mi(data = d$points, ggplot2::aes(x = x, y = prob), n_imps = d$n_imps)
            } else {
                geom_point_mi(data = d$points, ggplot2::aes(x = x, y = prob, colour = series, shape = series), n_imps = d$n_imps)
            }
        },
        if (NROW(lines)) {
            if (single) {
                ggplot2::geom_line(data = lines, ggplot2::aes(x = x, y = y, colour = ggplot2::from_theme(accent),
                    linewidth = ggplot2::from_theme(linewidth * curve_line_ratio)))
            } else {
                ggplot2::geom_line(data = lines, ggplot2::aes(x = x, y = y, colour = series, linetype = series,
                    linewidth = ggplot2::from_theme(linewidth * curve_line_ratio)))
            }
        },
        if (NROW(markers)) {
            if (single) {
                summary_points(ggplot2::geom_point(data = markers, ggplot2::aes(x = x, y = y, colour = ggplot2::from_theme(accent))))
            } else {
                summary_points(ggplot2::geom_point(data = markers, ggplot2::aes(x = x, y = y, colour = series, shape = series)))
            }
        },
        if (d$discrete) discrete_x_scale(d$levels, d$categories),
        if (d$facet) ggplot2::facet_wrap(ggplot2::vars(panel)),
        ggplot2::labs(title = d$title, x = d$x_label, y = d$y_label),
        if (!is.null(d$subtitle)) ggplot2::labs(subtitle = d$subtitle),
        # Shapes and line types: the theme's discrete palettes, or a fixed set of seven
        if (!single) list(
            ggplot2::discrete_scale(
                "shape", palette = NULL, fallback.palette = \(n) c(16, 17, 15, 18, 4, 8, 3)[seq_len(n)]
            ),
            ggplot2::discrete_scale(
                "linetype", palette = NULL,
                fallback.palette = \(n) c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "1F")[seq_len(n)]
            ),
            ggplot2::labs(colour = d$legend_title, shape = d$legend_title, linetype = d$legend_title)
        )
    ))
}
