# Copyright Brian Keller 2026, all rights reserved

#' Residual diagnostic plots for an outcome in a [`blimp_obj`]
#' @description
#' Plots an outcome's residuals against its predicted values, a predictor, the case
#' (record) number, or the cluster, using every saved imputation.
#' @param model a [`blimp_obj`] with saved imputations.
#' @param variable a character string naming one outcome. This is a level-1 outcome
#'   (e.g., `"y"`), a level-2 random-intercept residual (e.g., `"y[school]"`), or a level-2
#'   latent random effect declared on the cluster identifier's LATENT line. For a nominal
#'   outcome, the parent name (e.g., `"y"`) plots every non-reference category and a
#'   category name (e.g., `"y.2"`) plots one.
#' @param x what to plot the residuals against:
#'   - `"predicted"`: the predicted values (the default when the outcome has them).
#'   - one or more predictor names: a separate panel for each predictor.
#'   - `"case"`: the record number (an index plot; the default otherwise).
#'   - `"cluster"`: clusters ordered by their residual spread (clustered continuous outcomes).
#' @param se a logical value. If `TRUE`, adds 95% pointwise bands around the loess
#'   curve. See details.
#' @param index_thresholds a numeric vector of reference lines for standardized
#'   residuals. Cases beyond the largest absolute value are labeled on the index plot.
#' @param max_labels an integer. The maximum number of cases labeled on an index plot.
#' @returns a [`ggplot2::ggplot`] plot
#' @details
#' Every quantity is computed within each imputed data set and then pooled; nothing is
#' computed on an average of the imputations, because averaging an imputed predictor and
#' the residual together induces a spurious association between them. Points stack all
#' imputations, and the loess curve is fit within each imputation and the curves averaged.
#'
#' The residual scale is fixed by the plot, as in [`plot.lm`][stats::plot.lm]:
#' - against predicted values: raw residuals;
#' - against a predictor, the case number, or the cluster: standardized residuals, using
#'   the pooled residual standard deviation (the per-case model-implied standard deviation
#'   when a variance model is fit). Reference lines are drawn at `index_thresholds`.
#'
#' What is plotted depends on the outcome:
#' - **Continuous and ordinal outcomes** (ordinal on the latent scale): residual scatter
#'   plots with a loess curve, or category means when a predictor is declared discrete or
#'   has 10 or fewer distinct values.
#' - **Nominal outcomes**: binned residuals on the probability scale (Gelman & Hill, 2007),
#'   with a panel for each non-reference category. There is no index plot.
#' - **Count outcomes**: Pearson residuals, using the negative binomial dispersion
#'   parameter. Each imputation uses the dispersion value from the MCMC iteration it was
#'   saved at. There is no residual-vs-predicted plot.
#' - **Level-2 residuals**: one value per cluster, against level-2 predictors or as an
#'   index plot. In a clustered model, level-1 plots use within-cluster predictors only, and
#'   predictors centered at their latent group mean are shown as within-cluster deviations.
#'
#' With `se = TRUE`, each imputation's loess is evaluated with its standard error and the
#' curves are pooled with Rubin's rules: \eqn{T = W + B + B/M}, and the band is
#' \eqn{\pm 1.96\sqrt{T}}. The bands are drawn only on plots with a loess curve.
#'
#' Colors come from the plot theme: points use the theme's geom color (or `data_color` in
#' [`blimp_style`]) and curves use its accent. See [`ggplot2::element_geom`]. Point transparency, reference lines, labels, and the
#' category-mean markers are set with [`blimp_style`].
#' @seealso [`diagnostic_plots`], [`influence_plot`], [`probability_plot`], [`distribution_plot`],
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
#' # Residuals against predicted values
#' residual_plot(m1, 'y')
#'
#' # Against each predictor, with 95% bands
#' residual_plot(m1, 'y', x = c('x', 'm'), se = TRUE)
#'
#' # Index plot
#' residual_plot(m1, 'y', x = 'case') + ggplot2::theme_minimal()
#' @import ggplot2
#' @importFrom stats loess var
#' @importFrom methods is
#' @export
residual_plot <- function(
        model, variable, x, se = FALSE,
        index_thresholds = c(-3, 3), max_labels = 10
) {

    # Check inputs
    check_diagnostic_model(model)
    check_variable_arg(variable, "diagnostic_plots")
    if (!is.logical(se) || length(se) != 1 || is.na(se)) throw_error(
        "{.arg se} must be a single logical value"
    )
    if (!is.numeric(index_thresholds)) throw_error(
        "{.arg index_thresholds} must be numeric"
    )

    outcome <- diag_outcome(model, variable)
    imp_list <- lapply(model@imputations, as.data.frame)

    # Default x
    if (missing(x)) {
        x <- if (outcome$level == 1L && outcome$type != "count") "predicted" else "case"
    }
    if (!is.character(x) || !length(x)) throw_error(
        "{.arg x} must be a character vector"
    )

    if (length(x) == 1 && x %in% c("predicted", "case", "cluster")) {
        uses_se <- x == "predicted" && outcome$level == 1L && outcome$type %in% c("continuous", "ordinal")
        if (se && !uses_se) inform_no_se()
        return(switch(
            x,
            predicted = resid_vs_predicted(imp_list, outcome, se),
            case = resid_index(model, imp_list, outcome, index_thresholds, max_labels),
            cluster = resid_by_cluster(imp_list, outcome)
        ))
    }
    resid_vs_predictors(model, imp_list, outcome, x, se, index_thresholds)
}

#' Internal function for the message when bands do not apply
#' @noRd
inform_no_se <- function() {
    cli::cli_inform("{.arg se} applies only to loess curves; no bands are drawn on this plot.")
}

# Residual vs predicted =======================================================

#' Internal function for raw residuals against predicted values
#' @noRd
resid_vs_predicted <- function(imp_list, outcome, se) {
    if (outcome$level == 2L) throw_error(c(
        "Level-2 residuals have no predicted values.",
        "i" = "Use {.code x = \"case\"} or a level-2 predictor."
    ))
    if (outcome$type == "count") throw_error(c(
        "Residuals against predicted values are not plotted for count outcomes.",
        "i" = "Use {.code x = \"case\"} or a predictor for Pearson residuals."
    ))
    if (outcome$type == "nominal") return(binned_plot(imp_list, outcome, "predicted"))

    xy_list <- lapply(imp_list, \(d) data.frame(x = d[[outcome$predicted]], y = d[[outcome$residual]]))
    panel <- list(name = "Predicted", xy_list = xy_list, discrete = FALSE)
    scatter_panels(
        list(panel), se = se, thresholds = NULL,
        title = sprintf("Residual vs. Predicted: %s", outcome$name),
        x_label = "Predicted", y_label = "Raw Residual"
    )
}

# Residual vs predictors ======================================================

#' Internal function for residuals against one or more predictors
#' @noRd
resid_vs_predictors <- function(model, imp_list, outcome, x, se, index_thresholds) {
    preds <- diag_outcome_predictors(model, outcome)
    discrete_vars <- diag_discrete_vars(model)

    # Resolve requested names against the allowed predictors for this level
    allowed <- if (outcome$level == 1L) preds$level1 else preds$level2
    other <- if (outcome$level == 1L) preds$level2 else preds$level1
    cols <- vapply(x, \(p) resolve_predictor(p, allowed), "")
    bad <- x[is.na(cols)]
    if (length(bad)) {
        at_other <- bad[!is.na(vapply(bad, \(p) resolve_predictor(p, other), ""))]
        hint <- if (length(at_other) && outcome$level == 1L && length(outcome$level2_residuals)) {
            c("i" = "{.val {at_other}} {?is a/are} level-2 predictor{?s}; plot {?it/them} against {.val {outcome$level2_residuals[1]}}.")
        } else if (length(at_other)) {
            c("i" = "{.val {at_other}} {?is a/are} level-1 predictor{?s}.")
        }
        throw_error(c(
            "{.val {bad}} {?is/are} not {?a predictor/predictors} of {.val {outcome$name}}.",
            "i" = "Valid values: {.val {c('predicted', 'case', names(allowed))}}.",
            hint
        ))
    }

    if (outcome$type == "nominal") {
        if (se) inform_no_se()
        return(binned_plot(imp_list, outcome, cols, discrete_vars))
    }

    # Build one panel per predictor
    if (outcome$level == 2L) {
        sigma <- NULL
        cl <- outcome$cluster
    } else if (outcome$type == "count") {
        sigma <- pearson_sd(imp_list, outcome, model)
    } else {
        sigma <- pooled_sigma(imp_list, outcome$residual, paste0("var.", outcome$name, ".predicted"))
    }
    groupmean <- diag_groupmean_vars(model)
    panels <- lapply(seq_along(cols), \(i) {
        col <- cols[[i]]
        name <- names(allowed)[match(col, allowed)]
        is_discrete <- name %in% discrete_vars || sub("\\[.+\\]$", "", name) %in% discrete_vars
        xfun <- \(d) d[[col]]
        # Level-1 predictors are labeled by their model name; level-2 columns describe themselves
        x_label <- if (outcome$level == 1L) name else var_label(col)
        center_col <- paste0(name, ".mean[", outcome$cluster, "]")
        if (outcome$level == 1L && name %in% groupmean && center_col %in% names(imp_list[[1]])) {
            xfun <- \(d) d[[col]] - d[[center_col]]
            x_label <- paste0(name, " (group-mean centered)")
        }
        xy_list <- lapply(imp_list, \(d) {
            if (outcome$level == 2L) {
                keep <- !duplicated(d[[cl]])
                data.frame(x = xfun(d)[keep], y = d[[outcome$residual]][keep])
            } else if (outcome$type == "count") {
                data.frame(x = xfun(d), y = (d[[outcome$name]] - d[[outcome$predicted]]) / sigma)
            } else {
                data.frame(x = xfun(d), y = d[[outcome$residual]] / sigma)
            }
        })
        nux <- length(unique(xfun(imp_list[[1]])))
        if (!is_discrete && nux <= 5) warn_few_values(name, nux)
        list(name = x_label, xy_list = xy_list, discrete = is_discrete || nux <= 10)
    })

    if (se && all(vapply(panels, \(p) p$discrete, TRUE))) inform_no_se()
    if (outcome$level == 2L) {
        y_label <- var_label(outcome$name)
        title <- sprintf("%s vs. %s", y_label, paste(vapply(panels, \(p) p$name, ""), collapse = ", "))
        thresholds <- NULL
    } else if (outcome$type == "count") {
        y_label <- "Standardized Pearson Residual"
        title <- sprintf("Standardized Pearson Residual vs. Predictor: %s", outcome$name)
        thresholds <- NULL
    } else {
        y_label <- "Standardized Residual"
        title <- sprintf("Standardized Residual vs. Predictor: %s", outcome$name)
        thresholds <- index_thresholds
    }
    scatter_panels(
        panels, se = se, thresholds = thresholds, title = title,
        x_label = if (length(panels) == 1) panels[[1]]$name else NULL, y_label = y_label
    )
}

#' Internal function to match a requested predictor name to its column
#' Accepts the name as it appears in the model terms, the saved column name,
#' or a level-2 column without its cluster bracket.
#' @noRd
resolve_predictor <- function(p, allowed) {
    if (p %in% names(allowed)) return(allowed[[p]])
    if (p %in% allowed) return(p)
    stripped <- sub("\\[.+\\]$", "", allowed)
    if (p %in% stripped) return(allowed[[match(p, stripped)]])
    NA_character_
}

#' Internal function to build scatter panels with pooled curves
#' Each panel holds per-imputation (x, y) data. Continuous panels get a pooled
#' loess; discrete panels get category means. Several panels are faceted.
#' @noRd
scatter_panels <- function(panels, se, thresholds, title, x_label, y_label) {
    M <- length(panels[[1]]$xy_list)
    pts <- do.call(rbind, lapply(panels, \(p) {
        df <- do.call(rbind, p$xy_list)
        df <- df[is.finite(df$x) & is.finite(df$y), , drop = FALSE]
        if (nrow(df)) df$predictor <- p$name
        df
    }))
    if (is.null(pts) || !nrow(pts)) throw_error("No finite residuals to plot.")
    levels_order <- vapply(panels, \(p) p$name, "")
    pts$predictor <- factor(pts$predictor, levels = levels_order)

    p <- ggplot2::ggplot(pts, ggplot2::aes(x = x, y = y)) + ref_hline(0)
    if (length(thresholds)) p <- p + ref_hline(thresholds, dashed = TRUE)
    p <- p + geom_point_mi(n_imps = M)

    for (panel in panels) {
        if (panel$discrete) {
            df <- do.call(rbind, panel$xy_list)
            df <- df[is.finite(df$x) & is.finite(df$y), , drop = FALSE]
            md <- stats::aggregate(y ~ x, data = transform(df, x = round(x)), FUN = mean)
            md$predictor <- factor(panel$name, levels = levels_order)
            p <- p + summary_points(
                ggplot2::geom_point(data = md, ggplot2::aes(x = x, y = y, colour = ggplot2::from_theme(accent))),
                shape = 18
            )
        } else {
            curve <- pooled_loess(panel$xy_list, se = se)
            if (nrow(curve)) curve$predictor <- factor(panel$name, levels = levels_order)
            p <- p + curve_layers(curve, se)
        }
    }

    if (length(panels) > 1) {
        p <- p + ggplot2::facet_wrap(~predictor, scales = "free_x")
    } else if (panels[[1]]$discrete) {
        p <- p + ggplot2::scale_x_continuous(breaks = integer_breaks)
    }
    p + ggplot2::labs(title = title, x = x_label, y = y_label)
}

# Index plots =================================================================

#' Internal function for the standardized residual index plot
#' @noRd
resid_index <- function(model, imp_list, outcome, index_thresholds, max_labels) {
    if (outcome$type == "nominal") throw_error(c(
        "An index plot is not drawn for nominal outcomes.",
        "i" = "Nominal residuals have no meaningful standardized analogue; use binned residuals."
    ))
    n <- nrow(imp_list[[1]])
    if (outcome$level == 2L) {
        cl <- outcome$cluster
        ids <- unique(imp_list[[1]][[cl]])
        per_imp <- lapply(imp_list, \(d) {
            keep <- !duplicated(d[[cl]])
            data.frame(id = d[[cl]][keep], r = d[[outcome$residual]][keep])
        })
        # Centered within each imputation, scaled by one pooled SD
        s <- sqrt(mean(vapply(per_imp, \(d) stats::var(d$r, na.rm = TRUE), 0), na.rm = TRUE))
        df <- do.call(rbind, lapply(per_imp, \(d) data.frame(
            case = match(d$id, ids), z = (d$r - mean(d$r, na.rm = TRUE)) / s, label = d$id
        )))
        title <- sprintf("Standardized Residual Index: %s", var_label(outcome$name))
        x_label <- "Cluster Number"
        y_label <- "Standardized Level-2 Residual"
    } else if (outcome$type == "count") {
        sbar <- pearson_sd(imp_list, outcome, model)
        df <- do.call(rbind, lapply(imp_list, \(d) data.frame(
            case = seq_len(n), z = (d[[outcome$name]] - d[[outcome$predicted]]) / sbar
        )))
        title <- sprintf("Standardized Pearson Residual Index: %s", outcome$name)
        x_label <- "Record Number"
        y_label <- "Standardized Pearson Residual"
    } else {
        sigma <- pooled_sigma(imp_list, outcome$residual, paste0("var.", outcome$name, ".predicted"))
        df <- do.call(rbind, lapply(imp_list, \(d) data.frame(case = seq_len(n), z = d[[outcome$residual]] / sigma)))
        title <- sprintf("Standardized Residual Index: %s", outcome$name)
        x_label <- "Record Number"
        y_label <- "Standardized Residual"
    }
    df <- df[is.finite(df$z), , drop = FALSE]
    if (nrow(df) < 2) throw_error("Fewer than two finite residuals for {.val {outcome$name}}.")
    if (is.null(df$label)) df$label <- df$case

    p <- ggplot2::ggplot(df, ggplot2::aes(x = case, y = z)) + ref_hline(0)
    if (length(index_thresholds)) p <- p + ref_hline(index_thresholds, dashed = TRUE)
    p <- p + geom_point_mi(n_imps = length(imp_list))

    # Label cases whose mean standardized residual exceeds the largest threshold
    if (length(index_thresholds)) {
        cm <- stats::aggregate(z ~ case + label, data = df, FUN = mean)
        p <- p + outlier_labels(cm, "z", max(abs(index_thresholds)), max_labels, two_sided = TRUE)
    }
    p +
        ggplot2::scale_y_continuous(breaks = \(l) sort(unique(c(seq(floor(l[1]), ceiling(l[2])), index_thresholds)))) +
        ggplot2::labs(title = title, x = x_label, y = y_label)
}

#' Internal function for the pooled Pearson standard deviation of a count outcome
#' Var(Y) = mu + alpha * mu^2 under the negative binomial model. Each imputation uses
#' the dispersion draw from the MCMC iteration it was saved at; when those draws cannot
#' be matched, every imputation uses the posterior median.
#' @noRd
pearson_sd <- function(imp_list, outcome, model) {
    est <- model@estimates
    row <- paste(outcome$name, "Variance Dispersion Parameter")
    if (!row %in% rownames(est) || !"Estimate" %in% colnames(est)) throw_error(c(
        "The dispersion parameter for {.val {outcome$name}} was not found in the estimates.",
        "i" = "Pearson residuals need the {.val {row}} row."
    ))
    alpha <- rep(est[row, "Estimate"], length(imp_list))
    col <- which(tolower(names(model@iterations)) == tolower(paste0(outcome$name, ".variance.Dispersion Parameter")))
    draws <- imputation_draws(model)
    if (length(col) == 1 && !is.null(draws) && length(draws) == length(imp_list)) {
        alpha <- model@iterations[draws, col]
    }
    n <- nrow(imp_list[[1]])
    # Pool the variances, then take the square root, as for continuous outcomes
    sqrt(rowMeans(vapply(seq_along(imp_list), \(m) {
        mu <- imp_list[[m]][[outcome$predicted]]
        mu + alpha[m] * mu^2
    }, numeric(n))))
}

#' Internal function for the MCMC iteration each saved imputation came from
#' Returns row indices into `model@iterations`, or NULL when the counts do not fit
#' Blimp's schedule: the post-burn iterations and the imputations are split across
#' chains (the first chains take any remainder), chain `c` saves its imputations at
#' iterations `step, 2 * step, ...` with `step = iterations_c %/% imputations_c`, and
#' imputation `k` comes from chain `(k - 1) %% chains + 1`. `model@iterations` stacks
#' the chains in order.
#' @noRd
imputation_draws <- function(model) {
    nchain <- length(model@burn)
    niter <- NROW(model@iterations)
    nimp <- length(model@imputations)
    if (nchain < 1 || nimp < 1 || niter < nimp) return(NULL)
    split <- \(total) total %/% nchain + (seq_len(nchain) <= total %% nchain)
    iters <- split(niter)
    imps <- split(nimp)
    k <- seq_len(nimp)
    chain <- (k - 1) %% nchain + 1
    j <- (k - 1) %/% nchain + 1
    step <- pmax(1, iters %/% pmax(imps, 1))
    offset <- c(0, cumsum(iters))[chain]
    rows <- offset + j * step[chain]
    if (any(j > imps[chain]) || any(rows > offset + iters[chain])) return(NULL)
    rows
}

# Cluster spread ==============================================================

#' Internal function for within-cluster residual spread
#' Residuals are standardized by ONE pooled SD shared across clusters, so that
#' unequal spread across clusters stays visible.
#' @noRd
resid_by_cluster <- function(imp_list, outcome) {
    cl <- outcome$cluster
    if (outcome$level != 1L || outcome$type != "continuous" || is.null(cl) || is.na(cl)) throw_error(c(
        "Within-cluster spread is plotted only for continuous outcomes in a clustered model.",
        "i" = "Ordinal, nominal, and count outcomes have no free level-1 residual variance."
    ))
    s <- sqrt(mean(vapply(imp_list, \(d) stats::var(d[[outcome$residual]], na.rm = TRUE), 0), na.rm = TRUE))
    df <- do.call(rbind, lapply(imp_list, \(d) data.frame(cluster = d[[cl]], z = d[[outcome$residual]] / s)))
    df <- df[is.finite(df$z) & !is.na(df$cluster), , drop = FALSE]
    spread <- tapply(df$z, df$cluster, stats::sd, na.rm = TRUE)
    ord <- names(sort(spread[is.finite(spread)]))
    df$rank <- match(as.character(df$cluster), ord)
    df <- df[!is.na(df$rank), , drop = FALSE]

    ggplot2::ggplot(df, ggplot2::aes(x = rank, y = z)) +
        ref_hline(0) +
        ref_hline(c(-3, -2, -1, 1, 2, 3), dashed = TRUE, guide = TRUE) +
        geom_point_mi(n_imps = length(imp_list)) +
        ggplot2::scale_y_continuous(breaks = \(l) seq(floor(l[1]), ceiling(l[2]))) +
        ggplot2::labs(
            title = sprintf("Within-Cluster Residual Spread: %s", outcome$name),
            x = "Cluster (ordered by residual SD)",
            y = "Standardized Level-1 Residual"
        )
}

# Binned residuals ============================================================

#' Internal function for binned residuals of a nominal outcome
#' Within each imputation, residuals I(Y = k) - p are binned by the predicted
#' probability (`x = "predicted"`) or a predictor; bin edges are fixed from the
#' pooled values and the per-imputation bin means are averaged. A panel for each
#' non-reference category. No standard-error bands.
#' @noRd
binned_plot <- function(imp_list, outcome, x, discrete_vars = character(0)) {
    parent <- if (is.null(outcome$category_of)) outcome$name else outcome$category_of
    if (!parent %in% names(imp_list[[1]])) throw_error("Column {.val {parent}} not found in the imputations.")
    rows <- list()
    curves <- list()
    for (k in outcome$categories) {
        prob <- paste0(parent, ".", k, ".probability")
        for (xc in x) {
            by_pred <- xc == "predicted"
            xy_list <- lapply(imp_list, \(d) {
                z <- data.frame(x = if (by_pred) d[[prob]] else d[[xc]], y = as.numeric(d[[parent]] == k) - d[[prob]])
                z[is.finite(z$x) & is.finite(z$y), , drop = FALSE]
            })
            n_keep <- nrow(xy_list[[1]])
            if (n_keep < 16) next
            panel_x <- if (by_pred) "Mean Predicted Probability" else var_label(xc)
            panel_k <- sprintf("%s.%d", parent, as.integer(k))
            discrete <- !by_pred && xc %in% discrete_vars
            if (discrete) {
                per_imp <- lapply(xy_list, \(d) {
                    rm <- tapply(d$y, d$x, mean)
                    data.frame(x = as.numeric(names(rm)), y = as.numeric(rm))
                })
                all_lv <- do.call(rbind, per_imp)
                bs <- stats::aggregate(y ~ x, data = all_lv, FUN = mean)
            } else {
                allx <- unlist(lapply(xy_list, \(d) d$x), use.names = FALSE)
                edges <- unique(stats::quantile(allx, probs = seq(0, 1, length.out = max(1L, floor(sqrt(n_keep))) + 1)))
                if (length(edges) < 3) next
                per_imp <- lapply(xy_list, \(d) {
                    b <- cut(d$x, breaks = edges, include.lowest = TRUE, labels = FALSE)
                    data.frame(bin = sort(unique(b)), x = as.numeric(tapply(d$x, b, mean)), y = as.numeric(tapply(d$y, b, mean)))
                })
                all_b <- do.call(rbind, per_imp)
                bs <- stats::aggregate(cbind(x, y) ~ bin, data = all_b, FUN = mean)[, c("x", "y")]
                cv <- pooled_loess(xy_list)
                if (nrow(cv)) {
                    cv$category <- panel_k
                    cv$predictor <- panel_x
                    curves[[length(curves) + 1]] <- cv
                }
            }
            bs$category <- panel_k
            bs$predictor <- panel_x
            rows[[length(rows) + 1]] <- bs
        }
    }
    if (!length(rows)) throw_error("Too few cases to bin residuals for {.val {parent}}.")
    bins <- do.call(rbind, rows)
    curve <- if (length(curves)) do.call(rbind, curves) else data.frame()

    p <- ggplot2::ggplot(bins, ggplot2::aes(x = x, y = y)) +
        ref_hline(0) +
        geom_point_mi()
    if (nrow(curve)) p <- p + curve_layers(curve)
    n_k <- length(unique(bins$category))
    n_x <- length(unique(bins$predictor))
    if (n_k > 1 && n_x > 1) {
        p <- p + ggplot2::facet_grid(category ~ predictor, scales = "free_x")
    } else if (n_k > 1) {
        p <- p + ggplot2::facet_wrap(~category)
    } else if (n_x > 1) {
        p <- p + ggplot2::facet_wrap(~predictor, scales = "free_x")
    }
    by_pred <- identical(x, "predicted")
    p + ggplot2::labs(
        title = sprintf(if (by_pred) "Binned Residual vs. Predicted: %s" else "Binned Residual vs. Predictor: %s", outcome$name),
        x = if (n_x == 1) bins$predictor[1] else NULL,
        y = "Mean Residual"
    )
}
