# Copyright Brian Keller 2026, all rights reserved

#' Every applicable diagnostic plot for a [`blimp_obj`]
#' @description
#' Builds every residual and influence plot that applies to each outcome in a model, as a
#' named list of [`ggplot2::ggplot`] plots. Each element is one call to [`residual_plot`],
#' [`influence_plot`], or [`probability_plot`].
#' @param model a [`blimp_obj`] with saved imputations.
#' @param selector an optional character vector of outcome names. If missing, every outcome
#'   with saved residuals is included.
#' @param discrete_max_unique an integer, passed to [`residual_plot`] and
#'   [`probability_plot`]. A predictor with this many or fewer distinct values gets category
#'   means instead of a loess curve.
#' @param x a `diagnostic_plots` list.
#' @param e1,e2 a `diagnostic_plots` list (left) and a ggplot2 component such as a theme,
#'   scale, layer, or labels (right).
#' @param i indices or names of the plots to keep.
#' @param pattern a regular expression matched against the plot names.
#' @param ... for `subset()`, arguments passed to [`grepl`] (e.g., `fixed = TRUE`); otherwise
#'   unused.
#' @returns A named list of plots with class `diagnostic_plots`. Printing it draws every plot.
#'   `+` and `&` add a ggplot2 component to every plot (the list must be on the left), and
#'   `[` and `subset()` keep the class. `subset(x, pattern)` keeps the plots whose names
#'   match `pattern`, e.g. `subset(plots, "index")` or `plots |> subset("x1")`.
#' @details
#' The plots for each outcome depend on its type:
#'
#' | Outcome | Elements |
#' |---|---|
#' | Continuous, ordinal | `<dv>.predicted`, `<dv>.<predictor>`, `<dv>.index`, `<dv>.leverage`, `<dv>.cooks` |
#' | Ordinal | also `<dv>.<predictor>.probability` |
#' | Continuous, clustered | also `<dv>.cluster_variance` |
#' | Nominal | `<dv>.<k>.binned`, `<dv>.<k>.<predictor>` for each non-reference category `k`; `<dv>.<predictor>.probability` |
#' | Count | `<dv>.index`, `<dv>.<predictor>` (Pearson residuals) |
#' | Level-2 residual or latent | `<name>.index`, `<name>.<level-2 predictor>` |
#'
#' Leverage and Cook's distance require fitting the model with `influence = TRUE`; without
#' them, every other plot is drawn and a warning says how to add them.
#'
#' Element names are for browsing. To build a specific figure, call the single-plot
#' function directly, e.g. `residual_plot(model, "y", x = "x1")`.
#' @seealso [`residual_plot`], [`influence_plot`], [`probability_plot`], [`distribution_plots`]
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
#' m1 <- rblimp('y ~ x m', mydata, nimps = 10, seed = 10972, burn = 1000, iter = 1000)
#'
#' # Every diagnostic, restyled at once
#' plots <- diagnostic_plots(m1) + ggplot2::theme_minimal()
#' names(plots)
#' plots$y.index
#'
#' # Only the index plots
#' diagnostic_plots(m1) |> subset("index")
#' @import ggplot2
#' @export
diagnostic_plots <- function(model, selector, discrete_max_unique = 10L) {

    # Check inputs
    check_diagnostic_model(model)
    check_max_unique(discrete_max_unique)
    outcomes <- diag_outcomes(model)
    tt <- diag_term_table(model)
    # Outcomes of the user's model: with fitted terms, latent variables, or level-2 residuals.
    # This leaves out Blimp's auto-generated models for incomplete predictors.
    cols <- names(model@imputations[[1]])
    outcomes <- Filter(\(o) {
        o$level == 2L || any(o$terms %in% tt$base) || paste0(o$name, ".latent") %in% cols
    }, outcomes)
    if (!missing(selector)) {
        bad <- setdiff(selector, names(outcomes))
        if (length(bad)) throw_error(c(
            "{.val {bad}} {?is not an outcome/are not outcomes} with saved residuals.",
            "i" = "Available outcomes: {.val {names(outcomes)}}."
        ))
        outcomes <- outcomes[selector]
    }
    if (!length(outcomes)) throw_error("{.arg model} has no outcomes with saved residuals.")

    inf <- attr(model@average_imp, "influence")
    plots <- list()
    skipped <- character(0)
    add <- function(name, expr) {
        res <- tryCatch(expr, error = \(e) {
            skipped <<- c(skipped, paste0(name, ": ", conditionMessage(e)))
            NULL
        })
        if (!is.null(res)) plots[[name]] <<- res
    }

    for (o in outcomes) {
        dv <- o$name
        # Nominal categories are handled with their parent unless selected on their own
        if (!is.null(o$category_of) && o$category_of %in% names(outcomes)) next
        if (o$level == 2L) {
            add(paste0(dv, ".index"), residual_plot(model, dv, x = "#case"))
            for (col in diag_outcome_predictors(model, o)$level2) {
                add(paste0(dv, ".", col), residual_plot(model, dv, x = col, discrete_max_unique = discrete_max_unique))
            }
            next
        }
        preds <- diag_outcome_predictors(model, o)

        if (o$type %in% c("continuous", "ordinal")) {
            add(paste0(dv, ".predicted"), residual_plot(model, dv, x = "#predicted"))
            for (p in names(preds$level1)) {
                add(paste0(dv, ".", p), residual_plot(model, dv, x = p, discrete_max_unique = discrete_max_unique))
            }
            add(paste0(dv, ".index"), residual_plot(model, dv, x = "#case"))
            if (o$type == "continuous" && !is.na(o$cluster)) {
                add(paste0(dv, ".cluster_variance"), residual_plot(model, dv, x = "#cluster"))
            }
            if (o$type == "ordinal") {
                for (p in unique(c(names(preds$level1), names(preds$level2)))) {
                    add(paste0(dv, ".", p, ".probability"), probability_plot(model, dv, p, discrete_max_unique))
                }
            }
            if (!is.null(inf) && paste0(dv, ".leverage") %in% names(inf)) {
                add(paste0(dv, ".leverage"), influence_plot(model, dv, measure = "leverage"))
                add(paste0(dv, ".cooks"), influence_plot(model, dv, measure = "cooks"))
            }
        } else if (o$type == "nominal") {
            # One set of binned plots per non-reference category
            cats <- if (is.null(o$category_of)) paste0(dv, ".", o$categories) else dv
            for (ck in cats) {
                add(paste0(ck, ".binned"), residual_plot(model, ck, x = "#predicted"))
                for (p in names(diag_outcome_predictors(model, diag_outcome(model, ck))$level1)) {
                    add(paste0(ck, ".", p), residual_plot(model, ck, x = p, discrete_max_unique = discrete_max_unique))
                }
            }
            parent <- if (is.null(o$category_of)) dv else o$category_of
            for (p in unique(c(names(preds$level1), names(preds$level2)))) {
                add(paste0(parent, ".", p, ".probability"), probability_plot(model, parent, p, discrete_max_unique))
            }
        } else if (o$type == "count") {
            add(paste0(dv, ".index"), residual_plot(model, dv, x = "#case"))
            for (p in names(preds$level1)) {
                add(paste0(dv, ".", p), residual_plot(model, dv, x = p, discrete_max_unique = discrete_max_unique))
            }
        }
    }

    # Missing influence values
    single_level <- Filter(\(o) o$level == 1L && o$type %in% c("continuous", "ordinal") && is.na(o$cluster), outcomes)
    if (is.null(inf) && length(single_level)) cli::cli_warn(c(
        "Leverage and Cook's distance plots are not included.",
        "i" = "Refit with {.code influence = TRUE} to add them."
    ))
    if (length(skipped)) cli::cli_inform(c("Not plotted:", stats::setNames(skipped, rep("*", length(skipped)))))

    structure(plots, class = c("diagnostic_plots", "list"))
}

#' Every applicable distribution plot for a [`blimp_obj`]
#' @description
#' Builds a [`distribution_plot`] for every plottable variable in a model, and a [`qq_plot`]
#' for every continuous one, as a named list of [`ggplot2::ggplot`] plots.
#' @inheritParams distribution_plot
#' @param selector an optional character vector of variable names. If missing, includes the
#'   variables in the model, the model-implied quantities (residuals, latent variables,
#'   probabilities, and Yeo-Johnson values), and level-2 quantities.
#' @param qq a logical value. If `TRUE`, adds a `<var>.qq` normal Q-Q plot wherever one applies.
#' @param x a `distribution_plots` list.
#' @param e1,e2 a `distribution_plots` list (left) and a ggplot2 component such as a theme,
#'   scale, layer, or labels (right).
#' @param i indices or names of the plots to keep.
#' @param pattern a regular expression matched against the plot names.
#' @param ... for `subset()`, arguments passed to [`grepl`] (e.g., `fixed = TRUE`); otherwise
#'   unused.
#' @returns A named list of plots with class `distribution_plots`, named by variable, with
#'   Q-Q plots named `<var>.qq`. Printing it draws every plot. `+` and `&` add a ggplot2
#'   component to every plot (the list must be on the left), and `[` and `subset()` keep the
#'   class. `subset(x, pattern)` keeps the plots whose names match `pattern`, e.g.
#'   `plots |> subset("qq")`.
#' @details
#' Predicted values are not included by default; request them with `selector`.
#' @seealso [`distribution_plot`], [`qq_plot`], [`diagnostic_plots`]
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
#' plots <- distribution_plots(m1)
#' names(plots)
#' plots$x
#' @import ggplot2
#' @export
distribution_plots <- function(model, selector, qq = TRUE, bins = 30, discrete_max_unique = 5L) {

    # Check inputs
    check_diagnostic_model(model)
    vars <- if (missing(selector)) distribution_vars(model) else selector
    bad <- setdiff(vars, names(model@imputations[[1]]))
    if (length(bad)) throw_error(c(
        "{.val {bad}} {?is not a saved variable/are not saved variables}.",
        "i" = "Use {.code names(model)} to list the saved variables."
    ))

    plots <- list()
    skipped <- character(0)
    discrete <- diag_discrete_vars(model)
    for (v in vars) {
        res <- tryCatch(distribution_plot(model, v, bins, discrete_max_unique), error = \(e) {
            skipped <<- c(skipped, paste0(v, ": ", conditionMessage(e)))
            NULL
        })
        if (is.null(res)) next
        plots[[v]] <- res
        if (qq && is.null(qq_omitted(model, v)) &&
            !is_discrete(model@imputations[[1]][[v]], v %in% discrete, discrete_max_unique)) {
            q <- tryCatch(qq_plot(model, v, discrete_max_unique), error = \(e) NULL)
            if (!is.null(q)) plots[[paste0(v, ".qq")]] <- q
        }
    }
    if (length(skipped)) cli::cli_inform(c("Not plotted:", stats::setNames(skipped, rep("*", length(skipped)))))
    structure(plots, class = c("distribution_plots", "list"))
}

#' Internal function listing the default variables for `distribution_plots()`
#' Variables in the model's terms and its outcomes, then model-implied quantities,
#' then level-2 quantities. Predicted values are left out.
#' @noRd
distribution_vars <- function(model) {
    cols <- names(model@imputations[[1]])
    tt <- diag_term_table(model)
    term_vars <- unlist(lapply(tt$term, \(t) {
        ex <- diag_term_expr(t)
        if (is.null(ex)) character(0) else all.vars(ex)
    }), use.names = FALSE)
    model_vars <- c(tt$base, sub("\\.[0-9]+$", "", tt$base), term_vars, sub("\\.[0-9]+$", "", term_vars))
    raw <- cols[cols %in% model_vars & cols %in% names(model@variance_imp) & !grepl("\\[.+\\]$", cols)]

    implied <- grep("\\.(residual|latent|probability|yjt)$", cols, value = TRUE)
    implied <- implied[!grepl("\\[.+\\]$", implied)]

    level2 <- grep("\\[.+\\]$", cols, value = TRUE)
    unique(c(raw, implied, level2))
}

# Methods for plot lists =====================================================

#' Internal function keeping the plots in a list whose names match a pattern
#' @noRd
subset_plots <- function(x, pattern, ...) {
    if (!is.character(pattern) || length(pattern) != 1 || is.na(pattern)) throw_error(
        "{.arg pattern} must be a single character string."
    )
    keep <- grepl(pattern, names(x), ...)
    if (!any(keep)) cli::cli_warn("No plot names match {.val {pattern}}.", call = NULL)
    x[keep]
}

#' Internal function adding a ggplot2 component to every plot in a list
#' @noRd
add_to_plots <- function(e1, e2) {
    if (!inherits(e1, c("diagnostic_plots", "distribution_plots"))) throw_error(c(
        "Put the plot list on the left of {.code +} or {.code &}.",
        "i" = "For example, {.code plots + ggplot2::theme_minimal()}."
    ))
    if (missing(e2)) throw_error("Nothing to add to the plots.")
    structure(lapply(e1, \(p) p + e2), class = class(e1))
}

#' @rdname diagnostic_plots
#' @export
print.diagnostic_plots <- function(x, ...) {
    for (p in x) print(p)
    invisible(x)
}

#' @rdname diagnostic_plots
#' @export
`[.diagnostic_plots` <- function(x, i) {
    structure(unclass(x)[i], class = class(x))
}

#' @rdname diagnostic_plots
#' @export
subset.diagnostic_plots <- function(x, pattern, ...) subset_plots(x, pattern, ...)

#' @rdname diagnostic_plots
#' @export
`+.diagnostic_plots` <- function(e1, e2) add_to_plots(e1, e2)

#' @rdname diagnostic_plots
#' @export
`&.diagnostic_plots` <- function(e1, e2) add_to_plots(e1, e2)

#' @rdname distribution_plots
#' @export
print.distribution_plots <- function(x, ...) {
    for (p in x) print(p)
    invisible(x)
}

#' @rdname distribution_plots
#' @export
`[.distribution_plots` <- function(x, i) {
    structure(unclass(x)[i], class = class(x))
}

#' @rdname distribution_plots
#' @export
subset.distribution_plots <- function(x, pattern, ...) subset_plots(x, pattern, ...)

#' @rdname distribution_plots
#' @export
`+.distribution_plots` <- function(e1, e2) add_to_plots(e1, e2)

#' @rdname distribution_plots
#' @export
`&.distribution_plots` <- function(e1, e2) add_to_plots(e1, e2)

# ggplot2 also defines `+` and `&` for its components. These tell R to use the
# methods above whenever a plot list is an operand.

#' @exportS3Method base::chooseOpsMethod
#' @noRd
chooseOpsMethod.diagnostic_plots <- function(x, y, mx, my, cl, reverse) TRUE

#' @exportS3Method base::chooseOpsMethod
#' @noRd
chooseOpsMethod.distribution_plots <- function(x, y, mx, my, cl, reverse) TRUE
