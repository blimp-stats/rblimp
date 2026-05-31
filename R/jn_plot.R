
# Copyright Brian Keller 2026, all rights reserved


#' Internal function to Filter if significant or Not
#' @noRd
set_group <- function(x){ with(rle(x), {
    unlist(lapply(seq_along(lengths), \(i) rep(i, lengths[i])))
})}


#' Function to generate a Johnson-Neyman Plot of Conditional Slope based on a function to produce the conditional effect.
#' @description
#' Generates a Johnson-Neyman Plot using a function to produce the conditional effect
#' @param func a [`function`] used to compute the conditional effect on moderator.
#' @param xrange a [`numeric`] of length two with the min and max of the x-axis
#' @param ci a value between 0 and 1 specifying the credible interval size
#' @param ... values passed to internal boundary search algorithm. See Details below.
#' @returns a [`ggplot2::ggplot`] plot. The bounding values are saved in the attribute 'bounds'.
#' @details
#' To change colors use ggplot2's scale system. Both fill and color are used. See
#' [`ggplot2::aes_colour_fill_alpha`] for more information about setting a manual set of colors.
#'
#' For `...`, the arguments are passed to the internal boundary search algorithm.
#' This algorithm uses an initial grid search to locate boundaries based on the range
#' and then a binary search to refine the estimates.
#' The following arguments are available:
#' \describe{
#'   \item{n_initial}{Number of points in the initial coarse grid search used to
#'     locate approximate boundary positions. Higher values improve detection of
#'     closely-spaced boundaries but increase computation time. Default is 1000.}
#'   \item{refine_tol}{Tolerance for binary search refinement. The algorithm
#'     refines each boundary until the interval width is smaller than this value.
#'     Smaller values give higher precision but require more function evaluations.
#'     Default is 1e-12.}
#'   \item{max_iter}{Maximum number of iterations for binary search refinement
#'     per boundary. Prevents infinite loops if tolerance cannot be achieved.
#'     Default is 100.}
#'   \item{adaptive}{Logical indicating whether to perform additional refinement
#'     in regions where boundaries are detected to be closely spaced. When TRUE,
#'     uses a finer grid to resolve boundaries that may be missed by the initial
#'     coarse grid. Default is TRUE.}
#' }
#' @examplesIf has_blimp()
#' # Generate Data
#' mydata <- rblimp_sim(
#'     c(
#'         'x1 ~ normal(0, 1)',
#'         'x2 ~ normal(0, 1)',
#'         'm ~ normal(0, 1)',
#'         'y ~ normal(10 + 0.5*x1 + 0.5*x2 + m + 0.2*x1*x2 + 0.3*x2*m + 0.1*x1*m + 0.7*x1*x2*m, 1)'
#'     ),
#'     n = 100,
#'     seed = 981273
#' )
#'
#' # Run Rblimp
#' m1 <- rblimp(
#'     'y ~ x1 x2 m x1*x2 x1*m x2*m x1*x2*m',
#'     mydata,
#'     center = ~ m,
#'     seed = 10972,
#'     burn = 1000,
#'     iter = 1000
#' )
#'
#' # Get parameter values
#' params <- m1 |> as.matrix()
#'
#' # Generate Plot
#' (
#'     jn_plot_func(
#'         compute_condeff(params[,6], params[,9]),
#'         xrange = c(-3, 3)
#'     )
#'     + ggplot2::labs(
#'         title = 'Johnson-Neyman Plot for `x1` * `x2` Moderated by `x2`',
#'         subtitle = 'Red area represents 0 within 95% interval',
#'         y = 'y ~ x1 * x2',
#'         x = 'm'
#'     )
#'     + ggplot2::theme_minimal()
#' )
#' @seealso [compute_condeff()]
#' @import ggplot2
#' @export
jn_plot_func <- function(func, xrange, ci = 0.95, ...) {

    # Check inputs
    if (is.function(func) == FALSE) throw_error(
        "The {.arg func} must be a function"
    )

    if (ci >= 1.0 | ci <= 0.0) throw_error(
        "The {.arg ci} must be between 0 and 1"
    )

    if (!is.numeric(xrange) || !is.vector(xrange) || is.list(xrange) || length(xrange) != 2) {
        throw_error("{.arg {xrange}} must be a numeric vector of length 2")
    }

    if (any(is.na(xrange))) throw_error(
        "{.arg {xrange}} cannot contain NA or NaN values"
    )

    if (any(is.infinite(xrange))) throw_error(
        "{.arg {xrange}} cannot contain Inf values"
    )

    if (xrange[1] >= xrange[2]) throw_error(
        "{.arg {xrange}}[1] ({xrange[1]}) must be < {.arg {xrange}}[2] ({xrange[2]})"
    )


    # Handle probabilities
    ci <- (1 - ci) / 2
    probs <- c(ci, 1 - ci)

    # Create function
    f <- function(m) {
        # Check if 0 is within the interval (product will be negative)
        apply(func(m, quantile, probs = probs), 2, prod) >= 0
    }

    # Obtain boundaries
    boundaries <- find_boundaries(f, xrange[1], xrange[2], ...)
    bound_y <- sapply(boundaries, func, quantile, probs = probs)

    # Suppress R CMD check NOTEs about ggplot2 NSE
    x <- y <- NULL
    # Create plot
    p <- (
        ggplot()
        # Set 0 value line
        + geom_hline(yintercept = 0)
        # Create Ribbon
        + stat_function(
            fun = f,
            aes(
                # Draw ribbon along lower
                ymin = func(after_stat(x), quantile, probs = probs[1]),
                # Draw ribbon along upper
                ymax = func(after_stat(x), quantile, probs = probs[2]),
                # Set color based on 0 being in the interval
                fill = after_stat(y), group = set_group(after_stat(y))
            ),
            # Draws a ribbon transparency and
            geom = 'ribbon', alpha = 0.25, n = 1000
        )
        # Line for 2.5%
        + geom_function(
            fun = func,
            args = list(quantile, probs = probs[1]),
            color = 'black', linetype = 'dashed'
        )
        # Line for 97.5%
        + geom_function(
            fun = func,
            args = list(quantile, probs = probs[2]),
            color = 'black', linetype = 'dashed'
        )
        # Line for Median
        + geom_function(
            fun = func,
            args = list(median),
            color = 'black'
        )
    )

    if (NROW(bound_y) == 2) {
        p <- (
            p
            # Add boundary lines
            + geom_segment(
                aes(
                    x = boundaries,
                    xend = boundaries,
                    y = bound_y[1,],
                    yend = bound_y[2,],
                ),
                color = 'black', alpha = 0.50
            )
        )
    }

    p <- (
        p
        # Set range
        + xlim(xrange)
        # Remove guide
        + guides(fill = "none")
    )

    # Return plot
    return(
        structure(
            p,
            boundaries = boundaries
        )
    )
}

#' Convenience Function for computing conditional effects for [`jn_plot_func`]
#' @param value1 The base value
#' @param value2 The value to change as a function of moderator
#' @returns a [`function`]
#' @seealso [jn_plot_func()]
#' @examplesIf has_blimp()
#' # Generate Data
#' mydata <- rblimp_sim(
#'     c(
#'         'x1 ~ normal(0, 1)',
#'         'x2 ~ normal(0, 1)',
#'         'm ~ normal(0, 1)',
#'         'y ~ normal(10 + 0.5*x1 + 0.5*x2 + m + 0.2*x1*x2 + 0.3*x2*m + 0.1*x1*m + 0.7*x1*x2*m, 1)'
#'     ),
#'     n = 100,
#'     seed = 981273
#' )
#'
#' # Run Rblimp
#' m1 <- rblimp(
#'     'y ~ x1 x2 m x1*x2 x1*m x2*m x1*x2*m',
#'     mydata,
#'     center = ~ m,
#'     seed = 10972,
#'     burn = 1000,
#'     iter = 1000
#' )
#'
#' # Get parameter values
#' params <- m1 |> as.matrix()
#'
#' # Generate Plot
#' (
#'     jn_plot_func(
#'         compute_condeff(params[,6], params[,9]),
#'         xrange = c(-3, 3)
#'     )
#'     # Set custom colors
#'     + ggplot2::scale_fill_manual(
#'         values = c(`FALSE` = '#ca0020', `TRUE` = '#0571b0')
#'     )
#'     + ggplot2::labs(
#'         title = 'Johnson-Neyman Plot for `x1` * `x2` Moderated by `x2`',
#'         subtitle = 'Red area represents 0 within 95% interval',
#'         y = 'y ~ x1 * x2',
#'         x = 'm'
#'     )
#'     + ggplot2::theme_minimal()
#' )
#' @export
compute_condeff <- function(value1, value2) {
    force(value1); force(value2)
    function(m, func, ...) {
        o <- sapply(m, \(x) value1 + value2 * x, simplify = TRUE)
        apply(o, 2, func, ...)
    }
}

#' Function to generate a Johnson-Neyman Plot of Conditional Slope with [`rblimp`]
#' @description
#' Generates a Johnson-Neyman Plot based on the posterior summaries from the output of [`rblimp`].
#' @param formula an object of class [`formula`] to specify simple effect to plot.
#' The formula must have the following form: `outcome ~ focal | moderator`. See Details below for nominal moderators.
#' @param model an [`blimp_obj`].
#' @param ci a value between 0 and 1 specifying the credible interval size
#' @param ... passed bounds search algorithm. See [`jn_plot_func`] for details.
#' @returns a [`ggplot2::ggplot`] plot. The bounding values are saved in the attribute 'bounds'.
#' @details
#' To change colors use ggplot2's scale system. Both fill and color are used. See
#' [`ggplot2::aes_colour_fill_alpha`] for more information about setting a manual set of colors.
#'
#' When the model contains a SIMPLE command that evaluates the focal predictor at
#' multiple values of the formula moderator (e.g., `'x | m @ quantile'`), the
#' conditional slope is reconstructed from the SIMPLE slope draws via a
#' per-iteration linear fit across the evaluated points. If the same SIMPLE
#' command also holds one or two additional moderators at multiple values
#' (e.g., `'x | m @ quantile and z @ sd'`), the plot is faceted by those
#' additional moderators. When SIMPLE has no relevant rows, `jn_plot` falls
#' back to building the conditional slope directly from the model's fixed-effect
#' interaction parameter (the original behavior).
#'
#' @seealso [`jn_plot_func`], [`jn_map`], [`simple_plot`], [`at`], [`join`]
#' @examplesIf has_blimp()
#' \dontrun{
#' # ---- Basic single-moderator example ----
#' mydata <- rblimp_sim(
#'     c(
#'         'x ~ normal(0, 1)',
#'         'm ~ normal(0, 1)',
#'         'y ~ normal(10 + 0.5*x + m + 0.2*x*m, 1)'
#'     ),
#'     n = 100,
#'     seed = 981273
#' )
#' m1 <- rblimp(
#'     'y ~ x m x*m',
#'     mydata, center = ~ m, simple = 'x | m',
#'     seed = 10972, burn = 1000, iter = 1000
#' )
#' jn_plot(y ~ x | m, m1)
#'
#' # Custom significance-region fill
#' (
#'     jn_plot(y ~ x | m, m1)
#'     + ggplot2::scale_fill_manual(
#'         values = c(`FALSE` = '#ca0020', `TRUE` = '#0571b0')
#'     )
#' )
#'
#' # ---- Two-moderator fit: first mod = x-axis, second auto-faceted ----
#' three_way <- rblimp_sim(
#'     c(
#'         'x  ~ normal(0, 1)',
#'         'm1 ~ normal(0, 1)',
#'         'm2 ~ normal(0, 1)',
#'         'y  ~ normal(10 + 0.5*x*m1 + 0.3*x*m2 + 0.6*x*m1*m2, 1)'
#'     ),
#'     n = 500, seed = 2024
#' )
#' fit <- rblimp(
#'     'y ~ x m1 m2 x*m1 x*m2 m1*m2 x*m1*m2',
#'     three_way, center = ~ x + m1 + m2,
#'     simple = 'x | m1 @ quantile and m2 @ sd',
#'     seed = 1071, burn = 1000, iter = 1000
#' )
#' jn_plot(y ~ x | m1 + m2, fit)
#'
#' # Pin one moderator via `at()`
#' jn_plot(y ~ x | m1 + at(m2 = "0"), fit)
#'
#' # Restrict to a subset of SIMPLE values
#' jn_plot(y ~ x | m1 + at(m2 = c("-1 SD", "+1 SD")), fit)
#'
#' # Per-facet boundary x-values are also exposed as an attribute
#' attr(jn_plot(y ~ x | m1 + m2, fit), "bounds")
#' }
#' @import ggplot2
#' @importFrom methods is
#' @export
jn_plot <- function(formula, model, ci = 0.95, ...) {

    # Check inputs
    if (ci >= 1.0 | ci <= 0.0) throw_error(
        "The {.arg ci} must be between 0 and 1"
    )
    if (!is(model, 'blimp_obj')) throw_error(
        "{.arg model} is not a `blimp_obj`"
    )

    # Parse formula via language tree (handles bare mods, `at(...)`, `join(...)`)
    pf <- parse_plot_formula(formula)
    out                <- pf$outcome
    pre                <- pf$focal
    formula_mods       <- pf$bare_mods
    mod_components     <- pf$mod_components
    at_filter          <- pf$at_filter

    if (length(formula_mods) < 1) throw_error(c(
        "The {.arg formula} must specify at least one bare moderator after `|`.",
        "Must have the form: `outcome ~ focal | moderator`"
    ))
    mod <- formula_mods[1]
    extra_formula_mods <- formula_mods[-1]

    # Centering check
    centered_vars <- if (is.null(model@syntax$center)) character(0) else {
        model@syntax$center |> strsplit(' ') |> unlist() |> tolower() |>
            gsub(';', '', x = _)
    }
    var_is_cent <- function(name) {
        (tolower(name) |> sub("\\s*\\[[^]]*\\]$", "", x = _)) %in% centered_vars
    }
    pre_is_cent <- var_is_cent(pre)
    mod_is_cent <- var_is_cent(mod)

    # Moderator range from average_imp; fall back to a default range when
    # the imputed CSV has no usable column names (e.g. SIMULATE + nominal).
    ind <- (model@average_imp |> names() |> tolower()) == tolower(mod)
    if (sum(ind) != 1) {
        ind <- (model@average_imp |> names() |> tolower()) == tolower(paste0(mod, ".latent"))
    }
    if (sum(ind) != 1) {
        cli::cli_alert_warning(c(
            "Cannot locate moderator {.field {mod}} in imputed data; ",
            "falling back to a default range of (-3, 3)."
        ))
        mod_data <- NULL
        mu <- 0
        m_range <- c(-3, 3)
    } else {
        mod_data <- model@average_imp[, ind]
        mu <- if (mod_is_cent) mean(mod_data) else 0.0
        m_range <- (mod_data - mu) |> pretty() |> range()
    }

    # Try SIMPLE-based path: requires SIMPLE rows matching outcome/focal/mod
    simple_groups <- if (NROW(model@simple) > 0) {
        parse_simple_groups(model, out, pre, mod, at_filter = at_filter)
    } else NULL

    if (!is.null(simple_groups) && length(extra_formula_mods) > 0) {
        # Sanity check: any underlying SIMPLE moderator referenced by the
        # formula's extra terms (including `join(...)` components) must exist
        # in the SIMPLE output.
        sim_mods   <- attr(simple_groups, "all_mods")
        components <- unique(unlist(mod_components[extra_formula_mods]))
        missing    <- components[!(tolower(components) %in% tolower(sim_mods))]
        if (length(missing) > 0) throw_error(c(
            "Moderators listed in the formula are not in the SIMPLE output: {missing}",
            i = "Available moderators in SIMPLE: {sim_mods}"
        ))
    }

    if (!is.null(simple_groups)) {
        # SIMPLE-based: build a compute_condeff per group via per-draw OLS
        ce_list <- lapply(simple_groups, function(g) {
            x_points <- vapply(
                g$varying_vals, mod_label_to_numeric, double(1),
                mod_data = mod_data, iterations = model@iterations, mu = mu
            )
            keep <- !is.na(x_points)
            bad  <- g$varying_vals[!keep]
            # Distinguish expression-valued points (already warned in
            # mod_label_to_numeric) from genuinely uninterpretable labels.
            unknown <- bad[!grepl("^\\(.*\\)(?:\\s+sd)?$", trimws(bad), perl = TRUE)]
            if (length(unknown) > 0) throw_error(c(
                "Could not interpret SIMPLE moderator values for {.field {mod}}: {unknown}",
                i = "Expected quantile (Q25), SD (`+1 SD`), numeric, or a parameter name."
            ))
            x_points <- x_points[keep]
            slope_mat <- as.matrix(g$slope_draws)[, keep, drop = FALSE]
            if (length(x_points) == 0) throw_error(c(
                "All SIMPLE points for {.field {mod}} were expression-valued ({.code (expr)} / {.code (expr) sd}).",
                i = "Numeric placement on the JN axis is not yet supported for these.",
                i = "Re-run SIMPLE with quantile, SD, or numeric anchors to enable {.fn jn_plot}."
            ))
            if (length(unique(x_points)) < 2) throw_error(c(
                "Need at least 2 distinct {.field {mod}} values in SIMPLE to build a JN plot.",
                i = "Use {.code @ quantile}, {.code @ sd}, or supply multiple values."
            ))
            X    <- cbind(1, x_points)
            proj <- X %*% solve(crossprod(X))           # K x 2
            beta <- slope_mat %*% proj                  # T x 2
            compute_condeff(beta[, 1], beta[, 2])
        })
        names(ce_list) <- vapply(simple_groups, `[[`, character(1), "label")
    } else {
        # Legacy fallback: build a single compute_condeff from model@estimates
        pnames <- model@estimates |> row.names() |> tolower()
        bx_sel <- which(pnames == tolower(paste0(out, " ~ ", pre)))
        bxm_s1 <- which(pnames == tolower(paste0(out, " ~ ", pre, "*", mod)))
        bxm_s2 <- which(pnames == tolower(paste0(out, " ~ ", mod, "*", pre)))

        if (length(bx_sel) == 0) {
            pname <- paste0(out, " ~ ", pre)
            throw_error(c(
                x = "Cannot find required parameter `{pname}`",
                i = "Ensure that it is one of the row names in `summary(model)`"
            ))
        }
        if (length(bxm_s1) == 0 && length(bxm_s2) == 0) {
            pname <- paste0(out, " ~ ", pre, "*", mod)
            throw_error(c(
                x = "Cannot find required parameter `{pname}`",
                i = "Ensure that it is one of the row names in `summary(model)`"
            ))
        }
        if ((length(bxm_s1) == 1 && length(bxm_s2) == 1) ||
            length(bxm_s1) > 1 ||  length(bxm_s2) > 1) {
            throw_error(c(
                i = "Multiple interactions were found.",
                x = "Cannot parse model"
            ))
        }
        iter <- model |> as.matrix()
        ce_list <- list(compute_condeff(iter[, bx_sel], iter[, c(bxm_s1, bxm_s2)]))
        names(ce_list) <- ""
    }

    # Probability quantiles
    ci_p  <- (1 - ci) / 2
    probs <- c(ci_p, 0.5, 1 - ci_p)

    # Logical facet moderators (one column per logical mod). When the user
    # listed mods after the varying one in the formula -- including any
    # `join(...)` compounds -- those drive the facet layout. Otherwise we
    # fall back to whatever SIMPLE moderators were auto-detected.
    auto_facet_mods <- if (!is.null(simple_groups)) attr(simple_groups, "facet_mods")
                       else character(0)
    if (length(extra_formula_mods) > 0) {
        facet_mods <- extra_formula_mods
    } else {
        facet_mods <- auto_facet_mods
        for (af in auto_facet_mods) {
            if (is.null(mod_components[[af]])) mod_components[[af]] <- af
        }
    }

    # Drop facet mods whose value is constant across every SIMPLE panel --
    # a 1-tile strip just repeats info already in the subtitle's "Held
    # constant" line. Applies whether the mod came from the formula or
    # from auto-detection.
    held_constant_text <- character(0)
    if (length(facet_mods) > 0 && !is.null(simple_groups)) {
        n_panels <- length(simple_groups)
        per_mod_labels <- lapply(facet_mods, function(lfm) {
            comps <- mod_components[[lfm]]
            vapply(seq_len(n_panels), function(i) {
                mv <- simple_groups[[i]]$mod_vals
                paste(vapply(comps, function(c) paste(c, '@', mv[[c]]),
                             character(1)), collapse = ", ")
            }, character(1))
        })
        varies <- vapply(per_mod_labels, function(v) length(unique(v)) > 1,
                         logical(1))
        held_constant_text <- vapply(which(!varies),
                                     function(i) per_mod_labels[[i]][1],
                                     character(1))
        facet_mods <- facet_mods[varies]
    }

    # Build JN data per panel. Each panel gets the formatted "name @ value"
    # for every logical facet moderator -- compound (`join()`) moderators
    # combine their components into one column ("m2 @ X, m3 @ Y").
    m_grid <- seq(m_range[1], m_range[2], length.out = 1000)
    jn_data <- do.call(rbind, lapply(seq_along(ce_list), function(i) {
        ce <- ce_list[[i]]
        q  <- ce(m_grid, quantile, probs = probs)   # 3 x N
        df <- data.frame(
            facet  = names(ce_list)[i],
            m      = m_grid,
            lower  = q[1, ],
            median = q[2, ],
            upper  = q[3, ],
            sig    = (q[1, ] * q[3, ]) > 0
        )
        if (!is.null(simple_groups)) {
            mv <- simple_groups[[i]]$mod_vals
            for (lfm in facet_mods) {
                comps <- mod_components[[lfm]]
                vals  <- vapply(comps, function(c) paste(c, '@', mv[[c]]),
                                character(1))
                df[[lfm]] <- paste(vals, collapse = ", ")
            }
        }
        df
    }))

    # Run-length grouping per facet for ribbon fill consistency. `set_group()`
    # (defined at the top of this file) does the rle-based numbering; we run
    # it within each facet and then prefix with the facet label so the group
    # ids are unique across facets.
    jn_data$grp <- unlist(lapply(split(as.integer(jn_data$sig), jn_data$facet),
                                 set_group))
    jn_data$grp <- paste(jn_data$facet, jn_data$grp, sep = "/")

    # Boundaries per facet (where sig flips). We carry every facet moderator
    # column along so `geom_segment` can match segments to panels under
    # `facet_grid` -- otherwise ggplot can't tell which segment goes where
    # and renders them in every panel.
    bounds_df <- do.call(rbind, lapply(unique(jn_data$facet), function(fct) {
        d <- jn_data[jn_data$facet == fct, ]
        trans <- which(diff(d$sig) != 0)
        if (length(trans) == 0) return(NULL)
        row <- data.frame(
            facet = fct,
            x = (d$m[trans] + d$m[trans + 1]) / 2,
            ymin = (d$lower[trans] + d$lower[trans + 1]) / 2,
            ymax = (d$upper[trans] + d$upper[trans + 1]) / 2
        )
        for (fm in facet_mods) row[[fm]] <- d[[fm]][1]
        row
    }))

    bound_text_for_facet <- function(fct) {
        b <- if (!is.null(bounds_df)) bounds_df$x[bounds_df$facet == fct] else numeric(0)
        if (length(b) == 0) return("")
        head_word <- if (length(b) == 1) "Bound: " else "Bounds: "
        paste0("\n", head_word, paste(sprintf("%.3g", b), collapse = ", "))
    }

    # When there is exactly one facet moderator, the bound is unambiguous per
    # panel -- bake it straight into that mod column so any user override of
    # the facet keeps the bound text in the strip. Mirror the same baking
    # into `bounds_df` so `geom_segment` still matches the right panel.
    if (length(facet_mods) == 1) {
        fm <- facet_mods[1]
        bnd_text_per_row <- vapply(jn_data$facet, bound_text_for_facet,
                                   character(1))
        jn_data[[fm]] <- paste0(jn_data[[fm]], bnd_text_per_row)
        if (!is.null(bounds_df) && nrow(bounds_df) > 0) {
            bounds_df[[fm]] <- paste0(bounds_df[[fm]],
                vapply(bounds_df$facet, bound_text_for_facet, character(1)))
        }
    }

    # If you want bounds annotated inside each strip when more than one
    # moderator drives the facet, wrap them with `join(...)` in the formula
    # -- e.g. `y ~ x | m1 + join(m2, m3)`. That collapses the two extras
    # into a single logical facet whose values are unique per panel, which
    # lets the bound text be baked into the strip just like the 1-facet
    # case.

    # Order facet panels by the underlying numeric value of each moderator
    # (so "-1 SD", "0", "+1 SD" appear in that order instead of whatever
    # order Blimp emitted them in).
    for (lfm in facet_mods) {
        if (is.null(jn_data[[lfm]])) next
        lvls <- unique(jn_data[[lfm]])
        ord  <- order_by_mod_value(lvls)
        jn_data[[lfm]] <- factor(jn_data[[lfm]], levels = lvls[ord])
        if (!is.null(bounds_df) && !is.null(bounds_df[[lfm]])) {
            bounds_df[[lfm]] <- factor(bounds_df[[lfm]],
                                       levels = lvls[ord])
        }
    }

    # Suppress NSE NOTEs from the column names referenced inside `aes()` below.
    m <- lower <- upper <- median <- sig <- grp <- x <- ymin <- ymax <- facet <- NULL

    p <- (
        ggplot(jn_data, aes(x = m))
        + geom_hline(yintercept = 0)
        + geom_ribbon(aes(ymin = lower, ymax = upper, fill = sig, group = grp), alpha = 0.25)
        + geom_line(aes(y = median))
        + geom_line(aes(y = lower), linetype = 'dashed', color = 'black')
        + geom_line(aes(y = upper), linetype = 'dashed', color = 'black')
    )
    if (!is.null(bounds_df) && nrow(bounds_df) > 0) {
        p <- p + geom_segment(
            data = bounds_df,
            aes(x = x, xend = x, y = ymin, yend = ymax),
            color = 'black', alpha = 0.5,
            inherit.aes = FALSE
        )
    }
    # Always default to facet_grid. Layout:
    #   1 mod  -> .       ~ mod      (row of panels)
    #   2 mods -> mod1    ~ mod2     (2D grid)
    #   3+     -> mod1    ~ mod2 + ... (rest grouped on columns)
    if (length(facet_mods) > 0) {
        terms <- paste0("`", facet_mods, "`")
        lhs <- if (length(facet_mods) == 1) "." else terms[1]
        rhs <- if (length(facet_mods) == 1) terms
               else paste(terms[-1], collapse = " + ")
        facet_formula <- stats::as.formula(paste(lhs, "~", rhs))
        p <- p + facet_grid(facet_formula)
    }

    # Subtitle: held-constant moderators, plus the boundary values when the
    # plot has no facet strips (the strips already carry bound text for
    # multi-panel plots, since each moderator column is pre-baked).
    base_subtitle <- paste0("Red area represents 0 within ", round(ci * 100), "% interval")
    held_line <- if (length(ce_list) <= 1) {
        lbl <- names(ce_list)[1]
        if (!is.null(lbl) && nzchar(lbl)) paste0("\nHeld constant: ", lbl) else ""
    } else if (length(held_constant_text) > 0) {
        paste0("\nHeld constant: ", paste(held_constant_text, collapse = ", "))
    } else ""

    # Bounds in subtitle only when there are no facet strips at all.
    # With 2+ facet moderators the flat list is unhelpful (the user can't
    # tell which panel each bound belongs to); they should either use the
    # exposed `panel` column via `+ facet_wrap(~ panel)` to put the bound
    # in each strip, or read `attr(p, "bounds")` directly.
    bnd_line <- if (length(facet_mods) == 0 &&
                    !is.null(bounds_df) && nrow(bounds_df) > 0) {
        head_word <- if (nrow(bounds_df) == 1) "Bound: " else "Bounds: "
        paste0("\n", head_word, paste(sprintf("%.3g", bounds_df$x), collapse = ", "))
    } else ""

    subtitle <- paste0(base_subtitle, held_line, bnd_line)

    p <- (
        p
        + xlim(m_range)
        + guides(fill = "none")
        + labs(
            title = "Johnson-Neyman Plot of Conditional Slope",
            subtitle = subtitle,
            y = paste(out, "~", if (pre_is_cent) paste("Centered", pre) else pre),
            x = if (mod_is_cent) paste("Centered", mod) else mod
        )
    )

    bounds_value <- if (!is.null(bounds_df) && nrow(bounds_df) > 0) bounds_df
                    else data.frame()
    structure(p, bounds = bounds_value)
}

#' Internal: parse SIMPLE output and group rows by held-constant moderator values.
#' If `at_filter` is supplied (a named list of moderator -> allowed value(s)), the
#' matched SIMPLE rows are restricted to those satisfying every filter entry.
#' @noRd
parse_simple_groups <- function(model, out, pre, mod, at_filter = list()) {
    simple <- model@simple
    simple_names <- names(simple)
    if (!all(grepl('(SLOPE|INTER): ', simple_names))) return(NULL)
    names(simple) <- gsub('(SLOPE|INTER): ', '', simple_names)
    slope <- simple[, startsWith(simple_names, 'SLOPE:'), drop = FALSE]
    n <- names(slope)

    parsed <- parse_simple_colnames(n)

    keep <- vapply(parsed, function(p) {
        length(p$outcome) == 1 && length(p$predictor) == 1 &&
            is_equal(p$outcome, out) && is_equal(p$predictor, pre) &&
            (tolower(mod) %in% tolower(p$mods))
    }, logical(1))

    # Apply at() filter: each named moderator must take an allowed value.
    if (length(at_filter) > 0) {
        all_simple_mods <- unique(unlist(lapply(parsed, `[[`, "mods")))
        bad <- setdiff(tolower(names(at_filter)), tolower(all_simple_mods))
        if (length(bad) > 0) throw_error(c(
            "Moderators inside {.fn at} are not in the SIMPLE output: {bad}",
            i = "Available SIMPLE moderators: {all_simple_mods}"
        ))
        keep <- keep & vapply(parsed, function(p) {
            for (nm in names(at_filter)) {
                idx <- which(tolower(p$mods) == tolower(nm))[1]
                if (is.na(idx)) return(FALSE)
                if (!(p$vals[idx] %in% at_filter[[nm]])) return(FALSE)
            }
            TRUE
        }, logical(1))
    }

    if (!any(keep)) return(NULL)

    sel_cols   <- which(keep)
    sel_parsed <- parsed[sel_cols]

    # Identify extra (held-constant) moderators
    first_mods <- sel_parsed[[1]]$mods
    extra_mods <- first_mods[!(tolower(first_mods) %in% tolower(mod))]

    # Build a key per row based on extra moderator values; group rows by key
    keys <- vapply(sel_parsed, function(p) {
        if (length(extra_mods) == 0) return("")
        vals <- vapply(extra_mods, function(em) {
            idx <- which(tolower(p$mods) == tolower(em))[1]
            p$vals[idx]
        }, character(1))
        paste(paste(extra_mods, '@', vals), collapse = ", ")
    }, character(1))

    grouped <- split(seq_along(sel_cols), keys)
    out_list <- lapply(seq_along(grouped), function(gi) {
        idx  <- grouped[[gi]]
        cols <- sel_cols[idx]
        ps   <- sel_parsed[idx]
        varying_vals <- vapply(ps, function(p) {
            i <- which(tolower(p$mods) == tolower(mod))[1]
            p$vals[i]
        }, character(1))
        # Per-extra-moderator value for this panel (constant within the group)
        mod_vals <- if (length(extra_mods) == 0) {
            setNames(character(0), character(0))
        } else {
            p <- ps[[1]]
            setNames(vapply(extra_mods, function(em) {
                i <- which(tolower(p$mods) == tolower(em))[1]
                p$vals[i]
            }, character(1)), extra_mods)
        }
        list(
            label        = names(grouped)[gi],
            varying_vals = varying_vals,
            mod_vals     = mod_vals,
            slope_draws  = slope[, cols, drop = FALSE]
        )
    })
    attr(out_list, "all_mods")   <- unique(unlist(lapply(sel_parsed, `[[`, "mods")))
    attr(out_list, "facet_mods") <- extra_mods
    out_list
}

