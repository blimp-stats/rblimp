# Copyright Brian Keller 2026, all rights reserved

#' Function to generate conditional regression equation plots (i.e., simple effects) with [`rblimp`] and SIMPLE command
#' @description
#' Generates a conditional effect plots based on the posterior summaries from the output of [`rblimp`].
#' @param formula an object of class [`formula`] to specify simple effect to plot.
#' The formula must have the following form: `outcome ~ focal | moderator`. See Details below for
#' nominal moderators and for SIMPLE statements that include more than one moderator.
#' @param model an [`blimp_obj`]. The model must have a SIMPLE command output saved.
#' @param ci a value between 0 and 1 specifying the credible interval size
#' @param xvals a list of values to evaluate for the focal variable. If empty, they will automatically be determined
#' @param ... arguments passed to the internal [`ggplot2::geom_line`] call used to generate the median lines.
#' @returns a [`ggplot2::ggplot`] plot
#' @details
#' To change colors use ggplot2's scale system. Both fill and color are used. See
#' [`ggplot2::aes_colour_fill_alpha`] for more information about setting a manual set of colors.
#'
#' For nominal moderators, wrap the dummy codes in `join(...)` so they are
#' treated as a single compound moderator (one set of colored lines instead of
#' separate facets):
#'   \deqn{\code{focal | join(moderator.1, moderator.2)}}
#' `join()` works for any user-controlled bundling, not just nominal dummies;
#' however it is only valid in the first (color/legend) position of the formula.
#'
#' When the SIMPLE command contains more than one moderator (e.g.,
#' `mod1 @ values and mod2 @ value`), the right-hand side of the formula may list the
#' moderators that vary using `+` (e.g., `focal | mod1 + mod2`). Moderators that are
#' omitted from the formula are treated as held-constant context: their value must
#' be the same across the matched simple effects and is reported in the plot
#' subtitle.
#' @seealso [`jn_plot`], [`jn_map`], [`at`], [`join`]
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
#'     mydata,
#'     center = ~ m,
#'     simple = 'x | m',
#'     seed = 10972,
#'     burn = 1000,
#'     iter = 1000
#' )
#' simple_plot(y ~ x | m, m1)
#'
#' # ---- Two moderators: first colored, second auto-faceted ----
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
#' simple_plot(y ~ x | m1 + m2, fit)
#'
#' # Pin a moderator to one SIMPLE value via `at()`
#' simple_plot(y ~ x | m1 + at(m2 = "0"), fit)
#'
#' # Restrict to a subset of values
#' simple_plot(y ~ x | m1 + at(m2 = c("-1 SD", "+1 SD")), fit)
#'
#' # Bundle moderators (e.g. nominal dummy codes) via `join()`
#' simple_plot(y ~ x | join(m1, m2), fit)
#' }
#' @import ggplot2
#' @importFrom methods is
#' @export
simple_plot <- function(formula, model, ci = 0.95, xvals, ...) {

    # Check inputs
    if (ci >= 1.0 | ci <= 0.0) throw_error(
        "The {.arg ci} must be between 0 and 1"
    )
    if (!is(model, 'blimp_obj')) throw_error(
        "{.arg model} is not a `blimp_obj`"
    )
    if (NROW(model@simple) == 0) throw_error(c(
        "No SIMPLE command was specified.",
        "i" = "Specify {.arg simple} when running {.cli rblimp}."
    ))

    # Parse formula via language tree: extracts outcome, focal, bare moderators
    # (color + facet), held-constant filter (`at`), and compound bundles (`join`).
    pf <- parse_plot_formula(formula)
    out            <- pf$outcome
    pre            <- pf$focal
    formula_mods   <- pf$bare_mods            # logical mod display names
    mod_components <- pf$mod_components       # display name -> SIMPLE mod components
    at_filter      <- pf$at_filter

    if (length(formula_mods) < 1) throw_error(c(
        "The {.arg formula} must specify at least one bare moderator after `|`.",
        "Must have the form: `outcome ~ focal | moderator [+ ...]`"
    ))

    color_mod  <- formula_mods[1]
    facet_mods <- formula_mods[-1]

    # Extract simple slopes
    simple <- model@simple
    simple_names <- names(simple)

    # Check if blimp is supported
    if ((grepl('(SLOPE|INTER): ', simple_names) |> all()) == FALSE) throw_error(
        "The Blimp version used is unsupported. Update Blimp!"
    )

    # Remove slope and intercept prefixes
    names(simple) <- gsub('(SLOPE|INTER): ', '', simple_names)

    # Split into slope and intercept (drop = FALSE in case of a single column)
    slope <- simple[, startsWith(simple_names, 'SLOPE:'), drop = FALSE]
    icept <- simple[, startsWith(simple_names, 'INTER:'), drop = FALSE]

    # Parse each slope column name into structured pieces.
    n <- names(slope)
    parsed <- parse_simple_colnames(n)

    # Auto-expand any bare moderator that isn't directly in SIMPLE but is
    # declared as `nominal=` on the model -- treat its dummy codes as one
    # compound moderator (equivalent to wrapping in `join(...)`).
    all_simple_mods <- unique(unlist(lapply(parsed, `[[`, "mods")))
    for (bm in names(mod_components)) {
        comps_in  <- mod_components[[bm]]
        comps_out <- unique(unlist(lapply(comps_in, resolve_mod_name,
                                          simple_mods = all_simple_mods,
                                          model = model)))
        mod_components[[bm]] <- comps_out
    }

    # Helper: extract the joined value string for a logical moderator in a SIMPLE row.
    # For a plain moderator this is just the raw label; for a compound (join'd
    # or nominal-expanded) moderator it joins "name @ value, name @ value".
    get_mod_value <- function(p, bare_mod) {
        comps <- mod_components[[bare_mod]]
        idx   <- vapply(comps, function(c)
                        which(tolower(p$mods) == tolower(c))[1], integer(1))
        if (any(is.na(idx))) return(NA_character_)
        vals <- p$vals[idx]
        if (length(comps) == 1) return(vals[1])
        paste(paste(comps, '@', vals), collapse = ', ')
    }

    # All SIMPLE moderator names that must be present in each matched row.
    required_components <- unique(unlist(mod_components))

    # Filter: outcome matches, predictor matches, every required component present
    keep <- vapply(parsed, function(p) {
        length(p$outcome) == 1 && length(p$predictor) == 1 &&
            is_equal(p$outcome, out) && is_equal(p$predictor, pre) &&
            all(tolower(required_components) %in% tolower(p$mods))
    }, logical(1))

    # Apply `at()` filter: each named moderator must take one of the specified values
    if (length(at_filter) > 0) {
        all_mods_in_simple <- unique(unlist(lapply(parsed, `[[`, "mods")))
        bad <- setdiff(tolower(names(at_filter)),
                       tolower(all_mods_in_simple))
        if (length(bad) > 0) throw_error(c(
            "Moderators inside {.fn at} are not in the SIMPLE output: {bad}",
            i = "Available SIMPLE moderators: {all_mods_in_simple}"
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

    if (!any(keep)) {
        mod_list <- unique(vapply(parsed, function(p) paste(p$mods, collapse = ', '), character(1)))
        throw_error(c(
            "Unable to select out conditional effects",
            i = "If the moderator is nominal, include dummy code suffix.",
            i = "Otherwise the simple command doesn't exist for the moderator and outcome.",
            i = "List of moderators: { mod_list }"
        ))
    }

    sel_cols <- which(keep)
    sel_parsed <- parsed[sel_cols]

    # Of the moderators present in matched SIMPLE rows, classify each by how
    # many distinct values it actually takes (after `at()` filtering):
    #   - the color moderator (formula_mods[1])           -> drives line color
    #   - any other mod with >1 value                     -> becomes a facet
    #   - any other mod with exactly 1 value              -> held constant
    # This mirrors jn_plot: a multi-valued `at()` subset still facets.
    first_mods <- sel_parsed[[1]]$mods

    # "Extra" SIMPLE mods are those not part of any logical formula moderator.
    color_components <- mod_components[[color_mod]]
    used_components  <- unique(unlist(mod_components))
    extra_mods <- first_mods[!(tolower(first_mods) %in% tolower(used_components))]

    auto_facets    <- character()
    extra_constant <- list()
    # Classify formula-listed facet mods first (these are plain names by now).
    for (em in facet_mods) {
        vals_for_em <- vapply(sel_parsed, get_mod_value, character(1),
                              bare_mod = em)
        if (length(unique(vals_for_em)) > 1) {
            auto_facets <- c(auto_facets, em)
        } else {
            extra_constant[[em]] <- vals_for_em[1]
        }
    }
    # Then classify any extra (unmentioned) SIMPLE mods.
    for (em in extra_mods) {
        vals_for_em <- vapply(sel_parsed, function(p) {
            idx <- which(tolower(p$mods) == tolower(em))[1]
            p$vals[idx]
        }, character(1))
        if (length(unique(vals_for_em)) > 1) {
            auto_facets <- c(auto_facets, em)
            # Register it in `mod_components` so `get_mod_value` resolves it.
            mod_components[[em]] <- em
        } else {
            extra_constant[[em]] <- vals_for_em[1]
        }
    }
    facet_mods <- unique(auto_facets)

    # Mods that drive the plot: color + facets (whatever they ended up being).
    # The combo_label is the per-row grouping key for prediction; it must
    # distinguish every unique (color, facet) combination.
    plot_mods <- unique(c(color_mod, facet_mods))

    # Per-SIMPLE-mod columns: every underlying component plus auto-detected
    # facet mods (mods promoted to facet from `extra_mods` aren't in
    # `mod_components`) and any held-constant mods. Compound display names are
    # added separately below.
    raw_data_mods <- unique(c(unlist(mod_components),
                              facet_mods[vapply(facet_mods,
                                  function(fm) is.null(mod_components[[fm]]),
                                  logical(1))],
                              names(extra_constant)))

    # Build simple_data. Each row has:
    #   - intercept, slope (per MCMC draw)
    #   - combo_label / color_label (text labels for grouping & legend)
    #   - one column per individual SIMPLE moderator (raw label, for filtering)
    #   - one column per logical bare moderator that is COMPOUND (combined
    #     "name @ value, name @ value" label, for facet aesthetics)
    simple_data <- do.call(rbind, lapply(seq_along(sel_cols), function(i) {
        p   <- sel_parsed[[i]]
        col <- sel_cols[i]
        plot_vals <- vapply(plot_mods, get_mod_value, character(1), p = p)
        names(plot_vals) <- plot_mods
        raw_vals <- vapply(raw_data_mods, function(fm) {
            idx <- which(tolower(p$mods) == tolower(fm))[1]
            p$vals[idx]
        }, character(1))
        names(raw_vals) <- raw_data_mods

        combo_label <- unname(paste(paste(plot_mods, '@', unname(plot_vals)),
                                    collapse = ', '))
        color_label <- if (length(color_components) == 1) {
            unname(paste('@', plot_vals[[color_mod]]))
        } else {
            unname(plot_vals[[color_mod]])
        }
        row <- data.frame(
            b0          = icept[, col],
            b1          = slope[, col],
            combo_label = combo_label,
            color_label = color_label
        )
        for (fm in raw_data_mods) row[[fm]] <- raw_vals[fm]
        # Add a synthetic column for any compound bare moderator (so the
        # auto-facet can reference it via the display name).
        for (bm in plot_mods) {
            if (length(mod_components[[bm]]) > 1) row[[bm]] <- plot_vals[bm]
        }
        row
    }))

    # Preserve the order in which the simple statements appear in the data
    simple_data$mf <- factor(simple_data$combo_label, levels = unique(simple_data$combo_label))

    # Centering checks
    centered_vars <- if (is.null(model@syntax$center)) character(0) else {
        model@syntax$center |> strsplit(' ') |> unlist() |> tolower() |>
            gsub(';', '', x = _)
    }
    is_centered <- function(name) {
        (tolower(name) |> sub("\\s*\\[[^]]*\\]$", "", x = _)) %in% centered_vars
    }
    pre_is_cent <- is_centered(pre)
    mod_cent    <- vapply(plot_mods, is_centered, logical(1))

    ## Generate predicted scores

    # Handle xvals
    if (missing(xvals)) {
        ind <- is_equal(model@average_imp |> names(), pre)
        if (sum(ind) != 1) {
            ind <- is_equal(model@average_imp |> names(), paste0(pre, '.latent'))
        }
        if (sum(ind) != 1) {
            # Some Blimp configurations (e.g. SIMULATE with nominal predictors)
            # emit imputation CSVs without a header row, so the column names
            # are unusable. Fall back to a sensible default range with a
            # warning -- the user can still override via `xvals`.
            cli::cli_alert_warning(c(
                "Cannot locate focal predictor {.field {pre}} in imputed data; ",
                "falling back to xvals = c(-3, 3). ",
                "Pass {.arg xvals} explicitly to override."
            ))
            xvals <- seq(-3, 3, length.out = 100)
        } else {
            mu <- if (pre_is_cent) mean(model@average_imp[, ind]) else 0.0
            l  <- (model@average_imp[, ind] - mu) |> pretty() |> range()
            xvals <- seq(l[1], l[2], length.out = 100)
        }
    } else if (length(xvals) == 2) {
        xvals <- seq(xvals[1], xvals[2], length.out = 100)
    }

    # Probabilities for quantiles
    ci_p  <- (1 - ci) / 2
    probs <- c(ci_p, 0.5, 1 - ci_p)

    pred_score <- \(d) d[1] + d[2] * xvals
    pred <- lapply(split(simple_data[, c("b0", "b1")], simple_data$mf),
                   \(x) apply(x, 1, pred_score))
    quan <- lapply(pred, \(x) apply(x, 1, quantile, p = probs))

    # Look up moderator values for each combo label (one representative row per combo)
    combo_lookup <- simple_data[!duplicated(simple_data$combo_label), ,
                                drop = FALSE]
    rownames(combo_lookup) <- combo_lookup$combo_label

    rib_data <- do.call('rbind', lapply(names(quan), \(x) {
        ref <- combo_lookup[x, , drop = FALSE]
        df <- data.frame(
            l        = quan[[x]][1, ],
            outcome  = quan[[x]][2, ],
            h        = quan[[x]][3, ],
            focal    = xvals,
            m        = x,
            panel    = x,
            color    = ref$color_label
        )
        # Each moderator column carries its pre-formatted "name @ value"
        # label so `+ facet_wrap(~ <mod>, ncol = 1)` produces the same
        # strip text the default labeller would. Filtering is done via
        # `at()` in the formula -- no need to inspect the data directly.
        for (fm in raw_data_mods) df[[fm]] <- paste(fm, '@', ref[[fm]])
        for (bm in plot_mods) {
            if (length(mod_components[[bm]]) > 1) df[[bm]] <- ref[[bm]]
        }
        df
    }))

    # Order legend / facet panels by the underlying numeric value rather than
    # Blimp's emission order (so "-1 SD", "0", "+1 SD" appear in that order
    # instead of "+1 SD, -1 SD, 0").
    color_levels <- unique(simple_data$color_label)
    color_levels <- color_levels[order_by_mod_value(color_levels)]
    rib_data$moderator <- factor(rib_data$color, levels = color_levels)

    for (fm in unique(c(facet_mods, names(extra_constant)))) {
        if (is.null(rib_data[[fm]])) next
        lvls <- unique(rib_data[[fm]])
        rib_data[[fm]] <- factor(rib_data[[fm]],
                                 levels = lvls[order_by_mod_value(lvls)])
    }

    # Subtitle: centering + any held-constant extra moderators
    subtitle_parts <- character(0)
    if (pre_is_cent || any(mod_cent)) {
        cent_list <- c(if (pre_is_cent) pre else NULL, plot_mods[mod_cent])
        subtitle_parts <- c(
            subtitle_parts,
            paste0('Centered variables: ', paste(cent_list, collapse = ', '))
        )
    }
    if (length(extra_constant) > 0) {
        ctx <- paste(paste(names(extra_constant), '@', unlist(extra_constant)), collapse = ', ')
        subtitle_parts <- c(subtitle_parts, paste0('Held constant: ', ctx))
    }
    subtitle <- if (length(subtitle_parts) == 0) deparse(formula) else paste(subtitle_parts, collapse = '\n')

    # Suppress R CMD check NOTEs about ggplot2 NSE
    focal <- moderator <- h <- outcome <- NULL
    ## Make Conditional Effects Plot
    p <- (
        ggplot2::ggplot(rib_data, ggplot2::aes(focal, color = moderator, fill = moderator))
        + ggplot2::geom_ribbon(ggplot2::aes(ymin = l, ymax = h), color = NA, alpha = 0.2)
        + ggplot2::geom_line(ggplot2::aes(y = outcome), ...)
        + ggplot2::labs(
            title    = 'Plot of Conditional Regressions',
            subtitle = subtitle,
            y        = out,
            x        = pre,
            color    = color_mod,
            fill     = color_mod
        )
    )

    # Auto-facet by any moderators beyond the color one. We always default to
    # `facet_grid` (predictable layout, no automatic wrapping) using the layout:
    #   1 mod  -> .       ~ mod       (row of panels)
    #   2 mods -> mod1    ~ mod2      (2D grid)
    #   3+     -> mod1    ~ mod2 + ... (rest grouped on columns)
    # Column values are pre-formatted "name @ value", so user overrides like
    # `+ facet_grid(m2 ~ .)` keep the strip text without a custom labeller.
    if (length(facet_mods) > 0) {
        terms <- paste0("`", facet_mods, "`")
        lhs <- if (length(facet_mods) == 1) "." else terms[1]
        rhs <- if (length(facet_mods) == 1) terms
               else paste(terms[-1], collapse = " + ")
        facet_formula <- stats::as.formula(paste(lhs, "~", rhs))
        p <- p + ggplot2::facet_grid(facet_formula)
    }

    p
}
