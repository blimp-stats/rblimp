## Shared internals for the diagnostic plots
# Copyright Brian Keller 2026, all rights reserved

# Design notes ================================================================
#
# These helpers back `distribution_plot()`, `qq_plot()`, `residual_plot()`,
# `influence_plot()`, `probability_plot()` and the two "check everything"
# functions. Statistical rules shared by all of them:
#
#   * Every quantity is computed WITHIN each imputed data set and then pooled;
#     nothing is computed on an average of the imputations, because averaging an
#     imputed predictor and the residual together induces a spurious association.
#       - point clouds stack all M imputations (alpha scaled by M^(-1/3));
#       - smoothers and binned summaries are fit within each imputation and the
#         M results are averaged;
#       - leverage and Cook's D come from Blimp (`influence = TRUE`), averaged
#         over every post burn-in iteration.
#   * One pooled residual scale per outcome: sqrt of the average of the
#     per-imputation residual variances, or the per-case model-implied SD when a
#     variance model is fit.
#   * No uncertainty bands, except the opt-in `se = TRUE` loess bands in
#     `residual_plot()`, pooled with Rubin's rules.
#
# Model information comes from Blimp, never from re-parsing the MODEL syntax:
# outcomes and model-implied quantities from the imputation columns, and each
# outcome's terms from the `<outcome> ~ <term>` rows of `@estimates`. The syntax
# is read only for declared variable types, `clusterid`, the clusterid LATENT
# line, and group-mean centering.
#
# Blimp's naming of saved columns:
#   level-1  <dv>.residual  <dv>.predicted  <dv>.probability  <dv>.<k>.probability
#            <dv>.<k>.residual (nominal)  <var>.latent  <var>.yjt  var.<dv>.predicted
#   level-2  <dv>[cluster]      the DV's random-intercept residual
#            <x>.mean[cluster]  a latent group mean
#            <dv>$<x>[cluster]  a random-slope residual (not diagnosed)

utils::globalVariables(c(
    "x", "y", "z", "case", "cluster", "rank", "value", "type", "weight",
    "lower", "upper", "theoretical", "sample", "prob", "category", "predictor",
    "x_mean", "resid_mean", "pred_mean", "measure", "outcome_status", "label",
    "accent", "density", "fontsize", "pointsize", "linewidth", "family", "slope", "intercept",
    "yintercept"
))

# Fixed design constants ======================================================

# Size of the summary markers (category means) drawn over a point cloud
summary_point_size <- 3
# Point transparency when the theme does not set `blimp.point.alpha`; a stacked
# cloud of M imputations is drawn at this value / M^(1/3)
point_alpha_base <- 0.7
# Width of dashed reference lines (thresholds, cutoffs, Q-Q diagonal) as a fraction
# of the theme's line width; zero lines use the full width
ref_line_ratio <- 0.5
# Width of smooth curves as a multiple of the theme's line width
curve_line_ratio <- 2

# Model checks ================================================================

#' Internal function to check a model for the diagnostic plots
#' @noRd
check_diagnostic_model <- function(model) {
    if (!is(model, 'blimp_obj')) throw_error(
        "{.arg model} is not a `blimp_obj`"
    )
    if (length(model@imputations) == 0) throw_error(c(
        "{.arg model} does not contain imputations.",
        "i" = "Set {.arg nimps} to a positive number and rerun {.cli rblimp}."
    ))
    invisible(model)
}

#' Internal function to check a single name argument
#' @noRd
check_variable_arg <- function(variable, plural) {
    if (missing(variable) || is.null(variable)) throw_error(c(
        "{.arg variable} is required.",
        "i" = "To plot everything applicable, use {.fn {plural}}."
    ))
    if (!is.character(variable) || length(variable) != 1) throw_error(
        "{.arg variable} must be a single character string"
    )
    invisible(variable)
}

# Syntax information ==========================================================

#' Internal function to obtain declared ORDINAL, NOMINAL, or COUNT variables
#' Expands ranges such as `dep1:dep7`.
#' @noRd
diag_discrete_vars <- function(model, types = c("ordinal", "nominal", "count")) {
    sx <- model@syntax
    pieces <- unlist(lapply(types, \(t) if (t %in% names(sx)) as.character(sx[[t]])), use.names = FALSE)
    pieces <- pieces[!is.na(pieces)]
    if (!length(pieces)) return(character(0))
    toks <- unlist(strsplit(gsub("[;,]", " ", paste(pieces, collapse = " ")), "\\s+"), use.names = FALSE)
    toks <- toks[nzchar(toks)]
    expand_range <- function(tok) {
        m <- regmatches(tok, regexec("^([A-Za-z._]+)([0-9]+):\\1([0-9]+)$", tok))[[1]]
        if (length(m) != 4L) return(tok)
        paste0(m[2], seq.int(as.integer(m[3]), as.integer(m[4])))
    }
    unique(unlist(lapply(toks, expand_range), use.names = FALSE))
}

#' Internal function to obtain the cluster identifier(s) from the syntax
#' @noRd
diag_clusterids <- function(model) {
    cid <- model@syntax$clusterid
    if (is.null(cid)) return(character(0))
    toks <- unlist(strsplit(paste(as.character(cid), collapse = " "), "[[:space:];]+"), use.names = FALSE)
    toks[nzchar(toks)]
}

#' Internal function to obtain level-2 latent random effects
#' A latent is level-2 only when its LATENT declaration is grouped by a cluster
#' identifier (e.g. `person = b0j b1j`). Returns a named vector latent -> cluster id.
#' @noRd
diag_level2_latents <- function(model) {
    clusterids <- diag_clusterids(model)
    lat <- model@syntax$latent
    if (!length(clusterids) || is.null(lat)) return(character(0))
    pieces <- unlist(strsplit(paste(as.character(lat), collapse = "\n"), "[;\n]"), use.names = FALSE)
    out <- character(0)
    for (p in pieces) {
        if (!grepl("=", p, fixed = TRUE)) next
        lhs <- trimws(sub("=.*$", "", p))
        if (!lhs %in% clusterids) next
        vars <- unlist(strsplit(trimws(sub("^[^=]*=", "", p)), "\\s+"), use.names = FALSE)
        for (v in vars[nzchar(vars)]) out[v] <- lhs
    }
    out
}

#' Internal function to obtain predictors centered at their latent group mean
#' @noRd
diag_groupmean_vars <- function(model) {
    cen <- model@syntax$center
    if (is.null(cen)) return(character(0))
    lines <- sub("#.*$", "", unlist(strsplit(paste(as.character(cen), collapse = "\n"), "\n", fixed = TRUE)))
    out <- character(0)
    for (p in unlist(strsplit(paste(lines, collapse = " "), ";", fixed = TRUE))) {
        if (!grepl("=", p, fixed = TRUE)) next
        if (tolower(trimws(sub("=.*$", "", p))) != "groupmean") next
        vars <- unlist(strsplit(trimws(sub("^[^=]*=", "", p)), "\\s+"))
        out <- c(out, vars[nzchar(vars)])
    }
    unique(out)
}

# Fitted terms from @estimates ================================================

#' Internal function to map Blimp's outcome label to the base name of its saved columns
#' `yjt(y-6)` and `ln(y)` map to `y`; `var(y)` is the variance submodel of `y`.
#' @noRd
diag_outcome_base <- function(label) {
    first_ident <- function(s) regmatches(s, regexpr("[A-Za-z_][A-Za-z0-9_.]*(\\[[^]]*\\])?", s))
    v <- regmatches(label, regexec("^var\\((.+)\\)$", label))[[1]]
    if (length(v) == 2L) return(list(base = first_ident(v[2]), kind = "variance"))
    inner <- regmatches(label, regexec("^[A-Za-z_][A-Za-z0-9_]*\\((.+)\\)$", label))[[1]]
    if (length(inner) == 2L) return(list(base = first_ident(inner[2]), kind = "mean"))
    list(base = label, kind = "mean")
}

#' Internal function to obtain the fitted terms of every outcome
#' One row per `<outcome> ~ <term>` coefficient of a user-specified model block.
#' Standardized and odds-ratio re-expressions and Blimp's auto-generated
#' predictor models are dropped.
#' @noRd
diag_term_table <- function(model) {
    rn <- rownames(model@estimates)
    empty <- data.frame(base = character(), kind = character(), term = character())
    if (!length(rn)) return(empty)
    block <- attr(model@iterations, "block_name")
    if (is.null(block) || length(block) != length(rn)) block <- rep("model", length(rn))
    m <- regmatches(rn, regexec("^(.+?) ~ (.+)$", rn))
    is_coef <- lengths(m) == 3L
    if (!any(is_coef)) return(empty)
    out <- data.frame(
        outcome = vapply(m[is_coef], `[`, "", 2L),
        term = vapply(m[is_coef], `[`, "", 3L),
        block = block[is_coef]
    )
    out <- out[!startsWith(out$block, "#"), , drop = FALSE]
    out <- out[!grepl(" \\((standardized|odds ratio)\\)$", out$term), , drop = FALSE]
    ob <- lapply(out$outcome, diag_outcome_base)
    out$base <- vapply(ob, `[[`, "", "base")
    out$kind <- vapply(ob, `[[`, "", "kind")
    out[, c("base", "kind", "term")]
}

#' Internal function to parse a term label as an R expression
#' Bracketed names are backticked and Blimp's `ln(` becomes `log(`.
#' @noRd
diag_term_expr <- function(term) {
    s <- gsub("([A-Za-z_][A-Za-z0-9_.]*\\[[^]]*\\])", "`\\1`", term)
    s <- gsub("\\bln\\(", "log(", s)
    tryCatch(parse(text = s, keep.source = FALSE)[[1]], error = \(e) NULL)
}

#' Internal function to resolve a variable name from a term to a data column
#' In order: an exact column; a nominal dummy `<v>.<k>` (maps to `<v>`); `<name>.latent`.
#' @noRd
diag_resolve_column <- function(name, data) {
    if (name %in% names(data)) return(name)
    m <- regmatches(name, regexec("^(.+)\\.([0-9]+)$", name))[[1]]
    if (length(m) == 3L && m[2] %in% names(data)) return(m[2])
    lat <- paste0(name, ".latent")
    if (lat %in% names(data)) return(lat)
    NA_character_
}

#' Internal function to obtain the predictors to plot residuals against
#' The distinct variables in an outcome's fitted terms, resolved to data columns,
#' in first-appearance order (`x1*m` gives `x1` and `m`; `ln(x)` gives `x`;
#' `educ.2` gives `educ`). Returns a named character vector: names are the
#' variables as they appear in the terms; values are the columns (NA when a
#' variable has no saved column).
#' @noRd
diag_plot_predictors <- function(model, outcome_bases, data = model@imputations[[1]], exclude = character()) {
    tt <- diag_term_table(model)
    terms <- tt$term[tt$base %in% outcome_bases & tt$kind == "mean"]
    terms <- setdiff(terms, "Intercept")
    vars <- unique(unlist(lapply(terms, \(t) {
        ex <- diag_term_expr(t)
        if (is.null(ex)) t else all.vars(ex)
    }), use.names = FALSE))
    vars <- setdiff(vars, exclude)
    cols <- vapply(vars, diag_resolve_column, "", data = data)
    # A nominal dummy (`educ.2`) is named by its parent variable (`educ`)
    is_dummy <- !is.na(cols) & cols != vars & !endsWith(cols, ".latent")
    names(cols)[is_dummy] <- cols[is_dummy]
    keep <- !duplicated(cols) | is.na(cols)
    cols[keep]
}

# Outcomes ====================================================================

#' Internal function to list the outcomes that can be diagnosed
#' Returns a list of outcome descriptions keyed by the name a user passes.
#' @noRd
diag_outcomes <- function(model) {
    cols <- names(model@imputations[[1]])
    ordinal <- diag_discrete_vars(model, "ordinal")
    nominal <- diag_discrete_vars(model, "nominal")
    count <- diag_discrete_vars(model, "count")
    clusterids <- diag_clusterids(model)
    l2_latents <- diag_level2_latents(model)
    out <- list()

    # Level-1 outcomes with <dv>.residual and <dv>.predicted
    bases <- sub("\\.residual$", "", grep("\\.residual$", cols, value = TRUE))
    for (b in bases) {
        if (b %in% names(l2_latents)) next
        if (!paste0(b, ".predicted") %in% cols) next
        type <- if (b %in% ordinal) "ordinal" else if (b %in% count) "count" else "continuous"
        out[[b]] <- list(
            name = b, level = 1L, type = type, residual = paste0(b, ".residual"),
            predicted = paste0(b, ".predicted"), terms = b
        )
    }

    # Nominal outcomes with <dv>.<k>.residual and <dv>.<k>.probability
    for (b in nominal) {
        rx <- paste0("^", gsub(".", "\\.", b, fixed = TRUE), "\\.([0-9]+)\\.residual$")
        ks <- as.integer(sub(rx, "\\1", grep(rx, cols, value = TRUE)))
        ks <- sort(ks[paste0(b, ".", ks, ".probability") %in% cols])
        if (!length(ks)) next
        out[[b]] <- list(
            name = b, level = 1L, type = "nominal", categories = ks,
            terms = paste0(b, ".", ks)
        )
        # Each non-reference category is also an outcome, named as Blimp names it
        for (k in ks) {
            out[[paste0(b, ".", k)]] <- list(
                name = paste0(b, ".", k), level = 1L, type = "nominal", categories = k,
                terms = paste0(b, ".", k), parent = b, category_of = b
            )
        }
    }

    # Cluster information for level-1 outcomes. A nominal outcome's level-2
    # residuals are named by category (`<dv>.<k>[cluster]`).
    for (b in names(out)) {
        bases <- gsub(".", "\\.", out[[b]]$terms, fixed = TRUE)
        l2 <- grep(paste0("^(", paste(bases, collapse = "|"), ")\\[.+\\]$"), cols, value = TRUE)
        out[[b]]$level2_residuals <- l2
        cl <- intersect(c(sub("^.+\\[(.+)\\]$", "\\1", l2), clusterids), cols)
        out[[b]]$cluster <- if (length(cl)) cl[1] else NA_character_
    }

    # Level-2 random-intercept residuals <dv>[cluster] of a level-1 outcome
    for (b in names(out)) {
        for (v in out[[b]]$level2_residuals) {
            out[[v]] <- list(
                name = v, level = 2L, type = "continuous", residual = v, parent = b,
                parent_type = out[[b]]$type, cluster = sub("^.+\\[(.+)\\]$", "\\1", v),
                terms = out[[b]]$terms
            )
        }
    }

    # Level-2 latent random effects declared on the clusterid LATENT line
    for (v in names(l2_latents)) {
        rc <- paste0(v, ".residual")
        if (!rc %in% cols || !l2_latents[[v]] %in% cols) next
        out[[v]] <- list(
            name = v, level = 2L, type = "continuous", residual = rc, parent = v,
            parent_type = "continuous", cluster = l2_latents[[v]], terms = v
        )
    }
    out
}

#' Internal function to find one outcome or error with the available names
#' @noRd
diag_outcome <- function(model, variable) {
    outcomes <- diag_outcomes(model)
    if (!variable %in% names(outcomes)) {
        if (!length(outcomes)) throw_error(c(
            "{.arg model} has no outcomes with saved residuals.",
            "i" = "Residuals are saved by default; check {.arg add_save} in {.fn rblimp}."
        ))
        throw_error(c(
            "{.val {variable}} is not an outcome with saved residuals.",
            "i" = "Available outcomes: {.val {names(outcomes)}}."
        ))
    }
    outcomes[[variable]]
}

#' Internal function to obtain an outcome's predictors split by level
#' Returns the level-1 (within-cluster) and level-2 (cluster-constant) predictor columns.
#' @noRd
diag_outcome_predictors <- function(model, outcome) {
    d <- model@imputations[[1]]
    preds <- diag_plot_predictors(model, outcome$terms, d, exclude = names(diag_level2_latents(model)))
    missing_cols <- names(preds)[is.na(preds)]
    if (length(missing_cols)) cli::cli_inform(c(
        "!" = "No saved column for {.val {missing_cols}}; not plotted for {.val {outcome$name}}."
    ))
    preds <- preds[!is.na(preds)]
    cl <- outcome$cluster
    if (is.null(cl) || is.na(cl) || !cl %in% names(d)) {
        return(list(level1 = preds, level2 = character(0)))
    }
    is_l2 <- vapply(preds, \(p) is_cluster_constant(d[[p]], d[[cl]]), TRUE)
    # Cluster-constant predictors are plotted only against the outcome's level-2
    # residual when it has one, so they never appear twice
    drop_l2 <- outcome$level == 1L && length(outcome$level2_residuals) > 0
    list(level1 = if (drop_l2) preds[!is_l2] else preds, level2 = preds[is_l2])
}

#' Internal function to check whether a column is constant within every cluster
#' @noRd
is_cluster_constant <- function(x, g) {
    if (!is.numeric(x)) return(FALSE)
    all(vapply(split(x, g), \(v) {
        v <- v[is.finite(v)]
        length(v) == 0L || diff(range(v)) < 1e-8
    }, TRUE))
}

# Pooling =====================================================================

#' Internal function for the pooled residual scale
#' The square root of the average per-imputation residual variance, or the per-case
#' model-implied SD averaged over imputations when a variance model is fit.
#' @noRd
pooled_sigma <- function(imp_list, residual, variance_col = NULL) {
    n <- nrow(imp_list[[1]])
    if (!is.null(variance_col) && variance_col %in% names(imp_list[[1]])) {
        return(sqrt(rowMeans(vapply(imp_list, \(d) exp(d[[variance_col]]), numeric(n)))))
    }
    rep(sqrt(mean(vapply(imp_list, \(d) stats::var(d[[residual]], na.rm = TRUE), 0), na.rm = TRUE)), n)
}

#' Internal function for a point layer whose transparency follows `blimp.point.alpha`
#' When the theme does not set it, a stacked cloud of `n_imps` imputations is drawn
#' at `point_alpha_base` / n_imps^(1/3), and points that are not stacked at
#' `point_alpha_base`.
#' @noRd
geom_point_mi <- function(..., n_imps = 1) {
    themed(
        ggplot2::geom_point(...),
        alpha = list(point.alpha = point_alpha_base / max(n_imps, 1)^(1 / 3)),
        colour = list(data.color = NULL)
    )
}

#' Internal function for summary markers (category means) drawn over a point cloud
#' `shape` is the default marker shape; NULL keeps the theme's point shape.
#' @noRd
summary_points <- function(layer, shape = NULL) {
    themed(layer, size = list(summary.size = summary_point_size), shape = list(summary.shape = shape))
}

#' Internal function for a fit-then-pool loess curve
#' Fits a loess within each imputation, evaluates every fit on one common grid
#' (trimmed to the 1-99% quantiles), and averages the curves. With `se = TRUE`,
#' adds 95% pointwise bands pooled with Rubin's rules.
#' @noRd
pooled_loess <- function(xy_list, se = FALSE, span = 0.75, n_grid = 200) {
    allx <- unlist(lapply(xy_list, \(d) d$x), use.names = FALSE)
    allx <- allx[is.finite(allx)]
    if (length(allx) < 2) return(data.frame(x = numeric(0), y = numeric(0), lower = numeric(0), upper = numeric(0)))
    rng <- stats::quantile(allx, c(0.01, 0.99))
    grid <- seq(rng[1], rng[2], length.out = n_grid)
    fits <- lapply(xy_list, \(d) {
        d <- d[is.finite(d$x) & is.finite(d$y), ]
        if (nrow(d) < 10 || length(unique(d$x)) < 4) return(NULL)
        fit <- tryCatch(suppressWarnings(stats::loess(y ~ x, d, span = span)), error = \(e) NULL)
        if (is.null(fit)) return(NULL)
        suppressWarnings(stats::predict(fit, newdata = data.frame(x = grid), se = se))
    })
    fits <- Filter(Negate(is.null), fits)
    if (!length(fits)) return(data.frame(x = numeric(0), y = numeric(0), lower = numeric(0), upper = numeric(0)))
    f <- vapply(fits, \(p) if (se) p$fit else p, numeric(n_grid))
    f <- matrix(f, nrow = n_grid)
    # Only where every imputation's curve is defined (loess does not extrapolate),
    # so the average never jumps as imputations drop out near the edges
    out <- data.frame(x = grid, y = rowMeans(f), lower = NA_real_, upper = NA_real_)
    if (se) {
        M <- ncol(f)
        W <- rowMeans(matrix(vapply(fits, \(p) p$se.fit^2, numeric(n_grid)), nrow = n_grid))
        B <- if (M > 1) apply(f, 1, stats::var) else 0
        total <- W + B + B / M
        out$lower <- out$y - 1.96 * sqrt(total)
        out$upper <- out$y + 1.96 * sqrt(total)
    }
    out[is.finite(out$y), , drop = FALSE]
}

# Plot pieces =================================================================

#' Internal function for a reference line
#' `yintercept` is a vector, or a data frame with a `yintercept` column and the
#' facet columns, so each panel gets its own lines. A solid line (the zero line)
#' uses the theme's line color and width; dashed lines are grey and thinner, and
#' guide lines lighter still.
#' @noRd
ref_hline <- function(yintercept, dashed = FALSE, guide = FALSE) {
    data <- if (is.data.frame(yintercept)) yintercept else data.frame(yintercept = yintercept)
    ratio <- if (dashed) ref_line_ratio else 1
    themed(
        ggplot2::geom_hline(
            data = data, ggplot2::aes(yintercept = yintercept, linewidth = ggplot2::from_theme(linewidth * ratio)),
            linetype = if (dashed) "dashed" else "solid", inherit.aes = FALSE
        ),
        colour = if (guide) list(guide.color = "grey70")
                 else list(reference.color = if (dashed) "grey50")
    )
}

#' Internal function for a pooled loess curve layer (and optional bands)
#' @noRd
curve_layers <- function(curve, se = FALSE) {
    if (!nrow(curve)) return(list())
    layers <- list()
    # The band's edges are dashed lines, as in the Johnson-Neyman plots, so the band
    # stays visible over a dense point cloud
    if (se) layers <- c(layers, list(themed(
        ggplot2::geom_ribbon(
            data = curve, ggplot2::aes(x = x, ymin = lower, ymax = upper, fill = ggplot2::from_theme(accent)),
            colour = NA, inherit.aes = FALSE
        ),
        alpha = list(ribbon.alpha = 0.2)
    )), ggplot2::geom_line(
        data = curve,
        ggplot2::aes(x = x, y = lower, colour = ggplot2::from_theme(accent), linewidth = ggplot2::from_theme(linewidth)),
        linetype = "dashed", inherit.aes = FALSE, na.rm = TRUE
    ), ggplot2::geom_line(
        data = curve,
        ggplot2::aes(x = x, y = upper, colour = ggplot2::from_theme(accent), linewidth = ggplot2::from_theme(linewidth)),
        linetype = "dashed", inherit.aes = FALSE, na.rm = TRUE
    ))
    c(layers, list(ggplot2::geom_line(
        data = curve,
        ggplot2::aes(x = x, y = y, colour = ggplot2::from_theme(accent),
                     linewidth = ggplot2::from_theme(linewidth * curve_line_ratio)),
        inherit.aes = FALSE, na.rm = TRUE
    )))
}

#' Internal function for integer-only axis breaks
#' @noRd
integer_breaks <- function(limits) {
    if (any(!is.finite(limits))) return(numeric(0))
    rng <- ceiling(limits[1]):floor(limits[2])
    if (length(rng) <= 12) return(rng)
    seq(ceiling(limits[1]), floor(limits[2]), by = ceiling(length(rng) / 7))
}

#' Internal function to label the most extreme cases
#' Uses ggrepel when it is installed.
#' @noRd
outlier_labels <- function(df, value_col, threshold, max_labels, two_sided = FALSE) {
    v <- df[[value_col]]
    out <- df[if (two_sided) abs(v) >= abs(threshold) else v >= threshold, , drop = FALSE]
    out <- out[is.finite(out[[value_col]]), , drop = FALSE]
    if (!nrow(out)) return(list())
    key <- if (two_sided) abs(out[[value_col]]) else out[[value_col]]
    out <- out[order(key, decreasing = TRUE)[seq_len(min(nrow(out), max_labels))], , drop = FALSE]
    # Size and font follow the theme
    mapping <- ggplot2::aes(
        x = .data[["case"]], y = .data[[value_col]], label = .data[["label"]],
        size = ggplot2::from_theme(fontsize * 0.8), family = ggplot2::from_theme(family)
    )
    layer <- if (requireNamespace("ggrepel", quietly = TRUE)) {
        ggrepel::geom_text_repel(
            data = out, mapping = mapping, max.overlaps = Inf,
            seed = 42, min.segment.length = 0.2, inherit.aes = FALSE
        )
    } else {
        ggplot2::geom_text(data = out, mapping = mapping, vjust = -0.7, inherit.aes = FALSE)
    }
    themed(layer, colour = list(label.color = "black"))
}

#' Internal function for reader-friendly variable labels
#' @noRd
var_label <- function(var) {
    if (grepl("\\[.+\\]$", var)) {
        if (grepl("\\$", var)) return(paste(sub("^.+\\$(.+)\\[.+\\]$", "\\1", var), "Random Slope"))
        if (grepl("\\.mean\\[.+\\]$", var)) return(paste(sub("\\.mean\\[.+\\]$", "", var), "Latent Mean"))
        return(paste(sub("\\[.+\\]$", "", var), "Random Intercept"))
    }
    suffix_map <- c(
        residual = "Residual", latent = "Latent Variable",
        probability = "Predicted Probability", yjt = "Yeo-Johnson Transformed"
    )
    suffix <- sub("^.*\\.", "", var)
    if (!grepl(".", var, fixed = TRUE) || !suffix %in% names(suffix_map)) return(var)
    paste(sub("\\.[^.]*$", "", var), suffix_map[[suffix]])
}

#' Internal function to decide whether a variable is discrete
#' Discrete when `declared` (ORDINAL, NOMINAL, or COUNT in the model), stored as an R
#' factor, or with `max_unique` or fewer distinct finite values. Pass the values of one
#' completed dataset, so the count does not grow with the number of imputations.
#' @noRd
is_discrete <- function(x, declared = FALSE, max_unique) {
    if (declared || is.factor(x) || is.character(x) || is.logical(x)) return(TRUE)
    x <- as_codes(x)
    x <- x[is.finite(x)]
    length(x) > 0L && length(unique(x)) <= max_unique
}

#' Internal function for the numeric codes of a column
#' Factors become their level codes, characters their position in `levels`, and
#' logicals 0/1.
#' @noRd
as_codes <- function(x, levels = NULL) {
    if (is.factor(x)) return(as.numeric(unclass(x)))
    if (is.character(x)) return(as.numeric(match(x, levels %||% sort(unique(x)))))
    if (is.logical(x)) return(as.numeric(x))
    x
}

#' Internal function for the category labels of a factor or character column (else NULL)
#' Character values are sorted across every completed dataset, so codes agree between them.
#' @noRd
column_levels <- function(model, col) {
    x <- model@imputations[[1]][[col]]
    if (is.factor(x)) return(levels(x))
    if (is.character(x)) return(sort(unique(stats::na.omit(unlist(lapply(model@imputations, \(d) d[[col]]), use.names = FALSE)))))
    NULL
}

#' Internal function for the completed datasets as data frames, with categories as codes
#' @noRd
imputation_frames <- function(model) {
    d1 <- model@imputations[[1]]
    conv <- names(d1)[vapply(d1, \(x) is.factor(x) || is.character(x) || is.logical(x), TRUE)]
    lv <- lapply(conv, \(col) column_levels(model, col))
    names(lv) <- conv
    lapply(model@imputations, \(d) {
        d <- as.data.frame(d)
        for (col in conv) d[[col]] <- as_codes(d[[col]], lv[[col]])
        d
    })
}

#' Internal function for the x scale of a discrete axis: factor labels, else integer breaks
#' @noRd
discrete_x_scale <- function(levels = NULL) {
    if (length(levels)) ggplot2::scale_x_continuous(breaks = seq_along(levels), labels = levels)
    else ggplot2::scale_x_continuous(breaks = integer_breaks)
}

#' Internal function to check a `discrete_max_unique` argument
#' @noRd
check_max_unique <- function(x) {
    if (!is.numeric(x) || length(x) != 1 || is.na(x) || x < 0) throw_error(
        "{.arg discrete_max_unique} must be a single non-negative number."
    )
}

#' Internal function to warn about a continuous predictor with few values
#' @noRd
warn_few_values <- function(predictor, nux) {
    cli::cli_warn(c(
        "Predictor {.val {predictor}} is modeled as continuous but has only {nux} distinct value{?s}.",
        "i" = "If it is categorical, declare it as ORDINAL or NOMINAL and refit. The plot shows category means."
    ))
}
