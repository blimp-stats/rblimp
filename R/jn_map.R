# Copyright Brian Keller 2026, all rights reserved

#' Two-dimensional Johnson-Neyman map of a conditional slope
#'
#' @description
#' A 2-D analogue of [`jn_plot()`]. For a model with an `outcome ~ focal x m1 x m2`
#' interaction, the conditional slope of `outcome` on `focal` is a linear
#' surface over `(m1, m2)`:
#' \deqn{ \frac{\partial y}{\partial x} = a + b\,m_1 + c\,m_2 + d\,m_1 m_2. }
#' `jn_map()` recovers those four coefficients per MCMC draw from the
#' SIMPLE-evaluated points, evaluates them on a dense grid, and renders the
#' posterior median as a heatmap. A contour line marks the boundary of the
#' region where the `ci` posterior credible interval excludes zero -- the 2-D
#' "region of significance".
#'
#' @param formula an object of class [`formula`] of the form
#'   `outcome ~ focal | m1 + m2`. Exactly two bare moderators are required;
#'   additional moderators present in the SIMPLE statement must be pinned via
#'   [`at()`].
#' @param model an [`blimp_obj`]. The model must have a SIMPLE command output
#'   saved.
#' @param ci a value between 0 and 1 specifying the credible-interval size for
#'   the significance contour. Default `0.95`.
#' @param n_grid integer length-1 or length-2. Number of grid points along
#'   `m1` and `m2`. Default `100` (i.e. 100 x 100 cells).
#' @param ... currently unused.
#' @returns a [`ggplot2::ggplot`] plot. Attribute `"grid"` carries the per-cell
#'   data frame (with `lower`, `median`, `upper`, `sig` columns).
#' @seealso [`jn_plot`], [`simple_plot`], [`at`], [`join`]
#' @examplesIf has_blimp()
#' \dontrun{
#' # Generate data with a three-way interaction
#' mydata <- rblimp_sim(
#'     c(
#'         'x  ~ normal(0, 1)',
#'         'm1 ~ normal(0, 1)',
#'         'm2 ~ normal(0, 1)',
#'         'y  ~ normal(10 + 0.4*x + 0.3*m1 - 0.2*m2 +
#'                          0.5*x*m1 + 0.3*x*m2 + 0.2*m1*m2 +
#'                          0.6*x*m1*m2, 1)'
#'     ),
#'     n = 500,
#'     seed = 981273
#' )
#'
#' # Fit the model -- SIMPLE must evaluate both moderators
#' fit <- rblimp(
#'     'y ~ x m1 m2 x*m1 x*m2 m1*m2 x*m1*m2',
#'     mydata,
#'     center = ~ x + m1 + m2,
#'     simple = 'x | m1 @ quantile and m2 @ sd',
#'     seed   = 1071,
#'     burn   = 1000,
#'     iter   = 1000
#' )
#'
#' # Default 2-D heatmap of the conditional slope of y on x
#' jn_map(y ~ x | m1 + m2, fit)
#'
#' # Swap axes
#' jn_map(y ~ x | m2 + m1, fit)
#'
#' # Inspect the underlying per-cell data
#' head(attr(jn_map(y ~ x | m1 + m2, fit), 'grid'))
#' }
#' @import ggplot2
#' @importFrom methods is
#' @importFrom stats quantile
#' @export
jn_map <- function(formula, model, ci = 0.95, n_grid = 100, ...) {

    if (ci >= 1.0 || ci <= 0.0) throw_error(
        "The {.arg ci} must be between 0 and 1"
    )
    if (!is(model, 'blimp_obj')) throw_error(
        "{.arg model} is not a `blimp_obj`"
    )
    if (NROW(model@simple) == 0) throw_error(c(
        "No SIMPLE command was specified.",
        i = "Specify {.arg simple} when running {.cli rblimp}."
    ))

    pf <- parse_plot_formula(formula)
    out            <- pf$outcome
    pre            <- pf$focal
    formula_mods   <- pf$bare_mods
    mod_components <- pf$mod_components
    at_filter      <- pf$at_filter

    if (length(formula_mods) != 2) throw_error(c(
        "{.fn jn_map} requires exactly two moderators after {.code |}.",
        i = "Use {.fn at} to pin any additional moderators."
    ))
    mod1 <- formula_mods[1]
    mod2 <- formula_mods[2]
    if (length(mod_components[[mod1]]) != 1 ||
        length(mod_components[[mod2]]) != 1) throw_error(
        "{.fn jn_map} does not support {.fn join} moderators."
    )

    # Parse SIMPLE columns
    simple <- model@simple
    simple_names <- names(simple)
    if (!all(grepl('(SLOPE|INTER): ', simple_names))) throw_error(
        "The Blimp version used is unsupported. Update Blimp!"
    )
    names(simple) <- gsub('(SLOPE|INTER): ', '', simple_names)
    slope <- simple[, startsWith(simple_names, 'SLOPE:'), drop = FALSE]

    parsed <- parse_simple_colnames(names(slope))

    keep <- vapply(parsed, function(p) {
        length(p$outcome) == 1 && length(p$predictor) == 1 &&
            is_equal(p$outcome, out) && is_equal(p$predictor, pre) &&
            all(tolower(c(mod1, mod2)) %in% tolower(p$mods))
    }, logical(1))

    # Apply at() filter
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

    if (!any(keep)) throw_error(c(
        "No SIMPLE rows match the outcome, focal, and both moderators.",
        i = "Check that SIMPLE evaluates both {.field {mod1}} and {.field {mod2}}."
    ))

    sel_cols   <- which(keep)
    sel_parsed <- parsed[sel_cols]

    # Extras (other SIMPLE moderators) must take a single value after at().
    used_components <- c(mod1, mod2)
    first_mods <- sel_parsed[[1]]$mods
    extras <- first_mods[!(tolower(first_mods) %in% tolower(used_components))]
    extra_constant <- list()
    for (em in extras) {
        v <- vapply(sel_parsed, function(p) {
            idx <- which(tolower(p$mods) == tolower(em))[1]
            p$vals[idx]
        }, character(1))
        if (length(unique(v)) > 1) throw_error(c(
            "Moderator {.field {em}} varies across the selected SIMPLE rows.",
            i = "Pin it with {.code at({em} = \"value\")}."
        ))
        extra_constant[[em]] <- v[1]
    }

    # Centering info
    centered_vars <- if (is.null(model@syntax$center)) character(0) else {
        model@syntax$center |> strsplit(' ') |> unlist() |> tolower() |>
            gsub(';', '', x = _)
    }
    is_centered <- function(name) {
        (tolower(name) |> sub("\\s*\\[[^]]*\\]$", "", x = _)) %in% centered_vars
    }
    m1_is_cent <- is_centered(mod1)
    m2_is_cent <- is_centered(mod2)

    # Look up moderator data and range
    fetch_mod <- function(m) {
        ind <- (tolower(names(model@average_imp))) == tolower(m)
        if (sum(ind) != 1) {
            ind <- (tolower(names(model@average_imp))) == tolower(paste0(m, ".latent"))
        }
        if (sum(ind) != 1) return(NULL)
        model@average_imp[, ind]
    }
    m1_data <- fetch_mod(mod1)
    m2_data <- fetch_mod(mod2)
    m1_mu <- if (m1_is_cent && !is.null(m1_data)) mean(m1_data) else 0
    m2_mu <- if (m2_is_cent && !is.null(m2_data)) mean(m2_data) else 0

    label_to_num <- function(label, data, mu) {
        v <- mod_label_to_numeric(label, data, model@iterations, mu)
        # Legacy jn_map fallback: when `data` is NULL we still want a usable
        # axis value for `+/-k SD` / `Mean +/- k SD` labels (raw `k` in SD
        # units) so the slope surface is plottable in SD coordinates.
        if (is.na(v) && is.null(data)) {
            lab <- trimws(label)
            mean_re <- "^Mean\\s*(?:([+-])\\s*([0-9.]+)\\s*SD)?$"
            if (grepl(mean_re, lab, perl = TRUE)) {
                parts <- regmatches(lab, regexec(mean_re, lab, perl = TRUE))[[1]]
                if (nzchar(parts[2]) && nzchar(parts[3]))
                    return(as.numeric(paste0(parts[2], parts[3])))
                return(0)
            }
            if (grepl("\\s*SD\\s*$", lab)) {
                n_sd <- suppressWarnings(as.numeric(sub("\\s*SD\\s*$", "", lab)))
                if (!is.na(n_sd)) return(n_sd)
            }
        }
        v
    }

    m1_vals <- vapply(sel_parsed, function(p) {
        idx <- which(tolower(p$mods) == tolower(mod1))[1]
        label_to_num(p$vals[idx], m1_data, m1_mu)
    }, double(1))
    m2_vals <- vapply(sel_parsed, function(p) {
        idx <- which(tolower(p$mods) == tolower(mod2))[1]
        label_to_num(p$vals[idx], m2_data, m2_mu)
    }, double(1))

    keep <- !is.na(m1_vals) & !is.na(m2_vals)
    bad_m1 <- vapply(sel_parsed, function(p) {
        idx <- which(tolower(p$mods) == tolower(mod1))[1]; p$vals[idx]
    }, character(1))[!keep]
    bad_m2 <- vapply(sel_parsed, function(p) {
        idx <- which(tolower(p$mods) == tolower(mod2))[1]; p$vals[idx]
    }, character(1))[!keep]
    expr_re <- "^\\(.*\\)(?:\\s+sd)?$"
    unknown <- c(bad_m1[!grepl(expr_re, trimws(bad_m1), perl = TRUE)],
                 bad_m2[!grepl(expr_re, trimws(bad_m2), perl = TRUE)])
    if (length(unknown) > 0) throw_error(c(
        "Could not convert SIMPLE moderator labels to numeric values for both axes.",
        i = "Expected quantile (Q25), SD (`+1 SD`), numeric, or a parameter name."
    ))
    m1_vals  <- m1_vals[keep]
    m2_vals  <- m2_vals[keep]
    sel_cols <- sel_cols[keep]
    if (length(m1_vals) == 0) throw_error(c(
        "All SIMPLE points were expression-valued ({.code (expr)} / {.code (expr) sd}).",
        i = "Numeric placement on the JN axis is not yet supported for these.",
        i = "Re-run SIMPLE with quantile, SD, or numeric anchors to enable {.fn jn_map}."
    ))
    if (nrow(unique(cbind(m1_vals, m2_vals))) < 4) throw_error(c(
        "Need at least 4 unique (m1, m2) SIMPLE points to fit the slope surface.",
        i = "Have SIMPLE evaluate both moderators at multiple values, e.g. ",
        i = "{.code 'x | m1 @ quantile and m2 @ sd'}."
    ))

    # Per-iteration OLS fit: slope = a + b*m1 + c*m2 + d*m1*m2.
    slope_matrix <- as.matrix(slope[, sel_cols, drop = FALSE])   # T x K
    X    <- cbind(1, m1_vals, m2_vals, m1_vals * m2_vals)         # K x 4
    proj <- X %*% solve(crossprod(X))                             # K x 4
    beta <- slope_matrix %*% proj                                 # T x 4

    # Grid over m1 / m2 ranges
    if (length(n_grid) == 1) n_grid <- rep(n_grid, 2)
    m1_range <- if (!is.null(m1_data))
                    unname(quantile(m1_data - m1_mu, probs = c(0.01, 0.99),
                                    na.rm = TRUE))
                else range(m1_vals)
    m2_range <- if (!is.null(m2_data))
                    unname(quantile(m2_data - m2_mu, probs = c(0.01, 0.99),
                                    na.rm = TRUE))
                else range(m2_vals)
    # Round outward to a "nice" step (sized to the data magnitude) so the
    # axes land on tidy endpoints without `pretty()`-style overshoot.
    m1_range <- nice_outward(m1_range)
    m2_range <- nice_outward(m2_range)
    m1_grid <- seq(m1_range[1], m1_range[2], length.out = n_grid[1])
    m2_grid <- seq(m2_range[1], m2_range[2], length.out = n_grid[2])
    grid    <- expand.grid(m1 = m1_grid, m2 = m2_grid)

    # Vectorized per-cell slope across draws
    Dg     <- cbind(1, grid$m1, grid$m2, grid$m1 * grid$m2)       # N x 4
    slope_cells <- Dg %*% t(beta)                                 # N x T

    ci_p  <- (1 - ci) / 2
    probs <- c(ci_p, 0.5, 1 - ci_p)
    quan  <- apply(slope_cells, 1, quantile, probs = probs)       # 3 x N
    grid$lower   <- quan[1, ]
    grid$median  <- quan[2, ]
    grid$upper   <- quan[3, ]
    grid$sig     <- (grid$lower * grid$upper) > 0
    grid$sig_num <- as.integer(grid$sig)

    # Subtitle: held-constant moderators
    subtitle_parts <- character(0)
    if (length(extra_constant) > 0) {
        ctx <- paste(paste(names(extra_constant), '@', unlist(extra_constant)),
                     collapse = ', ')
        subtitle_parts <- c(subtitle_parts, paste0("Held constant: ", ctx))
    }
    subtitle_parts <- c(
        subtitle_parts,
        paste0("Black contour: edge of ", round(ci * 100),
               "% credible region excluding 0")
    )
    subtitle <- paste(subtitle_parts, collapse = "\n")

    # Suppress NSE NOTEs
    m1 <- m2 <- median <- sig_num <- NULL

    p <- (
        ggplot(grid, ggplot2::aes(x = m1, y = m2))
        + geom_raster(ggplot2::aes(fill = median), interpolate = TRUE)
        + scale_fill_gradient2(
            low      = "#2c7bb6",
            mid      = "white",
            high     = "#d7191c",
            midpoint = 0,
            name     = paste0("Slope of\n", out, " on ", pre)
        )
        + geom_contour(ggplot2::aes(z = sig_num), breaks = 0.5,
                       color = "black", linewidth = 0.6)
        + labs(
            title    = "Johnson-Neyman Map of Conditional Slope",
            subtitle = subtitle,
            x = if (m1_is_cent) paste("Centered", mod1) else mod1,
            y = if (m2_is_cent) paste("Centered", mod2) else mod2
        )
        + coord_cartesian(expand = FALSE)
    )

    structure(p, grid = grid)
}
