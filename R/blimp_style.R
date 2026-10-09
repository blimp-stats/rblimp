# Copyright Brian Keller 2026, all rights reserved

# Theme settings ==============================================================

# rblimp's own theme settings, `blimp.<name>`, and the kind of value each takes.
# Color settings use the `color` spelling; `blimp_style()` also accepts `colour`.
blimp_settings <- c(
    point.alpha = "alpha", data.color = "color", summary.size = "size", summary.shape = "shape",
    ribbon.alpha = "alpha", area.alpha = "alpha", curve.alpha = "alpha", reference.color = "color",
    guide.color = "color", label.color = "color",
    boundary.color = "color", boundary.alpha = "alpha", boundary.linewidth = "size"
)

#' Internal function to register rblimp's theme settings with ggplot2
#' @noRd
register_blimp_settings <- function() {
    classes <- list(alpha = c("numeric", "integer"), size = c("numeric", "integer"),
                    shape = c("numeric", "integer", "character"), color = "character")
    tree <- lapply(blimp_settings, \(k) ggplot2::el_def(classes[[k]]))
    names(tree) <- paste0("blimp.", names(blimp_settings))
    ggplot2::register_theme_elements(element_tree = tree)
}

#' Internal function to read a `blimp.*` setting from a plot's theme (NULL when unset)
#' @noRd
blimp_setting <- function(theme, name) {
    if (is.null(theme)) return(NULL)
    ggplot2::calc_element(paste0("blimp.", name), theme)
}

#' Internal function to let a layer's aesthetics default to `blimp.*` settings
#' Each argument names an aesthetic and gives `list(<setting> = <default>)`: the
#' layer draws with the setting when the plot's theme sets it, and with the
#' default otherwise (a NULL default keeps the geom's own). Mapped or fixed
#' aesthetics still take precedence.
#' @noRd
themed <- function(layer, ...) {
    spec <- list(...)
    parent <- layer$geom
    layer$geom <- ggplot2::ggproto(NULL, parent,
        use_defaults = function(self, data, params = list(), modifiers = ggplot2::aes(),
                                default_aes = NULL, theme = NULL, ...) {
            default_aes <- default_aes %||% self$default_aes
            for (a in names(spec)) {
                v <- blimp_setting(theme, names(spec[[a]])) %||% spec[[a]][[1]]
                if (!is.null(v)) default_aes[[a]] <- v
            }
            ggplot2::ggproto_parent(parent, self)$use_defaults(
                data, params, modifiers, default_aes = default_aes, theme = theme, ...
            )
        }
    )
    layer
}

#' Internal function to let a mapped color follow a theme value
#' `value(theme)` returns a color or NULL. When it returns a color and the layer's
#' `aes` (`"colour"` or `"fill"`) still has the default scale's color for `level`
#' (position `level` of `n` in the theme's discrete palette), the layer and its legend
#' key draw with the theme value. A color scale the user adds keeps its own colors.
#' @noRd
theme_color_override <- function(layer, aes, value, level, n = 2) {
    parent <- layer$geom
    layer$geom <- ggplot2::ggproto(NULL, parent,
        use_defaults = function(self, data, params = list(), modifiers = ggplot2::aes(),
                                default_aes = NULL, theme = NULL, ...) {
            data <- ggplot2::ggproto_parent(parent, self)$use_defaults(
                data, params, modifiers, default_aes = default_aes, theme = theme, ...
            )
            v <- if (!is.null(theme)) value(theme)
            if (!is.null(v) && nrow(data)) {
                pal <- ggplot2::calc_element(paste0("palette.", aes, ".discrete"), theme) %||% scales::pal_hue()
                default <- scales::as_discrete_pal(pal)(n)[level]
                if (all(data[[aes]] %in% default)) data[[aes]] <- v
            }
            data
        }
    )
    layer
}

#' Theme settings for rblimp plots
#' @description
#' rblimp's plots draw with the colors, line widths, point sizes, and fonts of the
#' plot's [`ggplot2`][ggplot2::ggplot2-package] theme. `blimp_style()` sets the
#' remaining defaults: transparency, summary markers, reference lines, labels, and
#' Johnson-Neyman lines and boundaries. Add it to a plot or a theme, as with
#' [`ggplot2::theme`].
#' @param point_alpha a number in \[0, 1\]. Transparency of the points in the
#'   diagnostic plots. Unset, a point cloud that stacks M imputations uses
#'   0.7 / M^(1/3) and other points use 0.7.
#' @param data_color a color. The data being checked: the points in the diagnostic
#'   plots (except points colored by category) and the imputed-value bars in
#'   [`distribution_plot`]. Unset: the theme's geom color for points and the second
#'   color of the discrete colour palette for bars.
#' @param summary_size a positive number. Size of the category-mean markers in
#'   [`residual_plot`] and [`probability_plot`]. Unset: 3.
#' @param summary_shape a point shape, as a number or name. Shape of the category-mean
#'   markers. Unset: a diamond (18) in [`residual_plot`] and the theme's point shape
#'   in [`probability_plot`]. Probability plots with several categories use one shape
#'   per category instead.
#' @param ribbon_alpha a number in \[0, 1\]. Transparency of interval bands. Unset:
#'   0.2 in [`simple_plot`] and the `se = TRUE` bands of [`residual_plot`]; 0.25 in
#'   [`jn_plot`] and [`jn_plot_func`].
#' @param area_alpha a number in \[0, 1\]. Transparency of filled areas. Unset: 0.25
#'   for the densities in [`posterior_plot`] and 0.3 for the observed-value bars in
#'   [`distribution_plot`].
#' @param curve_alpha a number in \[0, 1\]. Transparency of the per-imputation density curves
#'   in [`distribution_plot`]. Unset: 1 / M^(1/3) for M curves (opaque for one).
#' @param reference_color a color. Reference lines: the zero lines in the diagnostic
#'   plots, [`jn_plot`], and [`jn_plot_func`] (unset: the theme's geom color), and the
#'   dashed thresholds, Q-Q diagonal, and influence cutoffs in the diagnostic plots
#'   (unset: `"grey50"`).
#' @param guide_color a color. The guide lines at 1, 2, and 3 standard deviations in
#'   the within-cluster spread plot of [`residual_plot`]. Unset: `"grey70"`.
#' @param label_color a color. Text labels for cases and influence cutoffs. Unset:
#'   `"black"`.
#' @param boundary_color a color. Johnson-Neyman boundaries: the boundary segments
#'   in [`jn_plot`] and [`jn_plot_func`] and the contour in [`jn_map`]. Unset:
#'   `"black"`.
#' @param boundary_alpha a number in \[0, 1\]. Transparency of the Johnson-Neyman
#'   boundaries. Unset: 0.5 in [`jn_plot`] and [`jn_plot_func`]; opaque in [`jn_map`].
#' @param boundary_linewidth a positive number. Line width of the Johnson-Neyman
#'   boundaries. Unset: the theme's line width in [`jn_plot`] and [`jn_plot_func`];
#'   0.6 in [`jn_map`].
#' @param ... the color arguments spelled `colour` (e.g., `reference_colour`). All
#'   arguments must be named in full.
#' @returns a [`ggplot2::theme`] that sets the given settings.
#' @details
#' Each argument sets the theme setting `blimp.<argument>`, with the underscore
#' replaced by a dot: `blimp_style(point_alpha = 0.2)` is the same as
#' `ggplot2::theme(blimp.point.alpha = 0.2)`. In `ggplot2::theme()`, color settings use
#' the `color` spelling (`blimp.reference.color`). A setting that is not
#' set keeps each plot's own default, listed above. `blimp_style()` checks its values
#' right away; values given to `ggplot2::theme()` directly are checked only when the
#' plot is drawn.
#'
#' Everything else follows the ggplot2 theme ([`ggplot2::element_geom`]). Lines use the
#' theme's geom color (its ink unless the theme sets `colour`) and line width (dashed
#' reference lines in the diagnostic plots are half of it and curves twice it), points
#' its point size, curves its accent color, and text its font. In [`distribution_plot`],
#' the observed-value bars use the theme's geom fill, or the first color of its discrete
#' fill palette when the theme sets no fill; a fill or colour scale added to the plot
#' takes precedence over both. Groups use the theme's discrete palettes, and the fill of
#' [`jn_map`] uses its continuous fill palette (`palette.fill.continuous`) with 0 at the
#' palette's middle color, falling back to a blue-white-red scale.
#'
#' Requires ggplot2 4.0 or later.
#' @seealso [`theme_blimp`], [`residual_plot`], [`distribution_plot`], [`jn_plot`],
#'   [`jn_map`], [`simple_plot`], [`posterior_plot`]
#' @examplesIf has_blimp()
#' # Generate Data
#' mydata <- rblimp_sim(
#'     c(
#'         'x ~ normal(0, 1)',
#'         'y ~ normal(10 + 0.5*x, 1)'
#'     ),
#'     n = 100,
#'     seed = 981273
#' )
#'
#' # Run Rblimp
#' m1 <- rblimp('y ~ x', mydata, nimps = 20, seed = 10972, burn = 1000, iter = 1000)
#'
#' # One plot
#' residual_plot(m1, 'y', x = 'x') + blimp_style(point_alpha = 0.1, reference_color = 'red')
#'
#' # Every plot in the session
#' old <- ggplot2::theme_set(ggplot2::theme_minimal() + blimp_style(point_alpha = 0.1))
#' residual_plot(m1, 'y', x = 'x')
#' ggplot2::theme_set(old)
#' @export
blimp_style <- function(..., point_alpha = NULL, data_color = NULL, summary_size = NULL,
                        summary_shape = NULL, ribbon_alpha = NULL, area_alpha = NULL, curve_alpha = NULL,
                        reference_color = NULL, guide_color = NULL, label_color = NULL,
                        boundary_color = NULL, boundary_alpha = NULL, boundary_linewidth = NULL) {

    # Collect the settings given. `...` comes first so names must match in full; it
    # takes the `colour` spellings
    args <- gsub(".", "_", names(blimp_settings), fixed = TRUE)
    vals <- Filter(Negate(is.null), mget(args))
    dots <- list(...)
    if (length(dots)) {
        nms <- names(dots) %||% rep("", length(dots))
        if (any(nms == "")) throw_error("All arguments to {.fn blimp_style} must be named.")
        alias <- sub("colour", "color", nms)
        bad <- nms[!grepl("colour", nms) | !alias %in% args]
        if (length(bad)) throw_error(c(
            "Unknown argument{?s} to {.fn blimp_style}: {.arg {bad}}.",
            "i" = "Arguments use underscores, e.g. {.arg data_color} or {.arg data_colour}."
        ))
        twice <- alias[duplicated(alias) | alias %in% names(vals)]
        if (length(twice)) throw_error("{.arg {twice}} is given more than once (as `color` and `colour`).")
        vals[alias] <- dots
    }
    vals <- Filter(Negate(is.null), vals)

    # Check values
    for (nm in names(vals)) {
        v <- vals[[nm]]
        kind <- blimp_settings[[gsub("_", ".", nm, fixed = TRUE)]]
        ok <- length(v) == 1 && !is.na(v) && switch(kind,
            alpha = is.numeric(v) && v >= 0 && v <= 1,
            size  = is.numeric(v) && v > 0,
            shape = is.numeric(v) || is.character(v),
            color = is.character(v) && !inherits(try(grDevices::col2rgb(v), silent = TRUE), "try-error")
        )
        if (!isTRUE(ok)) throw_error(switch(kind,
            alpha = "{.arg {nm}} must be a single number between 0 and 1.",
            size  = "{.arg {nm}} must be a single positive number.",
            shape = "{.arg {nm}} must be a single point shape (a number or name).",
            color = "{.arg {nm}} must be a single color."
        ))
    }

    names(vals) <- paste0("blimp.", gsub("_", ".", names(vals), fixed = TRUE))
    do.call(ggplot2::theme, vals)
}
