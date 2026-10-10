# Copyright Brian Keller 2026, all rights reserved

#' Mark cases whose outcome was imputed
#' @description
#' Add `scale_imputed()` to a [`residual_plot`] or [`influence_plot`] (or a
#' [`diagnostic_plots`] list) to draw the cases whose outcome value was imputed differently
#' from the observed ones, with a legend. Without it, the plots do not distinguish them. See
#' details for what counts as observed.
#'
#' `scale_imputed()` takes any [`ggplot2`][ggplot2::ggplot2-package] scales for color, fill,
#' shape, size, alpha, linewidth, linetype, or stroke, including scales from other packages,
#' and marks the cases by those aesthetics. With none, it marks them by color and shape:
#' observed values are dark grey circles (35% of the way from the theme's ink to its paper
#' color) and imputed values are triangles in the `data_color` setting of [`blimp_style`]
#' (blue in [`theme_blimp`], as for the imputed values in [`distribution_plot`]), or the
#' second color of the theme's discrete color palette when it is not set. Curves and category
#' means keep the theme's accent color.
#'
#' `scale_imputed_manual()` sets the values directly, for any of the same aesthetics.
#' @param ... for `scale_imputed()`, ggplot2 scales, e.g.
#'   `ggplot2::scale_color_brewer(palette = "Dark2")` or
#'   `ggplot2::scale_shape_manual(values = c(21, 24))`. For `scale_imputed_manual()`, other
#'   arguments passed to [`ggplot2::scale_discrete_manual`] (e.g., `labels`).
#' @param name the legend title, `"Outcome"` by default, or `NULL` for no title. A title
#'   from a named scale (passed in or already on the plot) or from [`ggplot2::labs`] is used
#'   instead.
#' @param color,colour colors for `Observed` and `Imputed` cases, e.g.
#'   `c(Observed = "black", Imputed = "red")`. Give one of the two spellings.
#' @param shape,fill,size,alpha,linewidth,linetype,stroke values for `Observed` and `Imputed`
#'   cases, e.g. `shape = c(Observed = 21, Imputed = 24)` with
#'   `fill = c(Observed = "white", Imputed = "red")`.
#' @returns an object to add to a plot with `+`.
#' @details
#' Each point is marked by whether the plotted value is observed: a residual is observed
#' when the case's outcome value was observed and imputed when it was missing. Random
#' effects (level-2 residuals) are never observed, so they are marked as imputed.
#'
#' The points, and the bars of [`residual_plot`], are marked; curves, category means,
#' reference lines, and labels are not, and they stay out of the legend. Each layer takes the
#' aesthetics it supports: shape, fill, size, and stroke apply to points, and linewidth and
#' linetype to bars. Fill shows only on point shapes 21 to 25, so give such shapes with it;
#' their outline keeps the point color (`data_color` when color is not marked). Marking by
#' alpha replaces the automatic transparency of stacked points. The legend shows both groups
#' on every plot, also when only one is present (e.g., random effects), and draws their keys
#' opaque unless alpha is marked.
#'
#' The marking can also be styled with scales added before or after `scale_imputed()`, e.g.
#' `p + scale_imputed() + ggplot2::scale_color_viridis_d()`, and adding `scale_imputed()`
#' again marks the cases by more aesthetics. Give a named scale (one with `name =`) to
#' `scale_imputed()` or add it before: added after, its title differs from the other marked
#' aesthetics' and the legend splits in two. A scale added without
#' `scale_imputed()` has no effect, because nothing is mapped to it. While the colors are
#' still those of the theme's default discrete palette, the dark grey and `data_color` above
#' replace them, so a scale that reproduces the default palette exactly (e.g.,
#' `ggplot2::scale_color_hue()`) gives the default colors. The legend is controlled as usual,
#' e.g. with `ggplot2::theme(legend.position = "none")`.
#'
#' Plots in a [`diagnostic_plots`] list with nothing to mark ([`probability_plot`] and the
#' binned residual plots of nominal outcomes) are left unchanged. Adding `scale_imputed()` to any other plot is an error.
#' @seealso [`residual_plot`], [`influence_plot`], [`diagnostic_plots`]
#' @examplesIf has_blimp()
#' # Generate Data with missing outcome values
#' mydata <- rblimp_sim(
#'     c(
#'         'x ~ normal(0, 1)',
#'         'y ~ normal(10 + 0.5*x, 1)'
#'     ),
#'     n = 100,
#'     seed = 981273
#' )
#' mydata$y[1:20] <- NA
#'
#' # Run Rblimp
#' m1 <- rblimp('y ~ x', mydata, nimps = 10, seed = 10972, burn = 1000, iter = 1000)
#'
#' # The theme's colors and shapes
#' residual_plot(m1, 'y', x = '#case') + scale_imputed()
#'
#' # Any color or shape scale
#' residual_plot(m1, 'y', x = '#case') +
#'     scale_imputed(ggplot2::scale_color_brewer(palette = 'Dark2'))
#'
#' # Chosen colors and shapes
#' residual_plot(m1, 'y', x = 'x') +
#'     scale_imputed_manual(
#'         color = c(Observed = 'grey40', Imputed = 'red'),
#'         shape = c(Observed = 16, Imputed = 17)
#'     )
#'
#' # Filled shapes and sizes
#' residual_plot(m1, 'y', x = '#case', bars = FALSE) +
#'     scale_imputed_manual(
#'         shape = c(Observed = 21, Imputed = 24),
#'         fill = c(Observed = 'white', Imputed = 'red'),
#'         size = c(Observed = 1.5, Imputed = 2.5)
#'     )
#'
#' # Every diagnostic plot
#' diagnostic_plots(m1) + scale_imputed(ggplot2::scale_shape_manual(values = c(16, 1)))
#' @export
scale_imputed <- function(..., name = "Outcome") {
    scales <- list(...)
    if (!all(vapply(scales, ggplot2::is_scale, TRUE))) throw_error(
        "Arguments to {.fn scale_imputed} must be ggplot2 scales, e.g. {.code scale_color_brewer()}."
    )
    if (!is.null(name) && (!is.character(name) || length(name) != 1 || is.na(name))) throw_error(
        "{.arg name} must be a single character string, or {.code NULL} for no title."
    )
    # The aesthetics to mark: those of the scales given, or color and shape when none are
    aes <- c("colour", "shape")
    if (length(scales)) {
        bad <- setdiff(unlist(lapply(scales, \(s) s$aesthetics)), status_aesthetics)
        if (length(bad)) throw_error(c(
            "{.fn scale_imputed} cannot mark cases by {.val {bad[1]}}.",
            "i" = "Use scales for {.val {sub('colour', 'color', status_aesthetics)}}."
        ))
        aes <- unique(unlist(lapply(scales, \(s) s$aesthetics)))
    }
    structure(list(scales = scales, name = name, aes = aes), class = "rblimp_scale_imputed")
}

# Aesthetics `scale_imputed()` can map to the Observed/Imputed status
status_aesthetics <- c("colour", "fill", "shape", "size", "alpha", "linewidth", "linetype", "stroke")

# The status mapping for each of them. Made here rather than in ggplot_add(): a mapping
# keeps the environment it was made in, and that one would hold a copy of the plot.
status_mapping <- ggplot2::aes(
    colour = status, fill = status, shape = status, size = status, alpha = status,
    linewidth = status, linetype = status, stroke = status
)

#' @rdname scale_imputed
#' @export
scale_imputed_manual <- function(color = NULL, shape = NULL, fill = NULL, size = NULL, alpha = NULL,
                                 linewidth = NULL, linetype = NULL, stroke = NULL, ...,
                                 name = "Outcome", colour = NULL) {
    if (!is.null(color) && !is.null(colour)) throw_error("Give {.arg color} or {.arg colour}, not both.")
    values <- list(
        colour = color %||% colour, fill = fill, shape = shape, size = size, alpha = alpha,
        linewidth = linewidth, linetype = linetype, stroke = stroke
    )
    values <- Filter(Negate(is.null), values)
    if (!length(values)) throw_error(
        "Give values for at least one of {.arg color}, {.arg fill}, {.arg shape}, {.arg size}, {.arg alpha}, {.arg linewidth}, {.arg linetype}, or {.arg stroke}."
    )
    scales <- lapply(names(values), \(a) ggplot2::scale_discrete_manual(aesthetics = a, values = values[[a]], ...))
    do.call(scale_imputed, c(scales, list(name = name)))
}

#' @export
print.rblimp_scale_imputed <- function(x, ...) {
    cat(if (length(x$scales)) sprintf("<scale_imputed> with %d scale(s)\n", length(x$scales)) else "<scale_imputed>\n")
    invisible(x)
}

#' Internal method that maps the tagged layers to `status` when `scale_imputed()` is added
#' Each tagged layer is copied before its mapping changes: layers are shared environments,
#' so editing one in place would also change the plot the user started from. Each layer
#' gets the requested aesthetics its geom supports (e.g., shape on points but not on bars).
#' @noRd
#' @exportS3Method ggplot2::ggplot_add
ggplot_add.rblimp_scale_imputed <- function(object, plot, ...) {
    if (!isTRUE(plot@meta$rblimp)) throw_error(
        "{.fn scale_imputed} applies only to {.fn residual_plot}, {.fn influence_plot}, and {.fn diagnostic_plots}."
    )
    tagged <- which(vapply(plot@layers, \(l) isTRUE(l$rblimp_status), TRUE))
    if (!length(tagged)) return(plot)
    # Every aesthetic marked so far: a second scale_imputed() adds to the first
    previous <- vapply(plot@layers, \(l) !is.null(l$rblimp_groups), TRUE)
    marked <- union(unlist(lapply(plot@layers[previous], \(l) l$rblimp_groups)), object$aes)

    for (i in tagged) {
        layer <- copy_layer(plot@layers[[i]])
        aes <- intersect(object$aes, layer$geom$aesthetics())
        if (length(aes)) {
            mapping <- layer$mapping
            mapping[aes] <- status_mapping[aes]
            layer$mapping <- mapping
        }
        # Legend keys for both groups, even on a plot that has only one. Every aesthetic
        # is named: a length-1 show.legend would also put the layer in unrelated legends.
        all <- setdiff(layer$geom$aesthetics(), "group")
        shown <- intersect(marked, all)
        layer$show.legend <- if (length(shown)) stats::setNames(all %in% shown, all) else FALSE
        colours <- union(layer$geom$rblimp_colours, intersect(c("colour", "fill"), aes))
        # A geom made by an earlier scale_imputed() is replaced, not wrapped again
        base <- .subset2(layer$geom, "rblimp_base") %||% layer$geom
        layer$geom <- status_geom(base, colours, opaque = !"alpha" %in% marked)
        plot@layers[[i]] <- layer
    }

    # An invisible layer holding both groups, first so the scales list Observed then Imputed:
    # each group keeps its look even on a plot that has only one of them
    plot@layers <- c(list(status_groups(marked)), plot@layers[!previous])

    # One legend: every marked aesthetic gets the same title, so ggplot2 merges them. A title
    # from a named scale (passed in or already on the plot) or from labs() is used for all.
    has_name <- \(s) !is.null(s) && !is.null(s$name) && !inherits(s$name, "waiver")
    named <- c(
        Filter(has_name, object$scales),
        Filter(has_name, lapply(marked, \(a) plot@scales$get_scales(a)))
    )
    title <- if (length(named)) named[[1]]$name else {
        set <- Filter(Negate(is.null), plot@labels[marked])
        if (length(set)) set[[1]] else object$name
    }
    plot <- plot + do.call(ggplot2::labs, stats::setNames(rep(list(title), length(marked)), marked))
    for (s in object$scales) plot <- plot + s
    plot
}

#' Internal function for the invisible layer holding both groups
#' A function of its own because a layer keeps the environment it was made in.
#' @noRd
status_groups <- function(marked) {
    groups <- data.frame(status = factor(c("Observed", "Imputed"), levels = c("Observed", "Imputed")))
    blank <- ggplot2::geom_blank(data = groups, mapping = status_mapping[marked], inherit.aes = FALSE)
    blank$rblimp_groups <- marked
    blank
}

#' Internal function for a status layer's geom: default colors and opaque legend keys
#' `base` is the layer's geom before any scale_imputed(), so repeated calls do not add
#' levels, and it is the only geom the new one refers to, so earlier ones are not kept
#' alive. A geom something else wrapped after scale_imputed() becomes the next base.
#'
#' While the colors of `colours` (colour and/or fill) are still the default palette's (no
#' scale was added for them), observed values are a dark grey mixed from the theme's ink and
#' paper, and imputed values take the `data_color` setting when it is set (as in
#' [`distribution_plot`]). A scale the user adds keeps its colors.
#'
#' With `opaque`, legend keys are drawn opaque so faint stacked points still read in the
#' legend. ggplot2 applies `override.aes` after this, so a transparency set there is kept.
#' @noRd
status_geom <- function(base, colours, opaque) {
    # ggproto() looks up its parent by name later, so `base` is never reassigned here,
    # and is forced now: unforced, it would be read from the caller's loop variable
    force(base)
    ggplot2::ggproto(NULL, base,
        rblimp_base = base,
        rblimp_colours = colours,
        rblimp_opaque = opaque,
        use_defaults = function(self, data, params = list(), modifiers = ggplot2::aes(),
                                default_aes = NULL, theme = NULL, ...) {
            # Legend keys have no PANEL and carry the aesthetics they show; the data from
            # get_geom_defaults() has only .id
            key <- !is.null(data) && !"PANEL" %in% names(data) && any(names(data) != ".id")
            data <- ggplot2::ggproto_parent(base, self)$use_defaults(
                data, params, modifiers, default_aes = default_aes, theme = theme, ...
            )
            if (!nrow(data)) return(data)
            if (key && isTRUE(self$rblimp_opaque)) data$alpha <- 1
            el <- if (!is.null(theme)) ggplot2::calc_element("geom", theme)
            if (is.null(el)) return(data)
            # A transparent paper (e.g., theme_void) would change only the transparency
            paper <- if (grDevices::col2rgb(el$paper, alpha = TRUE)[4] < 255) "white" else el$paper
            observed_grey <- scales::col_mix(el$ink, paper, observed_ink_mix)
            data_color <- blimp_setting(theme, "data.color")
            for (a in intersect(self$rblimp_colours, names(data))) {
                pal <- ggplot2::calc_element(paste0("palette.", a, ".discrete"), theme) %||% scales::pal_hue()
                default <- scales::as_discrete_pal(pal)(2)
                if (!all(data[[a]] %in% default)) next
                observed <- data[[a]] == default[1]
                imputed <- data[[a]] == default[2]
                data[[a]][observed] <- observed_grey
                if (!is.null(data_color)) data[[a]][imputed] <- data_color
            }
            data
        }
    )
}

#' Internal function for a shallow copy of a layer
#' The copy has the layer's own fields (including `super`, so it still inherits from
#' the same parent) and its class, and no extra inheritance level.
#' @noRd
copy_layer <- function(layer) {
    new <- list2env(as.list.environment(layer, all.names = TRUE), parent = emptyenv())
    class(new) <- class(layer)
    new
}
