#' Styling and Saving rblimp Plots
#'
#' @description
#' Every rblimp plot is a [`ggplot2::ggplot`], so you style and save it as you would any
#' ggplot. This page shows the parts specific to rblimp, using a plot such as
#' `p <- residual_plot(model, 'y', x = 'x')`. The plotting functions themselves are listed
#' in [`rblimp_getting_started`].
#'
#' @section Themes:
#'
#' rblimp's plots draw with the colors, line widths, point sizes, and fonts of the plot's
#' ggplot2 theme, so adding a theme restyles the whole plot. Use any ggplot2 theme, or
#' [`theme_blimp`] for rblimp's ready-made look, and set one with
#' [`ggplot2::theme_set`] to use it for every plot in the session. Colors and sizes come
#' from the theme's geom settings: points and lines use the `ink` color and curves the
#' `accent` color.
#'
#' ```r
#' p + theme_blimp(base_size = 14)
#' p + ggplot2::theme(geom = ggplot2::element_geom(ink = "grey30", accent = "black"))
#'
#' old <- ggplot2::theme_set(theme_blimp())  # every plot from here on
#' ggplot2::theme_set(old)                   # back to the previous theme
#' ```
#'
#' @section rblimp's own settings:
#'
#' A few things have no ggplot2 theme setting: the transparency of points and bands, the
#' data color (which [`theme_blimp`] uses for its points), category-mean markers,
#' reference lines, case labels, and Johnson-Neyman boundaries. Set them with
#' [`blimp_style`]. To show which cases had their outcome imputed, add [`scale_imputed`]
#' (or [`scale_imputed_manual`] to pick the colors). Titles and labels work as in any
#' ggplot; for a figure, you might drop the title and the "Averaged over M imputations"
#' subtitle.
#'
#' ```r
#' p + blimp_style(point_alpha = 0.1, reference_color = "red")
#' p + scale_imputed()
#' p + ggplot2::labs(title = NULL, subtitle = NULL)
#' ```
#'
#' @section Styling many plots at once:
#'
#' Keep a style in a list and add it like a single component. [`diagnostic_plots`] and
#' [`distribution_plots`] return named lists of plots, and `+` adds to every plot in the
#' list; pick plots by name, or keep the ones whose names match a pattern with `subset()`.
#'
#' ```r
#' my_style <- list(theme_blimp(base_size = 14), blimp_style(point_alpha = 0.1))
#' p + my_style
#'
#' plots <- diagnostic_plots(model) + my_style
#' subset(plots, "index")
#' ```
#'
#' @section Saving:
#'
#' Save a plot as an image with [`ggplot2::ggsave`], or a whole list with a loop. To keep
#' a plot to draw or change later, use [`saveRDS`]; it keeps only the data it draws, not
#' the model.
#'
#' ```r
#' ggplot2::ggsave("residuals.png", p + my_style, width = 6, height = 4, dpi = 300)
#'
#' for (name in names(plots)) {
#'     ggplot2::ggsave(paste0(name, ".png"), plots[[name]], width = 6, height = 4, dpi = 300)
#' }
#' ```
#'
#' @examplesIf has_blimp()
#' # A model with some missing outcome values
#' mydata <- rblimp_sim(
#'     c('x ~ normal(0, 1)', 'y ~ normal(10 + 0.5*x, 1)'),
#'     n = 100, seed = 981273
#' )
#' mydata$y[1:20] <- NA
#' model <- rblimp('y ~ x', mydata, nimps = 20, seed = 10972, burn = 1000, iter = 1000)
#' p <- residual_plot(model, 'y', x = 'x')
#'
#' # A theme, rblimp's settings, and the imputed values marked
#' p + theme_blimp() + blimp_style(point_alpha = 0.2) + scale_imputed()
#'
#' # One style for a whole list of plots
#' my_style <- list(theme_blimp(base_size = 14), blimp_style(point_alpha = 0.1))
#' plots <- diagnostic_plots(model) + my_style
#' subset(plots, "index")
#'
#' # Save a plot
#' ggplot2::ggsave(file.path(tempdir(), "residuals.png"), p + my_style, width = 6, height = 4)
#' @name rblimp_plots
#' @keywords documentation
NULL
