# Copyright Brian Keller 2026, all rights reserved

#' A theme for rblimp plots
#' @description
#' A clean [`ggplot2`][ggplot2::ggplot2-package] theme for rblimp's plots: the layout of the
#' figures in *Applied Data Modeling in Blimp*, in color. Add it to any plot, a list of plots, or
#' set it for the session with [`ggplot2::theme_set`].
#' @param base_size a number. The base font size in points.
#' @param base_family a character string. The base font family. The default uses the system font.
#' @param pointsize a number. The default point size.
#' @param linewidth a number. The default line width. Smooth curves are drawn at twice it and
#'   dashed reference lines at half of it.
#' @returns a [`ggplot2::theme`]
#' @details
#' Lines and axis text are black. The theme adds three colors:
#' - **Data** (points in the diagnostic plots and imputed-value bars): blue `"#00B0F6"`, set
#'   with `blimp_style(data_color = )`.
#' - **Geom fill** (observed-value bars): olive `"#A3A500"`, ggplot2's [`ggplot2::element_geom`] fill.
#' - **Accent** (curves and category means): coral `"#F8766D"`, ggplot2's accent.
#'
#' Zero lines are black and other reference lines dashed grey. Two groups, such as the
#' Johnson-Neyman significance bands, use the coral and blue; more groups, such as the
#' moderator levels in [`simple_plot`], use ggplot2's default palette. Axis lines and text are
#' black, there is no grid, facet labels have no boxes, titles are bold and centered, and
#' legends sit below the plot.
#'
#' Change any part by adding [`ggplot2::theme`] or [`blimp_style`] after it.
#'
#' Requires ggplot2 4.0 or later, which reads default geom colors and sizes from the theme.
#' @seealso [`blimp_style`], [`residual_plot`], [`diagnostic_plots`], [`distribution_plots`]
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
#' m1 <- rblimp('y ~ x', mydata, nimps = 10, seed = 10972, burn = 1000, iter = 1000)
#'
#' # One plot
#' residual_plot(m1, 'y') + theme_blimp()
#'
#' # Every plot in the session
#' old <- ggplot2::theme_set(theme_blimp(base_size = 14))
#' residual_plot(m1, 'y', x = 'x')
#' ggplot2::theme_set(old)
#' @export
theme_blimp <- function(base_size = 11, base_family = "", pointsize = 1.5, linewidth = 0.5) {

    # Two groups in the theme's coral and blue; more groups in ggplot2's hues
    groups <- function(n) if (n <= 2) c("#F8766D", "#00B0F6")[seq_len(n)] else scales::pal_hue()(n)

    ggplot2::theme_classic(base_size = base_size, base_family = base_family) +
        ggplot2::theme(
            axis.text = ggplot2::element_text(colour = "black", size = base_size),
            axis.line = ggplot2::element_line(colour = "black", linewidth = 0.5, lineend = "square"),
            axis.ticks = ggplot2::element_line(colour = "black", linewidth = 0.5),
            axis.ticks.length = ggplot2::unit(4, "pt"),
            legend.text = ggplot2::element_text(size = base_size),
            legend.title = ggplot2::element_text(size = base_size),
            legend.position = "bottom",
            plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
            plot.subtitle = ggplot2::element_text(colour = "grey40", hjust = 0.5),
            plot.tag = ggplot2::element_text(face = "bold", size = base_size * 22 / 18),
            strip.background = ggplot2::element_blank(),
            geom = ggplot2::element_geom(
                ink = "black", fill = "#A3A500", accent = "#F8766D",
                linewidth = linewidth, pointsize = pointsize, family = base_family
            ),
            palette.colour.discrete = groups,
            palette.fill.discrete = groups
        ) +
        blimp_style(data_color = "#00B0F6")
}
