# Copyright Brian Keller 2025, all rights reserved

#' Function to generate conditional regression equation plots (i.e., simple effects) with [`rblimp`] and SIMPLE command
#' @description
#' Generates a conditional effect plots based on the posterior summaries from the output of [`rblimp`].
#' @param formula an object of class [`formula`] to specify simple effect to plot.
#' The formula must have the following form: `outcome ~ focal | moderator`. See Details below for nominal moderators.
#' @param model an [`blimp_obj`]. The model must have a SIMPLE command output saved.
#' @param ci a value between 0 and 1 specifying the credible interval size
#' @param xvals a list of values to evaluate for the focal variable. If empty, they will automatically be determined
#' @param ... arguments passed to the internal [`ggplot2::geom_line`] call used to generate the median lines.
#' @returns a [`ggplot2::ggplot`] plot
#' @details
#' To change colors use ggplot2's scale system. Both fill and color are used. See
#' [`ggplot2::aes_colour_fill_alpha`] for more information about setting a manual set of colors.
#'
#'For nominal moderators, the variable must include the nominal code used in the dummy codes (e.g., moderator.1, moderator.2, etc).
#'When there are multiple dummy codes, then all codes must be listed using a `+`.
#'For example, after the `~` the following statement can be included:
#'   \deqn{\code{focal | moderator.1 + moderator.2}}
#' @examplesIf has_blimp()
#' # set seed
#' set.seed(981273)
#'
#' # Generate Data
#' mydata <- data.frame(
#'     x = rnorm(100),
#'     m = rnorm(100)
#' )
#' mydata$y <- with(mydata, rnorm(100, 10 + x*0.5 + m + .2*x*m, 1))
#'
#' # Run Rblimp
#' m1 <- rblimp(
#'     'y ~ x m x*m',
#'     mydata,
#'     center = ~ m,
#'     simple = 'x | m',
#'     seed = 10972,
#'     burn = 1000,
#'     iter = 1000
#' )
#'
#' # Generate Plot
#' simple_plot(y ~ x | m, m1)
#' @import ggplot2
#' @importFrom methods is
#' @export
simple_plot <- function(formula, model, ci = 0.95, xvals, ...) {

    # Extract Characters
    f <- formula |> as.character() |>
        gsub('`', '', x = _) |> gsub(' \\+ ', ' ', x = _)

    # Check inputs
    if (length(f) != 3) throw_error(c(
        "The {.arg formula} was not correctly specified.",
        "Must have the form: `outcome ~ focal | moderator`"
    ))
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

    # Extract simple slopes
    simple <- model@simple

    # Get names
    simple_names <- names(simple)

    # Check if blimp is supported
    if ((grepl('(SLOPE|INTER): ', simple_names) |> all()) == FALSE) throw_error(
        "The Blimp version used is unsupported. Update Blimp!"
    )

    # Remove slope and intercept
    names(simple) <- gsub('(SLOPE|INTER): ', '', simple_names)

    # Split into slope and intercept
    slope <- simple[, startsWith(simple_names, 'SLOPE:')]
    icept <- simple[, startsWith(simple_names, 'INTER:')]

    # Split names into data frame
    n <- names(slope)

    # Split moderator statement
    mod_state <- regmatches(n, regexpr('(?<= (\\||\\|,) ).+', n, perl = TRUE)) |>
        strsplit(', ')

    m <- sapply(
        mod_state,
        \(n) regmatches(n, gregexpr("(\\S+)(?=\\s*@)", n, perl = TRUE))[[1]] |>
            paste(collapse = ' ')
    )

    v <- sapply(
        mod_state,
        \(n) regmatches(n, gregexpr("@\\s*([-+]?\\d+(\\.\\d+)?(?:\\s+[A-Za-z]+)?)(?=\\s|$)", n, perl = TRUE))[[1]] |>
            paste(collapse = ', ') |>
            gsub("^@\\s*", "", x = _)
    )


    # Create data.frame
    d <- data.frame(
        col = seq_along(n),
        outcome   = regmatches(n, regexpr('.+(?= ~ )', n, perl = TRUE)),
        predictor = regmatches(n, regexpr('(?<= ~ ).+(?= (\\||\\|,) )', n, perl = TRUE)),
        moderator = m,
        value     = v
    )

    # Subset out effects
    dsub <- d[is_equal(d$outcome, f[2]) & is_equal(paste(d$predictor, '|', d$moderator), f[3]), ]

    # Get moderator name
    mod <- dsub$moderator |> unique()
    pre <- dsub$predictor |> unique()

    # Check if dsub has rows
    if (NROW(dsub) == 0) {
        mod_list <- unique(d$moderator)
        throw_error(c(
            "Unable to select out conditional effects",
            i = "If the moderator is nominal, include dummy code suffix.",
            i = "Otherwise the simple command doesn't exist for the moderator and outcome.",
            i = "List of moderators: { mod_list }",
        ))
    }

    # Create data set
    apply(dsub, 1, \(x) {
        col <- x['col'] |> as.numeric()
        data.frame(b0 = icept[, col], b1 = slope[, col], m = x[['value']] )
    }) |> do.call(rbind, args = _ ) -> simple_data

    # Create factor based on levels of m
    simple_data$mf <- as.factor(simple_data$m)

    # Check if it is centered
    pre_is_cent <- if (is.null(model@syntax$center)) FALSE else{
        tolower(pre) %in% (
            model@syntax$center |> strsplit(' ') |> unlist() |> tolower()
        )
    }
    mod_is_cent <- if (is.null(model@syntax$center)) FALSE else{
        tolower(mod) %in% (
            model@syntax$center |> strsplit(' ') |> unlist() |> tolower()
        )
    }

    ## Generate predicted scores

    # Handle xvals
    if (missing(xvals)) {
        ind <- is_equal(model@average_imp |> names(), pre)

        # Check if it is found. If not check latent variables.
        if (sum(ind) != 1) {
            ind <- is_equal(model@average_imp |> names(), paste0(pre, '.latent'))
        }
        # If that isn't found crash out
        if (sum(ind) != 1)  throw_error(
            "Cannot find focal preditor in imputed data"
        )
        focal_val <- model@average_imp[,ind]
        m <- if (pre_is_cent) mean(model@average_imp[,ind]) else 0.0

        l <- (model@average_imp[,ind] - m) |> pretty() |> range()
        xvals <- seq(l[1], l[2], length.out = 100)
    } else if (length(xvals) == 2) {
        xvals <- seq(xvals[1], xvals[2], length.out = 100)
    }

    # Handle probabilities
    ci <- (1 - ci) / 2
    probs <- c(ci, 0.5, 1 - ci)

    # Create range of predictor scores
    pred_score <- \(d) d[1] + d[2] * xvals

    # Compute predicted scores
    pred <- lapply(split(simple_data[,1:2], simple_data$mf), \(x) apply(x, 1, pred_score))
    # Compute quantiles (2.5%, 50%, 97.5%)
    quan <- lapply(pred, \(x) apply(x, 1, quantile, p = probs))

    # Combine into data.frame
    rib_data  <- do.call('rbind', lapply(names(quan), \(x) {
        data.frame( l = quan[[x]][1,], outcome = quan[[x]][2,], h = quan[[x]][3,], focal = xvals, m = x)
    }))

    # Create factor with labels
    rib_data$moderator <- factor(
        rib_data$m,
        unique(simple_data$m),
        labels =  paste('@', unique(simple_data$m))
    )

    # Create subtitle based on centering
    subtitle <- if (pre_is_cent | mod_is_cent) {
        paste0('Centered variables: ')
    } else {
        deparse(formula)
    }
    if (pre_is_cent) subtitle <- paste(subtitle, pre)
    if (mod_is_cent) subtitle <- paste(subtitle, mod)

    ## Make Conditional Effects Plot
    (
        ggplot2::ggplot(rib_data, ggplot2::aes(focal, color = moderator, fill = moderator))
        + ggplot2::geom_ribbon(ggplot2::aes(ymin = l, ymax = h), color = NA, alpha = 0.2)
        + ggplot2::geom_line(ggplot2::aes(y = outcome), ...)
        + ggplot2::labs(
            title = 'Plot of Conditional Regressions',
            subtitle = subtitle,
            y = f[2], x = pre,
            color = mod, fill = mod
        )
    )
}

