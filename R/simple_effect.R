# Copyright Brian Keller 2025, all rights reserved

#' Function to generate conditional regression equation plots (i.e., simple effects) with [`rblimp`] and SIMPLE command
#' @description
#' Generates a conditional effect plots based based on the posterior summaries from the output of [`rblimp`].
#' @param formula an object of class [`formula`] to specify simple effect to plot. The formula must have the following form: `outcome ~ focal | moderator`
#' @param object an [`blimp_obj`]. The object must have a SIMPLE command output saved.
#' @param ci a value between 0 and 1 specifying the credible interval size
#' @param xvals a list of values to evaluate for the focal variable. If empty, they will automatically be determined
#' @param ... arguments passed to the internal [`ggplot2::geom_line`] call used to generate the median lines.
#' @returns a [`ggplot2::ggplot`] plot
#' @examples
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
#' simple_effect(y ~ x | m, m1)
#' @import ggplot2
#' @export
simple_effect <- function(formula, object, ci = 0.95, xvals, ...) {

    # Extract Characters
    f <- formula |> as.character()

    # Check inputs
    if (length(f) != 3) throw_error(
        "The {.arg formula} was not correctly specified.",
        "Must have the form: `outcome ~ focal | moderator`"
    )
    if (ci >= 1.0 | ci <= 0.0) throw_error(
        "The {.arg ci} must be between 0 and 1"
    )
    if (class(object) != 'blimp_obj') throw_error(
        "{.arg object} is not a `blimp_obj`"
    )
    if (NROW(object@simple) == 0) throw_error(
        "No SIMPLE command was specified."
    )

    # Extract simple slopes
    simple <- object@simple

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
    d <- data.frame(
        col = seq_along(n),
        outcome   = regmatches(n, regexpr('.+(?= ~ )', n, perl = T)),
        predictor = regmatches(n, regexpr('(?<= ~ ).+(?= \\| )', n, perl = T)),
        moderator = regmatches(n, regexpr('(?<= \\| ).+(?= @ )', n, perl = T)),
        value     = regmatches(n, regexpr('(?<= @ ).+', n, perl = T))
    )

    # Subset out effects
    dsub <- d[d$outcome == f[2] & paste(d$predictor, '|', d$moderator) == f[3], ]

    # Get moderator name
    mod <- dsub$moderator |> unique()
    pre <- dsub$predictor |> unique()

    # Check if dsub has rows
    if (NROW(dsub) == 0) throw_error(
        "Unable to select out conditional effects. Most likely simple command doesn't exist."
    )

    # Create data set
    apply(dsub, 1, \(x) {
        col <- x['col'] |> as.numeric()
        data.frame(b0 = icept[, col], b1 = slope[, col], m = x[['value']] )
    }) |> do.call(rbind, args = _ ) -> simple_data

    # Create factor based on levels of m
    simple_data$mf <- as.factor(simple_data$m)

    ## Generate predicted scores

    # Handle xvals
    if (missing(xvals)) {
        ind <- (object@average_imp |> names() |> tolower()) == tolower(pre)

        # handle if cannot find x values or if too many
        if (sum(ind) != 1) throw_error(
            "Cannot find focal preditor in imputed data"
        )
        focal_val <- object@average_imp[,ind]

        # Check if it is centered
        is_cent <- tolower(pre) %in% (
            object@syntax$center |> strsplit(' ') |> unlist() |> tolower()
        )
        m <- if(is_cent) mean(object@average_imp[,ind]) else 0.0

        l <- (object@average_imp[,ind] - m) |> pretty() |> range()
        xvals <- seq(l[1], l[2], length.out = 100)
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
    rib_data$moderator <-  factor(
        rib_data$m,
        unique(simple_data$m),
        labels =  paste('@', unique(simple_data$m))
    )

    ## Make Conditional Effects Plot
    (
        ggplot2::ggplot(rib_data, ggplot2::aes(focal, color = moderator, fill = moderator))
        + ggplot2::geom_ribbon(ggplot2::aes(ymin = l, ymax = h), alpha = 0.25)
        + ggplot2::geom_line(ggplot2::aes(y = outcome), ...)
        + ggplot2::ggtitle(
            'Plot of Conditional Regressions',
            deparse(formula)
        )
    )
}

