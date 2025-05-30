% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jn_plot.R
\name{jn_plot_func}
\alias{jn_plot_func}
\title{Function to generate a Johnson-Neyman Plot of Conditional Slope based on a function to produce the conditional effect.}
\usage{
jn_plot_func(func, xrange, ci = 0.95)
}
\arguments{
\item{func}{a \code{\link{function}} used to compute the conditional effect on moderator.}

\item{xrange}{a \code{\link{numeric}} of length two with the min and max of the x-axis}

\item{ci}{a value between 0 and 1 specifying the credible interval size}
}
\value{
a \code{\link[ggplot2:ggplot]{ggplot2::ggplot}} plot
}
\description{
Generates a Johnson-Neyman Plot using a function to produce the conditional effect
}
\details{
To change colors use ggplot2's scale system. Both fill and color are used. See
\code{\link[ggplot2:aes_colour_fill_alpha]{ggplot2::aes_colour_fill_alpha}} for more information about setting a manual set of colors.
}
\examples{
\dontshow{if (has_blimp()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# set seed
set.seed(981273)

# Generate Data
mydata <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100),
    m = rnorm(100)
)
mydata$y <- with(
    mydata,
    rnorm(100,
          10 + x1*0.5 + x2*0.5 + m + .2*x1*x2 + .3*x2*m + .1*x1*m + .7*x1*x2*m,
          1
    )
)

# Run Rblimp
m1 <- rblimp(
    'y ~ x1 x2 m x1*x2 x1*m x2*m x1*x2*m',
    mydata,
    center = ~ m,
    seed = 10972,
    burn = 1000,
    iter = 1000
)

# Get parameter values
params <- m1 |> as.matrix()

# Generate Plot
(
    jn_plot_func(
        compute_condeff(params[,6], params[,9]),
        xrange = c(-3, 3)
    )
    + ggplot2::labs(
        title = 'Johnson-Neyman Plot for `x1` * `x2` Moderated by `x2`',
        subtitle = 'Red area represents 0 within 95\% interval',
        y = 'y ~ x1 * x2',
        x = 'm'
    )
    + ggplot2::theme_minimal()
)
\dontshow{\}) # examplesIf}
}
\seealso{
\code{\link[=compute_condeff]{compute_condeff()}}
}
