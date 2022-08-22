\name{traceplots}
\alias{traceplots}
\alias{blimp_tp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Generates a list of traceplot objects to be plotted for rblimp}
\description{
This creates a list of blimp_tp object which can be used with plot to create traceplots
}
\usage{
    traceplots(object)
    plot(blimp_tp, colors=NULL,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{object}{ A blimp_obj to generate traceplots for.}
\item{colors}{ A list of colors to be used for each traceplot line.}
\item{...}{ Arguements that are passed to plot function}
}
\details{ }
\value{ returns blimp_obj }
\references{}
\author{Brian Keller}
\seealso{}
\examples{
mydata <- data.frame(x=rnorm(1000),y=rnorm(1000))

mdl <- rblimp('y <- x*2; f1 <- 1',
       mydata,
       seed=3927,
       nimps = 2,
       latent = 'f1',
       center = list(cgm = c('x')),
       fixed = 'x')

plots <- traceplots(mdl)
plot(plots[[1]], colors=c('red','blue'))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ }
