\name{blimp_obj}
\alias{blimp_obj}
\alias{algorithm}
\alias{datainfo}
\alias{modelinfo}
\alias{psr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Output object from rblimp }
\description{
This is the output object and methods for rblimp
}
\usage{
output(blimp_obj)
psr(blimp_obj)
datainfo(blimp_obj)
modelinfo(blimp_obj)
modelfit(blimp_obj)
algorithm(blimp_obj)
summary(blimp_obj, ...)


}
%- maybe also 'usage' for other objects documented here.
\arguments{ }
\details{
    Helper functions to extract information from blimp_obj. Some require saving of output instead of printing.
}
\value{  }
\references{}
\author{Brian Keller}
\seealso{}
\examples{
# Generate data
mydata <- data.frame(x = rnorm(1000), y = rnorm(1000))

# Nonsensical model
mdl <- rblimp(
    c(
        'y <- x*2',
        'f1 <- 1'
    ),
    mydata,
    seed = 3927,
    nimps = 2,
    latent = ~ f1,
    center = cgm ~ x,
    fixed = ~ x
)

# Prints psr
psr(mdl)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ }


