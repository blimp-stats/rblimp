\name{compare}
\alias{compare}
\alias{blimp_cp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Generates a blimp compare object to compare two models}
\description{
This creates a list of blimp_tp object which can be used with plot to create traceplots
}
\usage{
    compare(model0, model, use = "mean", greaterThan = TRUE,
        suffixes = c(
            "R2: Coefficients",
            "R2: Level-2 Random Intercepts", "R2: Level-2 Random Slopes",
            "R2: Level-3 Random Slopes", "R2: Level-3 Random Intercepts",
            "R2: Residual Variation", "R2: Level-1 Residual Variation"
        )
    )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{model0}{ Evaluates how many of the iterations are above the cutpoint obtained from model.}
\item{model}{ The model evaluated to get a cut point}
\item{use}{A character, numeric value, function, or list of all of them. If a character, it will recognize 'mean' and 'median', otherwise it will be converted to a number. If a numeric value less than 1 it will act as a proportion for the quantile funciton. If it is a numeric value greater than 1 it will convert it to a proportion out of 100 and supply as a quantile. If a function, it will apply that function. Use a list to have multiple columns.}
\item{greaterThan}{Logical. If true we look greater than value of model0}
\item{suffix}{ Defaults to all possible R-squared values. Can compare other parameters by matching the parameter names in the iteration output from blimp_obj}
}
\details{ Note, due to restrictions in R, lists of functions will not give very useful printed names. }
\value{ returns blimp_cp }
\references{}
\author{Brian Keller}
\seealso{blimp_obj}
% \examples{
%
% mydata <- na.omit(dropout[,c('math7','math8','math10','mothed')])
% mdl0 <- rblimp('math10 <- 1',
%        mydata, seed=3927,
%        fixed = 'math7')
%
% mdl <- rblimp('math10 <- 1 math7',
%        mydata, seed=3927,
%        fixed = 'math7')
% compare(mdl0, mdl)
%
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ }
