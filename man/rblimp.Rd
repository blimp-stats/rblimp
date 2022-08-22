\name{rblimp}
\alias{rblimp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A wrapper for blimp to run it via R. }
\description{
This is a wrapper to provide blimp funcitons in R. This will generate the input, run the script, and load most the saved data into an R object.
}
\usage{
    rblimp(model, data, burn = 1000, iter = 1000,
        seed, thin,  nimps, latent,
        clusterid, parameters, ordinal,
        nominal, center, parameters,
        chains, simple, waldtest,
        options, transform,
        tmpfolder, fixed,
        syntax = FALSE, print_output = TRUE,
        nopowershell = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{model}{ The Blimp Model. See Blimp User manua for syntax}
\item{data}{  A data frame to be be used by blimp}
\item{burn}{  Number of burn in iterations}
\item{iter}{  Number of iterations}
\item{seed}{  Random number generator seed}
\item{thin}{  Number of thin}
\item{nimps}{ Number of imputations}
\item{latent}{ Any latent variables to be generated}
\item{parameters} Any post-computed parameters to be saved}
\item{clusterid}{ Cluster identifier variable}
\item{ordinal}{ Ordinal variables}
\item{nominal}{ Nominal variables}
\item{center}{ Centered variables}
\item{parameters}{ Parameters command}
\item{chains}{ Chains command}
\item{simple}{ Simpple command}
\item{waldtest}{ Walte test command}
\item{options}{ Options command}
\item{transform}{ Transform command}
\item{tmpfolder}{ Alternate temp directory}
\item{fixed}{  Fixed variables}
\item{syntax}{ Bool, return syntax instead of running}
\item{print_output}{ The type of output printed to the console. 'iteration' or logical TRUE is only iteration history, 'none' or logical FALSE suppresses all output to console, and 'all' prints all output to console. Output is always saved in blimp_obj@output }
\item{nopowershell}{ Windows only. Uses cmd.exe with some limited functions (instead of powershell).}
}
\details{
    This includes a preview release of blimp.
    See Blimp User Manual for more details.
}
\value{ returns blimp_obj }
\references{}
\author{Brian Keller}
\seealso{blimp_obj}
\examples{
mydata <- data.frame(x=rnorm(1000),y=rnorm(1000))

mdl <- rblimp(
    c(
        'y ~ x*2;',
        'f1 ~ 1'
    ),
    mydata,
    seed=3927,
    nimps = 2,
    latent = 'f1',
    center = list(cgm = c('x')),
    fixed = 'x')
summary(mdl)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ }
