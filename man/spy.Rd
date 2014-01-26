\name{spy}
\alias{spy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function to do ...
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
spy(class_name, methods = list())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{class_name}{
%%     ~~Describe \code{class_name} here~~
}
  \item{methods}{
%%     ~~Describe \code{methods} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (class_name, methods = list()) 
{
    setRefClass(generate_class_name(class_name), contains = c(eval(class_name), 
        "Mocker"), methods = methods)$new()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
