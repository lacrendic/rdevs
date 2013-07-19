\name{readtable}
\alias{readtable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
test
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.table(files, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{files}{
%%     ~~Describe \code{files} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (files, ...) 
{
    library(plyr)
    read.table.aux <- function(name, ...) {
        ext <- tolower(unlist(strsplit(name, "\\."))[length(unlist(strsplit(name, 
            "\\.")))])
        ext_support <- c("txt", "csv", "xlsx", "xls", "dbf", 
            "sas7bdat", "sav", "tsv", "dat", "psv")
        if (!ext \%in\% ext_support) {
            stop("No posible format (extension)")
        }
        if (ext \%in\% c("txt", "tsv", "dat")) 
            return(read.table(name, sep = "\t", h = T, comment.char = "", 
                quote = "", ...))
        if (ext == "csv") 
            return(read.table(name, sep = ";", h = T, comment.char = "", 
                quote = "", dec = ",", ...))
        if (ext == "psv") 
            return(read.table(name, sep = "|", h = T, comment.char = "", 
                quote = "", dec = ",", ...))
        if (ext == "sav") {
            library(foreign)
            return(read.spss(name, to.data.frame = T))
        }
        if (ext == "sas7bdat") {
            library(sas7bdat)
            return(read.sas7bdat(name))
        }
        if (ext \%in\% c("xlsx", "xls")) {
            library(xlsx)
            return(read.xlsx(name, sheetIndex = 1, ...))
        }
        if (ext == "dbf") {
            library(foreign)
            write.table(read.dbf(name, as.is = TRUE), "data.txt", 
                quote = F, row.names = FALSE, col.names = TRUE, 
                append = FALSE, sep = "\t")
            write.table(read.table("data.txt", sep = "\t", h = T, 
                dec = ","), "data.txt", dec = ".", sep = "\t", 
                col.names = TRUE, quote = FALSE, row.names = F)
            d <- read.table("data.txt", h = T, sep = "\t", stringsAsFactors = F)
            shell("del data.txt")
            if (any(names(d) == "RUT")) {
                if (is.numeric(d$RUT)) {
                  d$RUT <- laply(d$RUT, RUT_to_RUT10)
                }
            }
            return(d)
        }
    }
    ldply(files, function(f) read.table.aux(f, ...), .progress = "text")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
