\name{exportable}
\alias{exportable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
test
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
exportable(data, name = "data.txt", row.names = F, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{name}{
%%     ~~Describe \code{name} here~~
}
  \item{row.names}{
%%     ~~Describe \code{row.names} here~~
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
function (data, name = "data.txt", row.names = F, ...) 
{
    if (class(data) == "table") 
        data <- as.data.frame.matrix(data)
    if (length(unlist(strsplit(name, "\\."))) == 1) {
        name <- paste(name, ".txt", sep = "")
    }
    ext <- tolower(unlist(strsplit(name, "\\."))[length(unlist(strsplit(name, 
        "\\.")))])
    if (!ext \%in\% c("txt", "csv", "xlsx", "xls", "psv")) 
        stop("No posible extension")
    if (ext == "txt") 
        write.table(data, name, dec = ".", sep = "\t", col.names = TRUE, 
            quote = FALSE, row.names = row.names, ...)
    if (ext == "csv") 
        write.table(data, name, dec = ",", sep = ";", col.names = TRUE, 
            quote = FALSE, row.names = row.names, ...)
    if (ext == "psv") 
        write.table(data, name, dec = ".", sep = "|", col.names = TRUE, 
            quote = FALSE, row.names = row.names, ...)
    if (ext \%in\% c("xlsx", "xls")) {
        library(xlsx)
        wb <- createWorkbook()
        style1 <- CellStyle(wb) + Font(wb, isBold = TRUE) + Border()
        addDataFrame(data, createSheet(wb), colnamesStyle = style1, 
            row.names = row.names)
        saveWorkbook(wb, name)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
