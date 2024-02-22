\name{stat_base}
\alias{stat_base}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Crea un tabella per divisioni in classi e le frquenze
}
\description{
Crea un lista di oggetti e di funzioni utili per il libro
}
\usage{
stat_base(samp,brk)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{samp}{ il campione, numerico }
  \item{brk} {vettore di breaks}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{dat2 }{tabella con intestazioni brevi}
  \item{dat3 }{tabella con intestazioni LaTeX}
  \item{F_print}{funzione per calcolo area}
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

brk <- c(0, 1.5, 3, 5, 10)
nnn <- c(10,20,5,2)
n <- 160

samp <- genera_dati(brk,n=n,nnn=nnn)

st <- stat_base(samp,brk)

st$histp(T)
st$h.int(1.5,3.2,density=20)
st$F_print(3.2,"<")

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
