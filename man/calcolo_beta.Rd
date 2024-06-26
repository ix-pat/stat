% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Inferenza.R
\name{calcolo_beta}
\alias{calcolo_beta}
\title{Stampa i Coefficienti di Regressione in LaTeX}
\usage{
calcolo_beta(semplice = F, inv = F)
}
\arguments{
\item{semplice}{Un valore booleano che, se TRUE, produce una stampa semplificata contenente solo i
coefficienti di regressione. Se FALSE, produce un output dettagliato con tutti i
calcoli intermedi. Il default è FALSE.}

\item{inv}{Un valore booleano che, se TRUE, inverte i ruoli di X e Y nel calcolo della regressione.
Il default è FALSE.}
}
\value{
La funzione non ritorna un valore ma stampa direttamente l'output in formato LaTeX
        nell'ambiente di chiamata.
}
\description{
Questa funzione genera l'output LaTeX per il calcolo dei coefficienti di regressione lineare,
includendo i coefficienti \eqn{\hat{\beta_1}} e \eqn{\hat{\beta_0}} \eqn{\hat{\alpha_1}} e \eqn{\hat{\alpha_0}} se viene
utilizzato il calcolo inverso) basati sui dati forniti. La funzione permette di scegliere tra
una stampa dettagliata dei calcoli intermedi o una più semplice che include solo i coefficienti.
}
\examples{
x <- rnorm(100)
y <- x + rnorm(100,0,.1)
ls2e(regr(x,y))
# Calcolo normale con output dettagliato
calcolo_beta(semplice = FALSE)

# Calcolo invertito con output semplice
calcolo_beta(semplice = TRUE, inv = TRUE)

}
