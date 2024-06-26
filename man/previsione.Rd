% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Inferenza.R
\name{previsione}
\alias{previsione}
\title{Stampa la Previsione di Y Data un Valore X in LaTeX}
\usage{
previsione(x)
}
\arguments{
\item{x}{Il valore di \(x\) per cui si desidera calcolare la previsione di \(y\).}
}
\value{
La funzione non ritorna un valore ma stampa direttamente l'output in formato LaTeX
        nell'ambiente di chiamata.
}
\description{
Questa funzione calcola e stampa l'equazione di previsione per un valore \(x\) dato,
utilizzando i coefficienti di regressione lineare \eqn{\hat\beta_0} e \eqn{\hat\beta_1}.
L'output è in formato LaTeX e mostra l'equazione di previsione completa, includendo i calcoli
effettivi per il valore specificato di \(x\).
}
\details{
Si assume che i coefficienti \eqn{\hat\beta_0} e \eqn{\hat\beta_1} siano definiti
         e accessibili nello scope da cui la funzione viene chiamata. La funzione controlla se
         \eqn{\hat\beta_1} è negativo, ma non esegue operazioni specifiche in tal caso; questa
         condizione può essere espansa per gestire scenari specifici.
}
\examples{
x <- rnorm(100)
y <- x + rnorm(100,0,.1)
ls2e(regr(x,y))
# Calcola la previsione per x = 10
previsione(10)

}
