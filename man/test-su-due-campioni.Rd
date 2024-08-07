% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Inferenza.R
\name{test_2c}
\alias{test_2c}
\alias{ttest_2c_et}
\alias{ttest_2c_om}
\alias{ztest_2c_pi}
\title{Test su Due Campioni: Proporzioni e Medie}
\usage{
test_2c(
  mu1,
  mu2,
  s1h = F,
  s2h = F,
  n1,
  n2,
  h1 = "\\\\neq",
  et = F,
  a = "A",
  b = "B",
  um = "",
  alpha = c(1/10, 5/100, 1/100, 1/1000),
  pv_only = FALSE,
  rbow = F
)

ttest_2c_et(
  mu1,
  mu2,
  s1h,
  s2h,
  n1,
  n2,
  h1 = "\\\\neq",
  a = "1",
  b = "2",
  um = "",
  alpha = c(1/10, 5/100, 1/100, 1/1000),
  pv_only = FALSE,
  rbow = FALSE
)

ttest_2c_om(
  mu1,
  mu2,
  s1h,
  s2h,
  n1,
  n2,
  h1 = "\\\\neq",
  a = "1",
  b = "2",
  um = "",
  alpha = c(1/10, 5/100, 1/100, 1/1000),
  pv_only = FALSE,
  rbow = FALSE
)

ztest_2c_pi(
  s1,
  s2,
  n1,
  n2,
  h1 = "\\\\neq",
  a = "1",
  b = "2",
  alpha = c(1/10, 5/100, 1/100, 1/1000),
  pv_only = TRUE,
  rbow = FALSE
)
}
\arguments{
\item{mu1, }{mu2 Medie campionarie o numeri di successi nei campioni per test su proporzioni.}

\item{s1h, }{s2h Deviazioni standard campionarie per i due campioni per test su medie.}

\item{n1, }{n2 Dimensioni dei campioni.}

\item{h1}{Ipotesi alternativa: può essere "\\neq", ">", "<".}

\item{et}{Specifica se il test su medie assume varianze eterogenee (`TRUE`) o omogenee (`FALSE`).}

\item{a, }{b Etichette per i gruppi, utili per l'output.}

\item{um}{Unità di misura (opzionale), utile per migliorare la leggibilità dell'output.}

\item{alpha}{Livello di significatività.}
}
\description{
Fornisce metodi per eseguire test statistici su due campioni, valutando differenze nelle proporzioni o nelle medie.
}
\details{
\itemize{
\item \code{ztest_2c_pi} applica un test z su due proporzioni.
\item \code{ttest_2c_et} e \code{ttest_2c_om} eseguono test t per due medie, rispettivamente con varianze eterogenee e omogenee.
\item \code{test_2c} è una funzione interna utilizzata per implementare la logica comune ai test.
}
}
\examples{
# Test Z per due proporzioni
ztest_2c_pi(s1 = 30, s2 = 45, n1 = 100, n2 = 150, h1 = "\\\\neq")

# Test T per due medie con varianze eterogenee
ttest_2c_et(mu1 = 5.1, mu2 = 5.8, s1h = 1.5, s2h = 1.7, n1 = 30, n2 = 30, h1 = "\\\\neq")

# Test T per due medie con varianze omogenee
ttest_2c_om(mu1 = 5.1, mu2 = 5.8, s1h = 1.5, s2h = 1.7, n1 = 30, n2 = 30, h1 = "\\\\neq")

}
