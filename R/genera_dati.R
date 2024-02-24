#' Genera Dati
#'
#' Questa funzione genera un insieme di dati basato sui parametri forniti. Può generare dati casuali o uniformi
#' in base all'intervallo specificato da `brk` e, opzionalmente, basato su `hhh` e `nnn`.
#'
#' @param brk Un vettore numerico che specifica gli intervalli per la generazione dei dati.
#' @param hhh Un valore opzionale utilizzato per calcolare `nnn` se quest'ultimo non è fornito.
#' @param n Il numero totale di dati da generare.
#' @param nnn Un vettore opzionale che specifica il numero di dati da generare per ogni intervallo specificato in `brk`.
#'           Se `NULL`, viene calcolato automaticamente basandosi su `hhh`.
#' @param rand Un booleano che, se `TRUE`, genera dati casuali; altrimenti, genera dati uniformi.
#' @return Un vettore contenente i dati generati.
#' @examples
#' brk <- c(1, 5, 10)
#' hhh <- c(2,3)
#' n <- 100
#' dati_generati <- genera_dati(brk, hhh, n)
#' print(dati_generati)
#' @rdname generatore-dati
#' 
genera_dati <- function(brk,hhh=NULL,n,nnn=NULL,rand = T){     # genera i dati fornendo brk, hhh, e n
  if (is.null(nnn))  nnn  <- round((hhh*diff(brk))/sum(hhh*diff(brk))*n) # riporto ad n
  if (!is.null(nnn)) nnn  <- round(nnn/sum(nnn)*n) # riporto ad n
  k <- length(brk)-1
  br1  <- brk[-(k+1)]
  br2  <- brk[-1]
  if (rand) {
    samp <- round(vunif(nnn,brk),2)
  } else {
    samp <- rep((br1+br2)/2,times = nnn)
  }
  #  names(samp) <- nomex
  if (length(samp)!=n) samp <- c(samp,rev(samp))[1:n]
  return(samp)
}

#' @rdname generatore-dati
vunif <- function(nnn, brk){           # genera i dati da una mistura di uniformi
  k <- length(brk)-1
  br1  <- brk[-(k+1)]
  br2  <- brk[-1]
  xi  <- runif(nnn[1],br1[1],br2[1])
  for (i in 2:k)
    xi <- c(xi,runif(nnn[i],br1[i],br2[i]))
  return(xi)
}

