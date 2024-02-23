
#' Test e Visualizzazione del Chi Quadrato
#'
#' Fornisce metodi per visualizzare tabelle di contingenza e per eseguire il test del chi quadrato per indipendenza e conformità.
#'
#' @param dat Matrice o data frame contenente i dati per `chi_print` e `chi_test`.
#' @param nome_x, nome_y Nomi delle variabili o delle categorie per `chi_print`.
#' @param print Se `TRUE`, stampa la tabella di contingenza per `chi_print`.
#' @param Freq_c, Freq_0 Frequenze osservate e attese per `chi_print_conf` e `chi_conf`.
#' @param X, Y Nomi delle variabili o delle categorie per `chi_print_conf` e `chi_conf`.
#' @param alpha Livello di significatività per `chi_test` e `chi_conf`.
#' @param mu1, mu2 Medie campionarie per i test su due campioni.
#' @param s1h, s2h Deviazioni standard campionarie per i test su due campioni.
#' @param n1, n2 Dimensioni dei campioni per i test su due campioni.
#' @param h1 Ipotesi alternativa: può essere "\\neq", ">", "<".
#' @param et Specifica se il test assume varianze eterogenee (`TRUE`) o omogenee (`FALSE`).
#' @param a, b Etichette per i gruppi, utili per l'output di `test_2c`.
#' @param um Unità di misura (opzionale), utile per migliorare la leggibilità dell'output di `test_2c`.
#'
#' @details
#' Le funzioni fornite includono:
#' \itemize{
#'   \item \code{chi_print}: Visualizza una tabella di contingenza arricchita con i totali per righe e colonne.
#'   \item \code{chi_print_conf}: Visualizza una tabella di confronto tra frequenze osservate e attese, utile per l'analisi di conformità.
#'   \item \code{chi_test}: Esegue il test del chi quadrato per indipendenza su una tabella di contingenza, valutando l'associazione tra due variabili categoriche.
#'   \item \code{chi_conf}: Esegue il test del chi quadrato per conformità, confrontando le frequenze osservate con quelle attese basate su una distribuzione specificata.
#' }
#'
#' Queste funzioni sono progettate per facilitare l'analisi statistica di dati categorici, permettendo agli utenti di eseguire test di ipotesi e visualizzare i risultati in modo intuitivo.
#' @examples
#' 
#' dat <- matrix(c(15,5,12,10,35,15),2,byrow = T)
#' 
#' nome_x<- c("Rai","Mediaset","La7")
#' nome_y <- c("Laureato","Non Laureato")
#' 
#' dat          <- chi_print(dat,nome_x = nome_x,nome_y = nome_y)[[1]]
#' dat_print    <- chi_print(dat,nome_x = nome_x,nome_y = nome_y)[[2]]
#' 
#' tabl(dat_print)
#' 
#' cat(chi_test(dat = dat,alpha = 0.01))
#' 
#' fx <- sample
#' Freqc <- sample(1:25,6,T)
#' Freq0 <- (numeric(6)+1/6)*100
#' chi_print_conf(Freqc,Freq0,X = letters[1:6],Y = letters[1:2])
#' chi_conf(Freqc,Freq0,X = letters[1:6],Y = letters[1:2])

#' @rdname chi-quadrato

chi_print <- function(dat,nome_x,nome_y,print=T){
  dat_print <- cbind(dat,rowSums(dat))
  dat_print <- rbind(dat_print,colSums(dat_print))
  dimnames(dat)[[1]] <- c(nome_y)
  dimnames(dat)[[2]] <- c(nome_x)
  dimnames(dat_print)[[1]] <- c(nome_y,"Tot")
  dimnames(dat_print)[[2]] <- c(nome_x,"Tot")
  if (print) {
    kable(dat,digits = 4,row.names = T) %>%
      kable_styling(full_width = F) %>%
      column_spec(column = 1,bold = T)
  }
  return(list(dat,dat_print))
}

#' @rdname chi-quadrato
chi_print_conf <- function(Freq_c,Freq_0,X,Y){
  Freq_0 <- round(Freq_0,2)
  S <- c(paste("$",Freq_c,"$",sep = ""), paste("$",sum(Freq_c),"$",sep = ""))
  N <- c(paste("$",Freq_0,"\\%$",sep=""),"$100\\%$")
  D <- data.frame(rbind(S, N), row.names = Y)
  names(D) <- c(X,"Totale")
  tabl(D,digits=4)
}

#' @rdname chi-quadrato
chi_test <- function(dat,alpha){
  
  n <- dim(dat)[1]
  m <- dim(dat)[2]
  gdl <- (n-1)*(m-1)
  chi_th <- round(qchisq(1-alpha,gdl),4)
  thr <- t(outer(colSums(dat),rowSums(dat))/sum(dat))
  # colnames(thr) <- nome_x
  # row.names(thr) <- nome_y
  chi_ob <- sum((thr-dat)^2/thr)
  segno <- ifelse(chi_ob>chi_th,">","<")
  decis <- ifelse(chi_ob>chi_th,"rifiuto","non rifiuto")
  
  cat("**Test $\\chi^2$ per indipendenza**\n\n")  
  cat("$\\fbox{A}$ FORMULAZIONE DELLE IPOTESI
$$
\\Big\\{H_0:\\pi_{ij}=\\pi_{i\\bullet}\\pi_{\\bullet j}
$$
$\\fbox{B}$ SCELTA E CALCOLO STATISTICA-TEST, $\\chi^2$\n\n
Si usa il test $\\chi^2$, si crea la tabella delle frequenze teoriche
$$
n_{ij}^*=\\frac{n_{i\\bullet}n_{\\bullet j}}{n}
$$
")
  
  cat(tabl(thr,digits = 3,row.names = T)  %>%
    column_spec(column = 1,bold = T))
  
  cat("
La tabella delle distanze
$$
\\frac{(n_{ij}-n_{ij}^*)^2}{n_{ij}^*}
$$"
  )
  
  cat(tabl((thr-dat)^2/thr,digits = 3,row.names = T) %>%
    column_spec(column = 1,bold = T))
  
  
  cat("$\\fbox{C}$ DECISIONE
$$
\\chi^2_{obs}=",chi_ob,"
$$
\n
i $gdl$
\n
$$
(",n,"-1)\\times(",m,"-1)=",gdl,"
$$
$\\alpha=",alpha,"$ e quindi $\\chi_{1,",alpha,"}^2=",chi_th,"$
\n\n
Essendo 
$$
\\chi^2_{obs}=",chi_ob," ",segno,"\\chi_{1,",alpha,"}^2=",chi_th,"
$$
\n
allora ",decis," $H_0$ al lds dell'",alpha*100," percento. 
\n\n
**Graficamente**",sep="")
  
  
  R <- c(chi_th,100); A <- c(0,chi_th)
  b <- qchisq(.9999,gdl)
  curve(dchisq(x,gdl),0,b,axes=F,xlab="T",ylab="")
  lines(c(0,chi_th),c(0,0),col=4,lwd=2)
  lines(c(chi_th,b),c(0,0),col=2,lwd=2)
  points(chi_ob,0,pch=4,cex=2)
  text(chi_ob,.05,expression(chi[obs]^2))
  axis(1,c(0,chi_th,round(b,0)))
  
  
  cat("
\n\n il $p_{\\text{value}}$ è
\\[
P(\\chi^2_{",gdl,"}> \\chi^2_{\\text{obs}})=",format((1-pchisq(chi_ob,gdl)),digits = 4,scientific = 8),"
\\]
")
}

#' @rdname chi-quadrato
chi_conf <- function(Freq_c,Freq_0,X,Y,alpha=0.05){
  Freq_0 <- round(Freq_0,2)
  n <- sum(Freq_c)
  S <- c(Freq_c,sum(Freq_c))
  N <- c(Freq_0/100,1)
  
  ns <- c(Freq_0/100*n,n)
  k  <- length(Freq_0)
  ch <- round((Freq_c-ns[1:k])^2/ns[1:k],4)
  ch <- c(ch,sum(ch))
  D1 <- data.frame(rbind(S,N,ns,ch),row.names = c(Y,"$n_j^*$","$\\chi^2$"))
  names(D1) <- c(X,"Tot")
  
  gdl <- (k-1)
  chi_th <- round(qchisq(1-alpha,gdl),gdl)
  # colnames(thr) <- nome_x
  # row.names(thr) <- nome_y
  chi_ob <- ch[k+1]
  segno <- ifelse(chi_ob>chi_th,"maggiore","minore")
  decis <- ifelse(chi_ob>chi_th,"rifiuto","non rifiuto")
  
  cat("**Test $\\chi^2$ per conformità**\n\n")  
  
  cat("$\\fbox{A}$ Formulazione delle ipotesi
\\[
\\{H_0:\\pi_\\text{",Y[1],"}= \\pi_\\text{",Y[2],"},~~\\forall j
\\]
$\\fbox{B}$ Scelta e calcolo della statistica test.

Si tratta di un test  chi quadro di conformità.
\\[
n^*_j = n\\cdot \\pi^*_{\\text{",Y[2],"},j} 
\\]\n\n")
  
  cat("La tabella delle distanze:\n\n")
  cat(tabl(D1))
  
  chi <- qchisq(c(.95,.99),gdl)
  
  cat("$\\fbox{C}$ Decisione \n\n
Il chi quadro osservato è ",ch[k+1]," è ",segno," di $\\chi^2_{",k-1,";",alpha,"}=",chi_th,"$, e quindi **",decis,"** $H_0$, al livello di significatività del ",alpha*100,"$\\%$. 
\n\n
**Graficamente**",sep="")
  
  R <- c(chi_th,100); A <- c(0,chi_th)
  b <- qchisq(.9999,gdl)
  curve(dchisq(x,gdl),0,b,axes=F,xlab="T",ylab="")
  lines(c(0,chi_th),c(0,0),col=4,lwd=2)
  lines(c(chi_th,b),c(0,0),col=2,lwd=2)
  points(chi_ob,0,pch=4,cex=2)
  text(chi_ob,.05,expression(chi[obs]^2))
  axis(1,c(0,chi_th,round(b,0)))
  cat("\n\n il $p_{\\text{value}}$ è
\\[
P(\\chi^2_{",gdl,"}> \\chi^2_{\\text{obs}})=",format((1-pchisq(chi_ob,gdl)),digits = 4,scientific = 8),"
\\]
") 
}
