C_chi <- function(lst){
  ls2e(lst)
  tsimb <- "\\chi^2"
  ped <- paste0("_{",gdl,";",alpha,"}")
  
  round_all(4)
  tobs_s <- paste0("\\chi^2","_\\text{obs}=",tobs)
  tcrit  <- paste0("\\chi^2",ped,"=",tc)
  cat( "\n\n\\(\\fbox{C}\\) CONCLUSIONE \n\n",
       sep = "")
  if (length(alpha)!=1){
    conc <- switch(livello+1,
                   "0"=paste0("$",tobs_s,ifelse(h1=="<",">","<"),tcrit[1],"$, quindi **non** rifiuto $H_0$ a **nessun** livello di significatività, $p_\\text{value}>0.1$"),
                   "1"=paste0("$",min(tcrit[1:2]),"<",tobs_s,"<",max(tcrit[1:2]),"$, indecisione sul rifiuto di $H_0$ al 10%, $0.05<p_\\text{value}<0.1$,\n\n _marginalmente significativo_ $\\fbox{.}$."),
                   "2"=paste0("$",min(tcrit[2:3]),"<",tobs_s,"<",max(tcrit[2:3]),"$, quindi **rifiuto** $H_0$ al 5%, $0.01<p_\\text{value}<0.05$,\n\n _significativo_   $\\fbox{*}$."),
                   "3"=paste0("$",min(tcrit[3:4]),"<",tobs_s,"<",max(tcrit[3:4]),"$, quindi **rifiuto** $H_0$ all'1%, $0.001<p_\\text{value}<0.01$,\n\n _molto significativo_  $\\fbox{**}$."),
                   "4"=paste0("$",tobs_s,">",tcrit[4],"$, quindi **rifiuto** $H_0$ sotto all'1‰, $p_\\text{value}<0.001$,\n\n _estremamente significativo_ $\\fbox{***}$."),
    )
    
    cat("I valori critici sono\n\n",
        "$",paste0(tcrit,collapse="$; $"),"$\n\n", 
        "Siccome ",conc,"\n\n",sep = "")
  } else {
    cat("La siginficatitività è $\\alpha=", alpha,"$, dalle tavole osserviamo $",tcrit,"$.\n\n",
        "Essendo $",tobs_s,ifelse(tobs>tc,">","<"),tcrit,"$ allora ", ifelse(tobs > tc,"**rifiuto** $H_0$ ","**non** rifiuto $H_0$ "),"al ",alpha*100,"%.\n\n",
        sep="")
  }
}

graf_chi <- function(lst){
  ls2e(lst)
    d_distr <- function(x) dchisq(x,gdl)
    q_distr <- function(p) qchisq(p,gdl)
    p_distr <- function(q) pchisq(q,gdl)
    tsimb <- expression(chi^2)
    ysimb <- expression(f(chi2))
  round_all(4)
  
  tac <- c(0,tc,ceiling(q_distr(1-1/5000)))
  kk <- length(tac)
  curve(d_distr,from = tac[1],to = tac[kk],n = 1001,axes=F,xlab = tsimb,ylab = ysimb)
  axis(1,tac[-kk],round(tac[-kk],4),las=2)
  axis(2)
  segments(tac[-kk],0,tac[-kk],d_distr(tac[-kk]),lty=2)
  col_ <- colorRampPalette(rev(c("red","pink",iblue)))(5)
  if (length(alpha)==1) col_ <-c("blue","red")
  
  for (i in 1:(kk-1)){
    segments(tac[i],0,tac[i+1],0,col=col_[i],lwd=2)
  }
  points(tobs,0,pch=4,cex=2)
}

p_value_chi <- function(lst){
  ls2e(lst)
  d_distr <- function(x) dchisq(x,gdl)
  q_distr <- function(p) qchisq(p,gdl)
  p_distr <- function(q) pchisq(q,gdl)
  tsimb <- paste0("\\chi^2_{",gdl,"}")
  round_all(4)
  
  tobs2 <- round(tobs,2)
  p_val <- 1-p_distr(tobs2)
  pval <- paste0("P(",tsimb,">",tobs2,")=",p_val)
  
  alpha_c <- c(1,1/10,5/100,1/100,1/1000,0)
  signif_ <- max(which(alpha_c>p_val))
  

    cat("\n\n Il \\(p_{\\text{value}}\\) è \n\n $$ p_{\\text{value}} =", pval,"$$\n\n")
  cat("Attenzione il calcolo del $p_\\text{value}$ con la distribuzione $\\chi^2$ è puramente illustrativo e non può essere riprodotto senza una calcolatrice statistica adeguata.")
  cat("\\[\n",alpha_c[signif_+1],"\\leq p_\\text{value}=",p_val, "<",alpha_c[signif_],"\n\\]")
}




#' Test e Visualizzazione del Chi Quadrato
#'
#' Fornisce metodi per visualizzare tabelle di contingenza e per eseguire il test del chi quadrato per indipendenza e conformità.
#'
#' @param dat Matrice o data frame contenente i dati per `chi_print` e `chi_test`.
#' @param nome_x, nome_y Nomi delle variabili o delle categorie per `chi_print`.
#' @param print Se `TRUE`, stampa la tabella di contingenza per `chi_print`.
#' @param Freq_c,  Frequenze osservate e attese per `chi_print_conf` e `chi_conf`.
#' @param Freq_0,  attese
#' @param X, Nomi delle variabili o delle categorie per `chi_print_conf` e `chi_conf`.
#' @param Y, nomi Y
#' @param alpha Livello di significatività per `chi_test` e `chi_conf`.
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
#' dat <- matrix(c(15,5,12,10,35,15),2,byrow = TRUE)
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
#' Freqc <- sample(1:25,6,TRUE)
#' Freq0 <- (numeric(6)+1/6)*100
#' chi_print_conf(Freqc,Freq0,X = letters[1:6],Y = letters[1:2])
#' chi_conf(Freqc,Freq0,X = letters[1:6],Y = letters[1:2])

#' @rdname chi-quadrato

chi_print <- function(dat,nome_x,nome_y,print=TRUE){
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
chi_test <- function(dat,alpha=c(1/10,5/100,1/100,1/1000)){
  
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
  
  livello <- max(which(chi_ob > chi_th))

cat("
  $$
    \\chi^2_{obs}=",chi_ob,"
  $$
  \n
  i $gdl$
  \n
  $$
    (",n,"-1)\\times(",m,"-1)=",gdl,"
  $$"
)
  lst <- list(alpha = alpha, tobs=chi_ob, tc = chi_th, h1 = ">",gdl = gdl,livello = livello)
  C_chi(lst)
  graf_chi(lst)
  p_value_chi(lst)
}

#' @rdname chi-quadrato
chi_conf <- function(Freq_c,Freq_0,X,Y,alpha=c(1/10,5/100,1/100,1/1000)){
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
  
cat("
  $$
    \\chi^2_{obs}=",chi_ob,"
  $$
  \n
  i $gdl$
  \n
  $$
    (",k,"-1)=",gdl,"
  $$"
)
  
  livello <- max(which(chi_ob > chi_th))
  lst <- list(alpha = alpha, tobs=chi_ob, tc = chi_th, h1 = ">",gdl = gdl,livello = livello)
  C_chi(lst)
  graf_chi(lst)
  p_value_chi(lst)
  
}
