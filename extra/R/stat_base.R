#' Calcoli Statistici: Varianza e Deviazione Standard
#'
#' Queste funzioni forniscono il calcolo della varianza e della deviazione standard per una popolazione,
#' e una funzione aggiuntiva per il calcolo della varianza quando si dispone di una distribuzione di probabilità.
#'
#' @details La funzione \code{s2c} calcola la varianza di una popolazione. La funzione \code{sc}
#' calcola la deviazione standard di una popolazione. La funzione \code{vvv} calcola la varianza
#' per una distribuzione di probabilità o per dati grezzi se le probabilità non sono fornite.
#'
#' @param x Un vettore di valori numerici.
#' @param p Un vettore opzionale di probabilità associato a \code{x}.
#'
#' @return
#' \code{s2c} e \code{vvv} restituiscono la varianza calcolata.
#' \code{sc} restituisce la deviazione standard calcolata.
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' p <- c(0.1, 0.2, 0.3, 0.2, 0.2)
#' s2c(x)
#' sc(x)
#' vvv(x, p)
#'
#' @rdname calcoli-varianza
s2c <- function(x) {
  (mean(x^2) - mean(x)^2)  # varianza di popolazione
}

#' @rdname calcoli-varianza
sc <- function(x) {
  sqrt(s2c(x))  # sd di popolazione
}

#' @rdname calcoli-varianza
vvv <- function(x, p = NULL) {
  # varianza per distribuzione di probabilità
  if (is.null(p)) v <- mean(x^2) - mean(x)^2
  else v <- sum(p*x^2) - (sum(p*x))^2
  return(v)
}
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

#' Stat Base - Analisi Statistica di Base
#'
#' Questa funzione esegue un'analisi statistica di base sui dati campionari forniti e ritorna
#' un insieme di funzioni e statistiche dettagliate. La funzione `F_print` inclusa nell'output
#' permette di stampare risultati specifici dell'analisi.
#'
#' @param samp Un vettore di dati numerici campionari generati dalla funzione `genera_dati`.
#' @param brk Un vettore numerico che specifica gli intervalli per la categorizzazione dei dati campionari.
#'
#' @return Un elenco contenente le seguenti componenti:
#'   \itemize{
#'     \item \code{dat2}: Un data frame con statistiche di base per ogni intervallo.
#'     \item \code{dat3}: Un data frame simile a \code{dat2} ma include anche i totali di colonna.
#'     \item \code{F_print(x, verso, x2)}: calcola l'area tra \code{x} e \code{x2}.
#'     \item \code{histp(axes = F)}: genera l'istogramma delle densità percentuali.
#'     \item \code{h.int(x1, x2, density = 20, ...)}: aggiunge l'area tratteggiata tra \code{x1} e \code{x2}.
#'     \item \code{percentile(p)}: calcola il percentile \code{p}.
#'     \item Altre funzioni per analisi statistiche dettagliate (\code{F.int}, \code{H.int}, \code{Q.int}).
#'   }
#'
#' @examples
#' brk <- c(0, 10, 20, 30, 40)
#' hhh <- c( 2, 4,  2,   1)
#' samp <- genera_dati(brk, hhh=hhh, n=100)
#' risultati <- stat_base(samp, brk)
#' risultati$F_print(x=15, verso="<")
#' risultati$histp(TRUE)
#' risultati$h.int(5,27,density=20)
#' risultati$percentile(.75)
#' @export


stat_base <- function(samp,brk){
  ####
  # Crea la Tabella
  ####
  
  k <- length(brk)-1
  br1  <- brk[-(k+1)]
  br2  <- brk[-1]
  n <- length(samp)
  
  K <- length(brk)
  dat2 <- data.frame(
    xinf = brk[1:(K-1)],
    xsup = brk[2:(K)],
    nj  = as.data.frame(table(cut(samp,brk)))$Freq
  )
  
  
  dat2$fj <- (dat2$nj/sum(dat2$nj))
  dat2$bj <- (dat2$xsup-dat2$xinf)
  dat2$hj <- (dat2$fj/dat2$bj*100)
  dat2$Fj <- cumsum(dat2$fj)
  dat2$x  <- apply(dat2[,1:2],1,mean)
  dat2$x2 <- dat2$x^2
  dat2$xn <- dat2$x *dat2$nj
  dat2$x2n<- dat2$x2*dat2$nj
  dat3 <- dat2
  dat3 <- rbind(dat3,colSums(dat3))
  dat3[K,c(1:2,6:9)] <- NA
  
  names(dat3) <- c("$[\\text{x}_j,$","$\\text{x}_{j+1})$","$n_j$","$f_j$","$b_j$","$h_j$","$F_j$","$\\bar{\\text{x}}_j$","$\\bar{\\text{x}}_j^2$","$\\bar{\\text{x}}_jn_j$","$\\bar{\\text{x}}_j^2 n_j$")
  dat3$`$f_{j\\%}$` <- dat3$`$f_j$`*100
  
  perc <- dat2$xinf[-1]
  sper <- ""
  for (i in 1:(K-2)) sper <- c(sper,paste("$x_{",dat2$Fj[i],"}=",perc[i],"$"))
  # dgl <- sapply(dat2,is.integer)
  # dg <- numeric(dim(dat2)[2]+1) + 2
  # dg[3]<- 0
  Q.int <- approxfun(c(0,dat2$Fj),brk)
  F.int <- approxfun(c(-1e10,brk,1e10),c(0,0,dat2$Fj,1))
  H.int <- approxfun(c(min(-100,min(brk)-1),brk,max(100,max(brk)+1)),c(0,dat2$hj,0,0),method = "constant",yleft = 0,yright = 0)
  
  return(list(dat2=dat2,dat3=dat3,k=k,brk=brk,samp=samp,Q.int=Q.int,F.int=F.int,H.int=H.int))
}

#' Funzioni per la manipolazione e visualizzazione dei dati
#'
#' Queste funzioni sono utilizzate per il calcolo dei percentili, la stampa di distribuzioni cumulative e la visualizzazione di istogrammi con densità percentuale.
#'
#' @details
#' Le seguenti funzioni sono incluse:
#'
#' \code{h.int}: Funzione per disegnare un'area sotto la curva interpolata.
#' 
#' \code{percentile}: Calcola e stampa l'approssimazione di un percentile dato.
#' 
#' \code{F_print}: Stampa la distribuzione cumulativa in LaTeX.
#' 
#' \code{histp}: Disegna un istogramma con densità percentuale.
#'
#' @examples
#' brk <- c(0,1,2,5,10)
#' hhh <- c(2,5,1,.5)
#' samp <- genera_dati(brk = brk, hhh = hhh, n = 150)
#' ls2e(stat_base(samp = samp, brk = brk))
#' 
#' # Utilizzo delle funzioni
#' 
#' # Disegna un'area sotto la curva interpolata
#' 
#' # Calcola il percentile
#' percentile(p = 0.5)
#' 
#' # Stampa la distribuzione cumulativa in LaTeX
#' F_print(x = 5, verso = "<")
#' 
#' # Disegna un istogramma con densità percentuale
#' histp(axes = TRUE)
#' h.int(x1 = 2, x2 = 8, density = 20)
#'
#' @export

h.int <- function(x1,x2,density=20,...){
  brtemp <- c(x1,brk[brk>x1 & brk<x2],x2)
  kk <- length(brtemp)
  brs <- sort(c(min(brtemp),rep(brtemp,each=2),max(brtemp)))
  
  hrs <- c(0,0,rep(H.int(brtemp[-(kk)]),each=2),0,0)
  
  kk <- length(brs)
  polygon(brs,hrs,density=density,...)
  lines(brs,hrs,...)
}
#' @rdname h.int
#' @export
percentile <- function(p=0.5){
  X<- dat2
  K <- nrow(X)+1
  xp_inf <- X$xinf[X$Fj>=p][1]
  xp_sup <- X$xsup[X$Fj>=p][2]
  kp    <- (1:(K-1))[X$Fj>=p][1]
  Fp_sup <- X$Fj[X$Fj>=p][1]
  bp    <- X$bj[X$Fj>=p][1]
  fp    <- X$fj[X$Fj>=p][1]
  hp    <- X$hj[X$Fj>=p][1]
  if (kp == 1) Fp_inf <- 0 else Fp_inf <- X$Fj[kp-1]
  xp_apr <- xp_inf + (p-Fp_inf)/fp*bp
  datp <- round(dat2,4)
  
  
  cat("\\begin{eqnarray*}
  p &=& ",p,", \\text{essendo }F_{",kp,"}=",Fp_sup," >",p," \\Rightarrow j_{",p,"}=",kp,"\\\\
  x_{",p,"} &=& x_{\\text{inf};",kp,"} + \\frac{ {",p,"} - F_{",kp-1,"}} {f_{",kp,"}} \\cdot b_{",kp,"} \\\\
            &=& ",xp_inf," + \\frac {{",p,"} - ",Fp_inf,"} {",fp,"} \\cdot ",bp," \\\\
            &=& ",xp_apr,"
\\end{eqnarray*}
")}
#' @rdname h.int
#' @export
F_print <- function(x,verso="<",x2=0,dc=4){
  x <- round(x,dc)
  x2 <- round(x2,dc)
  br1  <- brk[-(k+1)]
  br2  <- brk[-1]
  datp <- round(dat2,4)
  F_x <- round(F.int(x),dc)
  F_2 <-  F.int(x2)
  if (verso == "<"){
    j <- max(which(brk <= x))
    if(j==1) {
      cat("\\begin{eqnarray*}
     \\%(X<",x,") &=&",x,"\\times h_1 \\\\
              &=&",x,"\\times ",datp$hj[1],"\\\\
              &=& ",F_x,"\\times(100) \\\\
     \\#(X<",x,") &\\approx&",round(F_x*n,0),"
         \\end{eqnarray*}")
    } else {
      cat("\\begin{eqnarray*}
     \\%(X<",x,") &=& ",paste("f_{",1:(j-1),"}\\times 100",collapse="+"),"+(",x,"-",brk[j],")\\times h_{",j,"} \\\\
              &=& ",paste("(",datp$fj[1:(j-1)],")\\times 100",collapse="+"),"+(",x-brk[j],")\\times ",datp$hj[j]," \\\\
              &=& ",F_x,"\\times(100) \\\\
     \\#(X<",x,") &\\approx&",round(F_x*n,0),"
         \\end{eqnarray*}")
    }
  } else if (verso == ">") {
    j <- min(which(brk >= x))
    if(j==k+1) {
      cat("\\begin{eqnarray*}
     \\%(X>",x,") &=&(",brk[j],"-",x,")\\times h_1 \\\\
              &=&",brk[j]-x,"\\times ",datp$hj[k],"\\\\
              &=& ",1-F_x,"\\times(100)\\\\
     \\#(X>",x,") &\\approx&",round((1-F_x)*n,0),"
         \\end{eqnarray*}")
    } else {
      cat("\\begin{eqnarray*}
     \\%(X>",x,") &=& (",brk[j],"-",x,")\\times h_{",j-1,"}+",paste("f_{",(j):(k),"}\\times 100",collapse="+"),"\\\\
              &=& (",brk[j]-x,")\\times",datp$hj[j-1],"+",paste("(",datp$fj[(j):(k)],")\\times 100",collapse="+"), "\\\\
              &=& ",1-F_x,"\\times(100)\\\\
     \\#(X>",x,") &\\approx&",round((1-F_x)*n,0),"
         \\end{eqnarray*}")
    }
  } else  {
    j1 <- max(which(br1 <= x))
    j2 <- min(which(br2 >= x2))
    c00 <- ifelse(x == brk[j1],
                  paste0("\\%(",x,"<X<",x2,") &=&  f_{",j1,"}\\times 100+"),
                  paste0("\\%(",x,"<X<",x2,") &=& (",min(brk[j1+1],x2),"-",x,")\\times h_{",j1,"}+"))
    c10 <- ifelse(x == brk[j1],
                  paste0("&=&",datp$fj[j1],"\\times 100 +"),
                  paste0("&=& (",min(brk[j1+1],x2)-x,")\\times ",datp$hj[j1],"+"))
    c02 <- ifelse(x2 == brk[j2+1],
                  paste0("f_{",j2,"}\\times 100"),
                  paste0("(",x2,"-",brk[j2],")\\times h_{",j2,"}"))
    c12 <- ifelse(x2 == brk[j2+1],
                  paste0(datp$fj[j2],"\\times 100"),
                  paste0("(",x2-brk[j2],")\\times ",datp$hj[j2])
    )
    if (j1==j2) {
      c00 <- paste0("\\%(",x,"<X<",x2,") &=& (",min(brk[j1+1],x2),"-",x,")\\times h_{",j1,"}")
      c01  <- ""
      c02 <- ""
      c10 <- paste0("&=& (",min(brk[j1+1],x2)-x,")\\times ",datp$hj[j1],"")
      c11 <- ""
      c12 <- ""
    } else if (j1==(j2-1)){
      c01  <- ""
      c11 <- ""
    } else {
      c01  <- paste(paste("f_{",(j1+1):(j2-1),"}\\times 100",collapse="+"),"+")
      c11  <- paste(paste("(",datp$fj[(j1+1):(j2-1)],")\\times 100",collapse="+"),"+")
    }
    cat("\\begin{eqnarray*}",
        c00,c01,c02," \\\\ \n",
        c10,c11,c12," \\\\ \n",
        "&=& ",F_2-F_x,"\\times(100)\\\\
     \\#(",x,"< X <",x2,") &\\approx&",round((F_2-F_x)*n,0),"
         \\end{eqnarray*}")
  }
}
F_print2 <- function(x,verso="<",x2=0,dc=4){
  x <- round(x,dc)
  x2 <- round(x2,dc)
  br1  <- brk[-(k+1)]
  br2  <- brk[-1]
  datp <- round(dat2,4)
  F_x <- round(F.int(x),dc)
  F_2 <-  F.int(x2)
  if (verso == "<"){
    j <- max(which(brk <= x))
    if(j==1) {
      cat("\\begin{eqnarray*}
     \\%(X<",x,") &=&",x,"\\times h_1 \\\\
              &=&",x,"\\times ",datp$hj[1],"\\\\
              &=& ",F_x,"\\times(100) \\\\
     \\#(X<",x,") &\\approx&",round(F_x*n,0),"
         \\end{eqnarray*}")
    } else {
      cat("\\begin{eqnarray*}
     \\%(X<",x,") &=& ","F_{",j-1,"}\\times 100 + (",x,"-",brk[j],")\\times h_{",j,"} \\\\
              &=& ",datp$Fj[(j-1)],")\\times 100 + (",x-brk[j],")\\times ",datp$hj[j]," \\\\
              &=& ",F_x,"\\times(100) \\\\
     \\#(X<",x,") &\\approx&",round(F_x*n,0),"
         \\end{eqnarray*}")
    }
  } else if (verso == ">") {
    j <- min(which(brk >= x))
    if(j==k+1) {
      cat("\\begin{eqnarray*}
     \\%(X>",x,") &=&(",brk[j],"-",x,")\\times h_1 \\\\
              &=&",brk[j]-x,"\\times ",datp$hj[k],"\\\\
              &=& ",1-F_x,"\\times(100)\\\\
     \\#(X>",x,") &\\approx&",round((1-F_x)*n,0),"
         \\end{eqnarray*}")
    } else {
      cat("\\begin{eqnarray*}
     \\%(X>",x,") &=& (",brk[j],"-",x,")\\times h_{",j-1,"}+",paste("f_{",(j):(k),"}\\times 100",collapse="+"),"\\\\
              &=& (",brk[j]-x,")\\times",datp$hj[j-1],"+",paste("(",datp$fj[(j):(k)],")\\times 100",collapse="+"), "\\\\
              &=& ",1-F_x,"\\times(100)\\\\
     \\#(X>",x,") &\\approx&",round((1-F_x)*n,0),"
         \\end{eqnarray*}")
    }
  } else  {
    j1 <- max(which(br1 <= x))
    j2 <- min(which(br2 >= x2))
    c00 <- ifelse(x == brk[j1],
                  paste0("\\%(",x,"<X<",x2,") &=&  f_{",j1,"}\\times 100+"),
                  paste0("\\%(",x,"<X<",x2,") &=& (",min(brk[j1+1],x2),"-",x,")\\times h_{",j1,"}+"))
    c10 <- ifelse(x == brk[j1],
                  paste0("&=&",datp$fj[j1],"\\times 100 +"),
                  paste0("&=& (",min(brk[j1+1],x2)-x,")\\times ",datp$hj[j1],"+"))
    c02 <- ifelse(x2 == brk[j2+1],
                  paste0("f_{",j2,"}\\times 100"),
                  paste0("(",x2,"-",brk[j2],")\\times h_{",j2,"}"))
    c12 <- ifelse(x2 == brk[j2+1],
                  paste0(datp$fj[j2],"\\times 100"),
                  paste0("(",x2-brk[j2],")\\times ",datp$hj[j2])
    )
    if (j1==j2) {
      c00 <- paste0("\\%(",x,"<X<",x2,") &=& (",min(brk[j1+1],x2),"-",x,")\\times h_{",j1,"}")
      c01  <- ""
      c02 <- ""
      c10 <- paste0("&=& (",min(brk[j1+1],x2)-x,")\\times ",datp$hj[j1],"")
      c11 <- ""
      c12 <- ""
    } else if (j1==(j2-1)){
      c01  <- ""
      c11 <- ""
    } else {
      c01  <- paste(paste("f_{",(j1+1):(j2-1),"}\\times 100",collapse="+"),"+")
      c11  <- paste(paste("(",datp$fj[(j1+1):(j2-1)],")\\times 100",collapse="+"),"+")
    }
    cat("\\begin{eqnarray*}",
        c00,c01,c02," \\\\ \n",
        c10,c11,c12," \\\\ \n",
        "&=& ",F_2-F_x,"\\times(100)\\\\
     \\#(",x,"< X <",x2,") &\\approx&",round((F_2-F_x)*n,0),"
         \\end{eqnarray*}")
  }
}

#' @rdname h.int
#' @export
histp <- function(axes=F,...){ 
  br1  <- brk[-(k+1)]
  br2  <- brk[-1]
  
  if (!exists("nomex")) nomex <- ""
  plot(range(brk),range(c(0,dat2$hj),na.rm = T),type="n",axes=F,xlab = nomex,ylab = "Denistà percentuale")
  rect(xleft = br1,ybottom = 0,xright = br2,ytop = dat2$hj,...)
  if (axes){
    datp <- round(dat2,4)
    axis(1,brk)
    axis(2,c(0,dat2$hj),c(0,round(dat2$hj,2)),las=2)
    segments(br1[1]-1,datp$hj,br1,datp$hj,lty=2,col="grey40") 
  }
}
