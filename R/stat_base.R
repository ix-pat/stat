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
  h.int <- function(x1,x2,density=20,...){
    brtemp <- c(x1,brk[brk>x1 & brk<x2],x2)
    kk <- length(brtemp)
    brs <- sort(c(min(brtemp),rep(brtemp,each=2),max(brtemp)))
    
    hrs <- c(0,0,rep(H.int(brtemp[-(kk)]),each=2),0,0)
    
    kk <- length(brs)
    polygon(brs,hrs,density=density,...)
    lines(brs,hrs,...)
  }
  
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
  F_print <- function(x,verso="<",x2=0){
    datp <- round(dat2,4)
    if (verso == "<"){
      j <- max(which(brk <= x))
      if(j==1) {
        cat("\\begin{eqnarray*}
     \\%(X<",x,") &=&",x,"\\times h_1 \\\\
              &=&",x,"\\times ",datp$hj[1],"\\\\
              &=& ",F.int(x),"\\times(100) \\\\
     \\#(X<",x,") &=&",F.int(x)*n,"
         \\end{eqnarray*}")
      } else {
        cat("\\begin{eqnarray*}
     \\%(X<",x,") &=& ",paste("f_{",1:(j-1),"}\\times 100",collapse="+"),"+(",x,"-",brk[j],")\\times h_{",j,"} \\\\
              &=& ",paste("(",datp$fj[1:(j-1)],")\\times 100",collapse="+"),"+(",x-brk[j],")\\times ",datp$hj[j]," \\\\
              &=& ",F.int(x),"\\times(100) \\\\
     \\#(X<",x,") &=&",F.int(x)*n,"
         \\end{eqnarray*}")
      }
    } else if (verso == ">") {
      j <- min(which(brk >= x))
      if(j==k+1) {
        cat("\\begin{eqnarray*}
     \\%(X>",x,") &=&(",brk[j],"-",x,")\\times h_1 \\\\
              &=&",brk[j]-x,"\\times ",datp$hj[k],"\\\\
              &=& ",1-F.int(x),"\\times(100)\\\\
     \\#(X>",x,") &=&",(1-F.int(x))*n,"
         \\end{eqnarray*}")
      } else {
        cat("\\begin{eqnarray*}
     \\%(X>",x,") &=& (",brk[j],"-",x,")\\times h_{",j-1,"}+",paste("f_{",(j):(k),"}\\times 100",collapse="+"),"\\\\
              &=& (",brk[j]-x,")\\times",datp$hj[j-1],"+",paste("(",datp$fj[(j):(k)],")\\times 100",collapse="+"), "\\\\
              &=& ",1-F.int(x),"\\times(100)\\\\
     \\#(X>",x,") &=&",(1-F.int(x))*n,"
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
          "&=& ",F.int(x2)-F.int(x),"\\times(100)\\\\
     \\#(",x,"< X <",x2,") &=&",(F.int(x2)-F.int(x))*n,"
         \\end{eqnarray*}")
    }
  }
  
  
  histp <- function(axes=F,...){ 
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
  return(list(dat2=dat2,dat3=dat3,F_print=F_print,F.int=F.int,h.int=h.int,H.int=H.int,histp=histp,Q.int=Q.int,percentile=percentile,k=k))
}
