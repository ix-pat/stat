# Inizio di: Inferenza.R

## Inferenza ####

#' Calcolo dell'Intervallo di Confidenza
#'
#' Calcola l'intervallo di confidenza per la media o la proporzione di una popolazione, utilizzando la distribuzione Normale (z) o t di Student.
#'
#' @param xm Media campionaria o somma delle successi.
#' @param sd Deviazione standard della popolazione, o del campione.
#' @param alpha Livello di significatività per l'intervallo di confidenza.
#' @param n Dimensione del campione.
#' @param dist_ Tipo di distribuzione ("z" per Normale, "t" per t di Student).
#' @param mus Simbolo per la media, default \\hat\\mu.
#' @param ss Simobolo per la varianza
#'
#' @details
#' La funzione `idc` determina l'intervallo di confidenza utilizzando la formula appropriata in base alla distribuzione specificata.
#' Per le proporzioni, la deviazione standard viene calcolata internamente se non fornita. 
#' Per la distribuzione t, la deviazione standard campionaria viene corretta per la dimensione del campione.
#'
#' @examples
#' # Intervallo di confidenza per la media con distribuzione Normale
#' idc(xm = 5, sd = 2, alpha = 0.05, n = 30, dist_ = "z")
#'
#' # Intervallo di confidenza per la media con distribuzione t
#' idc(xm = 5, sd = 4, alpha = 0.05, n = 30, dist_ = "t")
#'
#' # Intervallo di confidenza per la proporzione
#' idc(xm = 120, alpha = 0.05, n = 200, dist_ = "z")
#'
#' # Intervallo di confidenza per lambda
#' idc(xm = 12, sd = sqrt(12), alpha = 0.05, n = 200, dist_ = "z", mus="\\hat\\lambda",ss="\\sqrt{\\hat\\lambda}")
#'
#' @rdname intervallo-di-confidenza

idc <- function(xm,sd=NULL,alpha=0.05,n,dist_,mus=NULL,ss=NULL){
  cat("\n\n $1-\\alpha =",1-alpha,"$ e quindi $\\alpha=",alpha,"\\rightarrow \\alpha/2=",alpha/2,"$\n\n",sep="")
  tstat <- ifelse(dist_=="z",qnorm(1-alpha/2),qt(1-alpha/2,n-1))
  tsimb <- ifelse(dist_=="z","z_{\\alpha/2}","t_{n-1;\\alpha/2}")
  
  if (is.null(mus)&is.null(ss)){
    mus <- "\\hat\\mu"
    ss <- ifelse(dist_=="t","S","\\sigma")
  }
  if (is.null(sd)){
    cat("\\[
  \\hat\\pi = \\frac{S_n}n = \\frac{",xm,"}{",n,"}=",xm/n,"
\\]\n\n")
    mus <- "\\hat\\pi"
    xm <- xm/n
    sd_ <- sqrt(xm*(1-xm))
    SEs  <- "\\sqrt{\\frac{\\hat\\pi(1-\\hat\\pi)}{n}}"
    SEn  <- paste("\\sqrt{\\frac{",p(xm),"(1-",p(xm),")}{",n,"}}")
    idcn <- xm+c(-1,1)*tstat*sd_/sqrt(n)
  }
  if (!is.null(sd)&dist_=="z"){
    SEs  <- paste("\\frac{",ss,"}{\\sqrt{n}}")
    SEn <- paste("\\frac{",sd,"}{\\sqrt{",n,"}}")
    idcn <- xm+c(-1,1)*tstat*sd/sqrt(n)
    sd_ <- sd
  }
  if (!is.null(sd)&dist_=="t"){
    sc <- sqrt(n/(n-1))*sd
    mus <- "\\hat\\mu"
    SEs  <- "\\frac{S}{\\sqrt{n}}"
    SEn <- paste("\\frac{",p(sc),"}{\\sqrt{",n,"}}")
    cat(
      "\\[
     ",ss," =\\sqrt{\\frac {n}{n-1}}\\cdot\\hat\\sigma =
     \\sqrt{\\frac {",n,"}{",n-1,"}}\\cdot", sd,"=",p(sc),
      "\n\\]\n")
    idcn <- xm+c(-1,1)*tstat*sc/sqrt(n)
    sd_ <- sc
  }
  
  cat(
    "\\begin{eqnarray*}
  Idc: & & ",mus,"\\pm ",tsimb,"\\times",SEs,"\\\\
     & & ",xm, "\\pm ",tstat, "\\times",SEn,"\\\\
     & & ",xm, "\\pm ",tstat,"\\times ",sd_/sqrt(n),"\\\\
     & & [",idcn[1],", ",idcn[2],"]
\\end{eqnarray*}\n")
}

test <- function(theta1,theta0,se,h1,gdl=NULL,n,alpha = c(1/10,5/100,1/100,1/1000))
{ 
  
  tobs <- (theta1 - theta0) / se
  
  # Adattare alpha per test a due code
  if (h1 == "\\neq") alpha <- alpha / 2
  
  # Calcolare i valori critici
  if (is.null(gdl)) {
    tc <- qnorm(1 - alpha)
    p_val <- switch(h1,
                     ">" = 1 - pnorm(tobs),
                     "<" = pnorm(tobs),
                     "\\neq" = 2 * (1 - pnorm(abs(tobs)))
    )
  } else {
    tc <- qt(1 - alpha, gdl)
    p_val <- switch(h1,
                     ">" = 1 - pt(tobs, gdl),
                     "<" = pt(tobs, gdl),
                     "\\neq" = 2 * (1 - pt(abs(tobs), gdl))
    )
  }
  
  # Modificare tc in base al tipo di test
  round_all(exclude = "p_val")
  tc <- switch(h1,
               ">" = tc,
               "<" = sort(-tc),
               "\\neq" = tc
  )
  
  # Determinare la significatività di tobs
  signif <- switch(h1,
                            ">" = tobs >= tc,
                            "<" = tobs <= tc,
                            "\\neq" = abs(tobs) >= abs(tc)
  )
  
  # Assegnare gli stars in base alla significatività
  livello=0
  if (any(signif)&length(alpha)!=0) {
    livello <- ifelse(h1 == "<",min(which(signif)),max(which(signif)))
  } else {livello==signif}
  if (h1 == "\\neq") alpha <- alpha * 2
  return(list(tc = tc, alpha = alpha,livello=livello,p_val=p_val,tobs=tobs,gdl=gdl,h1=h1,n=n,theta0=theta0,theta1=theta1,tobs=tobs))
}  

A_ <- function(lst,tipo,lab1="A",lab2="B",um=""){
  ls2e(lst)
  simb <-  ifelse(grepl("pi",tipo),"\\pi","\\mu")
  th0_s <- ifelse(grepl("1",tipo),paste0(simb,"_0=",theta0,um),paste0(simb,"_\\text{",lab2,"}"))
  th1_s <- ifelse(grepl("1",tipo),simb,paste0(simb,"_\\text{",lab1,"}"))
  if (grepl("beta",tipo)){
    simb <- "\\beta"
    th0_s <- ifelse(grepl("1",tipo),paste0(simb,"_{1;H_0}=",theta0),paste0(simb,"_{0;H_0}=",theta0))
    th1_s <- ifelse(grepl("1",tipo),paste0(simb,"_1"),paste0(simb,"_0"))
    }
  
  cat(" \\(\\fbox{A}\\) FORMULAZIONE DELLE IPOTESI \n\n
   $$\\begin{cases}
   H_0:",th1_s,"=",th0_s,"\\\\
   H_1:",th1_s,h1,th0_s,"
   \\end{cases}$$\n\n")
}


C_ <- function(lst,pv_only=FALSE,pvalue=TRUE,rbow=FALSE){
  if (pv_only) alpha <- c(1/10,1/20,1/100,1/1000)
  ls2e(lst)
  param <- n-gdl
  tsimb <- ifelse(is.null(gdl),"z","t")
  if (is.null(gdl)){
    if (h1 =="\\neq"){
    ped   <- paste0("_{",alpha/2,"}")
      } else {ped <- paste0("_{",alpha,"}") }
  } else {
    if (h1 =="\\neq"){
      ped   <- paste0("_{",n,"-",param,";",alpha/2,"}")
    } else {ped   <- paste0("_{",n,"-",param,";",alpha,"}")}
    
  }
  round_all(4)
  tobs_s <- paste0(tsimb,"_\\text{obs}=",tobs)
  tcrit  <- paste0(tsimb,ped[order(abs(tc))],"=",tc)
  if (h1 == "\\neq"){
    tobs_s <-  paste0("|",tsimb,"_\\text{obs}|=",abs(tobs))
  }
  H1 <- h1
  H1 <- ifelse(h1 == "\\neq",">",H1)
  if (length(alpha)!=1){
    if(h1 == "<") {
      tcrit <- rev(tcrit)
      livello <- 5-livello
      livello <- ifelse(livello==5,0,livello)
    }
    conc <- switch(livello+1,
                   "0"=paste0("$",tobs_s,ifelse(h1=="<",">","<"),tcrit[1], 
                              "$, quindi **non** rifiuto $H_0$ a **nessun** livello di significatività, \n\n $p_\\text{value}>0.1$, _non significativo_"),
                   "1"=paste0("$",min(tc[1:2]),"<",tobs_s,"<",max(tc[1:2]), 
                              "$, indecisione sul rifiuto di $H_0$ al 10%, \n\n $0.05<p_\\text{value}<0.1$, _marginalmente significativo_ $\\fbox{.}$."),
                   "2"=paste0("$",min(tc[2:3]),"<",tobs_s,"<",max(tc[2:3]),
                              "$, quindi **rifiuto** $H_0$ al 5%, \n\n $0.01<p_\\text{value}<0.05$, _significativo_   $\\fbox{*}$."),
                   "3"=paste0("$",min(tc[3:4]),"<",tobs_s,"<",max(tc[3:4]),
                              "$, quindi **rifiuto** $H_0$ all'1%, \n\n $0.001<p_\\text{value}<0.01$, _molto significativo_  $\\fbox{**}$."),
                   "4"=paste0("$",tobs_s,H1,tc[4],
                              "$, quindi **rifiuto** $H_0$ sotto all'1‰,\n\n $p_\\text{value}<0.001$, _estremamente significativo_ $\\fbox{***}$.")
    )
    conc_pv_only <- switch(livello+1,
                   "0"=paste0("**Non** rifiuto $H_0$ a **nessun** 
                             livello di significatività, \n\n $p_\\text{value}>0.1$, 
                             _non significativo_"),
                   "1"=paste0("Indecisione sul rifiuto di $H_0$ al 10%, \n\n $0.05<p_\\text{value}<0.1$, _marginalmente significativo_ $\\fbox{.}$."),
                   "2"=paste0("**Rifiuto** $H_0$ al 5%, \n\n $0.01<p_\\text{value}<0.05$, _significativo_   $\\fbox{*}$."),
                   "3"=paste0("**Rifiuto** $H_0$ all'1%, \n\n $0.001<p_\\text{value}<0.01$, _molto significativo_  $\\fbox{**}$."),
                   "4"=paste0("**Rifiuto** $H_0$ sotto all'1‰,\n\n $p_\\text{value}<0.001$,  _estremamente significativo_ $\\fbox{***}$.")
    )
  }  
    
  cat( "\n\n\\(\\fbox{C}\\) CONCLUSIONE \n\n")
  if (!pv_only){
    if (length(alpha)!=1){
  cat(       
    ifelse(h1=="\\neq",
        paste0( "Siccome \\(H_1\\) è bilaterale, considereremo \\(\\alpha/2\\), 
                anziché \\(\\alpha\\)\n\n $\\alpha=",paste0(alpha,collapse=", "), 
                "$ e quindi $\\alpha/2=",paste0(alpha/2,collapse=", "),"$"),
        paste0("Consideriamo $\\alpha=",paste0(alpha,collapse=", "),"$ \n\n")
        ), 
    sep = "")
  
    
    cat("\n\n I valori critici sono\n\n",
         "$",paste0(tcrit,collapse="$; $"),"$\n\n", 
         "Siccome ",conc,"\n\n",sep = "")
  } else {
      if (h1=="\\neq") tobs <-abs(tobs)
      cat("La siginficatitività è $\\alpha=", alpha,"$, dalle tavole osserviamo $",tcrit,"$.\n\n",
          "Essendo $",tobs_s,ifelse(tobs>tc,">","<"),tcrit,"$ allora ", ifelse((tobs > tc)&(H1==">")|(tobs < tcrit)&(H1=="<"),"**rifiuto** $H_0$ ","**non** rifiuto $H_0$ "),"al ",alpha*100,"%.\n\n",
          sep="")
  }
  graf(lst = lst,rbow = rbow)
  if (pvalue) p_value(lst = lst)
  
  } else {
    p_value(lst = lst)
    graf(lst = lst,pv = T,rbow=rbow)
    cat("\n\n ",conc_pv_only,"\n\n",sep = "")
    
  }
}

graf <- function(lst,pv = FALSE,rbow=F){
  ls2e(lst)
  segn <- ifelse(h1 == "<",-1,1)
  #cols_ <- c(iblue,ared )
  cols_ <- c(iblue, "red","orange", "yellow")
  if (is.null(gdl))  {
    d_distr <- function(x) dnorm(x)
    q_distr <- function(p) qnorm(p)
    p_distr <- function(q) pnorm(q)
    tsimb <- "z"
  } else {
    d_distr <- function(x) dt(x,gdl)
    q_distr <- function(p) qt(p,gdl)
    p_distr <- function(q) pt(q,gdl)
    tsimb <- "t"
  }
  round_all(4)
  if (h1 =="\\neq") tc1 <- c(-rev(tc),tc) else tc1 <- sort(c(segn*rep(floor(q_distr(1/5000)),times=length(alpha)),tc))
  tac <- c(floor(q_distr(1/5000)),tc1,ceiling(q_distr(1-1/5000)))
  kk <- length(tac)
  curve(d_distr,from = tac[1],to = tac[kk],n = 1001,axes=F,xlab = tsimb,ylab = paste0("f(",tsimb,")"))
  axis(2)
  if (!pv){
    axis(1,tac[-c(1,kk)],round(tac[-c(1,kk)],4),las=2)
    if (!rbow){
      col_ <- c("red",ared,mblue,ablue,iblue)
      col_ <- colorRampPalette(c("red","pink",iblue))(5)
      if (length(alpha)==1) col_ <-c("red","blue")
      k <- length(tac)
      for (i in 1:(k/2)){
      segments(tac[i],0,tac[k-i+1],0,col=col_[i],lwd=2)
        }
    } else {
      
      den <- 1000
      if (h1 =="\\neq") {
        tc1 <- c(seq(tac[1],q_distr(.05),length=den/2),seq(q_distr(.95),tac[kk],length=den/2)) 
        col1 <- colorRampPalette(rev(cols_))(den/2)
        col2 <- rev(col1)
        colors <- c(col1,col2)
        } else {
        tc1 <- c(segn*tac[1],segn*seq(min(abs(tac[-2])),max(abs(tac)),length=den))
        colors <- colorRampPalette((cols_))(den)
        }
      kk <- length(tc1)
      # Definire l'intervallo di x per il grafico
      # Creare una palette di colori sfumati

      # Disegnare i segmenti con colori sfumati
      for(i in 1:(kk-1)) {
        segments(tc1[i], 0, tc1[i+1], 0, col = colors[i],lwd = 2)
      }
      
    }
    points(tobs,0,pch=4,cex=2)
    segments(tac[-10],0,tac[-10],d_distr(tac[-10]),lty=2)
    
  } else {
    if (rbow){
      den <- 1000
      if (h1 =="\\neq") {
        tc1 <- c(seq(tac[1],q_distr(.05),length=den/2),seq(q_distr(.95),tac[kk],length=den/2)) 
        col1 <- colorRampPalette(rev(cols_))(den/2)
        col2 <- rev(col1)
        colors <- c(col1,col2)
      } else {
        tc1 <- c(segn*tac[1],segn*seq(min(abs(tac[-2])),max(abs(tac)),length=den))
        colors <- colorRampPalette((cols_))(den)
      }
      k1 <- length(tc1)
      # Definire l'intervallo di x per il grafico
      # Creare una palette di colori sfumati
      
      # Disegnare i segmenti con colori sfumati
      for(i in 1:(k1-1)) {
        segments(tc1[i], 0, tc1[i+1], 0, col = colors[i],lwd = 2)
      }
      
    }
    if (h1 == "<") {
      draw_dist(d_distr,z1 = tac[1],z2 = tobs,col = ared)
      axis(1,c(tac[1],tobs,tac[kk]))
    } else if (h1 == ">") {
      draw_dist(d_distr,z1 = tobs,z2 = tac[kk],col = ared)
      axis(1,c(tac[1],tobs,tac[kk]))
    } else {
      draw_dist(d_distr,z1 = tac[1],z2 = -abs(tobs),col = ared)
      draw_dist(d_distr,z1 = abs(tobs),z2 = tac[kk],col = ared)
      axis(1,c(tac[1],tobs,-tobs,tac[kk]))
    }
    
  }
}

p_value <- function(lst){
  ls2e(lst)
  segn <- ifelse(h1 == "<",-1,1)
  if (is.null(gdl))  {
    d_distr <- function(x) dnorm(x)
    q_distr <- function(p) qnorm(p)
    p_distr <- function(q) pnorm(q)
    tsimb <- "z"
  } else {
    d_distr <- function(x) dt(x,gdl)
    q_distr <- function(p) qt(p,gdl)
    p_distr <- function(q) pt(q,gdl)
    tsimb <- "t"
  }
  param <- n-gdl
  tsimb <- ifelse(is.null(gdl),"z","t")
  Tsimb <- ifelse(is.null(gdl),"Z",paste0("T_{",n,"-",param,"}"))
  tobs_s <- paste0(tsimb,"_\\text{obs}=",tobs)
  #round_all(4)
  
  alpha_c <- c(1,1/10,5/100,1/100,1/1000,0)
  signif_ <- max(which(alpha_c>p_val))

  H1 <- ifelse(h1=="\\neq",">",h1)
  p_val <- format(p_val,nsmall=6,scientific=ifelse((p_val< 1e-6),T,F),digits = 1)
  tobs2 <- round(tobs,2)
  if (h1 != "\\neq") {pval <- paste0("P(",Tsimb,H1,tobs2,")=",p_val)}
  if (h1 == "\\neq") {pval <- paste0("P(|",Tsimb,"|",H1,"|",tobs2,"|)=","2P(",Tsimb,H1,abs(tobs2),")=",p_val)}
  cat("\n\n Il \\(p_{\\text{value}}\\) è \n\n $$ p_{\\text{value}} =", pval,"$$\n\n")
  if (!is.null(gdl)) cat("Attenzione il calcolo del $p_\\text{value}$ con la $T$ è puramente illustrativo e non può essere riprodotto senza una calcolatrice statistica adeguata.")
  cat("\\[\n",alpha_c[signif_+1],"< p_\\text{value}=",p_val, "\\leq",alpha_c[signif_],"\n\\]")
}

#' Test Z e Test T: Proporzione e Media
#'
#' Fornisce metodi per eseguire test z su una proporzione (`ztest_pi`), test z su una media con varianza nota (`ztest_mu`), e test t su una media con varianza incognita (`ttest_mu`).
#'
#' @param sn Numero di successi o media campionaria.
#' @param n Dimensione del campione.
#' @param p0 Proporzione attesa nella popolazione per `ztest_pi`.
#' @param h1 Ipotesi alternativa: può essere "\\neq", ">", "<".
#' @param alpha Livello di significatività.
#' @param muh Media campionaria per `ztest_mu` e `ttest_mu`.
#' @param s Deviazione standard della popolazione per `ztest_mu`, o deviazione standard campionaria per `ttest_mu`.
#' @param mu0 Media attesa nella popolazione per `ztest_mu` e `ttest_mu`.
#' @param um Unità di misura (opzionale) per migliorare la leggibilità dell'output.
#' @param pvalue Se `TRUE`, mostra il p-value del test (solo per `ztest_mu`).
#'
#' @details
#' \itemize{
#' \item `ztest_pi` utilizza il test z per valutare se la proporzione osservata in un campione differisce significativamente dalla proporzione attesa, sotto l'ipotesi nulla.
#' \item `ztest_mu` applica il test z per determinare se la media campionaria differisce significativamente dalla media attesa, assumendo che la varianza della popolazione sia nota.
#' \item `ttest_mu` impiega il test t per testare differenze nella media campionaria rispetto a una media attesa, quando la varianza della popolazione non è nota.
#'}
#' @examples
#' # Test Z per una proporzione
#' ztest_pi(sn = 55, n = 100, p0 = 0.5, h1 = "\\neq", alpha = 0.05)
#'
#' # Test Z per una media con varianza nota
#' ztest_mu(muh = 5, s = 1.5, n = 30, mu0 = 5, h1 = "\\neq", alpha = 0.05)
#'
#' # Test T per una media con varianza incognita
#' ttest_mu(muh = 5, sh = 1.5, n = 30, mu0 = 5, h1 = "\\neq", alpha = 0.05)
#'
#' @rdname test-z-t

ztest_pi <- function(sn,n,p0,h1 = "\\neq",alpha = c(1/10,5/100,1/100,1/1000),pv_only = TRUE,rbow=FALSE){
  ph <- sn/n
  se <- sqrt(p0*(1-p0)/n)

  cat("**Test $Z$ per una proporzione**\n\n")
  lst <- test(theta1 = ph,theta0 = p0,se = se,h1 = h1,n = n,alpha = alpha)
  ls2e(lst)
  cat("La stima
   $$\\hat\\pi=\\frac {", sn,"} {", n,"}=", ph," $$\n\n")
  
  # A Formulazione delle Ipotesi
  
  A_(lst = lst,tipo = "pi 1") 
  
  # B Scelta e calcolo statistica test
  
  cat("\n\n \\(\\fbox{B}\\) SCELTA E CALCOLO STATISTICA-TEST, \\(Z\\)
        Test Binomiale per \\(n\\) grande: \\(\\Rightarrow\\) z-Test.\n\n
   \\begin{eqnarray*}
   \\frac{\\hat\\pi - \\pi_{0}} {\\sqrt {\\pi_0(1-\\pi_0)/\\,n}}&\\sim&N(0,1)\\\\
   z_{\\text{obs}}
   &=& \\frac{ (", ph,"- ", p0,")} {\\sqrt{", p0,"(1-", p0,")/", n,"}}
   =  ", tobs,"\\,.
   \\end{eqnarray*}")

  # C conclusioni
  
  C_(lst,pv_only,rbow=rbow)
  
  # graf(lst) # grafico
  # 
  # p_value(lst) # p-value

}



#' @rdname test-z-t
ztest_mu <- function(muh,s,n,mu0,h1 = "\\neq",um="",pvalue=T,alpha = c(1/10,5/100,1/100,1/1000),pv_only = FALSE,rbow=FALSE){
  se <- s/sqrt(n)
  lst <- test(theta1 = muh,theta0 = mu0,se = se,h1 = h1,n = n,alpha = alpha)
  ls2e(lst)
  cat("**Test $Z$ per una media, variazna nota**\n\n")

  A_(lst = lst,tipo = "mu 1",um = um)
  
  cat("\n\n \\(\\fbox{B}\\) SCELTA E CALCOLO STATISTICA-TEST, \\(Z\\)\n\n",
     "\\(\\sigma^{2}\\) di \\(\\cal{P}\\) è nota: \\(\\Rightarrow\\) z-Test.\n\n",
   "\\begin{eqnarray*}
   \\frac{\\hat\\mu - \\mu_{0}} {\\sigma/\\sqrt{n}}&\\sim&N(0,1)\\\\
   z_{\\text{obs}}
   &=& \\frac{ (",muh,"- ",mu0,")} {",s,"/\\sqrt{",n,"}}
   =  ",tobs,"\\, .
   \\end{eqnarray*}\n\n"
  )
  
  C_(lst = lst,pv_only = pv_only,pvalue = pvalue,rbow = rbow)
  
  # graf(lst = lst)
  # 
  # if (pvalue) p_value(lst)
}

#' @rdname test-z-t
ttest_mu <-  function(muh,sh,n,mu0,h1 = "\\neq",um="",alpha = c(1/10,5/100,1/100,1/1000),pv_only = FALSE,rbow =FALSE){
  s <- sqrt(n/(n-1))*sh
  se <- s/sqrt(n)

  lst <- test(theta1 = muh,theta0 = mu0,se = se,h1 = h1,n = n,gdl = n-1,alpha = alpha)
  ls2e(lst)
  
  cat("**Test $t$ per una media, varianza incognita**\n\n")
  
  A_(lst,tipo = "mu 1",um = um)

  cat("\\begin{eqnarray*}
   S    &=& \\sqrt{\\frac{n} {n-1}}\\ \\widehat{\\sigma} 
   =  \\sqrt{\\frac{", n,"} {", n,"-1}} \\times ", sh," = ", s,"
   \\end{eqnarray*}
   \\begin{eqnarray*}
   \\frac{\\hat\\mu - \\mu_{0}} {S/\\,\\sqrt{n}}&\\sim&t_{n-1}\\\\
   t_{\\text{obs}}
   &=& \\frac{ (", muh,"- ", mu0,")} {", s,"/\\sqrt{", n,"}}
   =  ", tobs,"\\, .
   \\end{eqnarray*}
   ")

  C_(lst = lst,pv_only = pv_only,rbow = rbow)
  # graf(lst = lst)
  #  p_value(lst)
  
}
#' Test su Due Campioni: Proporzioni e Medie
#'
#' Fornisce metodi per eseguire test statistici su due campioni, valutando differenze nelle proporzioni o nelle medie.
#'
#' @param mu1, mu2 Medie campionarie o numeri di successi nei campioni per test su proporzioni.
#' @param s1h, s2h Deviazioni standard campionarie per i due campioni per test su medie.
#' @param n1, n2 Dimensioni dei campioni.
#' @param h1 Ipotesi alternativa: può essere "\\neq", ">", "<".
#' @param alpha Livello di significatività.
#' @param a, b Etichette per i gruppi, utili per l'output.
#' @param um Unità di misura (opzionale), utile per migliorare la leggibilità dell'output.
#' @param et Specifica se il test su medie assume varianze eterogenee (`TRUE`) o omogenee (`FALSE`).
#'
#' @details
#' \itemize{
#' \item \code{ztest_2c_pi} applica un test z su due proporzioni.
#' \item \code{ttest_2c_et} e \code{ttest_2c_om} eseguono test t per due medie, rispettivamente con varianze eterogenee e omogenee.
#' \item \code{test_2c} è una funzione interna utilizzata per implementare la logica comune ai test.
#'}
#' @examples
#' # Test Z per due proporzioni
#' ztest_2c_pi(s1 = 30, s2 = 45, n1 = 100, n2 = 150, h1 = "\\neq")
#'
#' # Test T per due medie con varianze eterogenee
#' ttest_2c_et(mu1 = 5.1, mu2 = 5.8, s1h = 1.5, s2h = 1.7, n1 = 30, n2 = 30, h1 = "\\neq")
#'
#' # Test T per due medie con varianze omogenee
#' ttest_2c_om(mu1 = 5.1, mu2 = 5.8, s1h = 1.5, s2h = 1.7, n1 = 30, n2 = 30, h1 = "\\neq")
#'
#' @rdname test-su-due-campioni
test_2c <-  function(mu1,mu2,s1h=F,s2h=F,n1,n2,h1 = "\\neq",et=F,a="A",b="B",um="",alpha = c(1/10,5/100,1/100,1/1000),pv_only = FALSE,rbow=F){
  #### Test su due proporzioni ----------------
  if (!s1h) # s1h = F, non ci sono le sd, è un test su proporzioni
  {
    s1 <- mu1
    s2 <- mu2
    p1 <- s1/n1
    p2 <- s2/n2
    n <- n1 + n2
    pc <- (s1+s2)/n
    se <- sqrt((pc*(1-pc))/n1+(pc*(1-pc))/n2)

    lst <- test(theta1 = p1,theta0 = p2,se = se,h1 = h1,n = n1+n2,alpha = alpha)
    ls2e(lst)
    cat("**Test $Z$ per due proporzioni**\n\n")

    A_(lst = lst,tipo = "pi 2",lab1 = a,lab2 = b)
    cat("\n\n\\(\\fbox{B}\\) SCELTA E CALCOLO STATISTICA-TEST, \\(Z\\)\n\n")

    cat("$$\\hat\\pi_\\text{",a,"}=\\frac{s_\\text{",a,"}}{n_\\text{",a,"}}=\\frac{",s1,"}{",n1,"}=",p1,"\\qquad
   \\hat\\pi_\\text{",b,"}=\\frac{s_\\text{",b,"}}{n_\\text{",b,"}}=\\frac{",s2,"}{",n2,"}=",p2,"$$")

    cat("Calcoliamo la proporzione comune sotto \\(H_0\\)
     $$
     \\pi_C=\\frac{s_\\text{",a,"}+s_\\text{",b,"}}{n_\\text{",a,"}+n_\\text{",b,"}}=
     \\frac{",s1+s2,"}{",n1+n2,"}=",pc,"
   $$")

    cat("\\begin{eqnarray*}
   \\frac{\\hat\\pi_\\text{",a,"} - \\hat\\pi_\\text{",b,"}}
   {\\sqrt{\\frac {\\pi_C(1-\\pi_C)}{n_\\text{",a,"}}+\\frac {\\pi_C(1-\\pi_C)}{n_\\text{",b,"}}}}&\\sim&N(0,1)\\\\
   z_{\\text{obs}}
   &=& \\frac{ (",p1,"- ",p2,")} {\\sqrt{\\frac{",pc,"(1-",pc,")}{",n1,"}+\\frac{",pc,"(1-",pc,")}{",n2,"}}}
   =  ",tobs,"\\, .
   \\end{eqnarray*}
   ")
    C_(lst = lst,pv_only = pv_only,rbow = rbow)  
  # graf(lst = lst)
  # 
  # p_value(lst)

  } else
    #### Test su due medie (eterogeneità) ----------------
  {
    if (et) # et = T eterogeneo
    {   
    s1 <- sqrt(n1/(n1-1))*s1h
    s2 <- sqrt(n2/(n2-1))*s2h
    s2p <- (n1*s1h^2+n2*s2h^2)/(n1+n2-2)
    se <- sqrt(s1^2/n1+s2^2/n2)
    s2f1<- s1^2; s2f2 <- s2^2

    lst <- test(theta1 = mu1,theta0 = mu2,se = se,h1 = h1,gdl = n1+n2-2,n = n1+n2,alpha = alpha)
    ls2e(lst)

    cat("**Test $t$ per due medie, (eterogeneità)**\n\n")
    
    A_(lst = lst,tipo = "2 mu")

    cat("
  \\(\\fbox{B}\\) SCELTA E CALCOLO STATISTICA-TEST, \\(T\\)
$$
     S^2_\\text{",a,"}=\\frac{n_\\text{",a,"}}{n_\\text{",a,"}-1}\\hat\\sigma^2_\\text{",a,"}=\\frac{",n1,"}{",n1,"-1}",s1h,"^2=",s2f1," \\qquad
     S^2_\\text{",b,"}=\\frac{n_\\text{",b,"}}{n_\\text{",b,"}-1}\\hat\\sigma^2_\\text{",b,"}=\\frac{",n2,"}{",n2,"-1}",s2h,"^2=",s2f2,"
   $$\n\n ")


    cat("\\begin{eqnarray*}
   \\frac{\\hat\\mu_\\text{",a,"} - \\hat\\mu_\\text{",b,"}}
   {\\sqrt{\\frac {S^2_\\text{",a,"}}{n_\\text{",a,"}}+\\frac {S^2_\\text{",b,"}}{n_\\text{",b,"}}}}&\\sim&t_{n_\\text{",a,"}+n_\\text{",b,"}-2}\\\\
   t_{\\text{obs}}
   &=& \\frac{ (",mu1,"- ",mu2,")} {\\sqrt{\\frac{",s2f1,"}{",n1,"}+\\frac{",s2f2,"}{",n2,"}}}
   =  ",tobs,"\\, .
   \\end{eqnarray*}\n\n
   ")
    
    C_(lst = lst,pv_only = pv_only,rbow = rbow)  
    
    # graf(lst)
    # 
    # p_value(lst)

    } else # et = F omogeneo
      #### Test su due medie (omogeneità) ----------------
    {
      s1 <- sqrt(n1/(n1-1))*s1h
      s2 <- sqrt(n2/(n2-1))*s2h
      s2p <- (n1*s1h^2+n2*s2h^2)/(n1+n2-2)
      se <- sqrt(s2p/n1+s2p/n2)
      s2f1<- s1^2; s2f2 <- s2^2
      lst <- test(theta1 = mu1,theta0 = mu2,se = se,h1 = h1,gdl = n1+n2-2,n = n1+n2,alpha = alpha)
      ls2e(lst)
      cat("**Test $T$ per due medie, (omogeneità)**\n\n")

      A_(lst,tipo = "2 mu",lab1 = a,lab2 = b)
      cat("\\(\\fbox{B}\\) SCELTA E CALCOLO STATISTICA-TEST, \\(T\\)

  L'ipotesi è di omogeneità e quindi calcoliamo:")

      cat("$$
   S_p^2=\\frac{n_\\text{", a,"}\\hat\\sigma^2_\\text{", a,"}+n_\\text{", b,"}\\hat\\sigma^2_\\text{", b,"}}{n_\\text{", a,"}+n_\\text{", b,"}-2} =
   \\frac{", n1,"\\cdot", s1h,"^2+", n2,"\\cdot", s2h,"^2}{", n1,"+", n2,"-2}=", s2p,"
  $$\n\n
\\begin{eqnarray*}
  \\frac{\\hat\\mu_\\text{", a,"} - \\hat\\mu_\\text{", b,"}}
  {\\sqrt{\\frac {S^2_p}{n_\\text{", a,"}}+\\frac {S^2_p}{n_\\text{", b,"}}}}&\\sim&t_{n_\\text{", a,"}+n_\\text{", b,"}-2}\\\\
  t_{\\text{obs}}
  &=& \\frac{ (", mu1,"- ", mu2,")} {\\sqrt{\\frac{", s2p,"}{", n1,"}+\\frac{", s2p,"}{", n2,"}}}
  =  ", tobs,"\\, .
  \\end{eqnarray*}\n\n
  ")
      C_(lst,pv_only,rbow = rbow)
      
      # graf(lst)
      # 
      # p_value(lst)
    }
  }
}

#' @rdname test-su-due-campioni
ttest_2c_et <-  function(mu1,mu2,s1h,s2h,n1,n2,h1 = "\\neq",a="1",b="2",um="",alpha = c(1/10,5/100,1/100,1/1000) ,pv_only = FALSE,rbow=FALSE){
  test_2c(mu1=mu1,mu2 = mu2,s1h=s1h,s2h=s2h,n1,n2,h1 = h1, alpha = alpha,et=T,a=a,b=b,um=um, pv_only = pv_only,rbow=rbow)
}

#' @rdname test-su-due-campioni
ttest_2c_om <-  function(mu1,mu2,s1h,s2h,n1,n2,h1 = "\\neq",a="1",b="2",um="",alpha = c(1/10,5/100,1/100,1/1000),pv_only = FALSE,rbow=FALSE){
  test_2c(mu1=mu1,mu2 = mu2,s1h=s1h,s2h=s2h,n1,n2,h1 = h1, alpha = alpha,et=F,a=a,b=b,um=um,pv_only = pv_only,rbow=rbow)
}

#' @rdname test-su-due-campioni
ztest_2c_pi <-  function(s1,s2,n1,n2,h1 = "\\neq",a="1",b="2",alpha = c(1/10,5/100,1/100,1/1000),pv_only = TRUE,rbow=FALSE){
  test_2c(mu1 = s1,mu2 = s2,s1h = F,s2h = F,n1 = n1,n2 = n2,h1 = h1,alpha = alpha,a = a,b = b,pv_only = pv_only,rbow=rbow)
}

## Regressione ####

#' Regressione Lineare
#'
#' Questa funzione esegue una regressione lineare sui dati forniti, calcolando
#' varie statistiche e misure di adattamento. Può operare in diversi modi a seconda
#' degli argomenti forniti: direttamente su vettori `x` e `y`, o utilizzando statistiche
#' riassuntive.
#'
#' @param x Un vettore numerico che rappresenta la variabile indipendente. Non richiesto
#' se `stat1` o `stat2` sono forniti.
#' @param y Un vettore numerico che rappresenta la variabile dipendente. Non richiesto
#' se `stat1` o `stat2` sono forniti.
#' @param stat1 Un elenco contenente statistiche riassuntive dei dati. Può includere
#' elementi come `n`, `sumx`, `sumx2`, `sumy`, `sumy2`, e `sumxy`.
#' @param stat2 Un'altra forma di statistiche riassuntive, simile a `stat1` ma potrebbe
#' includere medie e varianze direttamente.
#' @param semp Un booleano che indica se utilizzare una versione semplificata dei calcoli.
#' Utile per grandi set di dati dove la precisione al di sotto di una certa soglia
#' decimale non è critica.
#' @param ax Numero di cifre decimali per l'arrotondamento nei calcoli e nell'output.
#' Valore predefinito a 2.
#'
#' @return Un elenco contenente vari componenti legati alla regressione effettuata,
#' inclusi coefficienti, misure di adattamento, e statistiche utili per valutazioni
#' successive.
#' @return \code{R2} calcola e commenta R²
#' 
#' @examples
#' require(kableExtra)
#' x <- 1:10
#' y <- 2 + 3 * x + rnorm(10)
#' risultato <- regr(x, y)
#' print(risultato$mx)
#' print(risultato$my)
#' print(risultato$R2())
#' 
#' @export

regr <- function(x=NULL,y=NULL,stat1=NULL,stat2=NULL,semp=F,ax=2){
  {
    if (!semp){
      if (!is.null(x)){
        n <- length(x)
        mx <- mean(x)
        vx <- mean(x^2)-mean(x)^2
        my <- mean(y)
        vy <- mean(y^2)-mean(y)^2
        co <- mean(x*y) - mx*my
        sumx <- sum(x)
        sumx2<- sum(x^2)
        sumy <- sum(y)
        sumy2<- sum(y^2)
        sumxy <- sum(x*y)
      } else if (!is.null(stat1)) {
        n <- stat1$n
        mx <- stat1$sumx/n
        vx <- stat1$sumx2/n - mx^2
        my <- stat1$sumy/n
        vy <- stat1$sumy2/n - my^2
        co <- stat1$sumxy/n - mx*my
        sumx <- stat1$sumx
        sumx2<- stat1$sumx2
        sumy <- stat1$sumy
        sumy2<- stat1$sumy2
        sumxy <- stat1$sumxy
      } else if (!is.null(stat2)){
        n <- stat2$n
        mx <- stat2$mx
        vx <- stat2$vx
        my <- stat2$my
        vy <- stat2$vy
        co <- stat2$co
        sumx <- n*mx
        sumx2<- n*(vx+mx^2)
        sumy <- n*my
        sumy2<- n*(vy+my^2)
        sumxy <- n*(co+mx*my)
      }} else {
        n <- length(x)
        sumx  <- round(sum(x),ax)
        sumx2 <- round(sum(x^2),ax)
        sumy  <- round(sum(y),ax)
        sumy2 <- round(sum(y^2),ax)
        sumxy <- round(sum(x*y),ax)
        mx <- sumx/n
        vx <- sumx2/n-mx^2
        my <- sumy/n
        vy <- sumy2/n-my^2
        co <- mean(x*y) - mx*my
      }
    
    sx <- sqrt(vx)
    sy <- sqrt(vy)
    r  <- co/sqrt(vy*vx)
    b1 <- co/vx
    b0 <- my - b1*mx
    ys <- b0 + b1 * x
    es <- y - ys
    rg <- ys - my
    sh2 <- vy*(1-r^2)
    se2 <- (n/(n-2))*sh2 
    vb0 <- se2 * (1/n+mx^2/(n*vx))
    vb1 <- se2 / (n*vx)
    seh <- sqrt((1-r^2)*vy)
    se <- sqrt(n/(n-2))*seh
    h <- 1/n + (x-mean(x))^2/(n*s2c(x))
    ss <- sd(y)*sqrt(1-r^2)*sqrt(1-h)
    
    # For old compatibility prin
    if (!is.null(x)){
      Dato <- c(1:n,"Totale","Totale/n")
      prn <- round(data.frame(x=c(x,0,0),y=c(y,0,0),x2 = c(x^2,0,0),y2 = c(y^2,0,0),xy=c(x*y,0,0)),ax)
      prn <- data.frame(Dato,prn)
      prn[n+1,2:6] <- colSums(prn[1:n,2:6])
      prn[n+2,2:6] <- colMeans(prn[1:n,2:6])
      names(prn)<-c("$i$","$x_i$","$y_i$","$x_i^2$","$y_i^2$","$x_i\\cdot y_i$") 
      
      ml <- lsfit(x,y)
      bb <- ml$coefficients
    } else {
      prn <- ml <- bb <- NULL
    }
  # round_all(exclude = c(sumx,sumy,sumx2,sumy2,sumxy))
  }  
  
  return(
    list(
      mx = mx,
      vx = vx,
      sx = sx,
      my = my,
      vy = vy,
      sy = sy,
      co = co,
      cv = co,
      r  = r,
      b1 = b1,
      b0 = b0,
      ys = ys,
      es = es,
      rxg = rg,
      sh2 = sh2,
      se2 = se2,
      vb0 = vb0,
      vb1 = vb1,
      seh = seh,
      se  = se,
      n  = n,
      sumx = sumx,
      sumy = sumy,
      sumx2 =sumx2,
      sumy2 = sumy2,
      sumxy = sumxy,
      prn = prn,
      ml = ml,
      bb = bb,
      h = h,
      ss=ss
    )
  )
}  

print_stat <- function(div = F)
{
cat("Si osservano le seguenti statistiche: \n")
if (!div){
  cat("\\begin{align*}\n")
  cat("  \\sum_{i=1}^{",n, "}x_i &= ",sumx,", \\sum_{i=1}^{",n,"}y_i &=",sumy,"\\\\ \n")
  cat("  \\sum_{i=1}^{",n,"}x_i^2 &=",sumx2,", \\sum_{i=1}^{",n,"}y_i^2 &= ",sumy2,"\\\\ \n")
  cat("  \\sum_{i=1}^{",n,"}x_iy_i &=",sumxy,"\n")
  cat("\\end{align*}\n")
} else {
  cat("\\begin{align*}\n")
  cat("  \\frac 1{",n,"}\\sum_{i=1}^{",n, "}x_i &= ",sumx/n,", \\frac 1{",n,"}\\sum_{i=1}^{",n,"}y_i &=",sumy/n,"\\\\ \n")
  cat("  \\frac 1{",n,"}\\sum_{i=1}^{",n,"}x_i^2 &=",sumx2/n,", \\frac 1{",n,"}\\sum_{i=1}^{",n,"}y_i^2 &= ",sumy2/n,"\\\\ \n")
  cat("  \\frac 1{",n,"}\\sum_{i=1}^{",n,"}x_iy_i &=",sumxy/n,"\n")
  cat("\\end{align*}\n")
  
}
}

#' Stampa i Coefficienti di Regressione in LaTeX
#'
#' Questa funzione genera l'output LaTeX per il calcolo dei coefficienti di regressione lineare,
#' includendo i coefficienti \eqn{\hat{\beta_1}} e \eqn{\hat{\beta_0}} \eqn{\hat{\alpha_1}} e \eqn{\hat{\alpha_0}} se viene
#' utilizzato il calcolo inverso) basati sui dati forniti. La funzione permette di scegliere tra
#' una stampa dettagliata dei calcoli intermedi o una più semplice che include solo i coefficienti.
#'
#' @param semplice Un valore booleano che, se TRUE, produce una stampa semplificata contenente solo i
#'                 coefficienti di regressione. Se FALSE, produce un output dettagliato con tutti i
#'                 calcoli intermedi. Il default è FALSE.
#' @param inv Un valore booleano che, se TRUE, inverte i ruoli di X e Y nel calcolo della regressione.
#'            Il default è FALSE.
#'
#' @return La funzione non ritorna un valore ma stampa direttamente l'output in formato LaTeX
#'         nell'ambiente di chiamata.
#'
#' @examples
#' x <- rnorm(100)
#' y <- x + rnorm(100,0,.1)
#' ls2e(regr(x,y))
#' # Calcolo normale con output dettagliato
#' calcolo_beta(semplice = FALSE)
#'
#' # Calcolo invertito con output semplice
#' calcolo_beta(semplice = TRUE, inv = TRUE)
#'
#' @export
calcolo_beta <- function(semplice=F,inv = F){
  if (!inv){
    if (!semplice) {
      cat("\\begin{eqnarray*}
           \\bar x &=&\\frac 1 n\\sum_{i=1}^n x_i = \\frac {1}{",n,"} ",p(sumx),"= ",mx,"\\\\
           \\bar y &=&\\frac 1 n\\sum_{i=1}^n y_i = \\frac {1}{",n,"} ",p(sumy),"= ",my,"\\\\
           \\hat\\sigma_X^2&=&\\frac 1 n\\sum_{i=1}^n x_i^2-\\bar x^2=\\frac {1}{",n,"} ",sumx2," -",p(mx),"^2=",vx,"\\\\
           \\hat\\sigma_Y^2&=&\\frac 1 n\\sum_{i=1}^n y_i^2-\\bar y^2=\\frac {1}{",n,"} ",sumy2," -",p(my),"^2=",vy,"\\\\
           \\text{cov}(X,Y)&=&\\frac 1 n\\sum_{i=1}^n x_i~y_i-\\bar x\\bar y=\\frac {1}{",n,"} ",sumxy,"-",p(mx),"\\cdot",p(my),"=",co,"\\\\
           \\hat\\beta_1 &=& \\frac{\\text{cov}(X,Y)}{\\hat\\sigma_X^2} \\\\
                    &=& \\frac{",co,"}{",vx,"}  = ",b1,"\\\\
           \\hat\\beta_0 &=& \\bar y - \\hat\\beta_1 \\bar x\\\\
                    &=& ",my,"-",p(b1),"\\times ",p(mx),"=",b0,"
         \\end{eqnarray*}"
      )
    } else 
    {
      cat("\\begin{eqnarray*}
       \\hat\\beta_1 &=& \\frac{\\text{cov}(X,Y)}{\\hat\\sigma_X^2} \\\\
            &=& \\frac{",co,"}{",vx,"}  = ",b1,"\\\\
      \\hat\\beta_0 &=& \\bar y - \\hat\\beta_1 \\bar x\\\\
          &=& ",my,"-",p(b1),"\\times ",p(mx),"=",b0,"
      \\end{eqnarray*}"
      )}
  }  else 
  {
    cat("\\begin{eqnarray*}
\\bar y &=&\\frac 1 n\\sum_{i=1}^n y_i = \\frac {1}{",n,"} ",sumy,"= ",my,"\\\\
\\bar x &=&\\frac 1 n\\sum_{i=1}^n x_i = \\frac {1}{",n,"} ",sumx,"= ",mx,"\\\\
\\hat\\sigma_y^2&=&\\frac 1 n\\sum_{i=1}^n y_i^2-\\bar y^2=\\frac {1}{",n,"} ",sumy2," -",p(my),"^2=",vy,"\\\\
\\hat\\sigma_x^2&=&\\frac 1 n\\sum_{i=1}^n x_i^2-\\bar x^2=\\frac {1}{",n,"} ",sumx2," -",p(mx),"^2=",vx,"\\\\
\\text{cov}(y,x)&=&\\frac 1 n\\sum_{i=1}^n y_i~x_i-\\bar y\\bar x=\\frac {1}{",n,"} ",sumxy,"-",p(my),"\\cdot",p(mx),"=",co,"\\\\
\\hat\\alpha_1 &=& \\frac{\\text{cov}(y,x)}{\\hat\\sigma_y^2} \\\\
         &=& \\frac{",co,"}{",vy,"}  = ",b1,"\\\\
\\hat\\alpha_0 &=& \\bar x - \\hat\\alpha_1 \\bar y\\\\
         &=& ",mx,"-",p(b1),"\\times ",p(my),"=",b0,"
\\end{eqnarray*}
")
  }
}

#' Stampa la Previsione di Y Data un Valore X in LaTeX
#'
#' Questa funzione calcola e stampa l'equazione di previsione per un valore \(x\) dato,
#' utilizzando i coefficienti di regressione lineare \eqn{\hat\beta_0} e \eqn{\hat\beta_1}.
#' L'output è in formato LaTeX e mostra l'equazione di previsione completa, includendo i calcoli
#' effettivi per il valore specificato di \(x\).
#'
#' @param x Il valore di \(x\) per cui si desidera calcolare la previsione di \(y\).
#'
#' @return La funzione non ritorna un valore ma stampa direttamente l'output in formato LaTeX
#'         nell'ambiente di chiamata.
#'
#' @details Si assume che i coefficienti \eqn{\hat\beta_0} e \eqn{\hat\beta_1} siano definiti
#'          e accessibili nello scope da cui la funzione viene chiamata. La funzione controlla se
#'          \eqn{\hat\beta_1} è negativo, ma non esegue operazioni specifiche in tal caso; questa
#'          condizione può essere espansa per gestire scenari specifici.
#'
#' @examples
#' x <- rnorm(100)
#' y <- x + rnorm(100,0,.1)
#' ls2e(regr(x,y))
#' # Calcola la previsione per x = 10
#' previsione(10)
#'
#' @export

previsione <- function(x){
  if (b1 < 0) {
  }
  cat("\\[\\hat y_{X=",x,"}=\\hat\\beta_0+\\hat\\beta_1 x=",b0,"+",p(b1),"\\times",p(x),"=",b0+b1*x,"\\]")
}

#' Stampa il Calcolo del Residuo per una Coppia (x, y) in LaTeX
#'
#' Questa funzione calcola e stampa il residuo \eqn{\hat\varepsilon_i} per una coppia di valori
#' \(x_i\) e \(y_i\), utilizzando i coefficienti di regressione lineare \eqn{\hat\beta_0} e
#' \eqn{\hat\beta_1}. L'output è in formato LaTeX e mostra i passaggi del calcolo del valore previsto
#' \eqn{\hat y_i} e del residuo \eqn{\hat\varepsilon_i}.
#'
#' @param x Il valore di \eqn{x_i} per il quale si desidera calcolare il residuo.
#' @param y Il valore di \eqn{y_i} corrispondente a \eqn{x_i}.
#'
#' @return La funzione non ritorna un valore ma stampa direttamente l'output in formato LaTeX
#'         nell'ambiente di chiamata.
#'
#' @details Si assume che i coefficienti \eqn{\hat{\beta_1}} e \eqn{\hat{\beta_0}}  siano definiti
#'          e accessibili nello scope da cui la funzione viene chiamata. La funzione stampa l'equazione
#'          per calcolare \eqn{\hat y_i}e il residuo \eqn{\hat{\varepsilon_i}}.
#'
#' @examples
#' x <- rnorm(100)
#' y <- x + rnorm(100,0,.1)
#' ls2e(regr(x,y))
#' # Calcolo del residuo per la coppia (x = 10, y = 20)
#' residuo(10, 20)
#'
#' @export

residuo <- function(x,y){
  cat("\\begin{eqnarray*}\n")
  cat("\\hat y_i &=&\\hat\\beta_0+\\hat\\beta_1 x_i=\\\\ \n")
  cat(        "&=&",b0,"+",p(b1),"\\times",p(x),"=",b0+b1*x,"\\\\ \n")
  cat("\\hat \\varepsilon_i &=& y_i-\\hat y_i\\\\ \n")
  cat(                "&=&",y,"-",b0+b1*x,"=",y - (b0+b1*x)," \n")
  cat("\\end{eqnarray*}\n")  
}

#' Stampa e Valuta il Coefficiente di Determinazione \(r^2\) in LaTeX
#'
#' Questa funzione calcola il coefficiente di correlazione \(r\), il suo quadrato \(r^2\),
#' e stampa queste statistiche in formato LaTeX. Inoltre, valuta se \(r^2\) indica un buon adattamento
#' del modello ai dati confrontando \(r^2\) con il valore soglia 0.75.
#'
#' @return La funzione non ritorna un valore ma stampa direttamente l'output in formato LaTeX
#'         e un messaggio testuale che valuta la bontà di adattamento del modello nell'ambiente di chiamata.
#'
#' @details Si assume che le variabili `r`, `co`, `sx`, e `sy` (correlazione, covarianza di X e Y,
#'          deviazione standard di X e deviazione standard di Y, rispettivamente) siano definite e
#'          accessibili nello scope da cui la funzione viene chiamata. La funzione determina se
#'          il modello si adatta bene ai dati basandosi su se \(r^2\) è maggiore o minore di 0.75.
#'
#' @examples
#' # Calcolo e valutazione di r^2 supponendo che r, co, sx, sy siano già definiti
#' x <- rnorm(100)
#' y <- x + rnorm(100,0,.1)
#' ls2e(regr(x,y))
#' R2()
#'
#' @export
R2 <- function(){
  sgn <- ifelse(r^2>.75,">","<")
  cat("\\begin{eqnarray*}\n")
  cat("r&=&\\frac{\\text{cov}(X,Y)}{\\sigma_X\\sigma_Y}=\\frac{",co,"}{",sx,"\\times",sy,"}=",r,"\\\\")
  cat("r^2&=&",r^2,sgn,"0.75\n")
  cat("\\end{eqnarray*}\n")  
  cat(ifelse(r^2>.75,"Il modello si adatta bene ai dati.","Il modello **non** si adatta bene ai dati."))
}
#' Stampa i Componenti della Varianza Totale dei Dati in LaTeX
#'
#' Questa funzione calcola e stampa il Total Sum of Squares (TSS), Explained Sum of Squares (ESS),
#' e Residual Sum of Squares (RSS) utilizzando il coefficiente di determinazione \(R^2\). La funzione
#' stampa queste statistiche in formato LaTeX, illustrando come TSS è suddiviso in ESS e RSS.
#'
#' @return La funzione non ritorna un valore ma stampa direttamente l'output in formato LaTeX
#'         nell'ambiente di chiamata.
#'
#' @details Si assume che le variabili `n`, `vy`, e `r^2` (numero di osservazioni, varianza di Y,
#'          e il quadrato del coefficiente di correlazione, rispettivamente) siano definite e
#'          accessibili nello scope da cui la funzione viene chiamata. La funzione dimostra la
#'          relazione \(TSS = ESS + RSS\).
#'
#' @examples
#' # Calcolo e stampa di TSS, ESS e RSS supponendo che n, vy e r^2 siano già definiti
#' x <- rnorm(100)
#' y <- x + rnorm(100,0,.1)
#' ls2e(regr(x,y))
#' TSS()
#'
#' @export
TSS <- function(){
  cat("\\begin{eqnarray*}
   TSS &=& n\\hat\\sigma^2_Y\\\\
      &=&",n,"\\times",vy,"\\\\
      &=& ",n*vy,"\\\\
   ESS &=& R^2\\cdot TSS\\\\
      &=& ",r^2,"\\cdot",n*vy,"\\\\
      &=&", r^2*n*vy,"\\\\
   RSS &=& (1-R^2)\\cdot TSS\\\\
      &=& (1-",r^2,")\\cdot",n*vy,"\\\\
      &=& ",(1-r^2)*n*vy, "\\\\
   TSS &=& ESS+RSS \\\\", 
      n*vy," &=& ", r^2*n*vy, "+", (1-r^2)*n*vy, "
  \\end{eqnarray*}")
}

#' Calcolo della varianza di epsilon
#'
#' Questa funzione calcola la varianza di epsilon (\eqn{S_\varepsilon^2}) utilizzando le formule mostrate.
#'
#' @details
#' La funzione visualizza le seguenti equazioni utilizzando la notazione LaTeX:
#' \deqn{\hat{\sigma_\varepsilon}^2 = (1-r^2)\hat{\sigma_Y}^2}
#' \deqn{(1-r^2)\times \text{vy}}
#' \deqn{S_\varepsilon^2 = \frac{n} {n-2} \hat{\sigma_\varepsilon}^2}
#' \deqn{\frac{n} {n-2} \times \text{sh2} = \text{se2}}
#'
#' @examples
#' # Esempio di utilizzo
#' x <- rnorm(100)
#' y <- x + rnorm(100,0,.1)
#' ls2e(regr(x,y))
#' S_epsilon()
#'
#' @export
S_epsilon <- function(){
  cat(
  "\\begin{eqnarray*}
\\hat{\\sigma_\\varepsilon}^2&=&(1-r^2)\\hat\\sigma_Y^2\\\\
&=& (1-",r^2,")\\times",vy,"\\\\
   &=& ",sh2,"\\\\
   S_\\varepsilon^2 &=& \\frac{n} {n-2} \\hat{\\sigma_\\varepsilon}^2\\\\
   &=&  \\frac{",n,"} {",n,"-2} \\hat{\\sigma_\\varepsilon}^2 \\\\
 &=&  \\frac{",n,"} {",n,"-2} \\times ",sh2," = ",se2," 
\\end{eqnarray*}")
  }

#' Stampa il Calcolo della Varianza Stimata e dell'Errore Standard per \eqn{\hat{\beta_0}}
#'
#' Questa funzione calcola e stampa, in formato LaTeX, la varianza stimata e l'errore standard
#' per il coefficiente di regressione \eqn{\hat{\beta_0}} utilizzando la varianza residua.
#' Il calcolo include la stima dell'errore standard basato su varianze corrette.
#'
#' @param sig_eps Se TRUE, calcola prima la varianza residua corretta per poi procedere al
#'                calcolo delle varianze e degli errori standard dei coefficienti di regressione.
#'
#' @return La funzione non ritorna un valore ma stampa direttamente l'output in formato LaTeX
#'         nell'ambiente di chiamata.
#'
#' @details Si assume che le variabili `n`, `vy`, `vx`, `mx`, `r^2`, `sh2`, `se2` e `vb0` siano definite
#'          e accessibili nello scope da cui la funzione viene chiamata. I calcoli mostrano come
#'          varianze e errori standard sono stimati per \eqn{\hat{\beta_0}}.
#'
#' @examples
#' x <- rnorm(100)
#' y <- x + rnorm(100, 0, .1)
#' ls2e(regr(x, y))  # Assume regr and ls2e are properly defined
#' se_beta0(TRUE)
#'
#' @rdname se_beta
se_beta1 <- function(sig_eps = TRUE){
  if (sig_eps){
    S_epsilon()
cat("\n\n E quindi")}
  
  cat("\\begin{eqnarray*}
V(\\hat\\beta_{1}) &=& \\frac{\\sigma_{\\varepsilon}^{2}} {n \\hat{\\sigma}^{2}_{X}} \\\\
\\widehat{V(\\hat\\beta_{1})} &=& \\frac{S_{\\varepsilon}^{2}} {n \\hat{\\sigma}^{2}_{X}} \\\\
 &=& \\frac{",se2,"} {",n,"\\times ",vx,"} = ",vb1,"\\\\
 \\widehat{SE(\\hat\\beta_{1})}        &=&  \\sqrt{",vb1,"}\\\\
 &=& ",sqrt(vb1),"
\\end{eqnarray*}
")
}

#' @rdname se_beta
se_beta0 <- function(sig_eps = TRUE)  {
  if (sig_eps){
    S_epsilon()
    cat("\n\n E quindi")}
  
  cat("\\begin{eqnarray*}
V(\\hat\\beta_{0}) &=& \\sigma_{\\varepsilon}^{2} \\left( \\frac{1} {n}  +  \\frac{\\bar{x}^{2}} {n \\hat{\\sigma}^{2}_{X}} \\right)\\\\
\\widehat{V(\\hat\\beta_{0})} &=& S_{\\varepsilon}^{2}\\left( \\frac{1} {n}  +  \\frac{\\bar{x}^{2}} {n \\hat{\\sigma}^{2}_{X}} \\right)\\ \\\\
 &=& ",se2,"\\times\\left( \\frac{1} {",n,"}  +  \\frac{",mx,"^{2}} {",n,"\\times ",vx,"} \\right)\\\\
 \\widehat{SE(\\hat\\beta_{0})}        &=&  \\sqrt{",vb0,"}\\\\
 &=& ",sqrt(vb0),"
\\end{eqnarray*}")
}

#' Conduce e Stampa l'Analisi di un t-Test per un Coefficiente di Regressione in LaTeX
#'
#' Questa funzione esegue un t-test su uno dei coefficienti di regressione \eqn{\hat{\beta}_0} o \eqn{\hat\beta_1},
#' stampando il calcolo della statistica del test e altri output correlati in formato LaTeX. 
#' La funzione può anche eseguire e stampare il calcolo dell'errore standard del coefficiente se specificato.
#'
#' @param cof Indice del coefficiente su cui eseguire il test (0 per \eqn{\hat{\beta}_0}, 1 per  \eqn{\hat{\beta}_1}).
#' @param bj0 Valore ipotizzato per il coefficiente sotto l'ipotesi nulla.
#' @param h1 L'ipotesi alternativa del test (\(\\neq\), \(>\), o \(<\)).
#' @param alpha Un vettore di livelli di significatività da utilizzare per il test.
#' @param SE Se TRUE, esegue e stampa il calcolo dell'errore standard del coefficiente specificato.
#'
#' @return La funzione non ritorna un valore ma stampa direttamente l'output in formato LaTeX
#'         e produce grafici e p-value associati al test, nell'ambiente di chiamata.
#'
#' @details Si assume che le variabili `b0`, `b1`, `vb0`, `vb1`, e `n` siano definite e accessibili
#'          nello scope da cui la funzione viene chiamata. Include funzioni di supporto come `A_`, `C_`,
#'          `graf`, e `p_value` che dovrebbero essere definite per eseguire analisi aggiuntive e stampa.
#'
#' @examples
#' x <- rnorm(100)
#' y <- x + rnorm(100, 0, .1)
#' ls2e(regr(x, y))  # Assume regr and ls2e are properly defined
#' ttest_beta(cof = 0, bj0 = 0)  # Test su beta_0
#' ttest_beta(cof = 1, bj0 = 1, SE = TRUE)  # Test su beta_1 con errore standard
#' 
#' @export
ttest_beta <-  function(cof,bj0,h1 = "\\neq",alpha = c(1/10,5/100,1/100,1/1000),SE=F,pv_only=F,rbow=FALSE){
  bj <- ifelse(cof==0,b0,b1)
  vbj <-  ifelse(cof==0,(vb0),(vb1))
  lst <- test(theta1 = bj,theta0 = bj0,se = sqrt(vbj),h1 = h1,alpha = alpha,gdl = n-2,n=n)
  ls2e(lst)
  A_(lst,tipo=paste("beta",cof))

    
  cat("$\\fbox{B}$ SCELTA E CALCOLO STATISTICA-TEST, $T$
Test su un coefficiente di regressione: $\\Rightarrow$ t-Test.\n\n")

  if (SE&(cof==0)) se_beta0(sig_eps = T)
  if (SE&(cof==1)) se_beta1(sig_eps = T)
  
cat("  
\\begin{eqnarray*}
 \\frac{\\hat\\beta_{",cof,"} - \\beta_{",cof,";H_0}} {\\widehat{SE(\\hat\\beta_{",cof,"})}}&\\sim&t_{n-2}\\\\
   t_{\\text{obs}}
&=& \\frac{ (",bj,"- ",bj0,")} {",sqrt(vbj),"}
 =  ",tobs,"\\, .
\\end{eqnarray*}
")
  
  C_(lst,pv_only,rbow = rbow)
  
  # graf(lst)
  # 
  # p_value(lst)
  
}
#' Spline Smussata con Bande di Confidenza
#'
#' Questa funzione adatta una spline smussata ai punti dati e traccia i valori
#' adattati insieme alle bande di confidenza.
#'
#' @param x Un vettore numerico dei valori x.
#' @param y Un vettore numerico dei valori y corrispondenti a `x`.
#' @param df1 Gradi di libertà per il primo adattamento della spline smussata. Default è 5.
#' @param df2 Gradi di libertà per l'adattamento della spline sui residui. Default è 5.
#' @param n Numero di punti da utilizzare per la griglia di previsione. Default è 100.
#' @param a Valore critico per le bande di confidenza. Default è il 97.5-esimo percentile della distribuzione normale (\code{qnorm(.975)}).
#' @importFrom stats qnorm
#' @importFrom graphics lines
#' @importFrom stats smooth.spline
#' @return La funzione non restituisce un valore ma traccia le linee della spline smussata e delle bande di confidenza sul grafico corrente.
#' @examples
#' 
#' x <- seq(0,10,length=200)
#' y <- sin(x) + rnorm(n = 200,0,x^2/100+.1)
#' plot(x,y)
#' spline_pat(x,y,df1=10,df2=10)

spline_pat <- function(x, y, df1 = 5, df2 = 5, n = 100, a = qnorm(.975)) {
  xx <- seq(min(x), max(x), length = n)
  fit <- smooth.spline(x, y, df = df1)
  hat_mu <- predict(fit, xx)$y
  eps <- fit$yin - fit$y
  eps2 <- eps^2
  fit2 <- smooth.spline(fit$x, eps2, df = df2)
  hat_sig <- sqrt(predict(fit2, xx)$y)
  lines(xx, hat_mu, col = 2, lwd = 2)
  lines(xx, hat_mu + a * hat_sig, lty = 2)
  lines(xx, hat_mu - a * hat_sig, lty = 2)
}

# Fine di: Inferenza.R

# Inizio di: PROVA.R
questa_e_una_prova <- function(){cat("Prova!!")}

# 
# library(pat.book)
# brk <- c(0,1,2,5,10)
# nnn <- c(15,27,18,5)
# n <- 314
# 
# samp <- genera_dati(brk=brk,nnn=nnn,n=n)
# length(samp)
# 
# st <- stat_base(samp,brk)
# st$dat2
# st$dat3
# st$F_print(2)
# st$histp(T)
# st$percentile()
# st$h.int(st$Q.int(.5),10)

# Fine di: PROVA.R

# Inizio di: chi-quadro.R
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
                   "0"=paste0("$",tobs_s,ifelse(h1=="<",">","<"),tc[1],"$, quindi **non** rifiuto $H_0$ a **nessun** livello di significatività, $p_\\text{value}>0.1$"),
                   "1"=paste0("$",min(tc[1:2]),"<",tobs_s,"<",max(tc[1:2]),"$, indecisione sul rifiuto di $H_0$ al 10%, $0.05<p_\\text{value}<0.1$,\n\n _marginalmente significativo_ $\\fbox{.}$."),
                   "2"=paste0("$",min(tc[2:3]),"<",tobs_s,"<",max(tc[2:3]),"$, quindi **rifiuto** $H_0$ al 5%, $0.01<p_\\text{value}<0.05$,\n\n _significativo_   $\\fbox{*}$."),
                   "3"=paste0("$",min(tc[3:4]),"<",tobs_s,"<",max(tc[3:4]),"$, quindi **rifiuto** $H_0$ all'1%, $0.001<p_\\text{value}<0.01$,\n\n _molto significativo_  $\\fbox{**}$."),
                   "4"=paste0("$",tobs_s,">",tc[4],"$, quindi **rifiuto** $H_0$ sotto all'1‰, $p_\\text{value}<0.001$,\n\n _estremamente significativo_ $\\fbox{***}$."),
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

# Fine di: chi-quadro.R

# Inizio di: main-functions.R
# Funzioni Usate nel libro ####
# Patrizio Frederic ####

#_______________________ ####

## funzioni speciali ####

srt <- ""
src_ <- function(x) {paste(srt,x,sep = "")}

#' Colori di base
#'
#' Questi colori di base sono definiti utilizzando il modello di colore RGB. 
#' I colori includono diverse tonalità di blu e un colore rosso.
#'
#' @details
#' I colori definiti sono:
#' \itemize{
#'   \item \code{iblue}: Una versione scurita del colore base blu.
#'   \item \code{mblue}: Il colore base blu.
#'   \item \code{ablue}: Una versione più chiara del colore base blu.
#'   \item \code{ared}: Il colore base rosso.
#' }
#'
#' La funzione \code{darken} viene utilizzata per regolare la luminosità del colore base blu per creare \code{iblue}.
#'
#' @examples
#' # Visualizza i colori
#' plot(1,1, col = iblue, pch = 16, cex = 5,xlim=c(0,5),ylim=c(0,5))
#' points(2,2, col = mblue, pch = 16, cex = 5)
#' points(3,3, col = ablue, pch = 16, cex = 5)
#' points(4,4, col = ared, pch = 16, cex = 5)
#'
#' @name colori_base
NULL

# Definizione dei colori di base
#' @rdname colori_base
iblue <- darken(rgb(0.024, 0.282, 0.478), amount = .4)

#' @rdname colori_base
mblue <- rgb(0.024, 0.282, 0.478)

#' @rdname colori_base
ablue <- rgb(0.729, 0.824, 0.878)

#' @rdname colori_base
ared  <- rgb(0.671, 0.161, 0.18)


#' Converte una Lista in un Ambiente e Assegna gli Elementi alla Global Environment
#'
#' Questa funzione prende una lista di oggetti e li assegna alla global environment, 
#' rendendo ogni elemento della lista direttamente accessibile come un oggetto 
#' indipendente nell'ambiente globale.
#'
#' @param x Lista degli oggetti da convertire e assegnare nell'ambiente globale.
#' 
#' @details La funzione utilizza `list2env()` per convertire la lista in un ambiente, 
#' poi assegna questo ambiente alla global environment. Gli oggetti all'interno della 
#' lista diventano così direttamente accessibili nell'ambiente globale. La funzione 
#' opera in modo invisibile, senza produrre output diretti.
#'
#' @examples
#' my_list <- list(a = 1, b = 2, c = 3)
#' ls2e(my_list)
#' a # Dovrebbe restituire 1
#' b # Dovrebbe restituire 2
#' c # Dovrebbe restituire 3
#'
#' @export
ls2e <- function(x) {
  n <- sys.parent()
  invisible(list2env(x, envir = parent.frame(n = n+1)))
  }

#' Arrotonda Tutte le Variabili Numeriche in un Ambiente
#'
#' Questa funzione arrotonda iterativamente tutte le variabili numeriche a un numero specificato
#' di cifre decimali all'interno dell'ambiente fornito. Di default, utilizza l'ambiente da cui
#' la funzione viene chiamata.
#'
#' @param dig Un intero che specifica il numero di cifre decimali a cui i valori numerici devono
#'            essere arrotondati. Il valore di default è 4.
#' @param exclude Un vettore di caratteri che specifica i nomi delle variabili da escludere
#'                dall'arrotondamento. Il valore di default è NULL.
#' @param env L'ambiente in cui le variabili numeriche devono essere arrotondate.
#'            Il valore di default è il frame genitore da cui viene chiamata la funzione.
#'
#' @return Nessun valore di ritorno, la funzione opera modificando direttamente le variabili
#'         nell'ambiente specificato.
#'
#' @examples
#' # Supponiamo di avere alcune variabili numeriche nell'ambiente globale:
#' x <- 3.14159
#' y <- 2.71828
#' z <- 1.61803
#' # Utilizziamo round_all per arrotondarle a 2 cifre decimali, escludendo 'y'
#' round_all(dig = 2, exclude = c("y"))
#' 
#' print(x) # 3.14
#' print(y) # 2.71828 (non arrotondato)
#' print(z) # 1.62
#'
#' @export

# round_all <- function(dig = 4, exclude = NULL, env = parent.frame()) {
#   nomi_variabili <- ls(envir = env)
#   
#   # Itera sui nomi delle variabili per arrotondare quelle numeriche
#   for (nome in nomi_variabili) {
#     # Controlla se la variabile è nella lista degli esclusi
#     if (!is.null(exclude) && nome %in% exclude) {
#       next
#     }
#     valore <- get(nome, envir = env)
#     if (is.numeric(valore)) {
#       # Arrotonda il valore e aggiornalo nell'ambiente specificato
#       assign(nome, round(valore, digits = dig), envir = env)
#     }
#   }
# }

round_all <- function(dig = 4, exclude = NULL) {
  # Ottenere i nomi delle variabili nell'ambiente specificato
  n_sys <- sys.parent()
  nomi_variabili <- ls(envir = parent.frame(n_sys+1))
  
  # Filtrare le variabili da escludere
  if (!is.null(exclude)) {
    nomi_variabili <- setdiff(nomi_variabili, exclude)
  }
  
  # Funzione per arrotondare i valori numerici
  arrotonda_se_numerico <- function(nome) {
    valore <- get(nome, envir = parent.frame(n_sys+1))
    if (is.numeric(valore)) {
      assign(nome, round(valore, digits = dig), envir = parent.frame(n_sys+1))
    }
  }
  
  # Applicare la funzione a tutte le variabili
  invisible(lapply(nomi_variabili, arrotonda_se_numerico))
}
#' Crea una Tabella Stilizzata con kable e kableExtra
#'
#' Questa funzione genera una tabella stilizzata partendo da un data frame o una matrice,
#' utilizzando `kable` di **knitr** per la creazione della tabella e `kable_styling` di
#' **kableExtra** per l'applicazione dello stile.
#'
#' @param x Un oggetto data frame o matrice da visualizzare come tabella.
#' @param ... Parametri addizionali passati a `kable`.
#'
#' @details La funzione imposta `booktabs = TRUE` per utilizzare la formattazione 
#' Booktabs, che migliora l'aspetto delle tabelle LaTeX e HTML, e `escape = FALSE` 
#' per evitare l'escape automatico di caratteri speciali LaTeX. Inoltre, applica
#' specifiche opzioni di styling attraverso `kable_styling` per controllare l'aspetto
#' finale della tabella, come `full_width = FALSE` e l'opzione `latex_options` impostata
#' su "HOLD_position" per gestire la posizione della tabella.
#'
#' @return Restituisce un oggetto di tipo tabella stilizzata pronto per essere stampato
#' o visualizzato.
#'
#' @examples
#' # Assicurati di avere i pacchetti knitr e kableExtra installati e caricati
#' if (require(knitr) && require(kableExtra)) {
#'   data(mtcars)
#'   tabl(mtcars[1:5, 1:5])
#' }
#'
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling
#' @export
tabl <- function(x, ...) {
  kable(x, ..., booktabs = TRUE, escape = FALSE, linesep = "") %>%
    kable_styling(full_width = FALSE, latex_options = "HOLD_position")
}

#' Genera Punto Elenco Personalizzato
#'
#' Questa funzione genera un punto elenco personalizzato nel formato numero.lettera.
#'
#' @param i1 Numero intero per il punto elenco.
#' @param i2 Indice per la lettera nel punto elenco, dove 1 = a, 2 = b, ecc.
#'
#' @return Restituisce una stringa che rappresenta il punto elenco personalizzato.
#'
#' @examples
#' item(1, 1) # Restituisce "1.a"
#' item(2, 3) # Restituisce "2.c"
#'
#' @export

item <- function(new=FALSE,num = FALSE){
  sp <- "."
  sp2 <- " "
  if (!exists("i1"))  {i1 <- NULL;sp <- "";sp2 <- "."}
  if (!exists("i2"))  {i2 <- 0} 
  if (new) assign("i2",1, envir = .GlobalEnv) else assign("i2",i2 + 1, envir = .GlobalEnv)
  it <- (paste(i1,sp,ifelse(num,i2,letters[i2]),sp2,sep = ""))
  return(it)}


item2 <- function(new=FALSE,start=FALSE,num = FALSE){
  sp <- "."
  sp2 <- " "
  if (!exists("i1")) assign("i1",1, envir = .GlobalEnv)
  if (!exists("i2")) assign("i2",0, envir = .GlobalEnv)
  if (start) assign("i1",0, envir = .GlobalEnv)
  if (new)   assign("i1",i1 + 1, envir = .GlobalEnv)
  if (new)   assign("i2",1, envir = .GlobalEnv) else assign("i2",i2 + 1, envir = .GlobalEnv)
  it <- (paste(i1,sp,ifelse(num,i2,letters[i2]),sp2,sep = ""))
  return(it)}

item_start <- function(num = TRUE) item2(new = TRUE,start = TRUE,num = num)
item_      <- function(num = TRUE) item2(new = FALSE,start = FALSE,num = num)
item_next  <- function(num = TRUE) item2(new = TRUE,start = FALSE,num = num)

#' Formatta Numeri con Parentesi per Valori Negativi
#'
#' Questa funzione formatta i numeri aggiungendo parentesi intorno ai valori negativi
#' e arrotonda il numero alla precisione specificata.
#'
#' @param x Numero da formattare.
#' @param ax Numero di cifre decimali a cui arrotondare il valore di `x`. 
#'           Il valore predefinito è 4.
#'
#' @return Restituisce una stringa che rappresenta il numero formattato,
#'         con parentesi intorno ai valori negativi.
#'
#' @examples
#' p(-3.14159265) # Restituisce "(-3.1416)"
#' p(3.14159265)  # Restituisce "3.1416"
#'
#' @export
p <- function(x, ax = 4) {
  p1 <- ifelse(x < 0, "(", "")
  p2 <- ifelse(x < 0, ")", "")
  paste(p1, round(x, ax), p2, sep = "")
}

p0 <- paste0

#' Prepara i punteggi e le enumerazioni per i compiti
#'
#'
#' @param start logico, si inizia dall'esercizio 1?.
#' @param nex logico, prossimo esercizio?
#' @param num logico, il secondo contatore è numerico?
#'
#' @return il numero progressivo dell'esercizio e il relativo punteggio
#' 
#' @details
#' l'oggetto \code{punti} va definito nel \code{.GlobalEnv}
#' 
#'
#' @examples
#' punti <- list(
#'   e1 = c(14,3,2,2),
#'   e2 = c(14,3,2,2),
#'   e3 = c(14),
#'   e4 = c(3,3,3,3),
#'   e5 = c(4,10),
#'   e6 = c(14,3,2,2,2)
#' )
#' punt_p(start=T)
#' punt_p()
#' punt_p()
#' punt_p(nex=T)
#'
#' punt_p(num=T)
#' @export
punt_p <- function(start=F,nex=F,num=F){
  tot <- sum(unlist(punti))
  it <- ifelse(start,item_start(num),item_(num))
  it <- ifelse(nex,item_next(num),it)
  ptt <- punti[[i1]][i2]
  ptt_30 <- round(ptt/tot*31,1)
  paste(it,"**(Punti ",ptt,"/",tot," $\\rightarrow$ ",ptt_30,"/31)**",sep="")
}


ellisse <- function (x, y, a = 1, b = 1, agl = 0, segment = NULL, arc.only = TRUE, 
                     deg = TRUE, nv = 100, border = NULL, col = NA, lty = 1, lwd = 1, 
                     ...) {
  if (is.null(segment)) {
    if (deg) 
      segment <- c(0, 360)
    else segment <- c(0, 2 * pi)
  }
  draw1ellipse <- function(x, y, a = 1, b = 1, agl = 0, segment = NULL, 
                           arc.only = TRUE, nv = 100, deg = TRUE, border = NULL, 
                           col = NA, lty = 1, lwd = 1, ...) {
    if (deg) {
      agl <- agl * pi/180
      segment <- segment * pi/180
    }
    z <- seq(segment[1], segment[2], length = nv + 1)
    xx <- a * cos(z)
    yy <- b * sin(z)
    alpha <- xyagl(xx, yy, directed = TRUE, deg = FALSE)
    rad <- sqrt(xx^2 + yy^2)
    xp <- rad * cos(alpha + agl) + x
    yp <- rad * sin(alpha + agl) + y
    if (!arc.only) {
      xp <- c(x, xp, x)
      yp <- c(y, yp, y)
    }
    polygon(xp, yp, border = border, col = col, lty = lty, 
            lwd = lwd, ...)
    invisible(NULL)
  }
  xyagl <- function(x, y, directed = FALSE, deg = TRUE) {
    if (missing(y)) {
      y <- x[, 2]
      x <- x[, 1]
    }
    out <- atan2(y, x)
    if (!directed) 
      out <- out%%pi
    if (deg) 
      out <- out * 180/pi
    out
  }
  if (missing(y)) {
    y <- x[, 2]
    x <- x[, 1]
  }
  n <- length(x)
  if (length(a) < n) 
    a <- rep(a, n)[1:n]
  if (length(b) < n) 
    b <- rep(b, n)[1:n]
  if (length(agl) < n) 
    agl <- rep(agl, n)[1:n]
  if (length(col) < n) 
    col <- rep(col, n)[1:n]
  if (length(border) < n) 
    border <- rep(border, n)[1:n]
  if (length(nv) < n) 
    nv <- rep(nv, n)[1:n]
  if (n == 1) 
    draw1ellipse(x, y, a, b, agl = agl, segment = segment, 
                 arc.only = arc.only, deg = deg, nv = nv, col = col, 
                 border = border, lty = lty, lwd = lwd, ...)
  else {
    if (length(segment) < 2 * n) 
      segment <- matrix(rep(segment, n), n, 2, byrow = TRUE)
    lapply(1:n, function(i) draw1ellipse(x[i], y[i], a[i], 
                                         b[i], agl = agl[i], segment = segment[i, ], arc.only = arc.only, 
                                         deg = deg, nv = nv[i], col = col[i], border = border[i], 
                                         lty = lty, lwd = lwd, ...))
  }
  invisible(NULL)
}



# Fine di: main-functions.R

# Inizio di: media_varianza.R
#' Calcoli Statistici: Media e Varianza
#'
#' Queste funzioni forniscono dettagli sui passaggi di calcolo per la media e la varianza,
#' e un'analisi completa combinando entrambi i calcoli.
#'
#' @details La funzione \code{media_} scrive i passaggi del calcolo della media,
#' mentre \code{var_} quelli della varianza. La funzione \code{stat_} chiama entrambe
#' per fornire un'analisi completa.
#'
#' @param x Un vettore di valori numerici.
#' @param p Un vettore opzionale di probabilità associato a \code{x}.
#' @param mnam Nome simbolico da utilizzare per la media, default è \eqn{\mu}.
#' @param vnam Nome simbolico da utilizzare per la varianza, default è \eqn{\sigma^2}.
#' @param semp Un booleano; se \code{TRUE}, calcola usando la semplificazione per dati campionari.
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' p <- c(0.1, 0.2, 0.3, 0.2, 0.2)
#' media_(x)
#' var_(x)
#' stat_(x, p=p)
#'
#' @rdname calcoli-statistici
media_ <- function(x,p=NULL,mnam="\\mu",semp = F){
  n <- length(x)
  if (!semp){
    if(is.null(p)){
      cat("\\[",mnam,"=\\frac 1{",n,"}(",paste(x,collapse = "+"),")=",mean(x),"\\]")
    }
    if(!is.null(p)){
      
      cat("\\[",mnam,"=",paste(paste(x,p,sep = " \\cdot "),collapse = "+"),"=",sum(x*p),"\\]")
    }} else {
      freq <- table(x)
      xx <- dimnames(freq)$x
      cat("\\[",mnam,"=\\left(",paste(paste(xx,"\\frac {",freq,"}{",n,"}"),collapse = "+"),"\\right)=",mean(x),"\\]")
    }
}
#' @rdname calcoli-statistici
var_ <- function(x,p=NULL,mnam="\\sigma^2",semp=F){
  n <- length(x)
  m <- ifelse(test = is.null(p),mean(x),sum(x*p))
  if (!semp){
    if(is.null(p)){
      cat("\\[",mnam,"=\\frac 1{",n,"}(",paste(paste(x,2,sep = "^"),collapse = "+"),")-(",m,")^2=",s2c(x),"\\]")
    }
    if(!is.null(p)){
      p <- sum(p)
      cat("\\[",mnam,"=(",paste(paste(paste(x,2,sep = "^"),p,sep = " \\cdot "),collapse = "+"),")-(",m,")^2=",vvv(x = x,p = p),"\\]")
    }} else {
      freq <- table(x)
      xx <- dimnames(freq)$x
      cat("\\[",mnam,"=\\left(",paste(paste(xx,"^2\\frac {",freq,"}{",n,"}"),collapse = "+"),"\\right)-(",m,")^2=",s2c(x),"\\]")
    }
}
#' @rdname calcoli-statistici
stat_ <- function(x,p=NULL,mnam="\\mu",vnam="\\sigma^2",semp=F){
  n <- length(x)
  m <- ifelse(test = is.null(p),mean(x),sum(x*p))
  p1 <- character(n)
  p2 <- character(n)
  p1[x<0]<-"("
  p2[x<0]<-")"
  xp <- paste(p1,x,p2)
  if (!semp){
    if(is.null(p)){
      cat("\\begin{eqnarray*}\n",
          mnam,"&=& \\frac 1{",n,"}(",paste(xp,collapse = "+"),")=",mean(x),"\\\\ \n",
          vnam,"&=& \\frac 1{",n,"}(",paste(paste(xp,2,sep = "^"),collapse = "+"),")-(",m,")^2=",s2c(x),
          "\n\\end{eqnarray*}\n")
    }
    if(!is.null(p)){
      p <- p/sum(p)
      pp <- round(p,4)
      cat("\\begin{eqnarray*}\n",
          mnam,"&=&",paste(paste(xp,pp,sep = " \\cdot "),collapse = "+"),"=",sum(x*p),"\\\\",
          vnam,"&=&(",paste(paste(paste(xp,2,sep = "^"),pp,sep = " \\cdot "),collapse = "+"),")-(",m,")^2=",vvv(x = x,p = p),
          "\\end{eqnarray*}")
    }} else {
      freq <- table(x)
      xx <- dimnames(freq)$x
      p1 <- character(length(xx))
      p2 <- character(length(xx))
      p1[xx<0]<-"("
      p2[xx<0]<-")"
      xx <- paste(p1,xx,p2)
      cat("\\begin{eqnarray*}",
          mnam,"&=& E(X_i) = \\sum_{x\\in S_X}x P(X=x)\\\\ \n",
          "&=&",paste(paste(xx,"\\frac {",freq,"}{",n,"}"),collapse = "+"),"\\\\ 
            &=&",mean(x),"\\\\ \n",
          vnam,"&=& V(X_i) = \\sum_{x\\in S_X}x^2 P(X=x)-\\mu^2\\\\ \n",
          "&=&\\left(",paste(paste(xx,"^2\\frac {",freq,"}{",n,"}"),collapse = "+"),"\\right)-(",m,")^2\\\\ 
            &=&",s2c(x),
          "\n\\end{eqnarray*}\n")
    }
}


# Fine di: media_varianza.R

# Inizio di: probabilita.R

## Probabilità ####
#' Calcolo della probabilità dell'unione di due eventi
#'
#' Questa funzione calcola la probabilità dell'unione di due eventi \eqn{A} e \eqn{B} dati le probabilità individuali \eqn{P(A)} e \eqn{P(B)}.
#'
#' @param pa La probabilità dell'evento \eqn{A}.
#' @param pb La probabilità dell'evento \eqn{B}.
#' @param A Il nome dell'evento \eqn{A} (opzionale, predefinito è "A").
#' @param B Il nome dell'evento \eqn{B} (opzionale, predefinito è "B").
#' @param dig Il numero di cifre decimali per l'arrotondamento (opzionale, predefinito è 4).
#'
#' @details
#' La probabilità dell'unione di due eventi \eqn{A} e \eqn{B} è data dalla formula:
#' \deqn{P(A \cup B) = P(A) + P(B) - P(A \cap B)}
#' dove \eqn{P(A \cap B) = P(A) \cdot P(B)} se \eqn{A} e \eqn{B} sono indipendenti.
#'
#' La funzione stampa la formula e il risultato calcolato in notazione LaTeX.
#'
#' @examples
#' p_aub(0.5, 0.3)
#' p_aub(0.5, 0.3, A = "Evento1", B = "Evento2", dig = 2)
#'
#' @export
p_aub <- function(pa,pb,A="A",B="B",dig = 4){
  pab <- pa+pb-pa*pb
  round_all(4)
  cat("\\begin{eqnarray}
      P(",A,"\\cup",B,") &=& P(",A,")+P(",B,")-P(",A,"\\cap",B,") \\\\
                         &=& P(",A,")+P(",B,")-P(",A,")\\cdot (",B,") \\\\
                         &=& ",pa,"+",pb,"-",pa,"\\times",pb," \\\\
                         &=& ", pab,
      "\\end{eqnarray}"
      )
}
#' Calcolo della probabilità dell'intersezione di due eventi
#'
#' Questa funzione calcola la probabilità dell'intersezione di due eventi \eqn{A} e \eqn{B} dati le probabilità individuali \eqn{P(A)} e \eqn{P(B)}.
#'
#' @param pa La probabilità dell'evento \eqn{A}.
#' @param pb La probabilità dell'evento \eqn{B}.
#' @param A Il nome dell'evento \eqn{A} (opzionale, predefinito è "A").
#' @param B Il nome dell'evento \eqn{B} (opzionale, predefinito è "B").
#' @param dig Il numero di cifre decimali per l'arrotondamento (opzionale, predefinito è 4).
#'
#' @details
#' La probabilità dell'intersezione di due eventi \eqn{A} e \eqn{B} è data dalla formula:
#' \deqn{P(A \cap B) = P(A) \cdot P(B)}
#' se \eqn{A} e \eqn{B} sono indipendenti.
#'
#' La funzione stampa la formula e il risultato calcolato in notazione LaTeX.
#'
#' @examples
#' p_ab(0.5, 0.3)
#' p_ab(0.5, 0.3, A = "Evento1", B = "Evento2", dig = 2)
#'
#' @export
p_ab <- function(pa,pb,A="A",B="B",dig = 4){
  pab <- pa*pb
  round_all(4)
  cat("\\begin{eqnarray}
      P(",A,"\\cap",B,") &=& P(",A,")\\cdot P(",B,") \\\\
                         &=& ", pa,"\\times",pb," \\\\
                         &=& ", pab,
      "\\end{eqnarray}"
  )
}

#' Convolution: Metodi Dettagliati
#'
#' Queste funzioni eseguono e stampano i passaggi di una convoluzione finita tra due insiemi di dati.
#' \code{two_way} utilizza le frequenze senza semplificare, mentre \code{two_way2} utilizza le frequenze calcolate.
#'
#' @param S_1 Insieme dei valori per la prima variabile.
#' @param S_2 Insieme dei valori per la seconda variabile.
#' @param num1 Frequenze o probabilità per \code{S_1} in \code{two_way}.
#' @param num2 Frequenze o probabilità per \code{S_2} in \code{two_way}.
#' @param p1 Probabilità per \code{S_1} in \code{two_way2}.
#' @param p2 Probabilità per \code{S_2} in \code{two_way2}.
#' @param op Operatore da utilizzare nella convoluzione (default è \code{`+`}).
#' @param EV Se \code{TRUE}, calcola valore atteso e varianza.
#' @param vnam Nome variabile per l'output.
#' @param size Dimensione del testo per l'output LaTeX.
#'
#' @return Entrambe le funzioni stampano i dettagli della convoluzione ma non restituiscono valori.
#'
#' @examples
#' S_1 <- c(1, 2)
#' S_2 <- c(3, 4)
#' num1 <- c(1, 1)
#' num2 <- c(1, 1)
#' p1 <- c(0.5, 0.5)
#' p2 <- c(0.5, 0.5)
#' two_way(S_1, S_2, num1, num2)
#' two_way2(S_1, S_2, p1, p2)
#'
#' @rdname convolution-methods
two_way <- function(S_1,S_2,num1,num2,op=`+`,EV=T,vnam="X",size="\\normalsize "){
  html <- ifelse(exists("html"),html,T)
  den1 <- rep(sum(num1),times=length(S_1))
  den2 <- rep(sum(num2),times=length(S_2))
  k1 <- length(S_1)
  k2 <- length(S_2)
  P_1 <- num1/den1
  P_2 <- num2/den2
  
  SS <- outer(S_2,S_1,op)
  p1 <- P_1
  NN <- outer(num2,num1)
  DD <- outer(den2,den1)
  
  frc  <- ifelse(html,"\\frac","\\sfrac")  
  hed  <- ifelse(html,"\\[\n","\\[\\arraycolsep=10pt\\def\\arraystretch{1}")  
  frac <- function(i,j){
    paste(op(S_2[i],S_1[j]),";&","\\color{red}{",frc,"{",num2[i]*num1[j],"}{",den2[i]*den1[j],"}}",sep="")
  }
  
  mat1 <- outer(1:length(S_2),1:length(S_1),frac)
  mat1 <- cbind(paste(S_2,";\\color{blue}{",num2,"/",den2,"}"),mat1)
  cols <- paste(paste(paste("&",paste(paste(S_1,";"),paste("{",num1,"}"),sep=paste("&\\color{blue}{", frc))),paste("{",den1,"}}"),collapse = " "),"\\\\ \n")
  
  allign <- paste("r|",paste(rep("r",times=2*k1),collapse = ""),sep="",collapse = "")
  
  cat(size,"\n\n")
  cat(hed,"\\begin{array}",paste("{",allign,"}\n"),
      cols,"\\hline \n",
      apply(mat1,1,function(x)c(paste(x,collapse = "& "),"\\\\ \n")),
      "\\end{array}\n \\]\n\n",sep ="")
  
  cat("\\normalsize E ricaviamo la distribuzione di,",vnam,"\n\n")
  
  S_3 <- sort(unique(as.numeric(outer(S_1,S_2,op))))
  num3 <- 1:length(S_3)
  den3 <- 1:length(S_3)
  for (i in 1:length(S_3)){
    num3[i] <- sum(NN[SS==S_3[i]])
    den3[i] <- DD[SS==S_3[i]][1]
  }
  
  k3 <- length(S_3)
  allign <- paste("r|",paste(rep("r",times=k3),collapse = ""),sep="",collapse = "")
  rig1 <- paste(vnam," &",paste(S_3,collapse = "& "),"\\\\ \n")
  rig2 <- paste("P(",vnam,") &",paste(frc,"{",num3,"}","{",den3,"}",collapse = "& ",sep=""))
  cat(size,"\n\n")
  cat("\\[
     \\begin{array}{",allign,"}\n",
      rig1,"\\hline \n",
      rig2,
      "\\\\ \n \\end{array}\n \\]\n")
  urn <- rep(S_3,times=num3)
  if (EV){   
    cat("\\normalsize Calcoliamo valore atteso e varianza\n\n")
    cat(size,"\n\n")
    cat(stat_(urn,semp = T),"\\normalsize\n\n")
  }
  return(list(S_3=S_3,num3=num3,den3=den3,urn=urn))
}

#' @rdname convolution-methods
two_way2 <- function(S_1,S_2,p1,p2,op=`+`,EV=T,vnam="X",size="\\normalsize "){
  html <- ifelse(exists("html"),html,T)
  k1 <- length(S_1)
  k2 <- length(S_2)
  
  SS <- outer(S_2,S_1,op)
  
  hed  <- ifelse(html,"\\[\n","\\[\\arraycolsep=10pt\\def\\arraystretch{1}")  
  frac <- function(i,j){
    paste(op(S_2[i],S_1[j]),";& ","\\color{red}{",p(p2[i]*p1[j]),"}",sep="")
  }
  
  mat1 <- outer(1:length(S_2),1:length(S_1),frac)
  cols <- paste(S_1,"~~\\color{blue}{",p(p1),"}")
  hed2 <- paste("&",paste(S_2,"&\\color{blue}{",p(p2),"}",collapse = " & "))
  
  
  allign <- paste("r|",paste(rep("r",times=2*k1),collapse = ""),sep="",collapse = "")
  
  cat(size,"\n\n")
  cat(hed,"\\begin{array}",paste("{",allign,"}\n"),
      hed2,"\\\\ \\hline \n",
      apply(cbind(cols,mat1),1,function(x)c(paste(x,collapse = "& "),"\\\\ \n")),
      "\\end{array}\n \\]\n\n",sep ="")
  
  cat("\\normalsize E ricaviamo la distribuzione di,",vnam,"\n\n")
  
  ord <- sort((as.numeric(outer(S_1,S_2,op))))
  pos <- order((as.numeric(outer(S_1,S_2,op))))
  p3  <- tapply(as.numeric(outer(p1,p2))[pos],INDEX = ord,FUN = sum)
  S_3 <- unique(ord)
  k3 <- length(S_3)
  allign <- paste("r|",paste(rep("r",times=k3),collapse = ""),sep="",collapse = "")
  rig1 <- paste("X &",paste(S_3,collapse = "& "),"\\\\ \n")
  rig2 <- paste("P(X) &",paste(p(p3),collapse = "& ",sep=""))
  cat(size,"\n\n")
  cat("\\[
     \\begin{array}{",allign,"}\n",
      rig1,"\\hline \n",
      rig2,
      "\\\\ \n \\end{array}\n \\]\n")
  if (EV){   
    cat("\\normalsize Calcoliamo valore atteso e varianza\n\n")
    cat(size,"\n\n")
    cat(stat_(S_3,p = p3),"\\normalsize\n\n")
  }
}

#' Distribuzioni di Probabilità: Binomiale, Poisson, Normale
#'
#' Queste funzioni calcolano e visualizzano i passaggi per le distribuzioni di probabilità binomiale (`bin_dis`), di Poisson (`pois_dis`), e normale (`norm_int`).
#' 
#' @param x1,x2 Valori di interesse per il calcolo della probabilità.
#' @param n Numero di prove in `bin_dis`.
#' @param pp Probabilità di successo in `bin_dis`.
#' @param verso Specifica se calcolare la probabilità cumulativa a sinistra (\eqn{\leq}) o a destra (\eqn{\geq}).
#' @param comp Se `TRUE`, calcola la probabilità complementare in `bin_dis`.
#' @param sing Se `TRUE`, calcola la probabilità per un singolo valore.
#' @param x0 Valore iniziale per il calcolo in `bin_dis`.
#' @param vnam Nome della variabile casuale.
#' @param size Dimensione del testo LaTeX.
#' @param ll Parametro \eqn{\lambda} per la distribuzione di Poisson in `pois_dis`.
#' @param mm Media della distribuzione in `norm_int`.
#' @param ss Varianza della distribuzione in `norm_int`.
#'
#' @details
#' \itemize{
#' \item `bin_dis` visualizza i passaggi per il calcolo della probabilità in una distribuzione binomiale.
#' \item `pois_dis` mostra i passaggi per una distribuzione di Poisson.
#' \item `norm_int` calcola e visualizza i passaggi per probabilità in una distribuzione normale.
#'}
#' @examples
#' # Distribuzione binomiale
#' bin_dis(x1 = 2, n = 5, pp = 0.5)
#'
#' # Distribuzione di Poisson
#' pois_dis(x1 = 3, ll = 2.5)
#'
#' # Distribuzione normale
#' norm_int(x1 = -1.96, x2 = 1.96, mm = 0, ss = 1)
#'
#' @rdname distribuzioni-probabilita
#' 
bin_dis <- function(x1,n,pp,verso="\\leq",comp=FALSE,sing=FALSE,x0=0,vnam="X",size="\\normalsize"){
  ver_c <- ifelse(verso=="\\leq",">","<")
  pp <- round(pp,4)
  
  if (sing){
    p0 <- paste("P(",vnam,"=",x1,") &=&","\\binom{",n,"}{",x1,"}",pp,"^{",x1,"}(1-",pp,")^{",n,"-",x1,"}","\\\\")
    p1 <- paste("                &=&",choose(n,x1),"\\times",pp,"^{",x1,"}(1-",pp,")^{",n-x1,"}","\\\\")
    p2 <- paste("                &=&",round(dbinom(x1,n,pp),4))
    res <- paste(p0,p1,p2)
  }
  if (!sing){
    if (!comp){
      if (verso == "\\leq")  xx <- x0:x1 else xx <- x1:n
      c0 <- paste("P(",vnam,verso,x1,") &=&",paste("\\binom{",n,"}{",xx,"}",pp,"^{",xx,"}(1-",pp,")^{",n,"-",xx,"}",collapse = "+"),"\\\\")
      c1 <- paste("                &=&",paste(round(dbinom(xx,n,pp),4),collapse = "+"),"\\\\")
      c2 <- paste("                &=&",sum(round(dbinom(xx,n,pp),4)))
      res <- paste(c0,c1,c2)
    } 
    if (comp) {
      if (verso == "\\leq")  xx <- (x1+1):n else xx <- 0:(x1-1)
      c00<- paste("P(",vnam,verso,x1,") &=&","1-P(",vnam,ver_c,x1,")","\\\\")
      if (max(xx)>n){
        c0 <- " &=& 1-0\\\\"
        c1 <- " &=& 1"
        c2 <- c3 <- ""
      } else
        if (min(xx)<0){
          c0 <- " &=& 1-1\\\\"
          c1 <- " &=& 0"
          c2 <- c3 <- ""
        } else {
          
          c0 <- paste("                &=& 1-\\left(",paste("\\binom{",n,"}{",xx,"}",pp,"^{",xx,"}(1-",pp,")^{",n,"-",xx,"}",collapse = "+"),"\\right)\\\\")
          c1 <- paste("                &=& 1-(",paste(round(dbinom(xx,n,pp),4),collapse = "+"),")\\\\")
          c2 <- paste("                &=& 1-",sum(round(dbinom(xx,n,pp),4)),"\\\\")
          c3 <- paste("                &=&  ",1-sum(round(dbinom(xx,n,pp),4)))
          
        }
      res <- paste(c00,c0,c1,c2,c3)  
    } 
  }
  
  cat("",size,"
   \\begin{eqnarray*}
     ",res,"
   \\end{eqnarray*}
   \\normalsize ")
}

#' @rdname distribuzioni-probabilita
#' 
pois_dis <- function(x1,ll,verso="\\leq",sing=FALSE,vnam="X"){
  ### Calcola la probabilità di una Binomiale P(X<=x) 
  ##  Input
  ##  x0 limite inf, se diverso da zero usare comp=F
  ##  x1 limite sup
  ##  verso ("\\leq" o "\\geq")
  ##  ll lambda
  ##  vnam nome X
  
  ver_c <- ifelse(verso=="\\leq",">","<")
  
  if (sing){
    p0 <- paste("P(",vnam,"=",x1,")  &=&","\\frac{",ll,"^{",x1,"}}{",x1,"!}e^{-",ll,"}\\\\")
    p1 <- paste("                &=&",ll^x1/factorial(x1),"\\times",round(exp(-ll),4),"\\\\")
    p2 <- paste("                &=&",round(dpois(x1,ll),4))
    res <- paste(p0,p1,p2)
  }
  if (!sing){
    if (verso == "\\leq"){
      xx <- 0:x1 
      c0 <- paste("P(",vnam,verso,x1,") &=&",paste("\\frac{",ll,"^{",xx,"}}{",xx,"!}e^{-",ll,"}",collapse = "+"),"\\\\")
      c1 <- paste("                &=&",paste(round(dpois(xx,ll),4),collapse = "+"),"\\\\")
      c2 <- paste("                &=&",sum(round(dpois(xx,ll),4)))
      res <- paste(c0,c1,c2)
    } else {
      xx <- 0:(x1-1)
      c00<- paste("P(",vnam,verso,x1,") &=&","1-P(",vnam,ver_c,x1,")","\\\\")
      c0 <- paste("                &=& 1-\\left(",paste("\\frac{",ll,"^{",xx,"}}{",xx,"!}e^{-",ll,"}",collapse = "+"),"\\right)\\\\")
      c1 <- paste("                &=& 1-(",paste(round(dpois(xx,ll),4),collapse = "+"),")\\\\")
      c2 <- paste("                &=& 1-",sum(round(dpois(xx,ll),4)),"\\\\")
      c3 <- paste("                &=&  ",1-sum(round(dpois(xx,ll),4)))
      res <- paste(c00,c0,c1,c2,c3)  
    } 
  }
  cat("\\begin{eqnarray*}
  ",res,"
\\end{eqnarray*}
")
}

#' @rdname distribuzioni-probabilita
#' 
norm_int <-  function(x1,x2=NULL,verso="<",mm,ss,vnam="X",mu="\\mu",sigma="\\sigma"){
  if (!is.null(verso)){
    z1 <- round((x1 - mm)/sqrt(ss),2)
    f1 <- round(pnorm(z1,lower.tail = (verso == "<")),4)
    
    if (verso == "<") {
      p1 <- ifelse(x1 >=mm, 
                   paste("\\Phi(", z1,") \\\\","&=& ",f1),
                   paste("1-\\Phi(", -z1,") \\\\","&=& ",f1))
      p0 <- ""
    }
    if (verso == ">") {
      p1 <- ifelse(x1 >=mm, 
                   paste("1-\\Phi(", z1,") \\\\", "&=& ",f1),
                   paste("1-(1-\\Phi(", -z1,")) \\\\", "&=& ",f1))
      p0 <- paste(" &=& 1-P(Z<",z1,")\\\\")
    }
    
    mm <- ifelse(mm>=0,mm,paste("(",mm,")"))
    res <- f1
    
    cat("\\begin{eqnarray*}
      P(",vnam," ",verso," ",x1,") 
        &=& P\\left(  \\frac {",vnam," - ",mu,"}{",sigma,"} ",verso," \\frac {",x1," - ",mm,"}{\\sqrt{",ss,"}} \\right)  \\\\
                 &=& P\\left(  Z  ",verso," ",z1,"\\right) \\\\  ",p0,"
                 &=& ",p1,"
      \\end{eqnarray*}")
  } else {
    z1 <- round((x1 - mm)/sqrt(ss),2)
    z2 <- round((x2 - mm)/sqrt(ss),2)
    f1 <- round(pnorm(abs(z1)),4)
    f2 <- round(pnorm(abs(z2)),4)
    res <- round(pnorm(z2),4)-round(pnorm(z1),4)
    x_2 <- ifelse(x2<Inf,x2,"+\\infty")
    z_2 <- ifelse(z2<Inf,z2,"+\\infty")
    
    if (x1<=mm & x2>=mm) {
      p1 <- paste("\\Phi(", z_2,")-(1-\\Phi(",-z1,")) \\\\", "&=& ",f2,"-(1-",f1,") \\\\")}
    if (x1>=mm & x2>=mm) {
      p1 <- paste(f2,"-",f1,"\\\\")
    }
    if (x1<=mm & x2<mm) {
      p1 <- paste("(1-\\Phi(", -z_2,"))-(1-\\Phi(",-z1,")) \\\\","&=& (1-",f2,")-(1-",f1,") \\\\")
    }
    
    mm <- ifelse(mm>=0,mm,paste("(",mm,")"))
    cat("\\begin{eqnarray*}
   P(",x1,"<",vnam,"\\leq ",x_2,") &=& P\\left( \\frac {",x1," - ",mm,"}{\\sqrt{",ss,"}} < \\frac {",vnam," - ",mu,"}{",sigma,"} \\leq \\frac {",x_2," - ",mm,"}{\\sqrt{",ss,"}}\\right)  \\\\
              &=& P\\left( ",z1," < Z \\leq ",z_2,"\\right) \\\\
              &=& \\Phi(",z_2,")-\\Phi(",z1,")\\\\
              &=& ",p1,"
              &=& ",res,"
   \\end{eqnarray*}
   ")
  }
}

norm_semp <- function(x1,mm,ss){
  z1 <- round((x1-mm)/sqrt(ss),2)
  pnorm(z1)
}

#' Teorema del Limite Centrale (TLC): Applicazioni
#'
#' La funzione `tlc` applica il Teorema del Limite Centrale a diverse situazioni: somma di variabili casuali, calcolo della media, proporzione, e la somma o media di variabili casuali di Poisson.
#'
#' @param tipo Specifica il tipo di calcolo: "somma", "media", "prop" (proporzione), "pois_media" (media di Poisson), o "pois_somma" (somma di Poisson).
#' @param x1,x2 Valori di interesse per il calcolo della probabilità. Se `x2` è fornito, il calcolo viene eseguito su un intervallo.
#' @param verso Specifica la direzione della probabilità (\eqn{\leq}, \eqn{\geq}).
#' @param mu Valore atteso \eqn{\mu} delle variabili casuali, o \eqn{\pi} per la proporzione, o \eqn{\lambda} per Poisson.
#' @param s2 Varianza \eqn{\sigma^2} delle variabili casuali (non necessario per "prop").
#' @param n Numero di variabili casuali indipendenti e identicamente distribuite (IID).
#'
#' @details
#' La funzione fornisce un'interfaccia unificata per dimostrare come il TLC può essere applicato in diversi contesti statistici, generando l'approssimazione normale di diverse distribuzioni di probabilità basate su condizioni specificate.
#'
#' @examples
#' # Somma di variabili casuali con varianza nota
#' tlc(tipo = "somma", x1 = 5, verso = "<", mu = 2, s2 = 1, n = 30)
#'
#' # Calcolo della media di variabili casuali
#' tlc(tipo = "media", x1 = 2.5, verso = "<", mu = 2, s2 = 1, n = 50)
#'
#' # Proporzione in una distribuzione binomiale approssimata
#' tlc(tipo = "prop", x1 = 0.6, verso = ">", mu = 0.5, n = 100)
#'
#' # Media di variabili casuali di Poisson
#' tlc(tipo = "pois_media", x1 = 3, verso = "<", mu = 2, n = 50)
#'
#' # Somma di variabili casuali di Poisson
#' tlc(tipo = "pois_somma", x1 = 150, verso = ">", mu = 3, n = 50)
#'
#' @rdname tlc-applicazioni


tlc <- function(tipo, x1, x2=NULL,verso, mu = F, s2 = NULL, n){
  if (tipo == "somma") {  
    if(!is.null(s2)){
      cat("**Teorema del Limite Centrale (somma VC qualunque)** \n\n Siano $X_1$,...,$X_n$, $n=",n,"$ VC IID, tc $E(X_i)=\\mu=",mu,"$ e $V(X_i)=\\sigma^2=",s2,
          ",\\forall i$, posto:
      \\[
      S_n = X_1 + ... + X_n
      \\]
      allora:",
          "\\begin{eqnarray*}
  S_n & \\mathop{\\sim}\\limits_{a}& N(n\\mu,n\\sigma^2) \\\\
     &\\sim & N(",n,"\\cdot",mu,",",n,"\\cdot",s2,") \\\\
     &\\sim & N(",n*mu,",",n*s2,") 
  \\end{eqnarray*}",sep="")
      cat(norm_int(x1 = x1,x2 = x2,verso = verso,mm = n*mu,ss = s2*n,vnam = "S_n",mu = "n\\mu",sigma = "\\sqrt{n\\sigma^2}"))
    } else {
      cat("**Teorema del Limite Centrale (somma di Bernoulli)** \n\n Siano $X_1$,...,$X_n$, $n=",n,"$ VC IID, tc $X_i\\sim\\text{Ber}(\\pi=",mu,")$",
          "$,\\forall i$, posto:
      \\[
      S_n = X_1 + ... + X_n
      \\]
      allora:",
          "\\begin{eqnarray*}
  S_n & \\mathop{\\sim}\\limits_{a}& N(n\\pi,n\\pi(1-\\pi)) \\\\
      &\\sim & N(",n,"\\cdot",mu,",",n,"\\cdot",mu,"\\cdot(1-",mu,")) \\\\
      &\\sim & N(",n*mu,",",n*mu*(1-mu),")
  \\end{eqnarray*}",sep="")
      s2 <- mu*(1-mu)
      cat(norm_int(x1 = x1,x2 = x2,verso = verso,mm = n*mu,ss = s2*n,vnam = "S_n",mu = "n\\pi",sigma = "\\sqrt{n\\pi(1-\\pi)}"))
      
    }
  }
  if (tipo == "media") {  
    cat("**Teorema del Limite Centrale (media VC qualunque)** \n\n Siano $X_1$,...,$X_n$, $n=",n,"$ VC IID, tc $E(X_i)=\\mu=",mu,"$ e $V(X_i)=\\sigma^2=",s2,
        ",\\forall i$, posto:
      \\[
      \\bar X=\\frac{S_n}n =\\frac{X_1 + ... + X_n}n
      \\]
      allora:",
        "\\begin{eqnarray*}
  \\bar X & \\mathop{\\sim}\\limits_{a}& N(\\mu,\\sigma^2/n) \\\\
     &\\sim & N\\left(",mu,",\\frac{",s2,"}{",n,"}\\right) \\\\
     &\\sim & N(",mu,",",s2/n,")
  \\end{eqnarray*}",sep="")
    cat(norm_int(x1 = x1,x2 = x2,verso = verso,mm = mu,ss = s2/n,vnam = "\\bar X",mu = "\\mu",sigma = "\\sqrt{\\sigma^2/n}"))
  }
  if (tipo == "prop") {  
    cat("**Teorema del Limite Centrale (proporzione)** \n\n Siano $X_1$,...,$X_n$, $n=", n, "$ VC IID, tc $X_i\\sim\\text{Ber}(\\pi=",mu,")$",
        "$,\\forall i$, posto:
      \\[
      \\hat\\pi=\\frac{S_n}n = \\frac{X_1 + ... + X_n}n
      \\]
      allora:",
        "\\begin{eqnarray*}
  \\hat\\pi & \\mathop{\\sim}\\limits_{a}& N(\\pi,\\pi(1-\\pi)/n) \\\\
  &\\sim & N\\left(",mu,",\\frac{",mu,"\\cdot(1-",mu,")}{",n,"}\\right) \\\\
     &\\sim & N(",mu,",",mu*(1-mu)/n,") 
  \\end{eqnarray*}",sep="")
    cat(norm_int(x1 = x1,x2 = x2,verso = verso,mm = mu,ss = mu*(1-mu)/n,vnam = "\\hat\\pi",mu = "\\pi",sigma = "\\sqrt{\\pi(1-\\pi)/n}"))
  }
  if (tipo == "pois_media") {  
    cat("**Teorema del Limite Centrale (media di Poisson)** \n\n Siano $X_1$,...,$X_n$, $n=", n, "$ VC IID, tc $X_i\\sim\\text{Pois}(\\lambda=",mu,")$",
        "$,\\forall i$, posto:
      \\[
      \\bar X=\\frac{S_n}n = \\frac{X_1 + ... + X_n}n
      \\]
      allora:",
        "\\begin{eqnarray*}
  \\hat\\pi & \\mathop{\\sim}\\limits_{a}& N(\\lambda,\\lambda/n) \\\\
  &\\sim & N\\left(",mu,",\\frac{",mu,"}{",n,"}\\right) \\\\
     &\\sim & N(",mu,",",mu/n,") 
  \\end{eqnarray*}",sep="")
    cat(norm_int(x1 = x1,x2 = x2,verso = verso,mm = mu,ss = mu/n,vnam = "\\bar X",mu = "\\lambda",sigma = "\\sqrt{\\lambda/n}"))
  }
  if (tipo == "pois_somma") {  
    cat("**Teorema del Limite Centrale (somma di Poisson)** \n\n Siano $X_1$,...,$X_n$, $n=", n, "$ VC IID, tc $X_i\\sim\\text{Pois}(\\lambda=",mu,")$",
        "$,\\forall i$, posto:
      \\[
      S_n = X_1 + ... + X_n
      \\]
      allora:",
        "\\begin{eqnarray*}
  S_n & \\mathop{\\sim}\\limits_{a}& N(n\\lambda,n\\lambda) \\\\
  &\\sim & N(",n,"\\cdot",mu,",",n,"\\cdot",mu,") \\\\
     &\\sim & N(",n*mu,",",n*mu,") 
  \\end{eqnarray*}",sep="")
    cat(norm_int(x1 = x1,x2 = x2,verso = verso,mm = n*mu,ss = n*mu,vnam = "\\bar X",mu = "n\\lambda",sigma = "\\sqrt{n\\lambda}"))
  }
  
}

#' Disegna una distribuzione tratteggiata
#'
#' Questa funzione aggiunge una distribuzione tratteggiata a un grafico esistente, utilizzando un poligono e una curva.
#'
#' @param dist Una funzione che definisce la distribuzione da tracciare.
#' @param z1 Il limite inferiore dell'intervallo su cui tracciare la distribuzione.
#' @param z2 Il limite superiore dell'intervallo su cui tracciare la distribuzione.
#' @param density La densità delle linee tratteggiate (opzionale, predefinito è 20).
#' @param border Il colore del bordo del poligono (opzionale, predefinito è NA).
#' @param col Il colore della distribuzione (opzionale, predefinito è 1).
#' @param ... Argomenti addizionali passati alla funzione \code{polygon}.
#'
#' @details
#' La funzione disegna un poligono tratteggiato per rappresentare la distribuzione tra \code{z1} e \code{z2}, e aggiunge una curva della distribuzione sopra il poligono.
#'
#' @examples
#' # Definisce una distribuzione normale standard
#' dist <- function(x) dnorm(x, mean = 0, sd = 1)
#' 
#' # Disegna la distribuzione normale tra -2 e 2
#' plot(0, type = "n", xlim = c(-3, 3), ylim = c(0, 0.5))
#' draw_dist(dist, -2, 2, col = "blue", density = 30, angle = 45)
#'
#' @export

draw_dist <- function(dist, z1, z2, density = 20, border = NA, col = 1, ...) {
  xx <- c(z1, seq(z1, z2, length = 100), z2)
  yy <- c(0, dist(seq(z1, z2, length = 100)), 0)
  polygon(xx, yy, ..., border = border, density = density, col = col)
  curve(dist, z1, z2, add = TRUE, col = col)
}

#' Traccia la funzione di distribuzione cumulativa
#'
#' Questa funzione traccia la funzione di distribuzione cumulativa (F(x)) utilizzando punti e segmenti per rappresentare la distribuzione discreta data dai valori di ingresso.
#'
#' @param sx Un vettore di valori x per i punti della distribuzione.
#' @param pp Un vettore di probabilità corrispondenti ai valori in \code{sx}.
#' @param xmin Il valore minimo di x per la trama.
#' @param xmax Il valore massimo di x per la trama.
#'
#' @details
#' La funzione prende i valori di ingresso \code{sx} e \code{pp} e traccia la funzione di distribuzione cumulativa utilizzando segmenti orizzontali e verticali. I valori cumulativi sono tracciati sull'asse y.
#'
#' @examples
#' sx <- c(1, 2, 3)
#' pp <- c(0.2, 0.5, 0.3)
#' xmin <- 0
#' xmax <- 4
#' plot_FdR(sx, pp, xmin, xmax)
#'
#' @export
plot_FdR <- function(sx, pp, xmin, xmax) {
  x <- c(xmin, sx, xmax)
  px <- c(0, pp, 0)
  kk <- length(px)
  round_all()
  plot(x, cumsum(px), type = "n", ylab = "F(x)", axes = FALSE)
  axis(1, x)
  axis(2, c(cumsum(px)[1:(kk-2)],1), las = 2)
  segments(x[1:(kk - 1)], cumsum(px)[1:(kk - 1)], x[2:kk], cumsum(px)[1:(kk - 1)],lwd=2)
  segments(x[2:kk], cumsum(px)[1:(kk - 1)], x[2:kk], cumsum(px)[2:kk], lty = 2)
  points(x[2:(kk - 1)], cumsum(px)[1:(kk - 2)])
  points(x[2:(kk - 1)], cumsum(px)[2:(kk - 1)], pch = 16)
}


#' Generazione di coppie di valori con media, deviazione standard e covarianza specificate
#'
#' Questa funzione genera coppie di valori `(x, y)` distribuiti normalmente, 
#' garantendo che abbiano le medie, le deviazioni standard e la covarianza desiderate, 
#' attraverso un processo iterativo di correzione.
#'
#' @param xbar Valore atteso della media di `x`. Default: `0`.
#' @param ybar Valore atteso della media di `y`. Default: `0`.
#' @param sx Deviazione standard desiderata per `x`. Default: `1`.
#' @param sy Deviazione standard desiderata per `y`. Default: `1`.
#' @param sxy Covarianza desiderata tra `x` e `y`. Default: `0`.
#' @param n Numero di coppie `(x, y)` da generare. Default: `10`.
#' @param max_iter Numero massimo di iterazioni per la convergenza della covarianza. Default: `10000`.
#' @param tol Tolleranza per la convergenza della covarianza. Default: `1e-7`.
#' @param dist Funzione di distribuzione da cui generare i valori di `x`. Default: `rnorm`.
#' @param contr Se `TRUE`, stampa a video le differenze tra i valori desiderati e quelli ottenuti. Default: `FALSE`.
#' @param prec Precisione decimale per i risultati della verifica (`contr = TRUE`). Default: `10`.
#' @param ... Argomenti aggiuntivi da passare alla funzione di generazione `dist`.
#'
#' @return Una matrice `n x 2` in cui la prima colonna rappresenta `x` e la seconda colonna rappresenta `y`.
#'
#' @examples
#' set.seed(123)
#' r_norm(xbar = 5, ybar = 10, sx = 2, sy = 3, sxy = 0.5, n = 100,contr=TRUE)
#'
#' @export

r_norm <- function(xbar=0,ybar=0,sx=1,sy=1,sxy=0,n=10,max_iter = 10000,tol= 1e-7,dist=rnorm,contr=FALSE,prec=6,...)
{
  x <- dist(n,...)
  y <- rnorm(n)
  pop_sd <- function(x) sqrt(vvv(x))
  for(i in 1:max_iter) {
    # 1) Shift sulle medie
    x <- x - mean(x) + xbar
    y <- y - mean(y) + ybar
    
    # 2) Aggiustamento sulle deviazioni standard (non corrette)
    x <- x * (sx / pop_sd(x))
    y <- y * (sy / pop_sd(y))
    
    # 3) Correzione finale sulla covarianza
    covar <- mean((x - mean(x)) * (y - mean(y)))  # anche questa non corretta
    if(abs(covar - sxy) < tol) break
    alfa <- (sxy - covar) / mean((x - mean(x))^2)
    xmed <- mean(x)
    y <- y + alfa * (x - xmed)
  }
  
  if (contr){
    cat("Media x:", round(abs(mean(x)-xbar),prec), "\n")
    cat("Media y:", round(abs(mean(y)-ybar),prec), "\n")
    cat("DevSt x non corretta:", round(abs(pop_sd(x)-sx),prec), "\n")
    cat("DevSt y non corretta:", round(abs(pop_sd(y)-sy),prec), "\n")
    cat("Cov non corretta:", round(abs(mean((x - mean(x)) * (y - mean(y)))-sxy),prec), "\n")
  }
  return(cbind(x,y))
}


# Fine di: probabilita.R

# Inizio di: stat_base.R
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

# Fine di: stat_base.R

