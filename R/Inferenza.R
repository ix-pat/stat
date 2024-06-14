
## Inferenza ####

#' Calcolo dell'Intervallo di Confidenza
#'
#' Calcola l'intervallo di confidenza per la media o la proporzione di una popolazione, utilizzando la distribuzione Normale (z) o t di Student.
#'
#' @param xm Media campionaria o somma delle successi.
#' @param sd Deviazione standard della popolazione, se nota.
#' @param alpha Livello di significatività per l'intervallo di confidenza.
#' @param n Dimensione del campione.
#' @param dist_ Tipo di distribuzione ("z" per Normale, "t" per t di Student).
#' @param mus Media della popolazione, se nota (utilizzato per calcoli specifici).
#' @param ss Varianza campionaria, se disponibile.
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
#' idc(xm = 5, ss = 4, alpha = 0.05, n = 30, dist_ = "t")
#'
#' # Intervallo di confidenza per la proporzione
#' idc(xm = 120, alpha = 0.05, n = 200, dist_ = "z")
#'
#' @rdname intervallo-di-confidenza

idc <- function(xm,sd=NULL,alpha=0.05,n,dist_,mus=NULL,ss=NULL){
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
  round_all()
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
  th0_s <- ifelse(grepl("1",tipo),paste0(simb,"_0=",theta0,um),paste0(simb,"_",lab2))
  th1_s <- ifelse(grepl("1",tipo),simb,paste0(simb,"_",lab1))
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


C_ <- function(lst){
  ls2e(lst)
  param <- n-gdl
  tsimb <- ifelse(is.null(gdl),"z","t")
  if (is.null(gdl)){
    if (h1 =="\\neq"){
    ped   <- paste0("_{",alpha/2,"}")
      } else {ped <- paste0("_{",alpha,"}") }
  } else {
    if (h1 =="\\neq"){
      ped   <- paste0("_{",alpha,"/2;",n,"-",param,"}")
    } else {ped <- paste0("_{",alpha,"/2;",n,"-",param,"}")}
    
  }
  round_all(4)
  tobs_s <- paste0(tsimb,"_\\text{obs}=",tobs)
  tcrit  <- paste0(tsimb,ped[order(abs(tc))],"=",tc)
  if (h1 == "\\neq"){
    tobs_s <-  paste0("|",tsimb,"_\\text{obs}|=",abs(tobs))
  }
  H1 <- h1
  H1 <- ifelse(h1 == "\\neq",">",H1)
  cat( "\n\n\\(\\fbox{C}\\) CONCLUSIONE \n\n",
       ifelse(h1=="\\neq","Siccome \\(H_1\\) è bilaterale, considereremo \\(\\alpha/2\\), anziché \\(\\alpha\\)\n\n","\n\n"),
       sep = "")
  if (length(alpha)!=1){
    if(h1 == "<") {
      tcrit <- rev(tcrit)
      livello <- 5-livello}
    conc <- switch(livello+1,
                  "0"=paste0("$",tobs_s,ifelse(h1=="<",">","<"),tcrit[1],"$, quindi **non** rifiuto $H_0$ a **nessun** livello di significatività, $p_\\text{value}>0.1$"),
                  "1"=paste0("$",min(tcrit[1:2]),"<",tobs_s,"<",max(tcrit[1:2]),"$, indecisione sul rifiuto di $H_0$ al 10%, $0.05<p_\\text{value}<0.1$,\n\n _marginalmente significativo_ $\\fbox{.}$."),
                  "2"=paste0("$",min(tcrit[2:3]),"<",tobs_s,"<",max(tcrit[2:3]),"$, quindi **rifiuto** $H_0$ al 5%, $0.01<p_\\text{value}<0.05$,\n\n _significativo_   $\\fbox{*}$."),
                  "3"=paste0("$",min(tcrit[3:4]),"<",tobs_s,"<",max(tcrit[3:4]),"$, quindi **rifiuto** $H_0$ all'1%, $0.001<p_\\text{value}<0.01$,\n\n _molto significativo_  $\\fbox{**}$."),
                  "4"=paste0("$",tobs_s,H1,tcrit[4],"$, quindi **rifiuto** $H_0$ sotto all'1‰, $p_\\text{value}<0.001$,\n\n _estremamente significativo_ $\\fbox{***}$."),
                   )
    
    cat("I valori critici sono\n\n",
         "$",paste0(tcrit,collapse="$;$"),"$\n\n", 
         "Siccome ",conc,sep = "")
  } else {
      if (h1=="\\neq") tobs <-abs(tobs)
      cat("La siginficatitività è $\\alpha=", alpha,"$, dalle tavole osserviamo $",tcrit,"$.\n\n",
          "Essendo $",tobs_s,ifelse(tobs>tc,">","<"),tcrit,"$ allora", ifelse((tobs > tc)&(H1==">")|(tobs < tcrit)&(H1=="<"),"**rifiuto** $H_0$","**non** rifiuto $H_0$"),"al ",alpha*100,"%\n\n",
          sep="")
    }
}

graf <- function(lst){
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
  round_all(4)
  if (h1 =="\\neq") tc1 <- c(-rev(tc),tc) else tc1 <- sort(c(segn*rep(floor(q_distr(1/5000)),times=length(alpha)),tc))
  tac <- c(floor(q_distr(1/5000)),tc1,ceiling(q_distr(1-1/5000)))
  kk <- length(tac)
  curve(d_distr,from = tac[1],to = tac[kk],n = 1001,axes=F,xlab = tsimb,ylab = paste0("f(",tsimb,")"))
  axis(1,tac,round(tac,4),las=2)
  axis(2)
  segments(tac[-10],0,tac[-10],d_distr(tac[-10]),lty=2)
  col_ <- c("red","purple","red4","grey","blue")
  if (length(alpha)==1) col_ <-c("red","blue")
  k <- length(tac)
  for (i in 1:(k/2)){
  segments(tac[i],0,tac[k-i+1],0,col=col_[i],lwd=2)
  }
  points(tobs,0,pch=4,cex=2)
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

  H1 <- ifelse(h1=="\\neq",">",h1)
  tobs2 <- round(tobs,2)
  if (h1 != "\\neq") {pval <- paste0("P(",Tsimb,H1,tobs2,")=",p_val)}
  if (h1 == "\\neq") {pval <- paste0("P(|",Tsimb,"|",H1,"|",tobs2,"|)=","2P(",Tsimb,H1,abs(tobs2),")=",p_val)}
  cat("\n\n Il \\(p_{\\text{value}}\\) è \n\n $$", pval,"$$\n\n")
  if (!is.null(gdl)) cat("Attenzione il calcolo del $p_\\text{value}$ con la $T$ è puramente illustrativo e non può essere riprodotto senza una calcolatrice statistica adeguata.")
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

test_pi <- function(sn,n,p0,h1 = "\\neq",alpha = c(1/10,5/100,1/100,1/1000)){
  ph <- sn/n
  se <- sqrt(p0*(1-p0)/n)

  cat("**Test $Z$ per una proporzione**\n\n")
  lst <- test(theta1 = ph,theta0 = p0,se = se,h1 = h1,n = n,alpha = alpha)
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
  
  C_(lst)
  
  graf(lst) # grafico
  
  p_value(lst) # p-value

}



#' @rdname test-z-t
ztest_mu <- function(muh,s,n,mu0,h1 = "\\neq",um="",pvalue=T,alpha = c(1/10,5/100,1/100,1/1000)){
  se <- s/sqrt(n)
  lst <- test(theta1 = muh,theta0 = mu0,se = se,h1 = h1,n = n,alpha = alpha)
  ls2e(lst)
  cat("**Test $Z$ per una media, variazna nota**\n\n")

  A_(lst = lst,tipo = "mu 1",um = um)
  
  cat("\n\n \\(\\fbox{B}\\) SCELTA E CALCOLO STATISTICA-TEST, \\(Z\\)\n\n
     \\(\\sigma^{2}\\) di \\(\\cal{P}\\) è nota: \\(\\Rightarrow\\) z-Test.
   \\begin{eqnarray*}
   \\frac{\\hat\\mu - \\mu_{0}} {\\sigma/\\sqrt{n}}&\\sim&N(0,1)\\\\
   z_{\\text{obs}}
   &=& \\frac{ (",muh,"- ",mu0,")} {",s,"/\\sqrt{",n,"}}
   =  ",tobs,"\\, .
   \\end{eqnarray*}\n\n"
  )
  
  C_(lst = lst)
  
  graf(lst = lst)
  
  if (pvalue) p_value(lst)
}

#' @rdname test-z-t
ttest_mu <-  function(muh,sh,n,mu0,h1 = "\\neq",um="",alpha = c(1/10,5/100,1/100,1/1000)){
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

  C_(lst = lst)
  graf(lst = lst)
   p_value(lst)
  
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
test_2c <-  function(mu1,mu2,s1h=F,s2h=F,n1,n2,h1 = "\\neq",et=F,a="A",b="B",um="",alpha = c(1/10,5/100,1/100,1/1000)){
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
  C_(lst = lst)
  
  graf(lst = lst)
  
  p_value(lst)

  } else
    #### Test su due medie (eterogeneità) ----------------
  {
    if (et) # et = T eterogeneo
    {   
    s1 <- sqrt(n1/(n1-1))*s1h
    s2 <- sqrt(n2/(n2-1))*s2h
    s2p <- (n1*s1h+n2*s2h)/(n1+n2-2)
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
    
    C_(lst)
    
    graf(lst)
    
    p_value(lst)

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
  &=& \\frac{ (", mu1,"- ", mu2,")} {\\sqrt{\\frac{", s2f1,"}{", n1,"}+\\frac{", s2f2,"}{", n2,"}}}
  =  ", tobs,"\\, .
  \\end{eqnarray*}\n\n
  ")
      C_(lst)
      
      graf(lst)
      
      p_value(lst)
    }
  }
}

#' @rdname test-su-due-campioni
ttest_2c_et <-  function(mu1,mu2,s1h,s2h,n1,n2,h1 = "\\neq",a="1",b="2",um="",alpha = c(1/10,5/100,1/100,1/1000) ){
  test_2c(mu1=mu1,mu2 = mu2,s1h=s1h,s2h=s2h,n1,n2,h1 = h1, alpha = alpha,et=T,a=a,b=b,um=um)
}

#' @rdname test-su-due-campioni
ttest_2c_om <-  function(mu1,mu2,s1h,s2h,n1,n2,h1 = "\\neq",a="1",b="2",um="",alpha = c(1/10,5/100,1/100,1/1000)){
  test_2c(mu1=mu1,mu2 = mu2,s1h=s1h,s2h=s2h,n1,n2,h1 = h1, alpha = alpha,et=F,a=a,b=b,um=um)
}

#' @rdname test-su-due-campioni
ztest_2c_pi <-  function(s1,s2,n1,n2,h1 = "\\neq",a="1",b="2",alpha = c(1/10,5/100,1/100,1/1000)){
  test_2c(mu1 = s1,mu2 = s2,s1h = F,s2h = F,n1 = n1,n2 = n2,h1 = h1,alpha = alpha,a = a,b = b)
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
  round_all()
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
\\hat\\sigma_y^2&=&\\frac 1 n\\sum_{i=1}^n y_i^2-\\bar y^2=\\frac {1}{",n,"} ",sumy2," -",my,"^2=",vy,"\\\\
\\hat\\sigma_x^2&=&\\frac 1 n\\sum_{i=1}^n x_i^2-\\bar x^2=\\frac {1}{",n,"} ",sumx2," -",mx,"^2=",vx,"\\\\
\\text{cov}(y,x)&=&\\frac 1 n\\sum_{i=1}^n y_i~x_i-\\bar y\\bar x=\\frac {1}{",n,"} ",sumxy,"-",my,"\\cdot",mx,"=",co,"\\\\
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
  if (sig_eps){cat(
    "\\begin{eqnarray*}
\\hat{\\sigma_\\varepsilon}^2&=&(1-r^2)\\hat\\sigma_Y^2\\\\
&=& (1-",r^2,")\\times",vy,"\\\\
   &=& ",sh2,"\\\\
   S_\\varepsilon^2 &=& \\frac{n} {n-2} \\hat{\\sigma_\\varepsilon}^2\\\\
   &=&  \\frac{",n,"} {",n,"-2} \\hat{\\sigma_\\varepsilon}^2 \\\\
 &=&  \\frac{",n,"} {",n,"-2} \\times ",sh2," = ",se2," 
\\end{eqnarray*}

E quindi")}
  
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
  if (sig_eps){cat(
    "\\begin{eqnarray*}
\\hat{\\sigma_\\varepsilon}^2&=&(1-r^2)\\hat\\sigma_Y^2\\\\
&=& (1-",r^2,")\\times",vy,"\\\\
   &=& ",sh2,"\\\\
   S_\\varepsilon^2 &=& \\frac{n} {n-2} \\hat{\\sigma_\\varepsilon}^2\\\\
   &=&  \\frac{",n,"} {",n,"-2} \\hat{\\sigma_\\varepsilon}^2 \\\\
 &=&  \\frac{",n,"} {",n,"-2} \\times ",sh2," = ",se2," 
\\end{eqnarray*}

E quindi")}
  
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
ttest_beta <-  function(cof,bj0,h1 = "\\neq",alpha = c(1/10,5/100,1/100,1/1000),SE=F){
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
  
  C_(lst = lst)
  
  graf(lst)
  
  p_value(lst)
  
}

