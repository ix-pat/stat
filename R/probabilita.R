
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
      P(",A,"\\cap",B,") &=& P(",A,")\\cdot (",B,") \\\\
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
#' r_norm(xbar = 5, ybar = 10, sx = 2, sy = 3, sxy = 0.5, n = 100)
#'
#' @export

r_norm <- function(xbar=0,ybar=0,sx=1,sy=1,sxy=0,n=10,max_iter = 10000,tol= 1e-7,dist=rnorm,contr=FALSE,prec=10,...)
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

