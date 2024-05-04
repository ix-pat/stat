# Funzioni Usate nel libro ####
# Patrizio Frederic ####

#_______________________ ####

## funzioni speciali ####

srt <- ""
src_ <- function(x) {paste(srt,x,sep = "")}

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
ls2e <- function(x) invisible(list2env(x, envir = globalenv()))


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

item <- function(new=FALSE){
  sp <- "."
  sp2 <- ""
  if (!exists("i1"))  {i1 <- NULL;sp <- "";sp2 <- "."}
  if (!exists("i2"))  {i2 <- 0} 
  if (new) assign("i2",1, envir = .GlobalEnv) else assign("i2",i2 + 1, envir = .GlobalEnv)
  it <- (paste(i1,sp,letters[i2],sp2,sep = ""))
  return(it)}


item2 <- function(new=FALSE,start=FALSE){
  if (start) assign("i1",0, envir = .GlobalEnv)
  if (new)   assign("i1",i1 + 1, envir = .GlobalEnv)
  if (new)   assign("i2",1, envir = .GlobalEnv) else assign("i2",i2 + 1, envir = .GlobalEnv)
  it <- (paste(i1,".",letters[i2],sep = ""))
  return(it)}


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


## Statistica Descrittiva ####

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
 
## Probabilità ####

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
  &\\sim & N\\left(",mu,",\\frac{",mu,"\\cdot(1-",mu,"))}{",n,"}\\right) \\\\
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
  

draw_dist <- function(dist,z1,z2,density = 20, border = NA,col=1,...){   # aggiunge una distribuzione tratteggiata
  xx <- c(z1,seq(z1,z2,length=100),z2)
  yy <- c(0 ,dist(seq(z1,z2,length=100)),0)
  polygon(xx,yy,...,border = border,density = density,col=col)
  curve(dist,z1,z2,add=T,col=col)
}

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
   sd <- sqrt(xm*(1-xm))
   SEs  <- "\\sqrt{\\frac{\\hat\\pi(1-\\hat\\pi)}{n}}"
   SEn  <- paste("\\sqrt{\\frac{",p(xm),"(1-",p(xm),")}{",n,"}}")
  }
  if (!is.null(sd)&dist_=="z"){
   SEs  <- paste("\\frac{",ss,"}{\\sqrt{n}}")
   SEn <- paste("\\frac{",sd,"}{\\sqrt{",n,"}}")
   idcn <- xm+c(-1,1)*tstat*sd/sqrt(n)
  }
  if (!is.null(sd)&dist_=="t"){
   mus <- "\\hat\\mu"
   SEs  <- "\\frac{S}{\\sqrt{n}}"
   SEn <- paste("\\frac{",sd*sqrt(n/(n-1)),"}{\\sqrt{",n,"}}")
   sc <- sqrt(n/(n-1))*sd
   cat(
     "\\[
     ",ss," =\\sqrt{\\frac {n}{n-1}}\\cdot\\hat\\sigma =
     \\sqrt{\\frac {",n,"}{",n-1,"}}\\cdot", sd,"=",p(sc),
     "\\]\n")
   idcn <- xm+c(-1,1)*tstat*sc/sqrt(n)
   sd <- sc
   }
  
  cat(
  "\\begin{eqnarray*}
  Idc: & & ",mus,"\\pm ",tsimb,"\\times",SEs,"\\\\
     & & ",xm, "\\pm ",tstat, "\\times",SEn,"\\\\
     & & ",xm, "\\pm ",tstat,"\\times ",sd/sqrt(n),"\\\\
     & & [",idcn[1],", ",idcn[2],"]
  \\end{eqnarray*}\n")
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
ztest_pi <- function(sn,n,p0,h1 = "\\neq", alpha = 0.05){
  ph <- sn/n
  se <- sqrt(p0*(1-p0)/n)
  tobs <- (ph-p0)/se
  if (h1==">") tc <- qnorm(1-alpha)
  if (h1=="<") tc <- -qnorm(1-alpha)
  if (h1=="\\neq") alpha <- alpha/2
  if (h1=="\\neq") tc <- sign((ph-p0))*qnorm(1-alpha)
  
  
  nomi_variabili <- ls(envir = environment())
  
  # Itera sui nomi delle variabili per arrotondare quelle numeriche
  for (nome in nomi_variabili) {
   valore <- get(nome, envir = environment())
   if (is.numeric(valore)) {
     # Arrotonda il valore e aggiornalo nell'ambiente della funzione
     assign(nome, round(valore, digits = 4), envir = environment())
   }
  }

  cat("**Test $Z$ per una proporzione**\n\n")  
  
  cat("La stima
   $$\\hat\\pi=\\frac {", sn,"} {", n,"}=", ph," $$")
  
  cat(" \\(\\fbox{A}\\) FORMULAZIONE DELLE IPOTESI
   $$\\begin{cases}
   H_0:\\pi=\\pi_0=", p0,"\\\\
   H_1:\\pi", h1," \\pi_0=", p0,"
   \\end{cases}$$", 
   ifelse(h1=="\\neq","Siccome \\(H_1\\) è bilaterale, considereremo \\(\\alpha/2\\), anziché \\(\\alpha\\) $$ $$",""),
   "\n\n \\(\\fbox{B}\\) SCELTA E CALCOLO STATISTICA-TEST, \\(Z\\)
        Test Binomiale per \\(n\\) grande: \\(\\Rightarrow\\) z-Test.
   \\begin{eqnarray*}
   \\frac{\\hat\\pi - \\pi_{0}} {\\sqrt {\\pi_0(1-\\pi_0)/\\,n}}&\\sim&N(0,1)\\\\
   z_{\\text{obs}}
   &=& \\frac{ (", ph,"- ", p0,")} {\\sqrt{", p0,"(1-", p0,")/", n,"}}
   =  ", tobs,"\\, .
   \\end{eqnarray*}")
  
  H1 <- h1
  if (h1=='\\neq') h1 <- ifelse(tc<0,"<",">")
  
  cat("\\(\\fbox{C}\\) DECISIONE
   Dalle tavole si ha \\(z_{", alpha,"} = ", tc,"\\).
   $$z_{\\text{obs}} = ", tobs," ", ifelse(tobs<tc,"<",">")," z_{", alpha,"} = ", tc,"$$
     CONCLUSIONE: i dati ", ifelse(tobs<tc & h1=="<" | tobs>tc & h1==">", " non sono", "sono")," coerenti con \\(H_{0}\\) al LdS del ", ifelse(H1=="\\neq",2*alpha*100,alpha*100),"%")
  
  cat("\n\n **Graficamente**\n\n")
  if (H1 == ">") {R <- c(tc,4); A <- c(-4,tc)}
  if (H1 == "<") {R <- c(-4,tc); A <- c(tc,4)}
  if (H1 == "\\neq") {A <- c(-abs(tc),abs(tc)); R1 <- c(-4,-abs(tc)); R <- c(4,abs(tc))}
  curve(dnorm,-4,4,axes=F,xlab="Z",ylab="")
  
  lines(A,c(0,0),lwd=2,col=4)
  if (H1 == "\\neq") {
   lines(R1,c(0,0),lwd=2,col=2) 
   axis(1,c(-4,-tc,0,tc,4),round(c(-4,-tc,0,tc,4),3))
  } else axis(1,round(c(-4,4,0,tc),3))
  lines(R,c(0,0),lwd=2,col=2)
  points(tobs,0,pch=4,cex=2)
  text(tobs,.05,expression(z[obs]))
  axis(2)
  
  if (H1 == ">") {pval <- paste("P(Z>z_{\\text{obs}})=P(Z>",round(tobs,2),")=",format(1-pnorm(tobs),digits=4,scipen=8))}
  if (H1 == "<") {pval <- paste("P(Z<z_{\\text{obs}})=P(Z<",round(tobs,2),")=",format(pnorm(tobs),digits=4,scipen=8))}
  if (H1 == "\\neq") {pval <- paste("P(|Z|>|z_{\\text{obs}}|)=2P(Z>|z_{\\text{obs}}|)=2P(Z>|",round(tobs,2),"|)=",format(2*pnorm(-abs(round(tobs,2))),digits=4,scipen=8))}
  
  cat("\n\n Il \\(p_{\\text{value}}\\) è $$", pval,"$$")
  
}
#' @rdname test-z-t
ztest_mu <- function(muh,s,n,mu0,h1 = "\\neq", alpha = 0.05,um="",pvalue=T){
  se <- s/sqrt(n)
  tobs <- round((muh-mu0)/se,4)
  if (h1==">") tc <- qnorm(1-alpha)
  if (h1=="<") tc <- -qnorm(1-alpha)
  if (h1=="\\neq") tc <- sign((muh-mu0)^0)*qnorm(1-alpha/2)
  if (h1=="\\neq") alpha <- alpha/2
  if (!exists("um")) um <- ""
  
  nomi_variabili <- ls(envir = environment())
  
  # Itera sui nomi delle variabili per arrotondare quelle numeriche
  for (nome in nomi_variabili) {
   valore <- get(nome, envir = environment())
   if (is.numeric(valore)) {
     # Arrotonda il valore e aggiornalo nell'ambiente della funzione
     assign(nome, round(valore, digits = 4), envir = environment())
   }
  }
  
  cat("**Test $Z$ per una media, variazna nota**\n\n")  
  
  cat("\\(\\fbox{A}\\) FORMULAZIONE DELLE IPOTESI
   $$\\begin{cases}
   H_0:\\mu=\\mu_0=",mu0,"\\text{",um,"}\\\\
   H_1:\\mu",h1," \\mu_0=",mu0,"\\text{",um,"}
   \\end{cases}$$",
   ifelse(h1=="\\neq","Siccome \\(H_1\\) è bilaterale, considereremo \\(\\alpha/2\\), anziché \\(\\alpha\\) $$ $$",""),
   "\n\n \\(\\fbox{B}\\) SCELTA E CALCOLO STATISTICA-TEST, \\(Z\\)
     \\(\\sigma^{2}\\) di \\(\\cal{P}\\) è nota: \\(\\Rightarrow\\) z-Test.
   \\begin{eqnarray*}
   \\frac{\\hat\\mu - \\mu_{0}} {\\sigma/\\sqrt{n}}&\\sim&N(0,1)\\\\
   z_{\\text{obs}}
   &=& \\frac{ (",muh,"- ",mu0,")} {",s,"/\\sqrt{",n,"}}
   =  ",tobs,"\\, .
   \\end{eqnarray*}"
  )
  tc <- round(tc,4)
  H1 <- h1
  if (h1=='\\neq') h1 <- ifelse(tc<0,"<",">")
  
  cat("\\(\\fbox{C}\\) DECISIONE
   Dalle tavole si ha \\(z_{",alpha,"} = ",tc,"\\).
   $$z_{\\text{obs}} = ",tobs,ifelse(tobs<tc,"<",">"), "z_{",alpha,"} = ",tc,"$$ ")
  
  cat("CONCLUSIONE: i dati",ifelse(tobs<tc & h1=="<" | tobs>tc & h1==">", " non sono", "sono")," coerenti con \\(H_{0}\\) al LdS del ", ifelse(H1=="\\neq",2*alpha*100,alpha*100),"%")
  
  cat("\n\n **Graficamente**\n\n")
  if (H1 == ">") {R <- c(tc,4); A <- c(-4,tc)}
  if (H1 == "<") {R <- c(-4,tc); A <- c(tc,4)}
  if (H1 == "\\neq") {A <- c(-abs(tc),abs(tc)); R1 <- c(-4,-abs(tc)); R <- c(4,abs(tc))}
  curve(dnorm,-4,4,axes=F,xlab="Z",ylab="")
  
  lines(A,c(0,0),lwd=2,col=4)
  if (H1 == "\\neq") {
   lines(R1,c(0,0),lwd=2,col=2) 
   axis(1,c(-4,-tc,0,tc,4),round(c(-4,-tc,0,tc,4),3))
  } else axis(1,round(c(-4,4,0,tc),3))
  lines(R,c(0,0),lwd=2,col=2)
  points(tobs,0,pch=4,cex=2)
  text(tobs,.05,expression(z[obs]))
  
  if (pvalue){
   if (H1 == ">") {pval <- paste("P(Z>z_{\\text{obs}})=P(Z>",round(tobs,2),")=",format(1-pnorm(tobs),digits = 4,scipen=8))}
   if (H1 == "<") {pval <- paste("P(Z<z_{\\text{obs}})=P(Z<",round(tobs,2),")=",format(pnorm(tobs),digits = 4,scipen=8))}
   if (H1 == "\\neq") {pval <- paste("P(|Z|>|z_{\\text{obs}}|)=2P(Z>|z_{\\text{obs}}|)=2P(Z>|",round(tobs,2),"|)=",format(2*pnorm(-abs(round(tobs,2))),digits = 4,scipen=8))}
   cat("\n\n Il \\(p_{\\text{value}}\\) è $$",pval,"$$")}
  
}
#' @rdname test-z-t
ttest_mu <-  function(muh,sh,n,mu0,h1 = "\\neq", alpha = 0.05,um=""){
  s <- sqrt(n/(n-1))*sh
  se <- s/sqrt(n)
  tobs <- (muh-mu0)/se
  if (h1==">") tc <- qt(1-alpha,n-1)
  if (h1=="<") tc <- -qt(1-alpha,n-1)
  if (h1=="\\neq") tc <- sign((muh-mu0))*qt(1-alpha/2,n-1)
  if (h1=="\\neq") alpha <- alpha/2
  if (!exists("um")) um <- ""
  
  nomi_variabili <- ls(envir = environment())
  
  # Itera sui nomi delle variabili per arrotondare quelle numeriche
  for (nome in nomi_variabili) {
   valore <- get(nome, envir = environment())
   if (is.numeric(valore)) {
     # Arrotonda il valore e aggiornalo nell'ambiente della funzione
     assign(nome, round(valore, digits = 4), envir = environment())
   }
  }
  
  cat("**Test $t$ per una media, varianza incognita**\n\n")  
  
  cat("\\(\\fbox{A}\\) FORMULAZIONE DELLE IPOTESI
   $$\\begin{cases}
   H_0:\\mu=\\mu_0=", mu0,"\\text{", um,"}\\\\
   H_1:\\mu", h1," \\mu_0=", mu0,"\\text{", um,"}
   \\end{cases}$$
     ", ifelse(h1=="\\neq","Siccome \\(H_1\\) è bilaterale, considereremo \\(\\alpha/2\\), anziché \\(\\alpha\\ \n\n)","\n\n")," 
   \\(\\fbox{B}\\) SCELTA E CALCOLO STATISTICA-TEST, \\(T\\)
   \\(\\sigma^{2}\\) di \\(\\cal{P}\\) non è nota: \\(\\Rightarrow\\) t-Test.
   \\begin{eqnarray*}
   S
   &=& \\sqrt{\\frac{n} {n-1}}\\ \\widehat{\\sigma}
   =  \\sqrt{\\frac{", n,"} {", n,"-1}} \\times ", sh," = ", s," 
   \\end{eqnarray*}
   \\begin{eqnarray*}
   \\frac{\\hat\\mu - \\mu_{0}} {S/\\,\\sqrt{n}}&\\sim&t_{n-1}\\\\
   t_{\\text{obs}}
   &=& \\frac{ (", muh,"- ", mu0,")} {", s,"/\\sqrt{", n,"}}
   =  ", tobs,"\\, .
   \\end{eqnarray*}
   ")
  
  H1 <- h1
  if (h1=='\\neq') h1 <- ifelse(tc<0,"<",">")
  
  cat("\\(\\fbox{C}\\) DECISIONE
   Dalle tavole si ha \\(t_{(", n,"-1);\\, ", alpha,"} = ", tc,"\\).
   $$t_{\\text{obs}} = ", tobs," ", ifelse(tobs<tc,"<",">")," t_{", n-1,";\\, ", alpha,"} = ", tc,"$$
     CONCLUSIONE: i dati ", ifelse(tobs<tc & h1=="<" | tobs>tc & h1==">", " non sono", "sono")," coerenti con \\(H_{0}\\) al LdS del ", ifelse(H1=="\\neq",2*alpha*100,alpha*100),"%")

  cat("\n\n **Graficamente**\n\n")
  
  if (H1 == ">") {R <- c(tc,4); A <- c(-4,tc)}
  if (H1 == "<") {R <- c(-4,tc); A <- c(tc,4)}
  if (H1 == "\\neq") {A <- c(-abs(tc),abs(tc)); R1 <- c(-4,-abs(tc)); R <- c(4,abs(tc))}
  curve(dt(x,n-1),-4,4,axes=F,xlab="T",ylab="")
  
  lines(A,c(0,0),lwd=2,col=4)
  if (H1 == "\\neq") {
   lines(R1,c(0,0),lwd=2,col=2) 
   axis(1,c(-4,-tc,0,tc,4),round(c(-4,-tc,0,tc,4),3))
  } else axis(1,round(c(-4,4,0,tc),3))
  lines(R,c(0,0),lwd=2,col=2)
  points(tobs,0,pch=4,cex=2)
  text(tobs,.05,expression(t[obs]))
  axis(2)
  
  if (H1 == ">") {pval <- paste("P(T_{n-1}>t_{\\text{obs}})=P(T_{n-1}>",round(tobs,3),")=",format(1-pt(tobs,n-1),digits = 4, scipen=8))}
  if (H1 == "<") {pval <- paste("P(T_{n-1}<t_{\\text{obs}})=P(T_{n-1}<",round(tobs,3),")=",format(pt(tobs,n-1),digits = 4, scipen=8))}
  if (H1 == "\\neq") {pval <- paste("P(|T_{n-1}|>|t_{\\text{obs}}|)=2P(T_{n-1}>|t_{\\text{obs}}|)=2P(T_{n-1}>|",round(tobs,4),"|)=",format(2*pt(-abs(tobs),n-1),digits = 4, scipen=8))}
  
  cat("\n\n Il \\(p_{\\text{value}}\\) è
   $$", pval,"$$")
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
test_2c <-  function(mu1,mu2,s1h=F,s2h=F,n1,n2,h1 = "\\neq", alpha = 0.05,et=F,a="1",b="2",um=""){
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
   tobs <- (p1-p2)/se
   
   if (h1==">") tc <- qnorm(1-alpha)
   if (h1=="<") tc <- -qnorm(1-alpha)
   if (h1=="\\neq") tc <- sign((p1-p2))*qnorm(1-alpha/2)
   if (h1=="\\neq") alpha <- alpha/2
   um <- ""
   a <- "A"
   b <- "B"
   
   nomi_variabili <- ls(envir = environment())
   
   # Itera sui nomi delle variabili per arrotondare quelle numeriche
   for (nome in nomi_variabili) {
     valore <- get(nome, envir = environment())
     if (is.numeric(valore)) {
      # Arrotonda il valore e aggiornalo nell'ambiente della funzione
      assign(nome, round(valore, digits = 4), envir = environment())
     }
   }

   cat("**Test $Z$ per due proporzioni**\n\n")  
   
   cat("Test $Z$ su due proporzioni")
   
   cat("\\(\\fbox{A}\\) FORMULAZIONE DELLE IPOTESI
   $$\\begin{cases}
   H_0:\\pi_\\text{",a,"} =     \\pi_\\text{ ",b,"}\\text{",um,"}\\\\
   H_1:\\pi_\\text{",a,"} ",h1," \\pi_\\text{ ",b,"}\\text{",um,"}
   \\end{cases}$$
     ",ifelse(h1=="\\neq","Siccome \\(H_1\\) è bilaterale, considereremo \\(\\alpha/2\\), anziché \\(\\alpha\\) $$ $$",""),"\n\n")
   
   cat("\\(\\fbox{B}\\) SCELTA E CALCOLO STATISTICA-TEST, \\(Z\\)")
   
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
   
   H1 <- h1
   if (h1=='\\neq') h1 <- ifelse(tc<0,"<",">")
   
   cat("\\(\\fbox{C}\\) DECISIONE Dalle tavole si ha \\(z_{",alpha,"} = ",tc,"\\).
   $$z_{\\text{obs}} = ",tobs," ",ifelse(tobs<tc,"<",">")," z_{",alpha,"} = ",tc,"$$")
   
   cat("CONCLUSIONE: i dati ",ifelse(tobs<tc & h1=="<" | tobs>tc & h1==">", " non sono", "sono")," coerenti con \\(H_{0}\\) al LdS del ",ifelse(H1=="\\neq",2*alpha*100,alpha*100),"%
   ")
   
   cat("\n\n **Graficamente**\n\n")
   if (H1 == ">") {R <- c(tc,4); A <- c(-4,tc)}
   if (H1 == "<") {R <- c(-4,tc); A <- c(tc,4)}
   if (H1 == "\\neq") {A <- c(-abs(tc),abs(tc)); R1 <- c(-4,-abs(tc)); R <- c(4,abs(tc))}
   curve(dt(x,n1+n2-2),-4,4,axes=F,xlab="Z",ylab="")
   
   lines(A,c(0,0),lwd=2,col=4)
   if (H1 == "\\neq") {
     lines(R1,c(0,0),lwd=2,col=2) 
     axis(1,c(-4,-tc,0,tc,4),round(c(-4,-tc,0,tc,4),3))
   } else axis(1,round(c(-4,4,0,tc),3))
   lines(R,c(0,0),lwd=2,col=2)
   points(tobs,0,pch=4,cex=2)
   text(tobs,.05,expression(z[obs]))
   axis(2)
   
   if (H1 == ">") {pval <- paste("P(Z>z_{\\text{obs}})=P(Z>",round(tobs,),")=",format(1-pnorm(tobs),digits=4, scipen=8))}
   if (H1 == "<") {pval <- paste("P(Z<z_{\\text{obs}})=P(Z<",round(tobs,2),")=",format(pnorm(tobs),digits=4, scipen=8))}
   if (H1 == "\\neq") {pval <- paste("P(|Z|>|z_{\\text{obs}}|)=2P(Z>|z_{\\text{obs}}|)=2P(Z>|",round(tobs,2),"|)=",format(2*pnorm(-abs(tobs)),digits=4, scipen=8))}
   
   cat("\n\n Il \\(p_{\\text{value}}\\) è
   $$",pval,"$$")
   
   } else 
     #### Test su due medie (eterogeneità) ----------------
   {
   if (et) # et = T eterogeneo
     {   s1 <- sqrt(n1/(n1-1))*s1h
   s2 <- sqrt(n2/(n2-1))*s2h
   s2p <- (n1*s1h+n2*s2h)/(n1+n2-2)
   se <- sqrt(s1^2/n1+s2^2/n2)
   tobs <- (mu1-mu2)/se
   if (h1==">") tc <- qt(1-alpha,n1+n2-2)
   if (h1=="<") tc <- -qt(1-alpha,n1+n2-2)
   if (h1=="\\neq") tc <- sign((mu1-mu2))*qt(1-alpha/2,n1+n2-2)
   if (h1=="\\neq") alpha <- alpha/2
   q1 <- a; q2 <- b
   s2f1<- s1^2; s2f2 <- s2^2
   
   nomi_variabili <- ls(envir = environment())
   
   # Itera sui nomi delle variabili per arrotondare quelle numeriche
   for (nome in nomi_variabili) {
     valore <- get(nome, envir = environment())
     if (is.numeric(valore)) {
      # Arrotonda il valore e aggiornalo nell'ambiente della funzione
      assign(nome, round(valore, digits = 4), envir = environment())
     }
   }
   
   cat("**Test $t$ per due medie, (eterogeneità)**\n\n")  
   
   
   cat("\\(\\fbox{A}\\) FORMULAZIONE DELLE IPOTESI
  
  $$\\begin{cases}
  H_0:\\mu_\\text{", a,"} =     \\mu_\\text{ ", b,"}\\text{", um,"}\\\\
  H_1:\\mu_\\text{", a,"} ", h1," \\mu_\\text{ ", b,"}\\text{", um,"}
  \\end{cases}$$
   ", ifelse(h1=="\\neq","Siccome \\(H_1\\) è bilaterale, considereremo \\(\\alpha/2\\), anziché \\(\\alpha\\)",""),"
  
  \\(\\fbox{B}\\) SCELTA E CALCOLO STATISTICA-TEST, \\(T\\)
$$
     S^2_\\text{",a,"}=\\frac{n_\\text{",a,"}}{n_\\text{",a,"}-1}\\hat\\sigma^2_\\text{",a,"}=\\frac{",n1,"}{",n1,"-1}",s1h,"^2=",s2f1," \\qquad
     S^2_\\text{",b,"}=\\frac{n_\\text{",b,"}}{n_\\text{",b,"}-1}\\hat\\sigma^2_\\text{",b,"}=\\frac{",n2,"}{",n2,"-1}",s2h,"^2=",s2f2,"
   $$\n\n ")
   
   
   cat("\\begin{eqnarray*}
   \\frac{\\hat\\mu_\\text{",a,"} - \\hat\\mu_\\text{",b,"}} 
   {\\sqrt{\\frac {S^2_\\text{",q1,"}}{n_\\text{",a,"}}+\\frac {S^2_\\text{",q2,"}}{n_\\text{",b,"}}}}&\\sim&t_{n_\\text{",a,"}+n_\\text{",b,"}-2}\\\\
   t_{\\text{obs}}
   &=& \\frac{ (",mu1,"- ",mu2,")} {\\sqrt{\\frac{",s2f1,"}{",n1,"}+\\frac{",s2f2,"}{",n2,"}}}
   =  ",tobs,"\\, .
   \\end{eqnarray*}\n\n
   ")
   
   H1 <- h1
   if (h1=='\\neq') h1 <- ifelse(tc<0,"<",">")
   
   
   cat("\\(\\fbox{C}\\) DECISIONE
   Dalle tavole si ha \\(t_{(",n1,"+",n2,"-2);\\, ",alpha,"} = ",tc,"\\).
   $$t_{\\text{obs}} = ",tobs," ",ifelse(tobs<tc,"<",">")," t_{",n1+n2-2,";\\, ",alpha,"} = ",tc,"$$
   CONCLUSIONE: i dati ",ifelse(tobs<tc & h1=="<" | tobs>tc & h1==">", " non sono", "sono")," coerenti con \\(H_{0}\\) al LdS del ",ifelse(H1=="\\neq",2*alpha*100,alpha*100),"%
   ")
   
   cat("\n\n **Graficamente**\n\n")
   if (H1 == ">") {R <- c(tc,4); A <- c(-4,tc)}
   if (H1 == "<") {R <- c(-4,tc); A <- c(tc,4)}
   if (H1 == "\\neq") {A <- c(-abs(tc),abs(tc)); R1 <- c(-4,-abs(tc)); R <- c(4,abs(tc))}
   curve(dt(x,n1+n2-2),-4,4,axes=F,xlab="T",ylab="")
   
   lines(A,c(0,0),lwd=2,col=4)
   if (H1 == "\\neq") {
     lines(R1,c(0,0),lwd=2,col=2) 
     axis(1,c(-4,-tc,0,tc,4),round(c(-4,-tc,0,tc,4),3))
   } else axis(1,round(c(-4,4,0,tc),3))
   lines(R,c(0,0),lwd=2,col=2)
   points(tobs,0,pch=4,cex=2)
   text(tobs,.05,expression(t[obs]))
   axis(2)
   
   if (H1 == ">") {pval <- paste("P(T_{n1+n2-2}>t_{\\text{obs}})=P(T_{n1+n2-2}>",round(tobs,3),")=",format(1-pt(tobs,n1+n2-2),digits = 4, scipen=8))}
   if (H1 == "<") {pval <- paste("P(T_{n1+n2-2}<t_{\\text{obs}})=P(T_{n1+n2-2}<",round(tobs,3),")=",format(pt(tobs,n1+n2-2),digits = 4, scipen=8))}
   if (H1 == "\\neq") {pval <- paste("P(|T_{n1+n2-2}|>|t_{\\text{obs}}|)=2P(T_{n1+n2-2}>|t_{\\text{obs}}|)=2P(T_{n1+n2-2}>|",round(tobs,4),"|)=",format(2*pt(-abs(tobs),n1+n2-2),digits = 4, scipen=8))}
   
   cat("\n\n Il \\(p_{\\text{value}}\\) è
   $$",pval,"$$")
   
   } else # et = F omogeneo
     #### Test su due medie (omogeneità) ----------------
     {
     s1 <- sqrt(n1/(n1-1))*s1h
     s2 <- sqrt(n2/(n2-1))*s2h
     s2p <- (n1*s1h^2+n2*s2h^2)/(n1+n2-2)
     se <- sqrt(s2p/n1+s2p/n2)
     tobs <- (mu1-mu2)/se
     if (h1==">") tc <- qt(1-alpha,n1+n2-2)
     if (h1=="<") tc <- -qt(1-alpha,n1+n2-2)
     if (h1=="\\neq") tc <- sign((mu1-mu2))*qt(1-alpha/2,n1+n2-2)
     if (h1=="\\neq") alpha <- alpha/2
     if (!exists("um")) um <- ""
     if (!exists("a")) a <- 1
     if (!exists("b")) b <- 2
     q1 <- "p"; q2 <- "p"
     s2f1<- s2p; s2f2 <- s2p
     nomi_variabili <- ls(envir = environment())
     
     # Itera sui nomi delle variabili per arrotondare quelle numeriche
     for (nome in nomi_variabili) {
      valore <- get(nome, envir = environment())
      if (is.numeric(valore)) {
        # Arrotonda il valore e aggiornalo nell'ambiente della funzione
        assign(nome, round(valore, digits = 4), envir = environment())
      }
     }
     
     cat("**Test $Z$ per due medie, (omogeneità)**\n\n")

     cat("\\(\\fbox{A}\\) FORMULAZIONE DELLE IPOTESI
  
  $$\\begin{cases}
  H_0:\\mu_\\text{", a,"} =     \\mu_\\text{ ", b,"}\\text{", um,"}\\\\
  H_1:\\mu_\\text{", a,"} ", h1," \\mu_\\text{ ", b,"}\\text{", um,"}
  \\end{cases}$$
   ", ifelse(h1=="\\neq","Siccome \\(H_1\\) è bilaterale, considereremo \\(\\alpha/2\\), anziché \\(\\alpha\\)",""),"
  
  \\(\\fbox{B}\\) SCELTA E CALCOLO STATISTICA-TEST, \\(T\\)

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
     
     H1 <- h1
     if (h1=='\\neq' & tc < 0) h1 <- ifelse(tc<0,"<",">")
     
     
     cat("\\(\\fbox{C}\\) DECISIONE
  Dalle tavole si ha \\(t_{(", n1,"+", n2,"-2);\\, ", alpha,"} = ", tc,"\\).
  $$t_{\\text{obs}} = ", tobs," ", ifelse(tobs<tc,"<",">")," t_{", n1+n2-2,";\\, ", alpha,"} = ", tc,"$$\n\n")
     
     cat("CONCLUSIONE: i dati ", ifelse(tobs<tc & h1=="<" | tobs>tc & h1==">", " non sono", "sono")," coerenti con \\(H_{0}\\) al LdS del ", ifelse(H1=="\\neq",2*alpha*100,alpha*100),"%")
     
     cat("\n\n **Graficamente**\n\n")
     if (H1 == ">") {R <- c(tc,4); A <- c(-4,tc)}
     if (H1 == "<") {R <- c(-4,tc); A <- c(tc,4)}
     if (H1 == "\\neq") {A <- c(-abs(tc),abs(tc)); R1 <- c(-4,-abs(tc)); R <- c(4,abs(tc))}
     curve(dt(x,n1+n2-2),-4,4,axes=F,xlab="T",ylab="")
     
     lines(A,c(0,0),lwd=2,col=4)
     if (H1 == "\\neq") {
      lines(R1,c(0,0),lwd=2,col=2) 
      axis(1,c(-4,-tc,0,tc,4),round(c(-4,-tc,0,tc,4),3))
     } else axis(1,round(c(-4,4,0,tc),3))
     lines(R,c(0,0),lwd=2,col=2)
     points(tobs,0,pch=4,cex=2)
     text(tobs,.05,expression(t[obs]))
     axis(2)
     
     if (H1 == ">") {pval <- paste("P(T_{n1+n2-2}>t_{\\text{obs}})=P(T_{n1+n2-2}>",round(tobs,3),")=",format(1-pt(tobs,n1+n2-2),digits = 4,scipen=8))}
     if (H1 == "<") {pval <- paste("P(T_{n1+n2-2}<t_{\\text{obs}})=P(T_{n1+n2-2}<",round(tobs,3),")=",format(pt(tobs,n1+n2-2),digits = 4,scipen=8))}
     if (H1 == "\\neq") {pval <- paste("P(|T_{n1+n2-2}|>|t_{\\text{obs}}|)=2P(T_{n1+n2-2}>|t_{\\text{obs}}|)=2P(T_{n1+n2-2}>|",round(tobs,4),"|)=",format(2*pt(-abs(tobs),n1+n2-2),digits = 4,scipen=8))}
     
     cat("\n\n Il \\(p_{\\text{value}}\\) è
  $$", pval,"$$")
     
   }
     }
}
#' @rdname test-su-due-campioni
ttest_2c_et <-  function(mu1,mu2,s1h,s2h,n1,n2,h1 = "\\neq", alpha = 0.05,a="1",b="2",um="",et=T){
  test_2c(mu1=mu1,mu2 = mu2,s1h=s1h,s2h=s2h,n1,n2,h1 = h1, alpha = alpha,et=T,a=a,b=b,um=um)
}
#' @rdname test-su-due-campioni
ttest_2c_om <-  function(mu1,mu2,s1h,s2h,n1,n2,h1 = "\\neq", alpha = 0.05,a="1",b="2",um="",et=F){
  test_2c(mu1=mu1,mu2 = mu2,s1h=s1h,s2h=s2h,n1,n2,h1 = h1, alpha = alpha,et=F,a=a,b=b,um=um)
}
#' @rdname test-su-due-campioni
ztest_2c_pi <-  function(s1,s2,n1,n2,h1 = "\\neq", alpha = 0.05,a="1",b="2"){
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
  nomi_variabili <- ls(envir = environment())
  
  # Itera sui nomi delle variabili per arrotondare quelle numeriche
  for (nome in nomi_variabili) {
   valore <- get(nome, envir = environment())
   if (is.numeric(valore)) {
     # Arrotonda il valore e aggiornalo nell'ambiente della funzione
     assign(nome, round(valore, digits = 4), envir = environment())
   }
  }
}  

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

  previsione <- function(x){
   if (b1 < 0) {
   }
   cat("\\[\\hat y_{X=",x,"}=\\hat\\beta_0+\\hat\\beta_1 x=",b0,"+",p(b1),"\\times",p(x),"=",b0+b1*x,"\\]")
  }
  
  residuo <- function(x,y){
   cat("\\begin{eqnarray*}\n")
   cat("\\hat y_i &=&\\hat\\beta_0+\\hat\\beta_1 x_i=\\\\ \n")
   cat(        "&=&",b0,"+",p(b1),"\\times",p(x),"=",b0+b1*x,"\\\\ \n")
   cat("\\hat \\varepsilon_i &=& y_i-\\hat y_i\\\\ \n")
   cat(                "&=&",y,"-",b0+b1*x,"=",y - (b0+b1*x)," \n")
   cat("\\end{eqnarray*}\n")  
  }
  
  R2 <- function(){
   sgn <- ifelse(r^2>.75,">","<")
   cat("\\begin{eqnarray*}\n")
   cat("r&=&\\frac{\\text{cov}(X,Y)}{\\sigma_X\\sigma_Y}=\\frac{",co,"}{",sx,"\\times",sy,"}=",r,"\\\\")
   cat("r^2&=&",r^2,sgn,"0.75\n")
   cat("\\end{eqnarray*}\n")  
   cat(ifelse(r^2>.75,"Il modello si adatta bene ai dati.","Il modello **non** si adatta bene ai dati."))
  }
  
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
   TSS &=& RSS+TSS \\\\", 
   n*vy," &=& ", r^2*n*vy, "+", (1-r^2)*n*vy, "
  \\end{eqnarray*}")
  }
  
  
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
  
  ttest_beta <-  function(cof,bj0,h1 = "\\neq", alpha = 0.05){
   bj <- ifelse(cof==0,b0,b1)
   vbj <-  ifelse(cof==0,(vb0),(vb1))
   tobs <- (bj - bj0)/sqrt(vbj)
   if (h1==">") tc <- qt(1-alpha,n-1)
   if (h1=="<") tc <- -qt(1-alpha,n-1)
   if (h1=="\\neq") tc <- sign(tobs)*qt(1-alpha/2,n-2)
   if (h1=="\\neq") alpha <- alpha/2
   cat("$\\fbox{A}$ FORMULAZIONE DELLE IPOTESI
\\[\\begin{cases}
H_0:\\beta_{",cof,"} =     ",bj0,"\\\\
H_1:\\beta_{",cof,"} ",h1," ",bj0,"
\\end{cases}\\]
",ifelse(h1=="\\neq","Siccome $H_1$ è bilaterale, considereremo $\\alpha/2$, anziché $\\alpha$\n\n","\n\n"),"
$\\fbox{B}$ SCELTA E CALCOLO STATISTICA-TEST, $T$
Test su un coefficiente di regressione: $\\Rightarrow$ t-Test.
\\begin{eqnarray*}
 \\frac{\\hat\\beta_{",cof,"} - \\beta_{",cof,";H_0}} {\\widehat{SE(\\hat\\beta_{",cof,"})}}&\\sim&t_{n-2}\\\\
   t_{\\text{obs}}
&=& \\frac{ (",bj,"- ",bj0,")} {",sqrt(vbj),"}
 =  ",tobs,"\\, .
\\end{eqnarray*}
")
   H1 <- h1
   if (h1=='\\neq') h1 <- ifelse(tc<0,"<",">")

   cat("$\\fbox{C}$ DECISIONE
   Dalle tavole si ha $t_{(",n,"-2);\\, ",alpha,"} = ",tc,"$.
   \\[t_{\\text{obs}} = ",tobs," ",ifelse(tobs<tc,"<",">")," t_{",n-2,";\\, ",alpha,"} = ",tc,"\\]
   CONCLUSIONE: i dati ",ifelse(tobs<tc & h1=="<" | tobs>tc & h1==">", " **non** sono", "**sono**")," coerenti con $H_{0}$ al LdS del ",ifelse(H1=="\\\\neq",2*alpha*100,alpha*100),"%
   \n\n **Graficamente** \n\n",sep="")
     

   t1 <- floor(max(-8,qt(0.0000316712,n-2)))
   t2 <- ceiling(min(8,qt(1-0.0000316712,n-2)))
   if (H1 == ">") {R <- c(tc,t2); A <- c(t1,tc)}
   if (H1 == "<") {R <- c(t1,tc); A <- c(tc,t2)}
   if (H1 == "\\neq") {A <- c(-abs(tc),abs(tc)); R1 <- c(t1,-abs(tc)); R <- c(t2,abs(tc))}
   curve(dt(x,n-2),t1,t2,axes=F,xlab="T",ylab="")
   
   lines(A,c(0,0),lwd=2,col=4)
   if (H1 == "\\neq") {
     lines(R1,c(0,0),lwd=2,col=2)
     axis(1,c(t1,-tc,0,tc,t2),round(c(t1,-tc,0,tc,t2),3))
   } else axis(1,round(c(t1,t2,0,tc),3))
   lines(R,c(0,0),lwd=2,col=2)
   points(tobs,0,pch=4,cex=2)
   text(tobs,.05,expression(t[obs]))
   axis(2)
   if (H1 == ">") {pval <- paste("P(T_{n-2}>t_{\\text{obs}})=P(T_{n-2}>",round(tobs,3),")=",format(1-pt(tobs,n-2),digits = 4, scipen=8))}
   if (H1 == "<") {pval <- paste("P(T_{n-2}<t_{\\text{obs}})=P(T_{n-2}<",round(tobs,3),")=",format(pt(tobs,n-2),digits = 4, scipen=8))}
   if (H1 == "\\neq") {pval <- paste("P(|T_{n-2}|>|t_{\\text{obs}}|)=2P(T_{n-2}>|t_{\\text{obs}}|)=2P(T_{n-2}>|",round(tobs,4),"|)=",format(2*pt(-abs(tobs),n-2),digits = 4, scipen=8))}

   cat("\n\n il $p_{\\text{value}}$ è
   $$",pval,"$$\n\n")
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
     ss=ss,
     calcolo_beta = calcolo_beta,
     previsione = previsione,
     residuo = residuo,
     R2 = R2,
     TSS = TSS,
     se_beta1 = se_beta1,
     se_beta0 = se_beta0,
     ttest_beta = ttest_beta
   )
  )
}  


## Varie ####

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



