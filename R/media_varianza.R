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
      cat("$$\n",mnam,"=\\frac 1{",n,"}(",paste(x,collapse = "+"),")=",mean(x),"\n$$")
    }
    if(!is.null(p)){
      p <- p/sum(p)
      cat("$$\n",mnam,"=",paste(paste(x,p,sep = " \\cdot "),collapse = "+"),"=",sum(x*p),"\n$$")
    }} else {
      freq <- table(x)
      xx <- dimnames(freq)$x
      cat("$$\n",mnam,"=\\left(",paste(paste(xx,"\\frac {",freq,"}{",n,"}"),collapse = "+"),"\\right)=",mean(x),"\n$$")
    }
}
#' @rdname calcoli-statistici
var_ <- function(x,p=NULL,mnam="\\sigma^2",semp=F){
  n <- length(x)
  m <- ifelse(test = is.null(p),mean(x),sum(x*p))
  if (!semp){
    if(is.null(p)){
      cat("$$\n",mnam,"=\\frac 1{",n,"}(",paste(paste(x,2,sep = "^"),collapse = "+"),")-(",m,")^2=",s2c(x),"\n$$")
    }
    if(!is.null(p)){
      p <- p/sum(p)
      cat("$$\n",mnam,"=(",paste(paste(paste(x,2,sep = "^"),p,sep = " \\cdot "),collapse = "+"),")-(",m,")^2=",vvv(x = x,p = p),"\n$$")
    }} else {
      freq <- table(x)
      xx <- dimnames(freq)$x
      cat("$$\n",mnam,"=\\left(",paste(paste(xx,"^2\\frac {",freq,"}{",n,"}"),collapse = "+"),"\\right)-(",m,")^2=",s2c(x),"\n$$")
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

