# Funzioni Usate nel libro ####
# Patrizio Frederic ####

#_______________________ ####

## funzioni speciali ####

srt <- ""
src_ <- function(x) {paste(srt,x,sep = "")}

ls2e <- function(x) invisible(list2env(x,envir = globalenv()))

tabl <- function(x,...){
  kable(x,...,booktabs=T,escape = F,linesep="") %>%
   kable_styling(full_width = F, latex_options = "HOLD_position")
}

### Punto elenco personalizzato numero.lettera 

item <- function(){
  it <- (paste(i1,".",letters[i2],sep = ""))
  return(it)}

### Funzione parentesi 

p <- function(x,ax=4){
  p1 <- ifelse(x < 0,"(","")
  p2 <- ifelse(x < 0,")","")
  paste(p1,round(x,ax),p2,sep="")
}
  

## Statistica Descrittiva ####

s2c <- function(x) {(mean(x^2)-mean(x)^2)}  # varianza di pop
sc  <- function(x) {sqrt(s2c(x))}        # sd di pop


vvv <- function(x,p=NULL) {           # varianza per distr tabella e prob
  if (is.null(p)) v <- mean(x^2)-mean(x)^2
  else v <- sum(p*x^2)-(sum(p*x))^2
  return(v)
}

vunif <- function(nnn, brk){           # genera i dati da una mistura di uniformi
  k <- length(brk)-1
  br1  <- brk[-(k+1)]
  br2  <- brk[-1]
  xi  <- runif(nnn[1],br1[1],br2[1])
  for (i in 2:k)
   xi <- c(xi,runif(nnn[i],br1[i],br2[i]))
  return(xi)
}

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
<<<<<<< HEAD
                   paste0("&=&",datp$fj[j1],"\\times 100 +"),
=======
                   paste0(datp$fj[j1],"\\times 100 +"),
>>>>>>> 36499b78c0abd9b23f47e10ca7c2f1603564c797
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
 
## Probabilità ####

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
     c0 <- paste("                &=& 1-\\left(",paste("\\binom{",n,"}{",xx,"}",pp,"^{",xx,"}(1-",pp,")^{",n,"-",xx,"}",collapse = "+"),"\\right)\\\\")
     c1 <- paste("                &=& 1-(",paste(round(dbinom(xx,n,pp),4),collapse = "+"),")\\\\")
     c2 <- paste("                &=& 1-",sum(round(dbinom(xx,n,pp),4)),"\\\\")
     c3 <- paste("                &=&  ",1-sum(round(dbinom(xx,n,pp),4)))
     res <- paste(c00,c0,c1,c2,c3)  
   } 
  }
  
  cat("",size,"
   \\begin{eqnarray*}
     ",res,"
   \\end{eqnarray*}
   \\normalsize ")
}

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
   
   if (x1<=mm & x2>=mm) {
     p1 <- paste("\\Phi(", z2,")-(1-\\Phi(",-z1,")) \\\\", "&=& ",f2,"-(1-",f1,") \\\\")}
   if (x1>=mm & x2>=mm) {
     p1 <- paste(f2,"-",f1,"\\\\")
   }
   if (x1<=mm & x2<mm) {
     p1 <- paste("(1-\\Phi(", -z2,"))-(1-\\Phi(",-z1,")) \\\\","&=& (1-",f2,")-(1-",f1,") \\\\")
   }
   
   mm <- ifelse(mm>=0,mm,paste("(",mm,")"))
   
   cat("\\begin{eqnarray*}
   P(",x1,"<",vnam,"\\leq ",x2,") &=& P\\left( \\frac {",x1," - ",mm,"}{\\sqrt{",ss,"}} < \\frac {",vnam," - ",mu,"}{",sigma,"} \\leq \\frac {",x2," - ",mm,"}{\\sqrt{",ss,"}}\\right)  \\\\
              &=& P\\left( ",z1," < Z \\leq ",z2,"\\right) \\\\
              &=& \\Phi(",z2,")-\\Phi(",z1,")\\\\
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
  

draw_dist <- function(dist,z1,z2,...){   # aggiunge una distribuzione tratteggiata
  xx <- c(z1,seq(z1,z2,length=100),z2)
  yy <- c(0 ,dist(seq(z1,z2,length=100)),0)
  polygon(xx,yy,...,border = NA)
  curve(dist,z1,z2,add=T)
}

## Inferenza ####

idc <- function(xm,sd=NULL,alpha,n,dist_,mus=NULL,ss=NULL){
  if (!is.null(mus)){
   sd <- ifelse(dist_=="t",sd*sqrt(n/(n-1)),sd)
   SEs <- paste("\\frac{",ss,"}{\\sqrt{n}}")
   SEn <- paste("\\frac{",p(sd),"}{\\sqrt{",n,"}}")
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
  if (!is.null(sd)&is.null(mus)&dist_=="z"){
   mus <- "\\hat\\mu"
   SEs  <- "\\frac{\\sigma}{\\sqrt{n}}"
   SEn <- paste("\\frac{",sd,"}{\\sqrt{",n,"}}")
  }
  if (!is.null(sd)&is.null(mus)&dist_=="t"){
   mus <- "\\hat\\mu"
   SEs  <- "\\frac{S}{\\sqrt{n}}"
   SEn <- paste("\\frac{",p(sd*sqrt(n/(n-1))),"}{\\sqrt{",n,"}}")
  }
  
  tstat <- ifelse(dist_=="z",qnorm(1-alpha/2),qt(1-alpha/2,n-1))
  tsimb <- ifelse(dist_=="z","z_{\\alpha/2}","t_{n-1;\\alpha/2}")
  if (dist_=="z"){
  sc <- sd
  idcn <- xm+c(-1,1)*tstat*sd/sqrt(n)
  }
  if (dist_=="t"){
   sc <- sqrt(n/(n-1))*sd
   cat(
     "\\[
     S =\\sqrt{\\frac {n}{n-1}}\\cdot\\hat\\sigma =
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

  cat("**Test Z per una proporzione**\n\n")  
  
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
  
  cat("**Test Z per una media, variazna nota**\n\n")  
  
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

ttest_2c_et <-  function(mu1,mu2,s1h,s2h,n1,n2,h1 = "\\neq", alpha = 0.05,a="1",b="2",um="",et=T){
  test_2c(mu1=mu1,mu2 = mu2,s1h=s1h,s2h=s2h,n1,n2,h1 = h1, alpha = alpha,et=T,a=a,b=b,um=um)
}

ttest_2c_om <-  function(mu1,mu2,s1h,s2h,n1,n2,h1 = "\\neq", alpha = 0.05,a="1",b="2",um="",et=F){
  test_2c(mu1=mu1,mu2 = mu2,s1h=s1h,s2h=s2h,n1,n2,h1 = h1, alpha = alpha,et=F,a=a,b=b,um=um)
}

ztest_2c_pi <-  function(s1,s2,n1,n2,h1 = "\\neq", alpha = 0.05,a="1",b="2"){
  test_2c(mu1 = s1,mu2 = s2,s1h = F,s2h = F,n1 = n1,n2 = n2,h1 = h1,alpha = alpha,a = a,b = b)
}

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

chi_print_conf <- function(Freq_c,Freq_0,X,Y){
  S <- c(paste("$",Freq_c,"$",sep = ""), paste("$",sum(Freq_c),"$",sep = ""))
  N <- c(paste("$",Freq_0,"\\%$",sep=""),"$100\\%$")
  D <- data.frame(rbind(S, N), row.names = Y)
  names(D) <- c(X,"Totale")
  tabl(D)
}

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
  
  tabl(thr,digits = 3,row.names = T)  %>%
   column_spec(column = 1,bold = T)
  
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

chi_conf <- function(Freq_c,Freq_0,X,Y,alpha=0.05){
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
\\]")
  
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

## Regressione ####


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
   ifelse(r^2>.75,"Il modello si adatta bene ai dati.","Il modello **non** si adatta bene ai dati.")
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
  
  
  se_beta1 <- function(x=NULL,y=NULL,stat1=NULL,stat2=NULL){
   cat(
     "\\begin{eqnarray*}
\\hat{\\sigma_\\varepsilon}^2&=&(1-r^2)\\hat\\sigma_Y^2\\\\
&=& (1-",r^2,")\\times",vy,"\\\\
   &=& ",sh2,"\\\\
   S_\\varepsilon^2 &=& \\frac{n} {n-2} \\hat{\\sigma_\\varepsilon}^2\\\\
   &=&  \\frac{",n,"} {",n,"-2} \\hat{\\sigma_\\varepsilon}^2 \\\\
 &=&  \\frac{",n,"} {",n,"-2} \\times ",sh2," = ",se2," 
\\end{eqnarray*}

E quindi

\\begin{eqnarray*}
V(\\hat\\beta_{1}) &=& \\frac{\\sigma_{\\varepsilon}^{2}} {n \\hat{\\sigma}^{2}_{X}} \\\\
\\widehat{V(\\hat\\beta_{1})} &=& \\frac{S_{\\varepsilon}^{2}} {n \\hat{\\sigma}^{2}_{X}} \\\\
 &=& \\frac{",se2,"} {",n,"\\times ",vx,"} = ",vb1,"\\\\
 \\widehat{SE(\\hat\\beta_{1})}        &=&  \\sqrt{",vb1,"}\\\\
 &=& ",sqrt(vb1),"
\\end{eqnarray*}
")
  }
  
  se_beta0 <- function(x=NULL,y=NULL,stat1=NULL,stat2=NULL)  {
   cat("
\\begin{eqnarray*}
\\hat{\\sigma_\\varepsilon}^2&=&(1-r^2)\\hat\\sigma_Y^2\\\\
&=& (1-",r^2,")\\times",vy,"\\\\
   &=& ",sh2,"\\\\
   S_\\varepsilon^2 &=& \\frac{n} {n-2} \\hat{\\sigma_\\varepsilon}^2\\\\
   &=&  \\frac{",n,"} {",n,"-2} \\hat{\\sigma_\\varepsilon}^2 \\\\
 &=&  \\frac{",n,"} {",n,"-2} \\times ",sh2," = ",se2," 
\\end{eqnarray*}

E quindi

\\begin{eqnarray*}
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
   \n\n **Graficamente**",sep="")
     

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



