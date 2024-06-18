## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(pat.book)

## ----include=FALSE------------------------------------------------------------
library(knitr)
library(kableExtra)
library(colorspace)
library(pat.book)

options(digits=4,knitr.kable.NA = '',scipen = 8,big.mark=" ")
opts_knit$set(global.par = TRUE,warning = FALSE, message = FALSE, fig.align = "center",fig.pos = "H", out.extra = "",results = 'asis',echo=FALSE)

par(lwd=.5,col.main=iblue,mfrow=c(1,1))
knitr::opts_chunk$set(echo = TRUE,warning = FALSE, message = FALSE)
virg <- "`"

## -----------------------------------------------------------------------------
library(knitr)
library(kableExtra)
library(colorspace)
library(pat.book)

## ----results='asis'-----------------------------------------------------------
set.seed(2)                      # per ottenere sempre la stessa simulazione
n <- 60                          # ampiezza campionaria

brk  <- c(0,1.5,3,5,7.5,15)      # intervalli (breaks)
hhh  <- c( 2,11,10, 2,1)         # aspetto presunto istogramma

nomex <- "Nome della X"          # nome della X
samp <- genera_dati(
  brk = brk,hhh = hhh,n = n)     # genera i dati dall'istogramma

ls2e(stat_base(samp = samp,brk = brk))      # crea il data set e la tabella dat3

tabl(dat3)

H.int(2:3)            # Calcolo della Densità percentuale
F.int(2:3)            # Calcolo della Ripartizione
Q.int((0:4)/4)        # Inverse della Ripartizione
histp(axes=T)         # Istogramma
h.int(2,8,col=ared,   # Aree selezionate
      density = 25)   

## ----results='asis'-----------------------------------------------------------
percentile(p = 0.45)       # Calcolo percentile
F_print(2,"<")             # calcolo della prop inferiore a 2
F_print(2,">")             # calcolo della prop superiore a 2
F_print(8,">")
F_print(x = 1.5,verso = "",x2 = 2.6)
F_print(x = 1.7,verso = "",x2 = 3.6)
F_print(x = 1.5,verso = "",x2 = 5)
F_print(x = 1.7,verso = "",x2 = 5)
F_print(x = 2,verso = "",x2 = 8) # intervallo 2-8
media_(1:4)                     # media dei dati 1,2,3,4
var_(1:4)                       # varianza dei dati 1,2,3,4
stat_(1:4)                      # media e varianza insieme (new)
stat_(1:4,p = c(2,4,5,1))       # vettore dei pesi p
stat_(rep(1:4,times=c(2,4,5,1)),semp = T) # in frazione


## ----results='asis'-----------------------------------------------------------
# Somma di due dadi
c1 <- 6
c2 <- 6
re1 <- (two_way(S_1 = 1:c1,S_2 = 1:c2,
                num1 = rep(1,times=c1),num2 = rep(1,times=c2),
                size = "\\footnotesize"))

# Differenza di due dadi
res<-two_way(S_1 = 1:c1,S_2 = 1:c2,size = "\\footnotesize",
             num1 = numeric(c1)+1,num2 = numeric(c2)+1,op = `-`)
res[[1]]
names(res)

two_way2(1:2,1:2,p1 = c(.5,.5),p2 = c(.3,.7))



## ----results='asis'-----------------------------------------------------------
bin_dis(x1 = 2,n = 5,pp = 0.34)
bin_dis(x1 = 4,n = 5,pp = 0.34,verso = "\\geq")
bin_dis(x1 = 2,n = 5,pp = 0.34,comp = T)
bin_dis(x1 = 2,n = 5,pp = 0.34,sing = T)

## ----results='asis'-----------------------------------------------------------
pois_dis(x1 = 2,ll = 1.5)
pois_dis(x1 = 2,ll = 1.5,verso = "\\geq")
pois_dis(x1 = 2,ll = 1.5,sing = T)


## ----results='asis'-----------------------------------------------------------
norm_int(x1 = 1,verso = "<",mm = 3,ss = 2.2,vnam = "\\theta",
             mu = "\\mu_\\theta",sigma = "\\sigma_\\theta")
norm_int(x1 = 4,verso = "<",mm = 3,ss = 2.2,vnam = "X",
             mu = "\\psi",sigma = "\\tau")
norm_int(x1 = 1,verso = ">",mm = 3,ss = 2.2,vnam = "Y",
             mu = "\\mu_Y",sigma = "\\sigma_Y")
norm_int(x1 = 4,verso = ">",mm = 3,ss = 2.2,)
norm_int(x1 = 1,verso = ">",mm = -3,ss = 2.2)
norm_int(x1 = 1,x2=2,mm = 3,ss = 2.2,verso = NULL)
norm_int(x1 = 1,x2=2,mm = -3,ss = 2.2,verso = NULL)
norm_int(x1 = -1,x2=2,mm = -3,ss = 2.2,verso = NULL)

## ----results='asis'-----------------------------------------------------------
tlc(tipo = "somma",x1 = 90,x2 = 110,verso = NULL,mu = 1,s2 = 1,n = 100)
tlc(tipo = "media",x1 = 9,x2 = 11,verso = NULL,mu = 10,s2 = 1,n = 100)
tlc(tipo = "prop",x1 = .1,verso = ">",mu = .2,n = 50)
tlc(tipo = "somma",x1 = 10,verso = ">",mu = .2,n = 50)

## ----results='asis'-----------------------------------------------------------
idc(xm = 10,sd = 1.1,alpha = .05,n = 15,dist_ = "z")
idc(xm = 10,sd = 1.1,alpha = .05,n = 15,dist_ = "t")
idc(xm = 10,alpha = .05,n = 15,dist_ = "z")
idc(xm = 7.4,sd = sqrt(7.4),alpha = .05,n = 75,dist_ = "z",mus = "\\lambda",
        ss = "\\sqrt\\lambda")

## ----results='asis'-----------------------------------------------------------
ztest_mu(muh = 0,s = 1,10,mu0 = .1,h1 = "\\neq",alpha = 0.05)
ttest_mu(muh = 0,sh = 1,n = 10,mu0 = .1,h1 = "<",alpha = 0.01)
ztest_pi(sn = 60,n = 100,p0 = .5,h1 = ">",alpha = 0.05)
test_2c(mu1 = 11,mu2 = 12,s1h = 1.1,s2h = 1.2,n1 = 10,n2 = 12,
            h1 = "\\neq",alpha = .05,et = T)
test_2c(mu1 = 11,mu2 = 12,s1h = 1.1,s2h = 1.2,n1 = 10,n2 = 12,
            h1 = "\\neq",alpha = .05,et = F)
test_2c(mu1 = 11,mu2 = 12,s1h = F,s2h = NULL,n1 = 50,n2 = 60,
            h1 = "\\neq",alpha = .05,et = T)
ttest_2c_et(mu1 = 11,mu2 = 12,s1h = 1.1,s2h = 1.2,n1 = 10,n2 = 12,
                h1 = "\\neq",alpha = .05)
ttest_2c_om(mu1 = 11,mu2 = 12,s1h = 1.1,s2h = 1.2,n1 = 10,n2 = 12,
                h1 = "\\neq",alpha = .001)
ztest_2c_pi(s1 = 120,s2 = 130,n1 = 250,n2 = 260,h1 = "<",alpha = .01)

dat <- matrix(c(15,5,12,10,35,15),2,byrow = T)

nome_x<- c("Rai","Mediaset","La7")
nome_y <- c("Laureato","Non Laureato")

dat          <- chi_print(dat,nome_x = nome_x,nome_y = nome_y)[[1]]
dat_print    <- chi_print(dat,nome_x = nome_x,nome_y = nome_y)[[2]]

kable(dat_print,digits = 4,row.names = T,linesep="",booktabs=T,escape = F) %>%
  kable_styling(full_width = F, latex_options = "HOLD_position") %>%
  column_spec(column = 1,bold = T)

cat(chi_test(dat = dat,alpha = 0.01))

frqc <- sample(1:25,6,T)
frq0 <- rep(1/6*100,6)
chi_print_conf(frqc,frq0,X = 1:6,Y = c("dado osservato","dado perfetto"))
chi_conf(frqc,frq0,X = 1:6,Y = c("dado osservato","dado perfetto"))

## ----results='asis'-----------------------------------------------------------
set.seed(12)                 # ripete le stesse generazioni casuali
n <- 100                     # fisso n
x <- rnorm(n,10)             # genero x
y <- x+rnorm(n,0,1)          # genero y
attach(regr(x = x,y = y))    # produco le statistiche di base

calcolo_beta()
calcolo_beta(semplice = T)
residuo(x[12],y[12])
se_beta0()
se_beta1()
ttest_beta(cof = 0,bj0 = 0,h1 = "<",alpha = 0.01)
ttest_beta(cof = 1,bj0 = 0,h1 = "\\neq",alpha = 0.01)

