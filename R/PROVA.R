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
