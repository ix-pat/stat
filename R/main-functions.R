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


