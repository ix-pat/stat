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
ls2e <- function(x) {invisible(list2env(x, envir = parent.frame()))}

#' Arrotonda Tutte le Variabili Numeriche in un Ambiente
#'
#' Questa funzione arrotonda iterativamente tutte le variabili numeriche a un numero specificato
#' di cifre decimali all'interno dell'ambiente fornito. Di default, utilizza l'ambiente da cui
#' la funzione viene chiamata.
#'
#' @param dig Un intero che specifica il numero di cifre decimali a cui i valori numerici devono
#'            essere arrotondati. Il valore di default è 4.
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
#' # Utilizziamo round_all per arrotondarle a 2 cifre decimali
#' round_all(dig = 2)
#' 
#' @export

round_all <- function(dig = 4, env = parent.frame()) {
  nomi_variabili <- ls(envir = env)
  
  # Itera sui nomi delle variabili per arrotondare quelle numeriche
  for (nome in nomi_variabili) {
    valore <- get(nome, envir = env)
    if (is.numeric(valore)) {
      # Arrotonda il valore e aggiornalo nell'ambiente specificato
      assign(nome, round(valore, digits = dig), envir = env)
    }
  }
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

