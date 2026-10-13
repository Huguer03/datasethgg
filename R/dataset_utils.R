#' Comprobar si un atributo es numérico
#'
#' @param a Objeto de clase \code{\linkS4class{atributo}}.
#' @return \code{TRUE} si \code{a} es de clase \code{\linkS4class{atributo.num}},
#'   \code{FALSE} en caso contrario.
#' @export
#' 
es.num <- function(a) is(a, "atributo.num")

#' Crear un dataset a partir de un data.frame
#'
#' @param df Data frame con los datos.
#' @param clase Caracter opcional con el nombre de la variable clase. Si es
#'   \code{NULL}, no se asigna clase.
#' @return Objeto de clase \code{\linkS4class{dataset}}.
#' @details Las columnas numéricas se convierten en \code{\linkS4class{atributo.num}}
#'   y el resto en \code{\linkS4class{atributo.cat}}.
#' @examples
#' df <- data.frame(x = 1:3, y = factor(c("a", "b", "a")))
#' ds <- crear.dataset(df)
#' ds
#' @export
#' 
crear.dataset <- function(df, clase = NULL){
  atributos <- list()
  for (n in names(df)){
    if (is.numeric(df[[n]])){
      atributos[[n]] <- new("atributo.num", nombre = n, valores = df[[n]])
    } else {
      atributos[[n]] <- new("atributo.cat", nombre = n, valores = as.factor(df[[n]]))
    }
  }
  if (is.null(clase)) clase <- character(0)
  new("dataset", atributos = atributos, clase = clase)
}

#' Convertir un dataset a data.frame
#'
#' @param ds Objeto de clase \code{\linkS4class{dataset}}.
#' @return Data frame con los valores de los atributos.
#' @export
#' 
dataset.a.df <- function(ds){
  data.frame(lapply(ds@atributos, function(a) a@valores), check.names = FALSE)
}

#' Leer un dataset desde CSV
#'
#' @param path Ruta al fichero CSV.
#' @param clase Caracter opcional con el nombre de la variable clase.
#' @param max.val Número máximo de valores distintos para considerar una variable
#'   categórica.
#' @return Objeto de clase \code{\linkS4class{dataset}}.
#' @details Si una columna tiene menos de \code{max.val} valores distintos se
#'   convierte en factor. Las columnas de tipo character también se convierten en
#'   factor.
#' @export
#' 
leer.dataset <- function(path, clase = NULL, max.val = 5){
  df <- read.csv(path, stringsAsFactors = FALSE)
  for (n in names(df)){
    if (length(unique(df[[n]])) < max.val) df[[n]] <- as.factor(df[[n]])
    else if (is.character(df[[n]]))        df[[n]] <- as.factor(df[[n]])
  }
  crear.dataset(df, clase)
}

#' Escribir un dataset en CSV
#'
#' @param ds Objeto de clase \code{\linkS4class{dataset}}.
#' @param path Ruta de destino.
#' @return Escribe el fichero.
#' @details La variable clase no se guarda en el CSV; hay que indicarla de nuevo
#'   al leer.
#' @export
#' 
escribir.dataset <- function(ds, path){
  write.csv(dataset.a.df(ds), path, row.names = FALSE)
}