# Dataset: lista de atributos + nombre de la variable clase (opcional)#' Dataset
#'
#' Clase S4 que representa un conjunto de datos formado por atributos y,
#' opcionalmente, el nombre de la variable clase.
#'
#' @slot atributos Lista de objetos de clase \code{\linkS4class{atributo}}.
#' @slot clase Caracter con el nombre de la variable clase. Si tiene longitud 1,
#'   debe existir en \code{atributos} y ser de clase \code{\linkS4class{atributo.cat}}.
#' @exportClass dataset
#' 
setClass("dataset", representation(atributos = "list", clase = "character"))

# Comprobaciones al crear un dataset
setValidity("dataset", function(object){
  n <- sapply(object@atributos, function(a) length(a@valores))
  if (length(unique(n)) > 1) return("Los atributos tienen distinta longitud")
  if (length(object@clase) == 1){
    if (!(object@clase %in% names(object@atributos))) return("La clase no esta en el dataset")
    if (!is(object@atributos[[object@clase]], "atributo.cat")) return("La clase debe ser categorica")
  }
  TRUE
})

#' Mostrar un dataset
#'
#' Método para imprimir un resumen de un objeto \code{\linkS4class{dataset}}.
#'
#' @param object Objeto de clase \code{\linkS4class{dataset}}.
#' @return Invisiblemente \code{NULL}; se usa por su efecto secundario de impresión.
#' @export
#' 
setMethod("show", "dataset", function(object){
  cat("Dataset:", length(object@atributos), "variables\n")
  cat("Clase:", ifelse(length(object@clase) == 1, object@clase, "(ninguna)"), "\n")
  for (a in object@atributos){
    tipo <- ifelse(is(a, "atributo.num"), "numerica", "categorica")
    cat(" -", a@nombre, ":", tipo, "\n")
  }
})