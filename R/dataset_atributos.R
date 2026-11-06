#' Atributo
#'
#' Clase S4 virtual que representa un atributo genérico. De ella heredan
#' \code{\linkS4class{atributo.num}} y \code{\linkS4class{atributo.cat}}.
#'
#' @slot nombre Caracter con el nombre del atributo.
#' @exportClass atributo
#' 
setClass("atributo", representation("VIRTUAL", nombre = "character"))

#' Atributo numérico
#'
#' Clase S4 que representa un atributo con valores numéricos.
#'
#' Hereda de \code{\linkS4class{atributo}}.
#'
#' @slot valores Vector numérico con los valores del atributo.
#' @exportClass atributo.num
#' 
setClass("atributo.num", contains = "atributo", representation(valores = "numeric"))

#' Atributo categórico
#'
#' Clase S4 que representa un atributo categórico.
#'
#' Hereda de \code{\linkS4class{atributo}}.
#'
#' @slot valores Factor con los valores del atributo.
#' @exportClass atributo.cat
#' 
setClass("atributo.cat", contains = "atributo", representation(valores = "factor"))