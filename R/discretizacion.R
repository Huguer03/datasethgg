#' Discretización de igual anchura
#'
#' Discretiza una variable numérica en intervalos de igual anchura.
#'
#' @param x Vector numérico que se quiere discretizar.
#' @param num.bins Número de intervalos (bins) a crear.
#' @return Un factor con la variable discretizada. El factor incluye un atributo
#'   \code{"cut.points"} con los puntos de corte utilizados (sin los extremos).
#' @details Los intervalos se calculan como
#'   \deqn{delta = (max(x) - min(x)) / num.bins}
#'   y se construyen secuencias desde el mínimo hasta el máximo. Se ajusta el
#'   último punto para evitar errores de redondeo. Se utiliza \code{\link{cut}}
#'   con \code{include.lowest = TRUE}.
#' @examples
#' x <- rnorm(100)
#' dx <- discretizacionEW(x, 5)
#' table(dx)
#' attr(dx, "cut.points")
#' @export
#' 
discretizacionEW <- function(x, num.bins){
  val.min <- min(x)
  val.max <- max(x)
  delta <- (val.max - val.min) / num.bins
  intervals <- seq(from = val.min, to = val.max, by = delta)
  intervals[num.bins + 1] <- val.max       # evita errores de redondeo
  discrete.x <- cut(x, breaks = intervals, include.lowest = TRUE)
  attr(discrete.x, "cut.points") <- intervals[2:num.bins]
  return(discrete.x)
}
#' Discretización de igual frecuencia
#'
#' Discretiza una variable numérica en intervalos con aproximadamente el mismo
#' número de observaciones.
#'
#' @param x Vector numérico que se quiere discretizar.
#' @param num.bins Número de intervalos (bins) a crear.
#' @return Un factor con la variable discretizada. El factor incluye un atributo
#'   \code{"cut.points"} con los puntos de corte utilizados (sin los extremos).
#' @details Los puntos de corte se obtienen a partir de los cuantiles
#'   \code{probs = 0:num.bins / num.bins}. Se utiliza \code{\link{cut}} con
#'   \code{include.lowest = TRUE}.
#' @examples
#' x <- rnorm(100)
#' dx <- discretizacionEF(x, 5)
#' table(dx)
#' attr(dx, "cut.points")
#' @export
#' 
discretizacionEF <- function(x, num.bins){
  intervals <- quantile(x, probs = 0:num.bins / num.bins)
  discrete.x <- cut(x, breaks = intervals, include.lowest = TRUE)
  attr(discrete.x, "cut.points") <- intervals[2:num.bins]
  return(discrete.x)
}

#' Discretizar con puntos de corte dados
#'
#' Discretiza una variable numérica utilizando unos puntos de corte
#' predefinidos.
#'
#' @param x Vector numérico que se quiere discretizar.
#' @param cut.points Vector numérico con los puntos de corte interiores. Los
#'   extremos se toman como \code{-Inf} e \code{Inf}.
#' @return Un factor con la variable discretizada.
#' @details Se ordenan los puntos de corte y se añaden \code{-Inf} e \code{Inf}
#'   para formar los intervalos. Se utiliza \code{\link{cut}} con
#'   \code{include.lowest = TRUE}.
#' @examples
#' x <- rnorm(100)
#' dx <- discretizacion(x, c(-1, 0, 1))
#' table(dx)
#' @export
#' 
discretizacion <- function(x, cut.points){
  intervalos <- c(-Inf, sort(cut.points), Inf)
  cut(x, breaks = intervalos, include.lowest = TRUE)
}

#' Entropía de Shannon
#'
#' Calcula la entropía de Shannon (en bits) de una variable categórica.
#'
#' @param x Vector o factor con los valores de la variable.
#' @return Un valor numérico con la entropía calculada mediante
#'   \deqn{- \sum p_i \log_2(p_i)}
#'   donde \eqn{p_i} son las frecuencias relativas de cada categoría.
#' @details Se eliminan las categorías con probabilidad cero para evitar
#'   problemas con el logaritmo.
#' @examples
#' x <- sample(letters[1:4], 100, replace = TRUE)
#' entropy(x)
#' @export
#'  
entropy <- function(x){
  probabilidades <- table(x) / length(x)
  probabilidades <- probabilidades[probabilidades > 0]
  sum(-probabilidades * log2(probabilidades))
}