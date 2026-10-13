#' Aplicar una función a los atributos numéricos
#'
#' Aplica una función a todos los atributos numéricos de un dataset y devuelve
#' el dataset resultante.
#'
#' @param ds Objeto de clase \code{\linkS4class{dataset}}.
#' @param f Función que recibe un objeto \code{\linkS4class{atributo.num}} y
#'   devuelve otro objeto de clase \code{\linkS4class{atributo}} (normalmente
#'   \code{\linkS4class{atributo.num}} o \code{\linkS4class{atributo.cat}}).
#' @return El dataset \code{ds} con los atributos numéricos transformados por
#'   \code{f}. Los atributos categóricos no se modifican.
#' @details Sólo se aplica \code{f} a los atributos para los que
#'   \code{\link{es.num}} devuelve \code{TRUE}. Al final se valida el objeto
#'   resultante con \code{\link{validObject}}.
#' @seealso \code{\link{es.num}}, \code{\link{discretizar.dataset}},
#'   \code{\link{normalizar.dataset}}, \code{\link{estandarizar.dataset}}.
#' @export
#' 
aplica.num <- function(ds, f){
  for (n in names(ds@atributos)){
    a <- ds@atributos[[n]]
    if (es.num(a)) ds@atributos[[n]] <- f(a)
  }
  validObject(ds)
  return(ds)
}

#' Discretizar los atributos numéricos de un dataset
#'
#' Convierte los atributos numéricos de un dataset en atributos categóricos
#' aplicando discretización de igual anchura o de igual frecuencia.
#'
#' @param ds Objeto de clase \code{\linkS4class{dataset}}.
#' @param num.bins Número de intervalos (bins) a crear.
#' @param metodo Caracter que indica el método de discretización. Valores
#'   válidos: \code{"EW"} (equal width, igual anchura) o \code{"EF"}
#'   (equal frequency, igual frecuencia, valor por defecto).
#' @return El dataset con los atributos numéricos transformados en objetos de
#'   clase \code{\linkS4class{atributo.cat}}.
#' @details Se delega en \code{\link{aplica.num}}. El atributo
#'   \code{"cut.points"} generado por las funciones de discretización se elimina
#'   antes de guardar los valores.
#' @seealso \code{\link{discretizacionEW}}, \code{\link{discretizacionEF}}.
#' @export
#' 
discretizar.dataset <- function(ds, num.bins, metodo = "EF"){
  f <- if (metodo == "EW") discretizacionEW else discretizacionEF
  aplica.num(ds, function(a){
    v <- f(a@valores, num.bins)
    attr(v, "cut.points") <- NULL       # quitamos el atributo extra antes de guardarlo
    new("atributo.cat", nombre = a@nombre, valores = v)
  })
}

#' Estandarizar los atributos numéricos de un dataset
#'
#' Estandariza (puntuaciones Z) todos los atributos numéricos de un dataset.
#'
#' @param ds Objeto de clase \code{\linkS4class{dataset}}.
#' @return El dataset con los atributos numéricos estandarizados.
#' @details Se delega en \code{\link{aplica.num}} usando la función
#'   \code{\link{standardize}}.
#' @seealso \code{\link{standardize}}.
#' @export
#' 
normalizar.dataset <- function(ds){
  aplica.num(ds, function(a) new("atributo.num", nombre = a@nombre, valores = normalize(a@valores)))
}

estandarizar.dataset <- function(ds){
  aplica.num(ds, function(a) new("atributo.num", nombre = a@nombre, valores = standardize(a@valores)))
}

#' Métricas por variable de un dataset
#'
#' Calcula un conjunto de métricas para cada variable de un dataset: varianza y
#' AUC para las variables numéricas, y entropía para las categóricas.
#'
#' @param ds Objeto de clase \code{\linkS4class{dataset}}.
#' @return Un data frame con una fila por variable (excluyendo la clase) y las
#'   columnas:
#'   \describe{
#'     \item{\code{variable}}{Nombre de la variable.}
#'     \item{\code{tipo}}{\code{"numerica"} o \code{"categorica"}.}
#'     \item{\code{varianza}}{Varianza (sólo para variables numéricas).}
#'     \item{\code{auc}}{AUC (sólo para numéricas y si la clase es binaria).}
#'     \item{\code{entropia}}{Entropía (sólo para variables categóricas).}
#'   }
#' @details El AUC se calcula sólo si el dataset tiene una variable clase con
#'   exactamente dos niveles. En ese caso, el nivel positivo es el segundo nivel
#'   del factor.
#' @seealso \code{\link{auc.variable}}, \code{\link{entropy}}.
#' @export
#' 
metricas.dataset <- function(ds){
  nombres <- setdiff(names(ds@atributos), ds@clase)
  res <- data.frame(variable = nombres, tipo = NA, varianza = NA, auc = NA, entropia = NA)
  
  hay.auc <- FALSE
  if (length(ds@clase) == 1){
    cl <- droplevels(ds@atributos[[ds@clase]]@valores)
    if (nlevels(cl) == 2){
      hay.auc <- TRUE
      etiqueta <- cl == levels(cl)[2]
    }
  }
  
  for (i in seq_along(nombres)){
    a <- ds@atributos[[nombres[i]]]
    if (es.num(a)){
      res$tipo[i] <- "numerica"
      res$varianza[i] <- var(a@valores)
      if (hay.auc) res$auc[i] <- auc.variable(a@valores, etiqueta)
    } else {
      res$tipo[i] <- "categorica"
      res$entropia[i] <- entropy(a@valores)
    }
  }
  return(res)
}

#' Filtrar variables de un dataset según una métrica
#'
#' Elimina del dataset las variables que no cumplen un umbral sobre una métrica
#' dada.
#'
#' @param ds Objeto de clase \code{\linkS4class{dataset}}.
#' @param metrica Caracter con el nombre de la métrica a usar. Valores válidos:
#'   \code{"varianza"}, \code{"auc"} o \code{"entropia"}.
#' @param umbral Valor numérico del umbral.
#' @param mayor Lógico. Si es \code{TRUE} (por defecto) se conservan las
#'   variables con métrica \eqn{\ge} \code{umbral}. Si es \code{FALSE} se
#'   conservan las que tienen métrica \eqn{\le} \code{umbral}.
#' @return El dataset sin las variables que no cumplen el criterio.
#' @details Las variables a las que no aplica la métrica (por ejemplo, la
#'   entropía sobre una variable numérica) y la variable clase no se eliminan.
#' @seealso \code{\link{metricas.dataset}}.
#' @export
#' 
filtrar.dataset <- function(ds, metrica, umbral, mayor = TRUE){
  m <- metricas.dataset(ds)
  v <- m[[metrica]]
  cumple <- if (mayor) v >= umbral else v <= umbral
  quitar <- m$variable[!is.na(v) & !cumple]
  ds@atributos <- ds@atributos[!(names(ds@atributos) %in% quitar)]
  validObject(ds)
  return(ds)
}

#' Matriz de correlación de las variables numéricas
#'
#' Calcula la matriz de correlación entre todas las variables numéricas de un
#' dataset.
#'
#' @param ds Objeto de clase \code{\linkS4class{dataset}}.
#' @return Matriz de correlaciones con nombres de fila y columna iguales a los
#'   nombres de las variables numéricas.
#' @details Sólo se consideran las variables para las que \code{\link{es.num}}
#'   devuelve \code{TRUE}. Se usa \code{\link{cor}} sobre el data frame
#'   correspondiente.
#' @seealso \code{\link{matriz.mi}}, \code{\link{dataset.a.df}}.
#' @export
#' 
matriz.cor <- function(ds){
  df <- dataset.a.df(ds)
  nombres <- names(ds@atributos)[sapply(ds@atributos, es.num)]
  cor(df[, nombres])
}

#' Matriz de información mutua entre variables categóricas
#'
#' Calcula la información mutua entre todas las variables categóricas de un
#' dataset. Opcionalmente, las variables numéricas pueden discretizarse y
#' entrar también en el cálculo.
#'
#' @param ds Objeto de clase \code{\linkS4class{dataset}}.
#' @param num.bins Entero opcional. Si se indica, las variables numéricas se
#'   discretizan con igual frecuencia en \code{num.bins} intervalos y se
#'   incluyen en el cálculo. Si es \code{NULL} (por defecto), las variables
#'   numéricas se ignoran.
#' @return Matriz cuadrada y simétrica con la información mutua entre pares de
#'   variables. Los nombres de fila y columna son los nombres de las variables
#'   consideradas.
#' @details Se usa \code{\link{mutual.info}} para calcular cada celda. La
#'   diagonal contiene la información mutua de cada variable consigo misma
#'   (su entropía).
#' @seealso \code{\link{mutual.info}}, \code{\link{discretizacionEF}},
#'   \code{\link{matriz.cor}}.
#' @export
#' 
matriz.mi <- function(ds, num.bins = NULL){
  facs <- list()
  for (n in names(ds@atributos)){
    a <- ds@atributos[[n]]
    if (!es.num(a)) facs[[n]] <- a@valores
    else if (!is.null(num.bins)) facs[[n]] <- discretizacionEF(a@valores, num.bins)
  }
  k <- length(facs)
  m <- matrix(0, k, k, dimnames = list(names(facs), names(facs)))
  for (i in 1:k){
    for (j in 1:k){
      m[i, j] <- mutual.info(facs[[i]], facs[[j]])
    }
  }
  return(m)
}