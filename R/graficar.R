#' Gráfico de la curva ROC
#'
#' Genera un gráfico de la curva ROC a partir de un data frame con las
#' puntuaciones de un clasificador y la etiqueta binaria.
#'
#' @param df Data frame con dos columnas: \code{Val} (puntuación numérica) y
#'   \code{Etiqueta} (lógica, \code{TRUE} para positivo).
#' @param name Caracter con el nombre de la curva, usado en la leyenda.
#' @param ref Lógico. Si es \code{TRUE} (por defecto) añade la línea diagonal
#'   de referencia.
#' @return Un objeto de clase \code{ggplot} con la curva ROC.
#' @details Requiere el paquete \pkg{ggplot2}. Se usa \code{\link{roc}} para
#'   calcular los puntos de la curva.
#' @seealso \code{\link{roc}}, \code{\link{graf.auc}}.
#' @examples
#' \dontrun{
#' df <- data.frame(Val = c(0.9, 0.8, 0.4, 0.3),
#'                  Etiqueta = c(TRUE, TRUE, FALSE, FALSE))
#' graf.roc(df)
#' }
#' @export
#' 
graf.roc <- function(df, name = "ROC curve", ref = TRUE){
  curva <- roc(df)
  df.plt <- data.frame(FPR = curva$FPR, TPR = curva$TPR, ROC = name)
  g <- ggplot(df.plt, aes(x = FPR, y = TPR, col = ROC)) + geom_line()
  if (ref) g <- g + geom_abline(slope = 1, intercept = 0, linetype = "dashed")
  return(g)
}


#' Gráfico de barras del AUC por variable
#'
#' Genera un gráfico de barras con el AUC de cada variable numérica de un
#' dataset.
#'
#' @param ds Objeto de clase \code{\linkS4class{dataset}}.
#' @return Un objeto de clase \code{ggplot} con las barras del AUC. Se añade
#'   una línea horizontal discontinua en \eqn{AUC = 0.5}.
#' @details Requiere el paquete \pkg{ggplot2}. Se usa
#'   \code{\link{metricas.dataset}} para obtener el AUC. Sólo se representan
#'   las variables numéricas para las que se ha podido calcular el AUC (es
#'   decir, cuando la clase es binaria).
#' @seealso \code{\link{metricas.dataset}}, \code{\link{graf.roc}}.
#' @examples
#' \dontrun{
#' ds <- crear.dataset(data.frame(x = rnorm(100), y = rnorm(100),
#'                                clase = factor(sample(c("A","B"), 100, TRUE))),
#'                     clase = "clase")
#' graf.auc(ds)
#' }
#' @export
#' 
graf.auc <- function(ds){
  m <- metricas.dataset(ds)
  m <- m[!is.na(m$auc), ]
  ggplot(m, aes(x = variable, y = auc)) +
    geom_bar(stat = "identity") +
    geom_hline(yintercept = 0.5, linetype = "dashed") +
    coord_flip()
}


#' Gráfico de barras de la entropía normalizada
#'
#' Genera un gráfico de barras con la entropía normalizada de cada variable de
#' un data frame en el que todas las columnas son factores.
#'
#' @param df Data frame en el que todas las columnas son factores.
#' @return Un objeto de clase \code{ggplot} con las barras de entropía
#'   normalizada.
#' @details Requiere el paquete \pkg{ggplot2}. La entropía normalizada se
#'   calcula dividiendo la entropía de Shannon entre \eqn{\log_2(n)}, donde
#'   \eqn{n} es el número de niveles del factor. Si una variable tiene un solo
#'   nivel, su entropía normalizada se define como 0. Se comprueba que todas
#'   las columnas sean factores; en caso contrario se lanza un error.
#' @seealso \code{\link{entropy}}.
#' @examples
#' \dontrun{
#' df <- data.frame(a = factor(sample(letters[1:3], 100, TRUE)),
#'                  b = factor(sample(letters[4:6], 100, TRUE)))
#' graf.entropy(df)
#' }
#' @export
#' 
graf.entropy <- function(df){
  if (!all(sapply(df, is.factor))) stop("Todas las variables deben ser factores")
  NS <- sapply(df, function(fact){
    n <- length(levels(fact))
    if (n <= 1) return(0)
    entropy(fact) / log2(n)
  })
  df.plot <- data.frame(Variable = names(NS), Entropy = NS)
  ggplot(df.plot, aes(x = Variable, y = Entropy)) + geom_bar(stat = "identity")
}


#' Mapa de calor de una matriz
#'
#' Genera un mapa de calor a partir de una matriz numérica con nombres de fila
#' y columna. Sirve tanto para matrices de correlación como de información
#' mutua.
#'
#' @param m Matriz numérica con nombres de fila y columna.
#' @param titulo Caracter con el título del gráfico.
#' @return Un objeto de clase \code{ggplot} con el mapa de calor. Los valores
#'   se muestran redondeados a dos decimales sobre cada celda.
#' @details Requiere los paquetes \pkg{ggplot2} y \pkg{reshape2}. Se usa
#'   \code{reshape2::melt} para convertir la matriz en formato largo.
#' @seealso \code{\link{graf.cor}}, \code{\link{graf.mi}}.
#' @export
#' 
graf.matriz <- function(m, titulo = ""){
  df.plot <- melt(m)
  ggplot(df.plot, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    geom_text(aes(label = round(value, 2)), size = 3) +
    labs(title = titulo, x = "", y = "")
}


#' Mapa de calor de correlaciones
#'
#' Genera un mapa de calor con la matriz de correlaciones de las variables
#' numéricas de un dataset.
#'
#' @param ds Objeto de clase \code{\linkS4class{dataset}}.
#' @return Un objeto de clase \code{ggplot}.
#' @details Es una envoltura de \code{\link{graf.matriz}} aplicada a
#'   \code{\link{matriz.cor}(ds)}.
#' @seealso \code{\link{matriz.cor}}, \code{\link{graf.matriz}}.
#' @export
#' 
graf.cor <- function(ds) graf.matriz(matriz.cor(ds), "Correlaciones")


#' Mapa de calor de información mutua
#'
#' Genera un mapa de calor con la matriz de información mutua entre las
#' variables de un dataset.
#'
#' @param ds Objeto de clase \code{\linkS4class{dataset}}.
#' @param num.bins Entero opcional. Si se indica, las variables numéricas se
#'   discretizan con igual frecuencia en \code{num.bins} intervalos y se
#'   incluyen en el cálculo. Si es \code{NULL} (por defecto), las variables
#'   numéricas se ignoran.
#' @return Un objeto de clase \code{ggplot}.
#' @details Es una envoltura de \code{\link{graf.matriz}} aplicada a
#'   \code{\link{matriz.mi}(ds, num.bins)}.
#' @seealso \code{\link{matriz.mi}}, \code{\link{graf.matriz}}.
#' @export
#' 
graf.mi  <- function(ds, num.bins = NULL) graf.matriz(matriz.mi(ds, num.bins), "Informacion mutua (bits)")