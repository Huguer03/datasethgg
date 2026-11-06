#' Normalizar al rango [0, 1]
#'
#' Normaliza un vector numérico al intervalo \eqn{[0, 1]} mediante
#' reescalado min-max.
#'
#' @param x Vector numérico a normalizar.
#' @return Vector numérico normalizado, con mínimo 0 y máximo 1.
#' @details Se aplica la transformación
#'   \deqn{x' = \frac{x - \min(x)}{\max(x) - \min(x)}}
#'   Si todos los valores de \code{x} son iguales, el resultado contiene
#'   \code{NaN} debido a una división por cero.
#' @examples
#' normalize(c(1, 2, 3, 4, 5))
#' @export
#' 
normalize <- function(x) (x - min(x)) / (max(x) - min(x))

#' Estandarizar (puntuaciones Z)
#'
#' Estandariza un vector numérico restándole la media y dividiéndolo por la
#' desviación típica.
#'
#' @param x Vector numérico a estandarizar.
#' @return Vector numérico estandarizado, con media 0 y desviación típica 1.
#' @details Se aplica la transformación
#'   \deqn{x' = \frac{x - \bar{x}}{s}}
#'   donde \eqn{\bar{x}} es la media y \eqn{s} la desviación típica muestral.
#' @examples
#' standardize(c(1, 2, 3, 4, 5))
#' @export
#' 
standardize <- function(x) (x - mean(x)) / sd(x)

#' Tasas TPR y FPR para un punto de corte
#'
#' Calcula la tasa de verdaderos positivos (TPR) y la tasa de falsos positivos
#' (FPR) al aplicar un punto de corte sobre las puntuaciones de un clasificador
#' binario.
#'
#' @param cut_value Valor numérico a partir del cual una observación se
#'   clasifica como positiva.
#' @param df Data frame con dos columnas: \code{Val} (puntuación numérica) y
#'   \code{Etiqueta} (lógica, \code{TRUE} para positivo).
#' @param shorted Lógico. Si es \code{FALSE} (por defecto), el data frame se
#'   ordena internamente por \code{Val} de forma decreciente. Si es \code{TRUE},
#'   se asume que ya está ordenado y no se reordena.
#' @return Vector numérico con nombres \code{TPR} y \code{FPR}.
#' @details Las tasas se calculan a partir de la matriz de confusión:
#'   \deqn{TPR = \frac{TP}{TP + FN}, \qquad FPR = \frac{FP}{FP + TN}}
#' @examples
#' df <- data.frame(Val = c(0.9, 0.8, 0.4, 0.3),
#'                  Etiqueta = c(TRUE, TRUE, FALSE, FALSE))
#' rates(0.5, df)
#' @export
#' 
rates <- function(cut_value, df, shorted = FALSE){
  if (!shorted){
    df.shorted <- df[order(df$Val, decreasing = TRUE), ]
  } else {
    df.shorted <- df
  }
  df.positive <- df.shorted[df.shorted$Val >= cut_value, ]
  df.negative <- df.shorted[df.shorted$Val <  cut_value, ]
  
  TP <- sum(df.positive$Etiqueta == TRUE,  na.rm = TRUE)
  FP <- sum(df.positive$Etiqueta == FALSE, na.rm = TRUE)
  TN <- sum(df.negative$Etiqueta == FALSE, na.rm = TRUE)
  FN <- sum(df.negative$Etiqueta == TRUE,  na.rm = TRUE)
  
  TPR <- TP / (TP + FN)
  FPR <- FP / (FP + TN)
  return(c(TPR = TPR, FPR = FPR))
}

#' Curva ROC
#'
#' Calcula la curva ROC a partir de las puntuaciones de un clasificador binario.
#'
#' @param df Data frame con dos columnas: \code{Val} (puntuación numérica) y
#'   \code{Etiqueta} (lógica).
#' @return Data frame con dos columnas, \code{TPR} y \code{FPR}, que representa
#'   los puntos de la curva ROC ordenados por FPR creciente. Se garantiza que
#'   cada FPR aparece una sola vez, conservando el TPR máximo.
#' @details Se calculan las tasas TPR y FPR para cada punto de corte candidato
#'   (valores únicos de \code{Val}), se añaden los extremos \eqn{(0,0)} y
#'   \eqn{(1,1)}, se dejan los puntos más altos para los duplicados.
#' @examples
#' df <- data.frame(Val = c(0.9, 0.8, 0.4, 0.3),
#'                  Etiqueta = c(TRUE, TRUE, FALSE, FALSE))
#' roc(df)
#' @export
roc <- function(df){
  df.shorted <- df[order(df$Val), ]
  cv <- unique(df.shorted$Val)
  curve <- lapply(cv, rates, df = df.shorted, shorted = TRUE)
  df.curve <- as.data.frame(do.call(rbind, curve))
  
  extremos <- data.frame(TPR = c(0, 1), FPR = c(0, 1))
  df.curve <- rbind(df.curve, extremos)
  
  df.curve <- df.curve[order(df.curve$FPR, -df.curve$TPR), ]   # ordena por FPR (empate: TPR mayor primero)
  df.curve <- df.curve[!duplicated(df.curve$FPR), ]            # deja el TPR mas alto de cada FPR
  return(df.curve)
}

#' Área bajo la curva ROC (AUC)
#'
#' Calcula el área bajo la curva ROC mediante la regla del trapecio.
#'
#' @param curva Data frame con columnas \code{FPR} y \code{TPR}, como el
#'   devuelto por \code{\link{roc}}.
#' @return Valor numérico con el AUC.
#' @details Se aplica la regla del trapecio:
#'   \deqn{AUC = \sum_{i} \frac{(x_{i+1} - x_i)(y_{i+1} + y_i)}{2}}
#'   donde \eqn{x} es FPR e \eqn{y} es TPR.
#' @examples
#' df <- data.frame(Val = c(0.9, 0.8, 0.4, 0.3),
#'                  Etiqueta = c(TRUE, TRUE, FALSE, FALSE))
#' auc(roc(df))
#' @export
#' 
auc <- function(curva){
  x <- curva$FPR
  y <- curva$TPR
  sum(diff(x) * (y[1:(length(y) - 1)] + y[2:length(y)]) / 2, na.rm = TRUE)
}

#' AUC de una variable
#'
#' Calcula el AUC de una variable a partir de sus valores y su etiqueta binaria.
#'
#' @param x Vector numérico con las puntuaciones o valores de la variable.
#' @param etiqueta Vector lógico con la etiqueta (\code{TRUE} para positivo).
#' @return Valor numérico con el AUC.
#' @details Es una función de conveniencia que combina \code{\link{roc}} y
#'   \code{\link{auc}}.
#' @examples
#' auc.variable(c(0.9, 0.8, 0.4, 0.3), c(TRUE, TRUE, FALSE, FALSE))
#' @export
#' 
auc.variable <- function(x, etiqueta){
  auc(roc(data.frame(Val = x, Etiqueta = etiqueta)))
}

#' Información mutua
#'
#' Calcula la información mutua (en bits) entre dos variables categóricas.
#'
#' @param x Vector o factor con los valores de la primera variable.
#' @param y Vector o factor con los valores de la segunda variable.
#' @return Valor numérico con la información mutua.
#' @details Se calcula como
#'   \deqn{MI(X, Y) = \sum_{i} \sum_{j} p(x_i, y_j) \log_2 \frac{p(x_i, y_j)}{p(x_i)\, p(y_j)}}
#'   donde \eqn{p(x_i, y_j)} es la probabilidad conjunta y \eqn{p(x_i)},
#'   \eqn{p(y_j)} son las marginales. Los términos con probabilidad conjunta
#'   nula se ignoran.
#' @examples
#' x <- sample(letters[1:3], 100, replace = TRUE)
#' y <- sample(letters[4:6], 100, replace = TRUE)
#' mutual.info(x, y)
#' @export
mutual.info <- function(x, y){
  pxy <- table(x, y) / length(x)      # probabilidad conjunta
  px <- rowSums(pxy)
  py <- colSums(pxy)
  mi <- 0
  for (i in 1:nrow(pxy)){
    for (j in 1:ncol(pxy)){
      if (pxy[i, j] > 0) mi <- mi + pxy[i, j] * log2(pxy[i, j] / (px[i] * py[j]))
    }
  }
  return(mi)
}