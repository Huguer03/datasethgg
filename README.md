# datasethgg (R)

Versión en R del paquete `datasethgg`: manejo y preprocesado de datasets con clases propias (como un objeto S4 `dataset`). Las implementaciones de discretización, entropía, información mutua, curva ROC y AUC son propias; para los gráficos se usan los paquetes `ggplot2` y `reshape2`.

## Instalación

La forma recomendada de instalar la versión en desarrollo desde GitHub es utilizando el paquete `devtools`:

```r
# Instala devtools si aún no lo tienes
install.packages("devtools")

# Instala el paquete desde GitHub
devtools::install_github("Huguer03/datasethgg")
```

## Documentación

Cada función está documentada con su correspondiente archivo de ayuda (generado a partir de roxygen2). Puedes consultar la documentación de cualquier función directamente en la consola de R:

```r
# Abre la página de ayuda de una función
?graf.roc
# o
help("graf.roc")
```

También puedes ver la lista completa de funciones y objetos exportados por el paquete:

```r
# Carga el paquete para acceder a su documentación
library(datasethgg)
# Muestra el índice de ayuda del paquete
help(package = "datasethgg")
```

## Autor

Hugo González González

---

**Nota sobre dependencias:** Asegúrate de tener instalados los paquetes de los que depende `datasethgg` para que todas las funciones (especialmente las de gráficos) funcionen correctamente. Puedes instalarlos con:

```r
install.packages(c("ggplot2", "reshape2"))
```