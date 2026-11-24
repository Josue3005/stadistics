# Análisis Estadístico de Conectividad y Plataformas Educativas en Estudiantes

Este repositorio contiene un script en **R** desarrollado con `tidyverse` para el procesamiento, análisis descriptivo y visualización de datos sobre el gasto en conectividad, frecuencia de demoras, tiempos de carga y horarios de saturación reportados por estudiantes ($n = 88$).

## Contenido del Análisis

El script está organizado en cuatro bloques principales:

1. **Gasto mensual en datos y recargas ($S/$):**
   - Cálculo de medidas de tendencia central y dispersión (media, mediana, moda y desviación estándar).
   - Distribución por intervalos de gasto mediante gráfico de barras.

2. **Frecuencia de demoras en plataformas educativas:**
   - Construcción de tabla de frecuencias ($f_i$, $h_i$, porcentaje, $F_i$, $H_i$).
   - Visualización ordenada de la percepción de demoras.

3. **Tiempo estimado de carga de páginas educativas pesadas:**
   - Limpieza y estandarización de respuestas textuales a segundos ($n = 79$ respuestas numéricas válidas).
   - Resumen estadístico, diagrama de caja (*boxplot*), curva de densidad y curva acumulada (ojiva).

4. **Horario con mayor lentitud percibida:**
   - Tabla de frecuencias y gráfico comparativo por momento del día.

## Requisitos

Para ejecutar el código necesitas tener instalado **R** (recomendado RStudio) y el paquete `tidyverse`:

```r
install.packages("tidyverse")
```

## Estructura del Repositorio

* `analisis_estudiantes.R` — Script principal con el código de limpieza, cálculo de estadísticos y generación de gráficos con `ggplot2`.
* `README.md` — Documentación general del proyecto.
