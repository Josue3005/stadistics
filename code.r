library(tidyverse)

gasto_est <- datos_est |>
  filter(!is.na(gasto_mensual), gasto_mensual >= 0, gasto_mensual < 500) |>
  pull(gasto_mensual)

moda_gasto <- as.numeric(names(sort(table(gasto_est), decreasing = TRUE))[1])

tibble(
  n = length(gasto_est),
  media = round(mean(gasto_est), 2),
  mediana = round(median(gasto_est), 2),
  moda = round(moda_gasto, 2),
  sd = round(sd(gasto_est), 2)
)

datos_est |>
  filter(!is.na(gasto_mensual), gasto_mensual >= 5, gasto_mensual <= 125) |>
  mutate(
    intervalo = cut(
      gasto_mensual,
      breaks = c(5, 20, 35, 50, 65, 80, 95, 110, 125.001),
      labels = c("[5-20)", "[20-35)", "[35-50)", "[50-65)", "[65-80)", "[80-95)", "[95-110)", "[110-125]"),
      right = FALSE,
      include.lowest = TRUE
    )
  ) |>
  count(intervalo, .drop = FALSE) |>
  ggplot(aes(x = intervalo, y = n)) +
  geom_col(fill = "#2E6B9E") +
  geom_text(aes(label = n), vjust = -0.4, size = 3.6) +
  expand_limits(y = 30) +
  labs(
    title = "Gasto mensual en datos / recargas (S/) — Estudiantes",
    subtitle = "n = 86 respuestas válidas",
    x = "Intervalos de gasto (S/)",
    y = "Frecuencia absoluta (fi)",
    caption = "Nota. Elaboración propia a partir de datos de estudiantes (n = 86; 2 sin respuesta)."
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0, size = 9, color = "gray30", margin = margin(t = 8))
  )

ggplot(df_gasto_plot, aes(x = intervalo, y = n)) +
  geom_col(fill = "#2E6B9E") +
  geom_text(aes(label = n), vjust = -0.4, size = 3.6) +
  expand_limits(y = 30) +
  labs(
    title = "Gasto mensual en datos / recargas (S/) — Estudiantes",
    subtitle = paste0("n = ", n_gasto_valido, " respuestas válidas (de 88 estudiantes)"),
    x = "Intervalos de gasto (S/)",
    y = "Frecuencia absoluta (fi)",
    caption = "Nota. Elaboración propia a partir de datos de estudiantes (n = 86; 2 sin respuesta)."
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0, size = 9, color = "gray30", margin = margin(t = 8))
  )

orden_demoras <- c(
  "Casi nunca",
  "Rara vez",
  "A veces",
  "Frecuentemente",
  "Casi siempre/ Es insoportable"
)

tabla_demoras <- datos_est |>
  filter(!is.na(frecuencia_demoras)) |>
  mutate(frecuencia_demoras = factor(frecuencia_demoras, levels = orden_demoras)) |>
  count(frecuencia_demoras, .drop = FALSE, name = "fi") |>
  mutate(
    hi = round(fi / sum(fi), 4),
    hi_pct = round(hi * 100, 2),
    Fi = cumsum(fi),
    Hi = round(Fi / sum(fi), 4)
  )

print(tabla_demoras)

tabla_demoras |>
  arrange(fi) |>
  mutate(
    categoria = fct_reorder(as.character(frecuencia_demoras), fi, .desc = FALSE)
  ) |>
  ggplot(aes(x = categoria, y = fi)) +
  geom_col(fill = "#2E6B9E") +
  geom_text(aes(label = paste0(fi, " (", hi_pct, "%)")), vjust = -0.4, size = 3.5) +
  expand_limits(y = 42) +
  labs(
    title = "Frecuencia de demoras en plataformas educativas — Estudiantes",
    subtitle = "n = 88 (orden ascendente por frecuencia)",
    x = "Nivel de frecuencia",
    y = "Frecuencia absoluta (fi)",
    caption = "Nota. Elaboración propia a partir de datos de estudiantes (n = 88)."
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 15, hjust = 1),
    plot.caption = element_text(hjust = 0, size = 9, color = "gray30")
  )

datos_tiempo <- datos_est |>
  mutate(
    t_txt = str_to_lower(str_squish(.data[[names(datos_raw)[11]]])),
    es_ambiguo = is.na(t_txt) | str_detect(t_txt, "depende|menos de un minuto") | !str_detect(t_txt, "\\d"),
    nums = map(t_txt, ~ as.numeric(str_extract_all(.x, "\\d+\\.?\\d*")[[1]])),
    val_base = map_dbl(nums, ~ if (length(.x) == 0) NA_real_ else mean(.x)),
    tiempo_seg = case_when(
      es_ambiguo ~ NA_real_,
      str_detect(t_txt, "min") ~ val_base * 60,
      TRUE ~ val_base
    )
  ) |>
  filter(!is.na(tiempo_seg))

datos_tiempo |>
  summarise(
    n = n(),
    media = round(mean(tiempo_seg), 1),
    mediana = round(median(tiempo_seg), 1),
    sd = round(sd(tiempo_seg), 1),
    pct_menor_61 = round(mean(tiempo_seg < 61) * 100, 1)
  )

ggplot(datos_tiempo, aes(y = tiempo_seg)) +
  geom_boxplot(fill = "#A9C4E2", outlier.color = "#B22222") +
  labs(
    title = "Tiempo estimado de carga de página educativa pesada (segundos)",
    subtitle = paste0("n = ", nrow(datos_tiempo), " respuestas numéricas válidas (de 88 estudiantes)"),
    y = "Segundos",
    caption = "Nota. Elaboración propia (n = 79; se excluyeron 2 sin respuesta y 7 respuestas cualitativas)."
  ) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0, size = 9, color = "gray30"))

ggplot(datos_tiempo, aes(x = tiempo_seg)) +
  geom_density(fill = "#4A7EBB", color = "#1F4E78", alpha = 0.45, linewidth = 1) +
  geom_vline(aes(xintercept = median(tiempo_seg)), color = "#B22222", linetype = "dashed", linewidth = 0.9) +
  annotate(
    "text",
    x = median(datos_tiempo$tiempo_seg) + 25,
    y = Inf,
    vjust = 2,
    hjust = 0,
    label = paste0("Mediana = ", median(datos_tiempo$tiempo_seg), " s"),
    color = "#B22222",
    size = 3.6
  ) +
  labs(
    title = "Curva de densidad: Tiempo de carga de página educativa pesada",
    subtitle = paste0("n = ", nrow(datos_tiempo), " respuestas numéricas válidas (de 88 estudiantes)"),
    x = "Tiempo estimado de carga (segundos)",
    y = "Densidad"
  ) +
  theme_minimal()

ggplot(datos_tiempo, aes(x = tiempo_seg)) +
  stat_ecdf(geom = "step", color = "#1F4E78", linewidth = 1.1) +
  geom_vline(xintercept = 60, linetype = "dotted", color = "#B22222", linewidth = 0.9) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Curva acumulada (Ojiva): Tiempo estimado de carga",
    subtitle = "El 84,8 % de los estudiantes estima un tiempo menor o igual a 60 segundos (n = 79)",
    x = "Tiempo estimado de carga (segundos)",
    y = "Porcentaje acumulado de estudiantes"
  ) +
  theme_minimal()

tabla_horario <- datos_est |>
  filter(!is.na(horario_lentitud)) |>
  mutate(
    momento = recode(
      horario_lentitud,
      "No noto diferencias / Funciona igual todo el día" = "Sin diferencias en el día"
    )
  ) |>
  count(momento, name = "fi") |>
  arrange(desc(fi)) |>
  mutate(
    hi = round(fi / sum(fi), 4),
    hi_pct = round(hi * 100, 2)
  )

print(tabla_horario)

tabla_horario |>
  arrange(fi) |>
  mutate(momento_ord = fct_reorder(momento, fi, .desc = FALSE)) |>
  ggplot(aes(x = momento_ord, y = fi)) +
  geom_col(fill = "#1F4E78") +
  geom_text(aes(label = paste0(fi, " (", hi_pct, "%)")), vjust = -0.4, size = 3.5) +
  expand_limits(y = 46) +
  labs(
    title = "Momento del día con mayor lentitud percibida — Estudiantes",
    subtitle = "n = 88 (orden ascendente por frecuencia)",
    x = "Momento del día",
    y = "Frecuencia absoluta (fi)",
    caption = "Nota. Elaboración propia a partir de datos de estudiantes (n = 88)."
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 12, hjust = 1),
    plot.caption = element_text(hjust = 0, size = 9, color = "gray30")
  )