# Funciones propias del trabajo (gráficos)
# Requiere: dplyr, ggplot2, ggrepel y TDecisionCodigo cargados.

# --- Gráficos propios ------------------------------------------------------

# Dispersión coste total vs. tiempo de viaje (tamaño = cartel, color = logística)
grafico_coste_tiempo = function(df) {
  ggplot(df, aes(x = tiempo_ida_min, y = coste_total)) +
    geom_point(aes(size = cartel, colour = logistica), alpha = 0.85) +
    ggrepel::geom_text_repel(aes(label = id), size = 3.6, seed = 1) +
    scale_colour_viridis_c(option = "plasma", end = 0.85) +
    scale_size_continuous(range = c(4, 12)) +
    labs(title = "Coste total frente a tiempo de viaje",
         subtitle = "Tamaño = valoración del cartel · color = valoración de la logística",
         x = "Tiempo de viaje de ida (min)", y = "Coste total por persona (€)",
         colour = "Logística", size = "Cartel") +
    theme_minimal()
}

# Mapa de calor de la matriz de decisión homogeneizada (0 = peor, 1 = mejor)
grafico_heatmap = function(mhomog) {
  d = as.data.frame(as.table(mhomog))
  names(d) = c("Alternativa", "Criterio", "Valor")
  ggplot(d, aes(x = Criterio, y = Alternativa, fill = Valor)) +
    geom_tile(colour = "white") +
    geom_text(aes(label = sprintf("%.2f", Valor)), size = 3.4) +
    scale_fill_gradient(low = "#fde0dd", high = "#31a354", limits = c(0, 1)) +
    labs(title = "Matriz de decisión homogeneizada (Nadir)",
         subtitle = "0 = peor valor del criterio, 1 = mejor valor", fill = NULL) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

# Solo se define si TDecisionCodigo no trae ya plot_promethee_I.
# Gráfico de PROMETHEE I: flujos Phi+ y Phi- (ambos entre 0 y 1; Phi- invertido, mayor es mejor)
if (!exists("plot_promethee_I")) plot_promethee_I = function(phi_mas, phi_menos) {
  esc = function(x) (x - min(x)) / (max(x) - min(x))
  d = data.frame(Alternativa = rep(names(phi_mas), 2),
                 Flujo = rep(c("Φ+", "Φ- (invertido)"), each = length(phi_mas)),
                 Valor = c(esc(phi_mas), 1 - esc(phi_menos)))
  d$Flujo = factor(d$Flujo, levels = c("Φ+", "Φ- (invertido)"))
  ggplot(d, aes(x = Flujo, y = Valor, group = Alternativa, colour = Alternativa)) +
    geom_line(linewidth = 1) + geom_point(size = 2) +
    labs(title = "PROMETHEE I: Φ+ frente a Φ-", x = NULL, y = "Valor (0 a 1)") +
    theme_minimal()
}
