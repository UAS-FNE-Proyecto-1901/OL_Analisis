blueMSPS <- "#3366CC"

graficoFrecuenciasHoriz <- function(data,
                                    xvar,
                                    yvar,
                                    label,
                                    fill = blueMSPS,
                                    fill_alpha = 0.8,
                                    expand = c(0.0, 0.0, 0.5, 0.0), 
                                    xlab = 'Frecuencia, proporción',
                                    ylab = 'Valor') {
  
  g1 <- data %>%
    ggplot(aes(y = {{yvar}}, x = {{xvar}})) +
    geom_bar(
      stat = 'identity', 
      fill = fill,
      color = 'black', 
      alpha = fill_alpha) + 
    scale_x_continuous(expand = expand) + 
    ylab(ylab) + 
    xlab(xlab)
  
    # labs(title = "Mención de manejo de medicamentos de control especial en contratos")
  
  if (!missing(label)) {
    g1 <- g1 + geom_text(aes(label = {{label}}), hjust = -0.3) 
  }
  
  return(g1)
}

