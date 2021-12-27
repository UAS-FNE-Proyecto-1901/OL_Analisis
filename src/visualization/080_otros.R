
require(tidyverse)
theme_set(theme_bw())

require(glue)

source(file.path('src', 'visualization', '850_fun_visualization.R'), encoding = 'UTF-8')

blueMSPS <- "#3366CC"

df1 <-
  read_csv(file.path('data', 'processed', '001_datosProcesados.csv'),
           lazy = FALSE) %>% 
  mutate(
    Tipo = case_when(
      Tipo == 'IPS' ~ 'Instituciones Prestadoras de Salud',
      Tipo == 'OL' ~ 'Operadores logísticos',
      TRUE ~ NA_character_
    )
  )

ordenVar <- c('Sí', 'No', NA_character_)

conteoTipos <- df1 %>% 
  group_by(Tipo) %>% 
  count()

#'-------------------------------------------------------------------------------
# Comunicaciones ------------------
#'-------------------------------------------------------------------------------

#' 5.00. Desde su experiencia ¿qué opinión tiene respecto a la comunicación 
#' con el FNE?

col1 <- "5.00._Desde_su_experiencia_qué"

likertVar <- c("Muy conforme", 'Algo conforme', 
               "Ni conforme ni inconforme", "Algo inconforme", 'Muy inconforme')

gOpinionComunica <- select(df1, Tipo, frec = col1) %>% 
  count(Tipo, frec) %>%
  complete(Tipo, frec, fill = list(n = 0)) %>% 
  group_by(Tipo) %>%
  mutate(
    frec = factor(frec, rev(likertVar)),
    prop = prop.table(n),
    label1 = paste0(n, ', ', round(prop, 3) * 100, '%')) %>% 
  graficoFrecuenciasHoriz(n, frec, label1, expand = c(0,0,0.5,0)) +
  facet_wrap(vars(Tipo), scales = 'free_x') + 
  theme(axis.title.y = element_blank(), 
        panel.grid = element_blank())

guardarGGplot(gOpinionComunica, '110_opinionComunicaciones', 9, 5)


#' 5.03 ¿Le parece que la divulgación de las circulares, recomendaciones o 
#' demás cosas desde el FNE tiene un buen mecanismo de socialización? ------------------

col1 <- "5.03_Explicado"

gDivulgacionCirculares <- select(df1, Tipo, frec = col1) %>% 
  count(Tipo, frec) %>%
  group_by(Tipo) %>%
  mutate(
    frec = factor(frec, rev(ordenVar)),
    prop = prop.table(n),
    label1 = paste0(n, ', ', round(prop, 3) * 100, '%')
  ) %>% 
  graficoFrecuenciasHoriz(n, frec, label1, expand = c(0, 0, 0.5, 0)) +
  facet_wrap(vars(Tipo), scales = 'free_x') +
  theme(axis.title.y = element_blank(),
        panel.grid = element_blank())

guardarGGplot(gDivulgacionCirculares, '111_divulgacionCirculares', 6, 3)


#'-------------------------------------------------------------------------------
# Temas de Anexo 13 ------------------
#'-------------------------------------------------------------------------------
#' 4.83. ¿Cuánto tiempo se demora en llenar el informe?

tiempoVar <- c("0  a 2 horas", "2 a 4 horas", "1 a 2 días", "más de 2 días")

col1 <- "4.83._Cuánto_tiempo_se_demora_"

gTiempoA13 <- select(df1, Tipo, frec = col1) %>% 
  count(Tipo, frec) %>% 
  complete(Tipo, frec, fill = list(n = 0)) %>% 
  group_by(Tipo) %>% 
  mutate(prop = prop.table(n)) %>% 
  ungroup() %>% 
  mutate(
    frec = factor(frec, rev(tiempoVar)),
    label1 = paste0(n, ', ', round(prop, 3) * 100, '%')) %>% 
  graficoFrecuenciasHoriz(n, frec, label1, expand = c(0,0,0.6,0)) +
  facet_wrap(vars(Tipo), scales = 'free_x') + 
  theme(axis.title.y = element_blank(), 
        panel.grid = element_blank())

guardarGGplot(gTiempoA13, '120_tiempoDiligenciamientoA13', 6, 3)

#' 4.84. ¿En qué fecha enviar el Anexo 13?
col1 <- "4.84._En_qué_fecha_enviar_el_A"

tiempoVar1 <- c("1-3 día del mes", "4-6 día del mes", "7-8 día del mes", "8-10 día del mes")

gFechaA13 <- select(df1, Tipo, frec = col1) %>% 
  count(Tipo, frec) %>% 
  complete(Tipo, frec, fill = list(n = 0)) %>% 
  group_by(Tipo) %>% 
  mutate(prop = prop.table(n)) %>% 
  ungroup() %>% 
  mutate(
    frec = factor(frec, rev(tiempoVar1)),
    label1 = paste0(n, ', ', round(prop, 3) * 100, '%')) %>% 
  graficoFrecuenciasHoriz(n, frec, label1, expand = c(0,0,0.6,0)) +
  facet_wrap(vars(Tipo), scales = 'free_x') + 
  theme(axis.title.y = element_blank(), 
        panel.grid = element_blank())

guardarGGplot(gFechaA13, '121_fechaDiligenciamientoA13', 9, 5)

#' 
col1 <- "4.85_Ha_existido_meses_donde_e"

gMesesVar <- select(df1, Tipo, frec = col1) %>% 
  count(Tipo, frec) %>% 
  drop_na() %>% 
  complete(Tipo, frec, fill = list(n = 0)) %>% 
  group_by(Tipo) %>% 
  mutate(prop = prop.table(n)) %>% 
  ungroup() %>% 
  mutate(
    frec = factor(frec, rev(ordenVar)),
    label1 = paste0(n, ', ', round(prop, 3) * 100, '%')) %>% 
  graficoFrecuenciasHoriz(n, frec, label1, expand = c(0,0,0.6,0)) +
  facet_wrap(vars(Tipo), scales = 'free_x') + 
  theme(axis.title.y = element_blank(), 
        panel.grid = element_blank())

guardarGGplot(gMesesVar, '122_mesesAnexo', 6, 3)


#'-------------------------------------------------------------------------------
# Códigos de Barra ------------------
#'-------------------------------------------------------------------------------
#' 4.49 ¿Usted usa código de barras en los MME? ¿Qué pensaría si el FNE empieza
#'  a utilizarlos?

col1 <- "4.49_Explicado"

gCodigosBarra <- select(df1, Tipo, frec = col1) %>% 
  count(Tipo, frec) %>% 
  # drop_na() %>% 
  complete(Tipo, frec, fill = list(n = 0)) %>% 
  group_by(Tipo) %>% 
  mutate(prop = prop.table(n)) %>% 
  ungroup() %>% 
  mutate(
    frec = factor(frec, rev(ordenVar)),
    label1 = paste0(n, ', ', round(prop, 3) * 100, '%')) %>% 
  graficoFrecuenciasHoriz(n, frec, label1, expand = c(0,0,0.6,0)) +
  facet_wrap(vars(Tipo), scales = 'free_x') + 
  theme(axis.title.y = element_blank(), 
        panel.grid = element_blank())

guardarGGplot(gCodigosBarra, '130_codigosBarra', 6, 3)

#'-------------------------------------------------------------------------------
# Presencia de averías ------------------
#'-------------------------------------------------------------------------------
#' 4.48 ¿Han presentado perdidas, daños o averías con estos medicamentos? En el 
#' caso de ser afirmativo explicar ¿cómo se manejó?

col1 <- "4.48_Explicado"

gAverías <- select(df1, Tipo, frec = col1) %>% 
  count(Tipo, frec) %>% 
  # drop_na() %>% 
  complete(Tipo, frec, fill = list(n = 0)) %>% 
  group_by(Tipo) %>% 
  mutate(prop = prop.table(n)) %>% 
  ungroup() %>% 
  mutate(
    frec = factor(frec, rev(ordenVar)),
    label1 = paste0(n, ', ', round(prop, 3) * 100, '%')) %>% 
  graficoFrecuenciasHoriz(n, frec, label1, expand = c(0,0,0.6,0)) +
  facet_wrap(vars(Tipo), scales = 'free_x') + 
  theme(axis.title.y = element_blank(), 
        panel.grid = element_blank())

guardarGGplot(gAverías, '140_averias', 6, 3)
