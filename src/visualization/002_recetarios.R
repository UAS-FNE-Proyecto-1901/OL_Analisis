
require(tidyverse)
theme_set(theme_bw())

source(file.path('src', 'visualization', '850_fun_visualization.R'), encoding = 'UTF-8')

blueMSPS <- "#3366CC"

df1 <- read_csv(file.path('data', 'processed', '001_datosProcesados.csv'), 
                lazy = FALSE)


df1 <- df1 %>% 
  mutate(Tipo = case_when(
    Tipo == 'IPS' ~ 'Instituciones Prestadoras de Salud', 
    Tipo == 'OL' ~ 'Operadores logísticos', 
    TRUE ~ NA_character_
  ))

ordenVar <- c('Sí', 'No', NA_character_)

#'-------------------------------------------------------------------------------
# Recetario Oficial Electrónico ------------------
#'-------------------------------------------------------------------------------

col1 <- '3.01._Conoce_la_iniciativa_FNE'

gROE1 <- select(df1, frec = col1, Tipo) %>%
  count(Tipo, frec) %>%
  group_by(Tipo) %>% 
  mutate(
    frec = factor(frec, levels = ordenVar),
    prop = prop.table(n),
    label1 = paste0(n, ', ', round(prop, 3) * 100, '%')
  ) %>%
  graficoFrecuenciasHoriz(n, frec, label1, expand = c(0,0,0.8,0)) + 
  facet_wrap(vars(Tipo))

guardarGGplot(gROE1, '010_ROE_1', 8, 4)

col1 <- '3.02._Qué_tan_de_acuerdo_se_en'


likertVar <- c("Muy conforme", 'Algo conforme', 
               "Ni conforme ni inconforme", "Algo inconforme", 'Muy inconforme')

gROE2 <- select(df1, frec = col1, Tipo) %>%
  count(Tipo, frec) %>%
  add_row(Tipo = 'Operadores logísticos',
          frec = c('Algo conforme', 'Muy inconforme')) %>% 
  complete(Tipo, frec, fill=list(n = 0)) %>% 
  group_by(Tipo) %>% 
  mutate(
    frec = factor(frec, levels = rev(likertVar)),
    prop = prop.table(n),
    label1 = paste0(n, ', ', round(prop, 3) * 100, '%')
  ) %>%
  graficoFrecuenciasHoriz(n, frec, label1, expand = c(0,0,0.5,0)) + 
  facet_wrap(vars(Tipo), scales = 'free_x')

guardarGGplot(gROE2, '011_ROE_2', 8, 4)

col2 <- '3.03._Justifique_la_respuesta'

gtROE1 <- select(df1, frec=col1, justif = col2) %>% 
  gt::gt(groupname_col = 'frec') %>% 
  gt::tab_options(table.font.size = 9) %>% 
  gt::cols_label(
    justif = 'Justificaciones') %>% 
  gt::opt_row_striping(row_striping = TRUE)

gt::gtsave(gtROE1, '001_tablaJustificaciones.html', 'references')

#'-------------------------------------------------------------------------------
# 3.04 ¿Qué opinión tiene respecto al Mipres, como ha sido su interacción y 
# experiencia con esta herramienta? ------------------
#'-------------------------------------------------------------------------------

# Sólo se tienen en cuenta IPS

col1 <- "3.04_Explicado"
col2 <- "3.04_Qué_opinión_tiene_respect"

likertVar1 <- c('Positiva', 'Neutral', 'Negativa')

gROE3 <- select(df1, frec = col1, Tipo) %>%
  filter(Tipo != 'Operadores logísticos') %>%
  count(frec) %>%
  mutate(
    frec = factor(frec, levels = rev(likertVar1)),
    prop = prop.table(n),
    label1 = paste0(n, ', ', round(prop, 3) * 100, '%')
  ) %>%
  graficoFrecuenciasHoriz(n, frec, label1, expand = c(0,0,0.8,0))

guardarGGplot(gROE3, '012_ROE_3', 6, 4)

gtROE3 <- select(df1, frec=col1, justif = col2) %>% 
  gt::gt(groupname_col = 'frec') %>% 
  gt::tab_options(table.font.size = 9) %>% 
  gt::cols_label(
    justif = 'Justificaciones') %>% 
  gt::opt_row_striping(row_striping = TRUE)

gt::gtsave(gtROE3, '002_tablaJustificacionesMIPRES.html', 'references')

#'-------------------------------------------------------------------------------
# 3.05 ¿Usted qué opina de que el ROE se maneje en la 
# herramienta MIPRES?  ------------------
#'-------------------------------------------------------------------------------

col1 <- "3.05_Explicado"
col2 <- "3.05_Usted_qué_opina_de_que_el"

gROE4 <- select(df1, frec = col1, Tipo) %>%
  filter(Tipo != 'Operadores logísticos') %>%
  count(frec) %>%
  mutate(
    frec = factor(frec, levels = rev(likertVar1)),
    prop = prop.table(n),
    label1 = paste0(n, ', ', round(prop, 3) * 100, '%')
  ) %>%
  graficoFrecuenciasHoriz(n, frec, label1, expand = c(0,0,0.8,0))

guardarGGplot(gROE4, '013_ROE_4', 6, 4)

gtROE4 <- select(df1, frec=col1, justif = col2) %>% 
  gt::gt(groupname_col = 'frec') %>% 
  gt::tab_options(table.font.size = 9) %>% 
  gt::cols_label(
    justif = 'Justificaciones') %>% 
  gt::opt_row_striping(row_striping = TRUE)

gt::gtsave(gtROE4, '003_tablaJustificacionesROE.html', 'references')

