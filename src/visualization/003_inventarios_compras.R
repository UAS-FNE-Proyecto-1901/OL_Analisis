
require(tidyverse)
theme_set(theme_bw())

require(ggwordcloud)

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

conteoTipos <- df1 %>% 
  group_by(Tipo) %>% 
  count()

#'-------------------------------------------------------------------------------
# Herramientas para el manejo de inventario ------------------
#'-------------------------------------------------------------------------------

col1 <- "Si_la_respuesta_anterior_fue_p...34"

#' Sólo en 3 IPS se menciona que se hace el registro de inventario de forma manual
#' mientras que ningún OL menciona que todo se hace con un sistema

gWordCloud1 <- select(df1, frec = col1, Tipo) %>%
  count(Tipo, frec) %>% 
  ggplot(aes(label = frec, size = n, color = frec)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 7) +
  facet_wrap(vars(Tipo))
# graficoFrecuenciasHoriz(n, frec, label1, expand = c(0,0,0.8,0)) + 

guardarGGplot(gWordCloud1, '020_wordcloud1', 6, 3)

#'-------------------------------------------------------------------------------
# 4.02. ¿Qué medios utiliza de manera frecuente para la 
# comunicación con el FNE? ------------------
#'-------------------------------------------------------------------------------

col1 <- '4.02._Qué_medios_utiliza_de_ma'

medioFNE <- c('Atención presencial en el FRE', "Teléfono", "Fax", 
  "Correo electrónico", "Plataforma virtual","Correspondencia")

df_res1 <- select(df1, col1) %>% 
  separarDummies(descartar = T) %>% 
  {bind_cols(df1, .)} %>% 
  pivot_longer(cols = c('Telefono', 'Correo electrónico')) %>%
  group_by(Tipo, name) %>% 
  summarise(value = sum(value)) %>%
  ungroup() %>% 
  add_row(Tipo = 'Operadores logísticos', name = medioFNE) %>% 
  filter(name != 'Correo electrónico' | !is.na(value)) %>% 
  filter(name != 'Teléfono') %>% 
  mutate(name = str_replace(name, 'Telefono', 'Teléfono')) %>% 
  complete(Tipo, name, fill = list(value = 0)) %>%
  left_join(conteoTipos, by = 'Tipo') %>% 
  mutate(
    prop = value/n, 
    label1 = paste0(value, ' / ', n)
  )

gComunicaciones1 <- df_res1 %>% 
  graficoFrecuenciasHoriz(prop, name, label1) +
  facet_wrap(vars(Tipo)) + 
  scale_x_continuous(
    breaks = seq(0,1,0.2),
    labels = scales::percent_format(), 
    expand = c(0,0,0.4,0)
  ) + 
  theme(axis.title.y = element_blank(), 
        panel.grid = element_blank())

guardarGGplot(gComunicaciones1, '030_comunicación', 8, 4)

#'-------------------------------------------------------------------------------
# 4.12. ¿Qué herramienta utiliza para realizar la estimación 
# de compra de MME? ------------------
#'-------------------------------------------------------------------------------

col1 <- "4.12._Qué_herramienta_utiliza_"

df_res1 <- select(df1, col1) %>% 
  separarDummies(descartar = T) %>% 
  {bind_cols(select(df1, ID, Tipo), .)} %>% 
  pivot_longer(cols = !matches('ID|Tipo')) %>%
  group_by(Tipo, name) %>% 
  summarise(value = sum(value)) %>%
  ungroup() %>% 
  complete(Tipo, name, fill = list(value = 0)) %>% 
  left_join(conteoTipos, by = 'Tipo') %>% 
  mutate(
    prop = value/n, 
    label1 = paste0(value, ' / ', n)
  )

gHerramientaEstimacion <- df_res1 %>% 
  graficoFrecuenciasHoriz(prop, name, label1) +
  facet_wrap(vars(Tipo)) + 
  scale_x_continuous(
    breaks = seq(0,1,0.2),
    labels = scales::percent_format(), 
    expand = c(0,0,0.4,0)
  ) + 
  theme(axis.title.y = element_blank(), 
        panel.grid = element_blank())

guardarGGplot(gHerramientaEstimacion, '040_herramientaEstimacion', 8, 4)

col2 <- 'Si_la_respuesta_anterior_fue_p...39'

gWordCloud2 <- select(df1, col2) %>% 
  separarDummies(descartar = TRUE) %>% 
  {bind_cols(select(df1, ID, Tipo), .)} %>% 
  pivot_longer(cols = !matches('ID|Tipo')) %>% 
  group_by(Tipo, name) %>% 
  summarise(value = sum(value)) %>%
  ungroup() %>% 
  filter(value != 0) %>% 
  ggplot(aes(label = name, size = value, color = name)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 11) +
  facet_wrap(vars(Tipo))

guardarGGplot(gWordCloud2, '041_wordcloud2', 6, 3)


#'-------------------------------------------------------------------------------
# 4.13. ¿Cuánto tiempo toma la etapa de estimación de la necesidad 
# de compra de MME (en días)? ------------------
#'-------------------------------------------------------------------------------
#'
col1 <- "4.13_Explicado"

ordenVar1 <- c('Menos de un día', 'Un día', 'Dos días', 'Cinco días', 'Una semana')

gTiempoEstimacion <- select(df1, frec = col1, Tipo) %>% 
  count(Tipo, frec) %>%
  group_by(Tipo) %>% 
  mutate(
    frec = factor(frec, levels = rev(ordenVar1)),
    prop = prop.table(n),
    label1 = paste0(n, ', ', round(prop, 3) * 100, '%')
  ) %>%
  graficoFrecuenciasHoriz(n, frec, label1, expand = c(0,0,0.8,0)) + 
  facet_wrap(vars(Tipo), scales = 'free_x') + 
  theme(axis.title.y = element_blank(), 
        panel.grid = element_blank())

guardarGGplot(gTiempoEstimacion, '050_tiempoEstimacion', 8, 5)


#'-------------------------------------------------------------------------------
# 4.14. ¿Cuánto tiempo toma la solicitud en plataforma
# tecnológica? (en días) ------------------
#'-------------------------------------------------------------------------------

col1 <- "4.14._Cuánto_tiempo_toma_la_so"

ordenVar2 <- c(
  'Un día o menos',
  'Entre 1 y dos días',
  'Entre dos días y una semana',
  'Entre una y dos semanas',
  'Entre dos semanas y un mes'
)


df_res3 <- select(df1, frec = col1, Tipo) %>% 
  drop_na() %>% 
  mutate(
    frec1 = cut(frec, breaks = c(0, 1, 2, 7, 15, 30)),
    frec1 = case_when(
      frec1 == '(0,1]' ~   ordenVar2[1],
      frec1 == '(1,2]' ~   ordenVar2[2],
      frec1 == '(2,7]' ~   ordenVar2[3],
      frec1 == '(7,15]' ~  ordenVar2[4],
      frec1 == '(15,30]' ~ ordenVar2[5]
    )
  ) %>%
  count(Tipo, frec1) %>% 
  complete(Tipo, frec1, fill = list(n = 0)) %>% 
  mutate(prop = prop.table(n),
         label1 = paste0(n, ', ', round(prop, 3) * 100, '%'))

gSolicitud <- df_res3 %>%
  mutate(frec1 = factor(frec1, rev(ordenVar2))) %>%
  graficoFrecuenciasHoriz(n, frec1, label1, expand = c(0, 0, 0.8, 0)) +
  facet_wrap(vars(Tipo), scales = 'free_x') +
  theme(axis.title.y = element_blank(),
        panel.grid = element_blank())

guardarGGplot(gSolicitud, '051_solicitudMME', 8, 4)

#'-------------------------------------------------------------------------------
# 4.15. ¿Cuánto tiempo toma el despacho de los MME? (en días) ------------------
#'-------------------------------------------------------------------------------

col1 <- "4.15._Cuánto_tiempo_toma_el_de"

df_res4 <- select(df1, frec = col1, Tipo) %>%
  drop_na() %>%
  mutate(
    frec1 = cut(frec, breaks = c(0, 1, 2, 7, 15, 30)),
    frec1 = case_when(
      frec1 == '(0,1]' ~   ordenVar2[1],
      frec1 == '(1,2]' ~   ordenVar2[2],
      frec1 == '(2,7]' ~   ordenVar2[3],
      frec1 == '(7,15]' ~  ordenVar2[4],
      frec1 == '(15,30]' ~ ordenVar2[5]
    )
  ) %>% 
  count(Tipo, frec1) %>% 
  complete(Tipo, frec1, fill = list(n = 0)) %>% 
  mutate(prop = prop.table(n),
         label1 = paste0(n, ', ', round(prop, 3) * 100, '%'))


gDespachoMME <- df_res4 %>%
  mutate(frec1 = factor(frec1, rev(ordenVar2))) %>%
  graficoFrecuenciasHoriz(n, frec1, label1, expand = c(0, 0, 0.8, 0)) +
  facet_wrap(vars(Tipo), scales = 'free_x') +
  theme(axis.title.y = element_blank(),
        panel.grid = element_blank())

guardarGGplot(gDespachoMME, '052_despachoMME', 8, 4)

#'-------------------------------------------------------------------------------
# 4.17. ¿En qué meses suelen realizarse las compras de MME? ------------------
#'-------------------------------------------------------------------------------

col1 <- "4.17._Explicado"

ordenVar3 <- c("Cada semana", "Mensualmente", "Cada dos meses",
               "Cada tres meses", "Cada cuatro meses", "Cada seis meses",
               "Una vez al año")

gMesesCompras <- select(df1, frec = col1, Tipo) %>% 
  count(Tipo, frec) %>%
  group_by(Tipo) %>% 
  complete(Tipo, frec, fill = list(n = 0)) %>% 
  mutate(
    frec = factor(frec, levels = rev(ordenVar3)),
    prop = prop.table(n),
    label1 = paste0(n, ', ', round(prop, 3) * 100, '%')
  ) %>%
  graficoFrecuenciasHoriz(n, frec, label1, expand = c(0,0,0.8,0)) + 
  facet_wrap(vars(Tipo), scales = 'free_x') + 
  theme(axis.title.y = element_blank(), 
        panel.grid = element_blank())

guardarGGplot(gMesesCompras, '053_mesesCompras', 8, 4)


#'-------------------------------------------------------------------------------
# 4.18. La cantidad solicitada al FNE es cubierta por el mismo (¿se venden todos 
# los productos en las cantidades solicitadas?). Especificar si se pide 
# medicamentos por lapsos largos de tiempo. ------------------
#'-------------------------------------------------------------------------------

col1 <- "4.18_Explicada"

gCantidadSolicitda <- select(df1, frec = col1, Tipo) %>% 
  count(Tipo, frec) %>%
  group_by(Tipo) %>% 
  mutate(
    # frec = factor(frec, levels = rev(ordenVar3)),
    prop = prop.table(n),
    label1 = paste0(n, ', ', round(prop, 3) * 100, '%')
  ) %>%
  graficoFrecuenciasHoriz(n, frec, label1, expand = c(0,0,0.8,0)) + 
  facet_wrap(vars(Tipo), scales = 'free_x') + 
  theme(axis.title.y = element_blank(), 
        panel.grid = element_blank())


guardarGGplot(gCantidadSolicitda, '060_cantidadesSolicitadas', 8, 4)


df_res_5 <- select(df1, frec = col1) %>% 
  count(frec)

col2 <- "4.18_Medicamentos"

nEncuestados <- df_res_5[df_res_5$frec== 'No', ]$n

df_res5 <- select(df1, col1 = col1, col2 = col2) %>%
  filter(col1 == 'No') %>% 
  select(!col1) %>% 
  separarDummies(delimitador = "; ") %>% 
  pivot_longer(cols = everything()) %>% 
  group_by(name) %>% 
  summarise(val = sum(value)) %>% 
  mutate(prop = val/nEncuestados)


gProblemasAbastecimiento <- df_res5 %>%
  mutate(name = fct_reorder(name, prop),
         label1 = paste0(val, ', ', round(prop, 3) * 100, '%')) %>% 
  graficoFrecuenciasHoriz(prop, name, label1) +
  scale_x_continuous(
    breaks = seq(0,1,0.2),
    labels = scales::percent_format(), 
    expand = c(0,0,0.4,0)
  ) + 
  theme(axis.title.y = element_blank(), 
        panel.grid = element_blank())


guardarGGplot(gProblemasAbastecimiento, '061_medicamentosProblemasAbast', 6, 4)

#'-------------------------------------------------------------------------------
# 4.19. ¿Qué percepción tiene del proceso de compra de MME con el 
# FNE? ------------------
#'-------------------------------------------------------------------------------

col1 <- "4.19._Qué_percepción_tiene_pro"

likertVar <- c("Muy conforme", 'Algo conforme', 
               "Ni conforme ni inconforme", "Algo inconforme", 'Muy inconforme')

gPercepcion <- select(df1, frec = col1, Tipo) %>% 
  count(Tipo, frec) %>% 
  complete(Tipo, frec, fill = list(n = 0)) %>% 
  mutate(
    frec = factor(frec, rev(likertVar)),
    prop = prop.table(n),
    label1 = paste0(n, ', ', round(prop, 3) * 100, '%')) %>% 
  
  graficoFrecuenciasHoriz(n, frec, label1, expand = c(0,0,0.5,0)) +
  facet_wrap(vars(Tipo), scales = 'free_x') + 
  theme(axis.title.y = element_blank(), 
        panel.grid = element_blank())

guardarGGplot(gPercepcion, '062_medicamentosProblemasAbast', 9, 5)

#'-------------------------------------------------------------------------------
# 4.20. ¿Qué tan conforme se encuentra con la plataforma de pago de los MME 
# del FNE? ------------------
#'-------------------------------------------------------------------------------

col1 <- "4.20._Qué_tan_conforme_se_encu"

gConformidad1 <- select(df1, frec = col1, Tipo) %>% 
  count(Tipo, frec) %>% 
  add_row(Tipo = 'Operadores logísticos', frec = 'Muy inconforme') %>% 
  complete(Tipo, frec, fill = list(n = 0)) %>% 
  group_by(Tipo) %>% 
  mutate(prop = prop.table(n)) %>% 
  ungroup() %>% 
  mutate(
    frec = factor(frec, rev(likertVar)),
    label1 = paste0(n, ', ', round(prop, 3) * 100, '%')) %>% 
  graficoFrecuenciasHoriz(n, frec, label1, expand = c(0,0,0.5,0)) +
  facet_wrap(vars(Tipo), scales = 'free_x') + 
  theme(axis.title.y = element_blank(), 
        panel.grid = element_blank())

guardarGGplot(gConformidad1, '063_conformidad', 9, 5)

#'-------------------------------------------------------------------------------
# 4.23. ¿Se han presentado no conformidades o averías en los productos MME
# recibidos por parte del FNE? En el caso de presentarse, ¿Qué acción ha 
# realizado ------------------
#'-------------------------------------------------------------------------------

col1 <- "4.23_Explicado"

#' En este caso no se presentaron diferencias 

gAverias <- select(df1, frec = col1, Tipo) %>% 
  count(Tipo, frec) %>%
  group_by(Tipo) %>% 
  mutate(
    frec = factor(frec, rev(ordenVar)),
    prop = prop.table(n),
    label1 = paste0(n, ', ', round(prop, 3) * 100, '%')) %>% 
  graficoFrecuenciasHoriz(n, frec, label1, expand = c(0,0,0.5,0)) +
  facet_wrap(vars(Tipo), scales = 'free_x') + 
  theme(axis.title.y = element_blank(), 
        panel.grid = element_blank())

guardarGGplot(gAverias, '064_averias', 8, 4)

#'-------------------------------------------------------------------------------
# 4.24. Lista de procedimientos que tengan relación con MME. 
# Lista ------------------
#'-------------------------------------------------------------------------------

col1 <- "4.24_POE_MCE"

gProcedimientos <- select(df1, frec = col1, Tipo) %>%
  count(Tipo, frec) %>%
  group_by(Tipo) %>% 
  mutate(
    frec = factor(frec, rev(ordenVar)),
    prop = prop.table(n),
    label1 = paste0(n, ', ', round(prop, 3) * 100, '%')) %>% 
  graficoFrecuenciasHoriz(n, frec, label1, expand = c(0,0,0.5,0)) +
  facet_wrap(vars(Tipo), scales = 'free_x') + 
  theme(axis.title.y = element_blank(), 
        panel.grid = element_blank())

guardarGGplot(gProcedimientos, '065_procedimientos', 8, 4)

#'-------------------------------------------------------------------------------
# 4.26 Consideraría que el transporte estuviera a cargo del FNE con un costo
# adicional. ------------------
#'-------------------------------------------------------------------------------

col1 <- "4.26_Explicado"

gTransporteCost <- select(df1, frec = col1, Tipo) %>%
  count(Tipo, frec) %>%
  group_by(Tipo) %>% 
  mutate(
    frec = factor(frec, rev(ordenVar)),
    prop = prop.table(n),
    label1 = paste0(n, ', ', round(prop, 3) * 100, '%')) %>% 
  graficoFrecuenciasHoriz(n, frec, label1, expand = c(0,0,0.5,0)) +
  facet_wrap(vars(Tipo), scales = 'free_x') + 
  theme(axis.title.y = element_blank(), 
        panel.grid = element_blank())

guardarGGplot(gTransporteCost, '066_transportes', 8, 4)

