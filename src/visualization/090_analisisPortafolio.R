
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
# Caso Fenobarbital ------------------
#'-------------------------------------------------------------------------------

#' 4.97 ¿Qué tipo de fenobarbital usa (10,50 o 100 mg)? ¿Para qué tipo de 
#' pacientes lo usan y para dosis menores como se adecua? ------------------

col1 <- "4.97_Detallado"

gFenobarbital <- select(df1, frec = col1) %>% 
  separarDummies(TRUE, ";") %>% 
  select(matches("\\w")) %>%
  pivot_longer(cols = everything()) %>% 
  group_by(name) %>% 
  summarise(value = sum(value),
            N = n()) %>% 
  mutate(label1 = glue("{value}/{N}")) %>% 
  ggplot(aes(x = value, y = fct_reorder(name, value))) + 
  geom_bar(stat = 'identity', fill = blueMSPS, 
           alpha = 0.8, col = 'black') + 
  geom_label(aes(label = label1), hjust = -0.4) +
  scale_x_continuous(expand = c(0, 0, 0.4, 0)) +
  xlab('Frecuencia') +
  theme(axis.title.y = element_blank(),
        panel.grid = element_blank())

guardarGGplot(gFenobarbital, '200_fenobarbital', 7, 4)

#'-------------------------------------------------------------------------------
# Preguntas Portafolio ------------------
#'-------------------------------------------------------------------------------
#' Sólo para IPS

# El fondo debería contar con otras FF en su portafolio
col1 <- "4.98_Detallado"

#' Considera que existe una diferencia en productos de metadona para fabricación 
#' nacional e importado.

col2 <- "4.99_Detallado"


#' Conoce el medicamento Hidrato de Cloral; ¿lo ha manejado?
col3 <- "4.96_Detallado"

#' En su institución se ha utilizado Morfina Solución Oral
col4 <- "4.95_UsoMorfinaOral"

#' Casos de farmacovigilancia de MME 
col5 <- "4.87_Explicado"


df_resPortafolio <- df1 %>%
  filter(Tipo != 'Operadores logísticos') %>%
  select(all_of(c(col1, col5, col2, col3, col4))) %>%
  apply(2, function(x) sum(ifelse(x=='Sí', 1, 0), na.rm = T)/length(x)) %>% 
  as.data.frame(.) %>% 
  rownames_to_column('ID') %>% 
  rename("Prop_Si" = ".") %>% 
  mutate("Prop_No" = 1- Prop_Si) %>% 
  add_column(CID = paste0('C', 1:5), .before = 'ID')


ylabelScale <- c(
  "¿Debería el FNE incluir otros medicamentos en su portafolio establecido en la actualidad?",
  "¿Se han presentado reportes de eventos adversos relacionados a MME en su institución?",
  "¿Considera que exíste una diferencia en el producto de Metadona de fabricación nacional y el importado?", 
  "¿Conoce o ha escuchado sobre el medicamento hidrato de cloral?",
  "¿En su institución se utiliza el medicamento Morfina Sol. Oral al 3%?"
)


gresPortafolio <- df_resPortafolio %>%
  pivot_longer(cols = contains('Prop')) %>% 
  ggplot(aes(x = value, y = CID, fill = name)) +
  geom_bar(stat = 'identity', col = 'black') + 
  geom_label(aes(label = formatC(value, 2)), 
             fill = 'white',
            size = 4, position = position_stack(vjust = 0.5)) + 
  xlab('Frecuencia') + 
  scale_y_discrete(
    labels = rev(str_wrap(ylabelScale, 35)), limits = rev) +
  scale_x_continuous(
    breaks = seq(0,1,0.2),
    labels = scales::percent_format()
  ) + 
  scale_fill_manual(values=c('gray80', 'red'), labels = c("No", 'Sí'), 
                    name = 'Respuesta') + 
  coord_cartesian(xlim = c(0, 1))  +
  theme(axis.title.y = element_blank(),
        panel.grid = element_blank())

guardarGGplot(gresPortafolio, '201_portafolio', 7, 4)

#'-------------------------------------------------------------------------------
# 4.94 ¿Qué tan conforme se encuentra con las presentaciones comerciales
# de los MME? ------------------
#'-------------------------------------------------------------------------------

likertVar <- c("Muy conforme", 'Algo conforme', 
               "Ni conforme ni inconforme", "Algo inconforme", 'Muy inconforme')


col1 <- "4.94_Detallado"


gConfPresentac <- select(df1, frec = col1) %>% 
  count(frec) %>%
  drop_na() %>% 
  mutate(
    frec = ifelse(frec == "Muy infonforme", "Muy inconforme", frec),
    frec = factor(frec, levels = rev(likertVar)),
    prop = prop.table(n),
    label1 = paste0(n, ', ', round(prop, 3) * 100, '%')
  ) %>%
  graficoFrecuenciasHoriz(n, frec, label1, expand = c(0,0,0.5,0)) +
  theme(axis.title.y = element_blank(),
        panel.grid = element_blank())

guardarGGplot(gConfPresentac, '210_conformidadPresentaciones', 7, 4)

#'-------------------------------------------------------------------------------
# Magistrales ------------------
#'-------------------------------------------------------------------------------
#' Sólo se seleccionan Instituciones Prestadoras de Salud
#' 
#' Adecuación de MCE para la utilización de pacientes
col1 <- "4.90._Realiza_la_adecuación_de"
#' 
#' Elaboración de formas magistrales
#' 
col2 <- "4.92._Realiza_la_elaboración_d"


df_resMagistrales <- df1 %>%
  filter(Tipo != 'Operadores logísticos') %>%
  select(C1 = col1, C2 = col2) %>%
  summarise(across(everything(), ~sum(ifelse(.x=='Sí', 1, 0))/length(.x)))


ylabelScale1 <- c(
  "En su institución, ¿se adecuan Medicamentos de Control Especial en una central de mezcla?",
  "En su institución, ¿se elaboran fórmulas magistrales?"
)

gMagistrales <- df_resMagistrales %>% 
  pivot_longer(cols = c('C1', 'C2'), values_to = 'prop_norm', 
               names_to = 'CID') %>% 
  mutate(prop_comp = 1 - prop_norm) %>% 
  pivot_longer(cols = 2:3) %>% 
  ggplot(aes(x = value, y = CID, fill = name)) +
  geom_bar(stat = 'identity', col = 'black') + 
  geom_label(aes(label = formatC(value, 2)), 
             fill = 'white',
             size = 4, position = position_stack(vjust = 0.5)) + 
  xlab('Frecuencia') + 
  scale_y_discrete(
    labels = rev(str_wrap(ylabelScale1, 35)), limits = rev) +
  scale_x_continuous(
    breaks = seq(0,1,0.2),
    labels = scales::percent_format()
  ) + 
  scale_fill_manual(values=c('gray80', 'red'), labels = c("No", 'Sí'), 
                    name = 'Respuesta') + 
  coord_cartesian(xlim = c(0, 1))  +
  theme(axis.title.y = element_blank(),
        panel.grid = element_blank())

guardarGGplot(gMagistrales, '211_magistrales1', 7, 4)

#'-------------------------------------------------------------------------------


col1 <- "4.90._Detalle"

df_resSolucion <- df1 %>%
  filter(Tipo != 'Operadores logísticos') %>%
  select(C1 = col1) %>% 
  # Se debe realizar la consideración como carácter simple (no regex)
  separarDummies(TRUE, ";", regex = FALSE) %>% 
  select(matches("\\w")) %>% 
  pivot_longer(cols = everything()) %>% 
  group_by(name) %>% 
  summarise(val = sum(value)) %>% 
  mutate(name = str_replace_all(name, "\\\\", "")) %>% 
  filter(!str_detect(name, "Midazolam|Ketamina|enta"))


gMagistrales1 <- df_resSolucion %>% 
  mutate(name = fct_reorder(name, val)) %>% 
  graficoFrecuenciasHoriz(val, name, expand = c(0, 0, 0.5, 0)) +
  theme(axis.title.y = element_blank(),
        panel.grid = element_blank())

guardarGGplot(gMagistrales1, '212_magistrales2', 6, 4)















