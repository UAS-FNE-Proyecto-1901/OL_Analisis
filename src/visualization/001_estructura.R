
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


#'-------------------------------------------------------------------------------
# 1. ¿Cómo están compuestos los encuestados? ------------------
#'-------------------------------------------------------------------------------

col1 <- "2.01_Composicion"

roles <- c('Químico farmacéutico', 'Regente de farmacia', 'Auxiliar de farmacia')

df2 <- rename(df1, var = '2.01_Composicion') %>% 
  mutate(
    var = str_replace_all(var, '\\w{2}\\:\\s', ''), 
    var = str_replace_all(var, '\\s', '')) %>% 
  separate(var, roles, "\\;") %>% 
  mutate(across(all_of(roles), ~as.integer(.x)))

gDistribEstructura <- df2 %>% 
  pivot_longer(cols = roles) %>% 
  mutate(name = factor(name, levels = roles)) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram(bins = 8, fill = blueMSPS, 
                 color = 'black', alpha = 0.8) +
  # geom_density(stat = 'count') + 
  facet_grid(cols = vars(name), rows = vars(Tipo), scales = 'free_x', 
             labeller = labeller(Tipo = function(x) str_wrap(x, width = 20))) +
  # stat_summary(fun.data = "mean", geom = 'label') +
  ylab('Frecuencia, conteo') + 
  xlab('Valor')

guardarGGplot(gDistribEstructura, '001_distribucionPersonal', 8, 6)

df2_res <- df2 %>% 
  pivot_longer(cols = roles) %>% 
  mutate(name = factor(name, levels = roles)) %>% 
  group_by(name, Tipo) %>% 
  mutate(value = cut(value, breaks = c(-1, 0, 2, 5, 10, 20, 30, 50, 110))) %>% 
  count(Tipo, name, value)
  

lvl2 <- levels(df2_res$value)
lvl2 <- setNames(lvl2, c("Ninguno", "Entre 1 y 2", "Entre 2 y 5", "Entre 5 y 10", "Entre 10 y 20", 
                 "Entre 20 y 30", "Entre 30 y 50", "Entre 50 y 110"))

df2_res['value1'] <- fct_recode(df2_res[['value']], !!!lvl2)


gDistribEstructura1 <- df2_res %>%
  ungroup() %>%
  ggplot(aes(x = n, y = fct_rev(value1))) + 
  geom_bar(stat = 'identity', fill = blueMSPS, 
           color = 'black', alpha = 0.8) + 
  facet_grid(cols = vars(name), rows = vars(Tipo), 
             labeller = labeller(Tipo = function(x) str_wrap(x, width = 20))) +
  xlab('Frecuencia, conteo') +
  theme(axis.title.y = element_blank())

guardarGGplot(gDistribEstructura1, '001_distribucionPersonal_b', 10, 6)


#'-------------------------------------------------------------------------------
# 2.02 ¿En los contratos o en los perfiles laborales requeridos tienen 
# obligaciones referentes a MCE? ------------------
#'-------------------------------------------------------------------------------
col1 <- "2.02_Explicado"

ordenVar <- c('Sí', 'No', NA_character_)

gContratos1 <- select(df1, frec = col1) %>%
  count(frec) %>%
  mutate(
    frec = factor(frec, levels = ordenVar),
    prop = prop.table(n),
    label1 = paste0(frec, ', ', round(prop, 3) * 100, '%')
  ) %>%
  graficoFrecuenciasHoriz(n, frec, label1)


guardarGGplot(gContratos1, '002_MCE_contratos_1', 6, 4)

gContratos2 <- select(df1, frec = col1, Tipo) %>%
  count(Tipo, frec) %>%
  add_row(Tipo = 'Instituciones Prestadoras de Salud', frec = NA, n = 0) %>% 
  add_row(Tipo = 'Operadores logísticos', frec = 'No', n = 0) %>% 
  group_by(Tipo) %>% 
  mutate(
    frec = factor(frec, levels = ordenVar),
    prop = prop.table(n),
    label1 = paste0(frec, ', ', round(prop, 3) * 100, '%')
  ) %>%
  graficoFrecuenciasHoriz(n, frec, label1, expand = c(0,0,0.8,0)) + 
  facet_wrap(vars(Tipo), 
             labeller = labeller(Tipo = function(x) str_wrap(x, width = 20)))

guardarGGplot(gContratos2, '003_MCE_contratos_2', 8, 4)

#'-------------------------------------------------------------------------------
# 2.03 ¿Poseen carta de corresponsabilidad para el manejo de MCE firmada por el 
# personal que tiene contacto con estos medicamentos? ------------------
#'-------------------------------------------------------------------------------

col1 <- '2.03_Poseen_carta_de_correspon'

gCorresponsabilidad <- select(df1, frec = col1, Tipo) %>%
  count(Tipo, frec) %>%
  complete(Tipo, frec, fill=list(n = 0)) %>% 
  group_by(Tipo) %>% 
  mutate(
    frec = factor(frec, levels = ordenVar),
    prop = prop.table(n),
    label1 = paste0(frec, ', ', n, '\n', round(prop, 3) * 100, '%')
  ) %>%
  graficoFrecuenciasHoriz(n, frec, label1, expand = c(0,0,0.8,0)) + 
  facet_wrap(vars(Tipo))

guardarGGplot(gCorresponsabilidad, '004_corresponsab_1', 8, 4)

#'-------------------------------------------------------------------------------
# 2.04 ¿Usted realiza capacitaciones respecto a MCE, cuáles son los temas 
# que trata en estas actividades, cada cuanto se realizan y como evalúan estas 
# capacitaciones?  ------------------
#'-------------------------------------------------------------------------------

#' Se realizan capacitaciones
col1 <- '2.04_Explicado'

gEducacion1 <- select(df1, frec = col1, Tipo) %>%
  count(Tipo, frec) %>%
  complete(Tipo, frec, fill=list(n = 0)) %>% 
  group_by(Tipo) %>% 
  mutate(
    frec = factor(frec, levels = ordenVar),
    prop = prop.table(n),
    label1 = paste0(frec, ', ', n, '\n', round(prop, 3) * 100, '%')
  ) %>%
  graficoFrecuenciasHoriz(n, frec, label1, expand = c(0,0,0.8,0)) + 
  facet_wrap(vars(Tipo))

guardarGGplot(gEducacion1, '005_educacion_1', 8, 4)

# Con que frecuencia se realizan las capacitaciones
col1 <- '2.04_Frecuencia'

gEducacion2 <- select(df1, frec = col1, Tipo) %>%
  count(Tipo, frec) %>%
  filter(!is.na(frec)) %>% 
  complete(Tipo, frec, fill=list(n = 0)) %>% 
  group_by(Tipo) %>% 
  mutate(
    frec = factor(frec, levels = rev(c('Cada dos meses', 'Dos veces por año', 'Una vez al año'))),
    prop = prop.table(n),
    label1 = paste0(n, ', ', round(prop, 3) * 100, '%')
  ) %>%
  graficoFrecuenciasHoriz(n, frec, label1, expand = c(0,0,0.5,0)) + 
  facet_wrap(vars(Tipo))

guardarGGplot(gEducacion2, '006_educacion_2', 8, 4)


# Las capacitaciones se evalúan

col1 <- '2.04_Evaluar'

gEducacion3 <- select(df1, frec = col1, Tipo) %>%
  count(Tipo, frec) %>%
  filter(!is.na(frec)) %>% 
  complete(Tipo, frec, fill=list(n = 0)) %>% 
  group_by(Tipo) %>% 
  mutate(
    frec = factor(frec, levels = ordenVar),
    prop = prop.table(n),
    label1 = paste0(n, ', ', round(prop, 3) * 100, '%')
  ) %>%
  graficoFrecuenciasHoriz(n, frec, label1, expand = c(0,0,0.4,0)) + 
  facet_wrap(vars(Tipo))

guardarGGplot(gEducacion3, '007_educacion_3', 8, 4)


#'-------------------------------------------------------------------------------
# 4.46 ¿Cuantos servicios farmacéuticos tiene el hospital y en cuantos y 
# cuales de estos se manejan MCE?  ------------------
#'-------------------------------------------------------------------------------

col1 <- "4.46_Cuantos_servicios_farmacé"

gServicios <- select(df1, frec = col1, Tipo) %>% 
  mutate(frec = cut(frec, breaks = c(0,1,2,5,10, 50))) %>%
  count(Tipo, frec) %>% 
  ggplot(aes(x = n, y = fct_rev(frec))) +
  geom_bar(stat = 'identity', fill = blueMSPS, alpha = 0.8, col = 'black') + 
  geom_label(aes(label = n), hjust = -0.2) + 
  scale_x_continuous(expand = c(0, 0, 0.4, 0)) + 
  facet_wrap(vars(Tipo), scales = 'free_x') +
  xlab('Frecuencia') +
  theme(axis.title.y = element_blank(), 
        panel.grid = element_blank())

guardarGGplot(gServicios, '008_servicios', 8, 4)
