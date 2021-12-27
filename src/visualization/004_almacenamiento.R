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
# 4.30 Con que medidas de seguridad almacena los MCE (especificar quien es el 
# responsable y en caso de llaves cuantas existen)  ------------------
#'-------------------------------------------------------------------------------

col1 <- "4.30_Explicado"

df_res4 <- select(df1, frec = col1) %>% 
  separarDummies(TRUE, ";") %>% 
  select(matches("\\w")) %>% 
  {bind_cols(select(df1, Tipo), .)} %>% 
  pivot_longer(!matches('Tipo')) %>% 
  group_by(Tipo, name) %>% 
  summarise(value = sum(value), .groups = 'drop') %>% 
  left_join(conteoTipos, by = 'Tipo') %>% 
  mutate(prop = value / n,
         label1 = paste0(value, "/", n))


gMedidasSeguridad <- df_res4 %>% 
  ggplot(aes(x = prop, y = fct_reorder(name, prop))) + 
  geom_bar(stat = 'identity', fill=blueMSPS, 
           color = 'black', alpha = 0.85) + 
  xlab('Proporción de encuestados') + 
  geom_label(aes(label = label1), hjust = -0.2) + 
  facet_wrap(vars(Tipo), scales = 'free_x') + 
  scale_x_continuous(
    breaks = seq(0,1,0.2),
    labels = scales::percent_format(), 
    expand = c(0,0,0.4,0)
  ) +
  theme(axis.title.y = element_blank(), 
        panel.grid = element_blank())

guardarGGplot(gMedidasSeguridad, '070_medidasSeguridad', 8, 4)

#'-------------------------------------------------------------------------------
# 4.32 ¿Cada cuanto se hace revisión de condiciones ambientales? ------------------
#'-------------------------------------------------------------------------------

col1 <- "4.32_Explicado"

ordenVar1 <- c('Una vez al día', 'Dos veces al día', 'Tres veces al día', 'Continuo')

gRevisionCondiciones <- select(df1, frec = col1, Tipo) %>%
  drop_na() %>% 
  count(Tipo, frec) %>%
  add_row(Tipo = 'Operadores logísticos', frec = 'Una vez al día') %>% 
  complete(Tipo, frec, fill = list(n = 0)) %>% 
  group_by(Tipo) %>%
  mutate(
    frec = factor(frec, ordenVar1),
    prop = prop.table(n),
    label1 = paste0(n, ', ', round(prop, 3) * 100, '%')) %>% 
  graficoFrecuenciasHoriz(n, frec, label1, expand = c(0,0,0.5,0)) +
  facet_wrap(vars(Tipo), scales = 'free_x') + 
  theme(axis.title.y = element_blank(), 
        panel.grid = element_blank())

guardarGGplot(gRevisionCondiciones, '071_revisionCondiciones', 8, 4)


#'-------------------------------------------------------------------------------
# 4.34 Se tiene semaforización de medicamentos de control especial? ¿Cual es el 
# criterio de esta (3 o 6 meses)? ------------------
#'-------------------------------------------------------------------------------

col1 <- "4.34_Umbral"

select(df1, frec = col1) %>% 
  separate(frec, into = c(NA, 'R','A','V'), sep = "[A-Z]") %>% 
  pivot_longer(cols = c('R', 'A', 'V')) %>% 
  count(name, value)

#'-------------------------------------------------------------------------------
# 4.40. ¿Tiene niveles de seguridad en el inventario definidos para 
# MME? ------------------
#'-------------------------------------------------------------------------------

col1 <- "4.40._Tiene_niveles_de_segurid"

gNivelesSeguridad <- select(df1, frec = col1, Tipo) %>%
  count(Tipo, frec) %>%
  complete(Tipo, frec, fill = list(n = 0)) %>%
  group_by(Tipo) %>%
  mutate(
    frec = factor(frec, ordenVar),
    prop = prop.table(n),
    label1 = paste0(n, ', ', round(prop, 3) * 100, '%')
  ) %>%
  graficoFrecuenciasHoriz(n, frec, label1, expand = c(0, 0, 0.5, 0)) +
  facet_wrap(vars(Tipo), scales = 'free_x') +
  theme(axis.title.y = element_blank(),
        panel.grid = element_blank())

guardarGGplot(gNivelesSeguridad, '080_nivelesSeguridad', 8, 4)


#'-------------------------------------------------------------------------------
# 4.43. ¿Tiene reglas de decisión que indiquen la necesidad inminente de compra 
# de MME? ------------------
#'-------------------------------------------------------------------------------

col1 <- "4.43._Tiene_reglas_de_decisión"

df_res5 <- select(df1, frec = col1) %>% 
  separarDummies() %>% 
  {bind_cols(select(df1, Tipo), .)} %>% 
  pivot_longer(!matches('Tipo')) %>% 
  group_by(Tipo, name) %>% 
  summarise(value = sum(value), .groups = 'drop') %>% 
  left_join(conteoTipos, by = 'Tipo') %>% 
  mutate(prop = value / n,
         label1 = paste0(value, "/", n))

gReglasDecision <- df_res5 %>% 
  ggplot(aes(x = prop, y = fct_reorder(name, prop))) + 
  geom_bar(stat = 'identity', fill=blueMSPS, 
           color = 'black', alpha = 0.85) + 
  xlab('Proporción de encuestados') + 
  geom_label(aes(label = label1), hjust = -0.2) + 
  facet_wrap(vars(Tipo)) + 
  scale_x_continuous(
    breaks = seq(0,1,0.2),
    labels = scales::percent_format(), 
    expand = c(0,0,0.4,0)
  ) +
  theme(axis.title.y = element_blank(), 
        panel.grid = element_blank())

guardarGGplot(gReglasDecision, '081_reglasDecision', 8, 4)

#'-------------------------------------------------------------------------------
# 4.43. ¿Con qué frecuencia (número de ventas por año) realiza compra de 
# MME al FNE? ------------------
#'-------------------------------------------------------------------------------

col1 <- "4.43._Con_qué_frecuencia_númer"

select(df1, frec = col1, Tipo) %>% 
  ggplot(aes(x = frec, color = Tipo)) + 
  geom_density()


select(df1, frec = col1, Tipo) %>% 
  group_by(Tipo, frec) %>% 
  count()
  
col1 <- "4.44._Cuándo_se_realizan_compr"

select(df1, frec = col1, Tipo) %>% 
  ggplot(aes(x = frec, color = Tipo)) + 
  geom_density()

select(df1, frec = col1, Tipo) %>% 
  group_by(Tipo, frec) %>% 
  count()


#'-------------------------------------------------------------------------------
# 4.45 ¿Ha presentado unidades vencidas de MME? En el caso de ser afirmativa, 
# nos puede indicar de cuales productos y cuales razones han ocasionado
# este suceso. ------------------
#'-------------------------------------------------------------------------------
col1 <- "4.45_Explicado"

df_res6 <- select(df1, frec = col1) %>% 
  separarDummies(TRUE, ";") %>% 
  select(matches("\\w")) %>% 
  {bind_cols(select(df1, Tipo), .)} %>% 
  pivot_longer(!matches('Tipo')) %>% 
  group_by(Tipo, name) %>% 
  summarise(value = sum(value), .groups = 'drop') %>% 
  left_join(conteoTipos, by = 'Tipo') %>% 
  mutate(prop = value / n,
         label1 = paste0(value, "/", n))

gVencimientos <- df_res6 %>% 
  ggplot(aes(x = prop, y = fct_reorder(name, prop))) + 
  geom_bar(stat = 'identity', fill=blueMSPS, 
           color = 'black', alpha = 0.85) + 
  xlab('Proporción de encuestados') + 
  geom_label(aes(label = label1), hjust = -0.2) + 
  facet_wrap(vars(Tipo)) + 
  scale_x_continuous(
    breaks = seq(0,1,0.2),
    labels = scales::percent_format(), 
    expand = c(0,0,0.4,0)
  ) +
  theme(axis.title.y = element_blank(), 
        panel.grid = element_blank())

guardarGGplot(gVencimientos, '090_vencimientos', 8, 4)

#'-------------------------------------------------------------------------------
# 1. ------------------
#'-------------------------------------------------------------------------------
