#' --- 
#' title: 'Tratamiento inicial de datos de operadores logísticos' 
#' subtitle: 'Operadores Logísticos' 
#' date: '14-12-2021' 
#' author: 
#'        - name: Daniel S. Parra G. 
#'          email: dsparrag@minsalud.gov.co 
#'          institute: FNE 
#' institute: 
#'        - FNE: Misión PRI 1901 - Fondo Nacional de Estupefacientes 
#' abstract: abstract 
#' output:  
#'      - html_document: default 
#'      - pdf_document: default 
#' always_allow_html: true 
#' --- 

require(readxl)
require(tidyverse)



data <- read_excel(file.path('data', 'raw', '001_instrumento_OL.xlsx'), 
                   na = c('N.A', 'NA', '-', 'NN'))


col1 <- colnames(data)

reemplazarCaracteres <- function(string, end = 30) {
  str1 <- str_replace_all(string, 'de la', '')
  str1 <- str_replace_all(str1, '\\.{3}', ' ')
  str1 <- str_replace_all(str1, 'del', '')
  str1 <- str_replace_all(str1, '[^\\w\\d\\s\\.]', '')
  str1 <- str_replace_all(str1, '\\s{1,}', '_')
  
  str2 <- str_sub(str1, 1, end)
  
  return(str2)
}

col2 <- map_chr(col1, ~reemplazarCaracteres(.x))


colnames(data) <- col2


write_csv(data, file.path('data', 'processed', '001_datosProcesados.csv'))
