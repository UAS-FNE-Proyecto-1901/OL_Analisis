#'-------------------------------------------------------------------------------
#' Gráfico de torta
#'
#' @param data_frame 
#' @param nvar 
#' @param textvar 
#'
#' @return
#' @export
#'
#' @examples
#' 
pieChart <- function(data_frame, nvar, textvar, repel = FALSE){
  gPie <- data_frame %>% 
    arrange(desc({{textvar}})) %>% 
    mutate(prop = {{nvar}} / sum({{nvar}}),
           ncumsum = cumsum(prop) - 0.5 * prop) %>%
    ggplot(aes(x = "", y = prop,  fill = {{textvar}})) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0) + 
    theme_void() +
    theme(legend.position="bottom")
  
  if (repel) {
    gPie1 <- gPie +
      ggrepel::geom_text_repel(aes(y = ncumsum, label = {{textvar}}), color = "white", size=4)
  } else {
    gPie1 <- gPie +
      geom_text(aes(y = ncumsum, label = {{textvar}}), color = "white", size=4)
  }
  return(gPie1)
}