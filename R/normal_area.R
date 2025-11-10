#'
#'
#'
#'
#'
#'
#'
#'
#'
#' @export
normal_area = function(distance_vector, varies, alpha = 800, u = 400, tau = 600, gamma = 200){
  distance = distance_vector
  vector = c()
  values <- seq(0.01, 1, 0.01)
  for (i in values) {
    distance[varies] = i
    data = normal_countries(distance_vector = distance, u = u, alpha = alpha) %>% 
      mutate(area_normal = decision_B0 + decision_B1, .keep = 'none')
    vector = append(vector, data[varies, 1])
  }
  d = data.frame(vector, values)
  grid <- expand.grid(first_vary = values,
                      second_vary = values) %>% 
    inner_join(d, by = c('first_vary' = 'values'))  %>%
    mutate(vector = factor(vector)) %>%
    ggplot(aes(x = first_vary, y = second_vary, group = vector, color = vector)) +
    geom_path(size = 2, alpha = 1) +
    scale_color_manual(
      values = c(
        "0" = "#F8766D", 
        "1" = "#56B4E9", 
        "2" = "#009E73"
      )
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid = element_blank(),
      legend.title = element_blank(),
      legend.position = "top"
    ) # Tem como arrumar pra só u e alpha ser os inputs!!
  return(grid)
  
}

