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
war_probability = function(distance_vector, alpha = 800, u = 400){
  new_df = normal_countries(distance_vector = distance_vector, alpha = alpha, u = u) %>% 
    mutate(probability_B0 = 1 - probability_B0*decision_B0,
           probability_B1 = 1 - probability_B1*decision_B1)
  B0_war = 1 - prod(new_df$probability_B0)
  B1_war = 1 - prod(new_df$probability_B1)
  war_vector = c(B0_war, B1_war)
  return(war_vector)
}