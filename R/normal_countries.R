#'
#'
#'
#'
#'
#'
#'@export
normal_countries = function(distance_vector, u = 400, alpha = 800){
  distances = distance_vector[-length(distance_vector)]
  probability_B0 = 1 - distances
  probability_B1 = ifelse(.8 < distances, 0, .8 - distances)

  decision_B0 <- ifelse(u - alpha * probability_B0 > 0, 1, 0)
  decision_B1 <- ifelse(u - alpha * probability_B1 > 0, 1, 0)

  data_return = data.frame(distances, probability_B0, probability_B1,
                           decision_B0, decision_B1)
  return(data_return)
}
