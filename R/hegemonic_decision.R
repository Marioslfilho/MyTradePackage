#'
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
hegemonic_decision = function(distance_vector, alpha = 800, u = 400, tau = 600){
  distance = distance_vector[-length(distance_vector)]
  hegemonic_position = distance_vector[length(distance_vector)]
  hegemon_utilities = normal_countries(distance_vector = distance_vector, alpha = alpha, u = u) %>%
    mutate(utility_B0 = tau*decision_B0*(1 - abs(hegemonic_position - distance)),
           utility_B1 = tau*decision_B1*(1 - abs(hegemonic_position - distance)),
           utility_MT = tau*(1 - decision_B0)*(1 - abs(hegemonic_position - distance)))
  utility_vector = c(sum(hegemon_utilities$utility_B0),
                     sum(hegemon_utilities$utility_B1)- 100*sum(hegemon_utilities$decision_B1),
                     sum(hegemon_utilities$utility_MT)+ tau*(1 - hegemonic_position)) %>%
    which.max()
  return(c('B0', 'B1', 'MT')[utility_vector])
}
