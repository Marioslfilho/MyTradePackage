#' Compute the Hegemon’s Strategic Decision
#'
#' @description
#' Determines the hegemon’s optimal strategic choice given the configuration of
#' the international system. The decision is based on the utilities associated
#' with three possible strategies: \emph{opposition without benefit (B0)},
#' \emph{opposition with benefit (B1)}, and \emph{alignment with the military
#' threat (MT)}.
#'
#' @details
#' The function evaluates the hegemon’s expected utility for each strategy based
#' on distances between states and on the model’s structural parameters
#' (\code{alpha}, \code{u}, and \code{tau}).
#'
#' Specifically:
#' \itemize{
#'   \item \strong{B0}: Oppose the military threat without providing benefits.
#'   \item \strong{B1}: Oppose the military threat and provide a benefit to opposing states.
#'   \item \strong{MT}: Align with the military threat and its allies.
#' }
#'
#' The hegemon’s position is assumed to correspond to the last element of
#' \code{distance_vector}. The function calls \code{normal_countries()} to obtain
#' the system’s equilibrium configuration and derives the hegemon’s total utility
#' under each alternative. The strategy yielding the highest total utility is
#' returned as a character code.
#'
#' @param distance_vector A numeric vector representing the distances between
#'   states in the system. The last element corresponds to the hegemon’s own
#'   position.
#' @param alpha A numeric scalar. Represents the hegemon’s strategic capacity or
#'   power parameter. Default is \code{800}.
#' @param u A numeric scalar. Represents the economic utility threshold.
#'   Default is \code{400}.
#' @param tau A numeric scalar. Represents the military cost parameter.
#'   Default is \code{600}.
#'
#' @return A character string indicating the hegemon’s equilibrium strategy:
#'   \describe{
#'     \item{\strong{"B0"}}{Oppose without benefit.}
#'     \item{\strong{"B1"}}{Oppose with benefit.}
#'     \item{\strong{"MT"}}{Align with the military threat.}
#'   }
#'
#' @seealso
#' \code{\link{normal_countries}} for the computation of regular states’ decisions,
#' and \code{\link{hegemonic_area}} for a graphical representation of the
#' hegemon’s decision regions.
#'
#' @importFrom dplyr mutate
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' dist_vec <- c(0.2, 0.4, 0.6, 0.8)  # last element = hegemon position
#' hegemonic_decision(dist_vec)
#' }
#'
#' @export
hegemonic_decision <- function(distance_vector, alpha = 800, u = 400, tau = 600) {
  distance <- distance_vector[-length(distance_vector)]
  hegemonic_position <- distance_vector[length(distance_vector)]

  hegemon_utilities <- normal_countries(
    distance_vector = distance_vector,
    alpha = alpha,
    u = u
  ) %>%
    mutate(
      utility_B0 = tau * decision_B0 * (1 - abs(hegemonic_position - distance)),
      utility_B1 = tau * decision_B1 * (1 - abs(hegemonic_position - distance)),
      utility_MT = tau * (1 - decision_B0) * (1 - abs(hegemonic_position - distance))
    )

  utility_vector <- c(
    sum(hegemon_utilities$utility_B0),
    sum(hegemon_utilities$utility_B1) - 100 * sum(hegemon_utilities$decision_B1),
    sum(hegemon_utilities$utility_MT) + tau * (1 - hegemonic_position)
  ) %>%
    which.max()

  return(c("B0", "B1", "MT")[utility_vector])
}
