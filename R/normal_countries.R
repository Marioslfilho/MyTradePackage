#' Compute Regular States’ Strategic Decisions
#'
#' @description
#' Calculates the strategic decisions of regular (non-hegemonic) countries given
#' their distance from a military threat and the hegemon. The function derives
#' the probabilities of alignment or opposition under two possible strategic
#' configurations (\emph{B0} and \emph{B1}), based on the parameters of the model.
#'
#' @details
#' The function assumes that the last element of \code{distance_vector}
#' corresponds to the hegemon’s position, and that the remaining values represent
#' the distances between regular states and the military threat.
#'
#' For each state, it computes:
#' \itemize{
#'   \item \strong{probability_B0}: Probability of opposing without benefit
#'     (decreases with distance).
#'   \item \strong{probability_B1}: Probability of opposing with benefit
#'     (zero if distance > 0.8).
#' }
#'
#' Based on these, it determines binary decisions:
#' \itemize{
#'   \item \strong{decision_B0 = 1} if \eqn{u - \alpha p_{B0} > 0}.
#'   \item \strong{decision_B1 = 1} if \eqn{u - \alpha p_{B1} > 0}.
#' }
#'
#' The output can be used as input to higher-level functions such as
#' \code{\link{hegemonic_decision}} and \code{\link{normal_area}} to analyze
#' the system’s equilibrium structure.
#'
#' @param distance_vector A numeric vector of distances among all actors in the
#'   system. The last element represents the hegemon’s position.
#' @param u A numeric scalar. Economic utility threshold for regular states.
#'   Default is \code{400}.
#' @param alpha A numeric scalar. Strategic influence parameter (strength of the
#'   hegemon). Default is \code{800}.
#'
#' @return A \code{data.frame} with the following columns:
#'   \describe{
#'     \item{\code{distances}}{Distances of each state from the military threat.}
#'     \item{\code{probability_B0}}{Probability of opposing without benefit.}
#'     \item{\code{probability_B1}}{Probability of opposing with benefit.}
#'     \item{\code{decision_B0}}{Binary indicator for opposition without benefit.}
#'     \item{\code{decision_B1}}{Binary indicator for opposition with benefit.}
#'   }
#'
#' @seealso
#' \code{\link{hegemonic_decision}} for the hegemon’s response rule, and
#' \code{\link{normal_area}} for visualizing how regular states’ decisions vary
#' with system parameters.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' dist_vec <- c(0.2, 0.5, 0.8)  # last element = hegemon position
#' normal_countries(dist_vec)
#' }
#'
#' @export
normal_countries <- function(distance_vector, u = 400, alpha = 800) {
  distances <- distance_vector[-length(distance_vector)]
  probability_B0 <- 1 - distances
  probability_B1 <- ifelse(0.8 < distances, 0, 0.8 - distances)

  decision_B0 <- ifelse(u - alpha * probability_B0 > 0, 1, 0)
  decision_B1 <- ifelse(u - alpha * probability_B1 > 0, 1, 0)

  data_return <- data.frame(
    distances,
    probability_B0,
    probability_B1,
    decision_B0,
    decision_B1
  )

  return(data_return)
}
