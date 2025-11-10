#' Plot Normal States’ Strategic Area
#'
#' @description
#' This function computes and visualizes the decision regions for \emph{regular states}
#' (or normal countries) in the model. It evaluates how these states' strategic
#' choices vary across a two-dimensional grid of distance parameters, conditional
#' on the hegemon's and the military threat's relative positions and strength.
#'
#' @details
#' The function iterates over a sequence of possible distances and computes, for each
#' configuration, the outcome of \code{normal_countries()} — namely, the strategic
#' areas corresponding to each decision outcome (e.g., \emph{alignment}, \emph{opposition
#' with benefit}, \emph{opposition without benefit}).
#'
#' The resulting plot displays the regions in which each equilibrium strategy arises,
#' helping visualize how changes in the political or geographic distance structure
#' affect normal states’ alignment behavior.
#'
#' The color mapping corresponds to:
#' \describe{
#'   \item{\strong{"0"}}{Alignment with the military threat (red).}
#'   \item{\strong{"1"}}{Opposition with benefit (blue).}
#'   \item{\strong{"2"}}{Opposition without benefit (green).}
#' }
#'
#' @param distance_vector A numeric vector representing the baseline distances
#'   among all actors in the system.
#' @param varies A numeric vector of length 2 indicating which distance dimensions
#'   vary across the grid.
#' @param alpha A numeric scalar. Represents the hegemon’s strategic strength or
#'   capacity parameter. Default is \code{800}.
#' @param u A numeric scalar. Represents the economic utility threshold.
#'   Default is \code{400}.
#' @param tau A numeric scalar. Represents the military cost parameter.
#'   Default is \code{600}.
#' @param gamma A numeric scalar. Represents an additional structural parameter
#'   for the regular states’ payoff. Default is \code{200}.
#'
#' @return A \code{ggplot} object visualizing the equilibrium regions for regular
#'   states' strategic choices.
#'
#' @seealso
#' \code{\link{normal_countries}} for the underlying computation of regular
#' states' payoffs and equilibrium conditions.
#'
#' @import ggplot2
#' @importFrom dplyr mutate inner_join %>%
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' dist_vec <- c(0.2, 0.4, 0.6)
#' normal_area(dist_vec, varies = c(1, 2))
#' }
#'
#' @export
normal_area <- function(distance_vector, varies, alpha = 800, u = 400, tau = 600, gamma = 200) {
  distance <- distance_vector
  vector <- c()
  values <- seq(0.01, 1, 0.01)

  for (i in values) {
    distance[varies] <- i
    data <- normal_countries(distance_vector = distance, u = u, alpha = alpha) %>%
      mutate(area_normal = decision_B0 + decision_B1, .keep = "none")
    vector <- append(vector, data[varies, 1])
  }

  d <- data.frame(vector, values)
  grid <- expand.grid(first_vary = values, second_vary = values) %>%
    inner_join(d, by = c("first_vary" = "values")) %>%
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
    )

  return(grid)
}

