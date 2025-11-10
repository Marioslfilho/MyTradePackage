#' Plot Hegemon’s Strategic Area
#'
#' This function computes and visualizes the hegemon’s strategic decision space
#' based on two varying distance parameters. It evaluates the hegemon’s choice
#' (alignment, opposition, or defiance) over a grid of possible distances and
#' plots the resulting regions.
#'
#' @param distance_vector A numeric vector of distances representing the
#'   configuration of the international system.
#' @param varies A numeric vector of length 2 indicating which two elements of
#'   \code{distance_vector} will vary across the grid.
#' @param alpha A numeric scalar. Represents the hegemon’s strategic strength or
#'   capacity parameter. Default is \code{800}.
#' @param u A numeric scalar. Represents the economic utility threshold. Default
#'   is \code{400}.
#' @param tau A numeric scalar. Represents the military cost parameter. Default
#'   is \code{600}.
#'
#' @return A \code{ggplot} object showing the regions corresponding to the
#'   hegemon’s strategic choices:
#'   \describe{
#'     \item{\strong{"MT"}}{Aligns with the military threat (red region).}
#'     \item{\strong{"B1"}}{Opposes and offers benefit (blue region).}
#'     \item{\strong{"B0"}}{Opposes without benefit (green region).}
#'   }
#'
#' @details
#' The function iterates over a two-dimensional grid of distance values and
#' evaluates the hegemon’s optimal decision using the helper function
#' \code{hegemonic_decision()}. The resulting data frame is then visualized
#' using \pkg{ggplot2}.
#'
#' Dashed vertical lines indicate the theoretical thresholds
#' \eqn{d_{i,m}^{*}} and \eqn{d_{i,M}^{**}} in the model.
#'
#' @seealso \code{\link{hegemonic_decision}} for the underlying decision rule.
#'
#' @import ggplot2
#' @importFrom dplyr %>%
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' dist_vec <- c(0.2, 0.4, 0.6)
#' hegemonic_area(dist_vec, varies = c(1, 2))
#' }
#'
#' @export
hegemonic_area <- function(distance_vector,
                           varies,
                           alpha = 800,
                           u = 400,
                           tau = 600) {

  # Define parameter grid
  seq_i <- seq(0.01, 1, 0.01)
  seq_j <- seq(0.01, 1, 0.01)

  n <- length(seq_i) * length(seq_j)
  first_vary       <- numeric(n)
  second_vary      <- numeric(n)
  hegemonic_choice <- character(n)

  idx <- 1L

  # Evaluate hegemonic decision over parameter grid
  for (i in seq_i) {
    distance_vector[varies[1]] <- i
    for (j in seq_j) {
      distance_vector[varies[2]] <- j
      first_vary[idx]  <- i
      second_vary[idx] <- j
      hegemonic_choice[idx] <- hegemonic_decision(
        distance_vector,
        alpha = alpha,
        u = u,
        tau = tau
      )
      idx <- idx + 1L
    }
  }

  # Build data frame for plotting
  df <- data.frame(
    first_vary = first_vary,
    second_vary = second_vary,
    hegemonic_choice = hegemonic_choice
  )

  # Generate ggplot
  plot <- ggplot(df, aes(
    x = first_vary,
    y = second_vary,
    group = hegemonic_choice,
    color = hegemonic_choice
  )) +
    geom_path(size = 2, alpha = 1) +
    geom_vline(
      xintercept = (alpha - u) / alpha,
      alpha = 0.6,
      linetype = "dashed"
    ) +
    geom_vline(
      xintercept = (alpha - u - alpha * 0.2) / alpha,
      alpha = 0.6,
      linetype = "dashed"
    ) +
    scale_color_manual(
      values = c(
        "MT" = "#d81a40",
        "B1" = "#2898e9",
        "B0" = "#289866"
      ),
      labels = c(
        "MT" = "Ally",
        "B1" = "Oppose & Benefit",
        "B0" = "Oppose & No Benefit"
      )
    ) +
    scale_x_continuous(
      breaks = c(
        (alpha - u - alpha * 0.2) / alpha,
        (alpha - u) / alpha
      ),
      labels = c(
        expression(d[r_i,m]^"*"),
        expression(d[r_i,m]^"**")
      )
    ) +
    theme_minimal(base_size = 14) +
    labs(
      x = expression(p[r_i]),
      y = expression(p[h])
    ) +
    theme(
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      legend.title = element_blank(),
      axis.text.y = element_blank(),
      legend.position = "top"
    )

  return(plot)
}
