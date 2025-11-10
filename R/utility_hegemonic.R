#' Plot Hegemon’s Utility Functions
#'
#' @description
#' This function computes and visualizes the hegemon’s utility across different
#' positions in the political or geographic space, given the distribution of regular
#' states. It considers three possible strategies for the hegemon: \emph{ally},
#' \emph{oppose}, and \emph{defy}.
#'
#' @param regulars_position A numeric vector indicating the positions of regular states
#'   on the unit interval \code{[0, 1]}.
#' @param alpha A numeric scalar representing the hegemon’s strategic strength or
#'   influence parameter. Default is \code{800}.
#' @param u A numeric scalar representing the economic utility threshold. Default is
#'   \code{400}.
#' @param tau A numeric scalar representing the military or trade benefit parameter.
#'   Default is \code{600}.
#' @param c A numeric scalar representing the cost of defying the military threat.
#'   Default is \code{100}.
#'
#' @details
#' The function computes the hegemon’s utility under three strategic options:
#' \describe{
#'   \item{\strong{Ally}}{Trading or aligning with the military threat.}
#'   \item{\strong{Oppose}}{Trading with states that oppose the military threat.}
#'   \item{\strong{Defy}}{Opposing while also providing benefits to conditional states.}
#' }
#'
#' The plot includes dashed lines representing the regular states’ positions and
#' dotted lines for the theoretical thresholds \eqn{d^*} and \eqn{d^{**}}.
#'
#' @return A \code{ggplot} object displaying the hegemon’s utility as a function of
#' its position, under different strategic choices.
#'
#' @examples
#' \dontrun{
#' regular_positions <- seq(0.1, 0.9, by = 0.2)
#' utility_hegemonic(regular_positions)
#' }
#'
#' @import ggplot2
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr %>%
#'
#' @export
utility_hegemonic = function(regulars_position, alpha = 800, u = 400, tau = 600, c = 100){
  d_star = .8 - u/alpha
  d_2start = 1 - u/alpha

  m_allies = regulars_position[regulars_position < d_2start]
  m_opposers = regulars_position[regulars_position > d_2start]
  m_conditional = regulars_position[regulars_position < d_2start & regulars_position > d_star]

  useful_distance = seq(0, 1, by = .05)
  data = matrix(ncol = 4, nrow = length(useful_distance), dimnames = list(
    rep(NA, length(useful_distance)),
    c("hegemonic_position", "utility_a_h", "utility_o_h", 'utility_d_h')
  ))

  for (i in 1:length(useful_distance)) {
    hegemonic_position = useful_distance[i]
    utility_a_h = tau * sum(1 - abs(hegemonic_position - m_allies)) + tau*(1 - hegemonic_position)
    utility_o_h = tau * sum(1 - abs(hegemonic_position - m_opposers))
    utility_d_h = utility_o_h + tau * sum(1 - abs(hegemonic_position - m_conditional)) -
      c*(length(m_conditional) + length(m_opposers))
    data[i, ] = c(hegemonic_position, utility_a_h, utility_o_h, utility_d_h)
  }

  final_plot = data %>%
    as.data.frame() %>%
    pivot_longer(cols = utility_a_h:utility_d_h) %>%
    ggplot(aes(x = hegemonic_position, y = value, group = name, color = name)) +
    geom_line(linewidth = 1.2) +
    theme_bw() +
    theme(legend.position = 'top') +
    labs(x = 'Hegemonic Position', y = 'Utility', color = '') +
    scale_color_manual(
      values = c("utility_a_h" = "#d81a40","utility_o_h" = "#289866","utility_d_h" = "#2898e9"),
      labels = c("utility_a_h" = "Ally","utility_o_h" = "Oppose", "utility_d_h" = "Defy")
    ) +
    geom_vline(xintercept = regulars_position, linetype = 'dashed', alpha = .6) +
    geom_vline(xintercept = c(d_star, d_2start), linetype = 'dotted', linewidth = 1)

  return(final_plot)
}

