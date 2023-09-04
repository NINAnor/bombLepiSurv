#' plot.comm_index Plot function for comm_index class
#'
#' Just an ugly stub at this point
#'
#' @param x a comm_index object
#' @param ...
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#'
#' \dontrun{
#' plot(sInd2022OstGress)
#'
#'
#' }
plot.comm_index <- function(x,
                            ...){

  to_plot <- tibble(boot_vals = x[[7]])

  point_est <- x[[1]][3]
  lower_2.5 <- x[[1]][1]
  upper_97.5 <- x[[1]][5]


  p <- ggplot(to_plot) +
       geom_density(aes(y = boot_vals)) +
       coord_flip()

  p <- p +
    geom_hline(aes(yintercept = point_est),
               lty = 2,
               col = NinaR::nina_colors[3])

  p <- p +
    geom_hline(aes(yintercept = lower_2.5),
               lty = 2,
               col = NinaR::nina_colors[2])
  p <- p +
    geom_hline(aes(yintercept = upper_97.5),
               lty = 2,
               col = NinaR::nina_colors[2])

  return(p)

}
