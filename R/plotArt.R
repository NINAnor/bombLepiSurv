#' plotArt
#'
#' Plot bumblebee and butterfly occurrences in the shiny app style
#'
#'
#'
#' @param obs observed community (Its species names will be used)
#' @param exp expected community
#'
#'
#'
#' @export

##This is old and pretty ugly. Should be replaced with a ggplot version.

plotArt <- function(obs,
                    exp,
                    ...) {

  sum.obs <- sort(colSums(obs[, 4:length(obs)] > 0), decreasing = T)
  sum.obs <- sum.obs[sum.obs > 0] ## Loose the zeroes
  perc.obs <- sum.obs / nrow(obs)

  cols <- exp$amount[match(names(perc.obs), exp$species)]
  cols[is.na(cols)] <- "black"
  cols[cols == ""] <- "black"
  cols[cols == "g"] <- "grey"
  cols[cols == "s"] <- "red"
  cols[cols == "m"] <- "blue"
  cols[cols == "v"] <- "green"

  x.ticks <- seq(0.15, 1, 0.05)

  max <- min(x.ticks[x.ticks > max(perc.obs)])

  par(mar = c(5, 12, 4, 2))
  barplot(
    rep(0, length(perc.obs)),
    horiz = T,
    xlim = c(0, max),
    axes = F,
    col = cols,
    plot = T,
    xlab = "",
    ylab = ""
  )
  axis(1, seq(0, max, 0.05), labels = seq(0, max * 100, 5))
  lim <- par("usr")
  rect(
    0,
    0,
    0.01,
    lim[4],
    col = rgb(255, 0, 0, alpha = 100, maxColorValue = 255),
    border = NA
  )
  rect(
    0.01,
    0,
    0.05,
    lim[4],
    col = rgb(0, 0, 255, alpha = 100, maxColorValue = 255),
    border = NA
  )
  rect(
    0.05,
    0,
    lim[2],
    lim[4],
    col = rgb(0, 255, 0, alpha = 100, maxColorValue = 255),
    border = NA
  )
  barplot(
    perc.obs,
    horiz = T,
    las = 1,
    xlim = c(0, max),
    axes = F,
    col = cols,
    add = T,
    ...
  )
  #abline(v=c(0.01,0.05),lty=2,col="black",lwd=1.5)
  lines(
    x = c(0.01, 0.01),
    y = c(0, lim[4]),
    lty = 2,
    col = "black",
    lwd = 1.5
  )
  lines(
    x = c(0.05, 0.05),
    y = c(0, lim[4]),
    lty = 2,
    col = "black",
    lwd = 1.5
  )
  mtext("Prosentvis transektforekomst",
        side = 1,
        line = 3)

}
