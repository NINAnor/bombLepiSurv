#' calcInd
#'
#' Calculate indicator values, with uncertainty
#'
#'
#' @param obs Optional. Dataframe or tibble of observation community. Typically gotten from
#' @param exp Optional. Expectation community. Typically gotten from
#' @param type Needed if obs and exp not provided. Type of taxa (Bumblebees, Butterflies)
#' @param region_short Needed if obs and exp not provided. Shortname of region (sor, trond, ost, vest).
#' @param habitat Needed if obs and exp not provided. Type of habitat (skogsmark, gressmark).
#' @param year Needed if obs not provided. Year of observations.
#' @param aWeight Named vector of weights for amounts. Relative weights for common, average and rare species. Typically gotten from
#' @param dWeights Named matrix of differnce weights. Relative weights for differences between expectation and observation communities.
#' Typically gotten from
#' @param nIter Numeric. How many replicate draws for the uncertainty calculations. Set 0 or NULL for no uncertainty estimation.
#' @param save.draws Boolean. Should the draws be saved
#' @param digits Integer. How many digits of precision to use.
#'
#'
#' @examples
#' \dontrun{
#'
#' hInd2023TrondGress <- calcInd(type = "Bumblebees",
#' region_short = "Trond",
#' habitat = "Gressmark",
#' year = 2023,
#' nIter = nIter,
#' save.draws = T)
#'
#' }
#'
#'
#'
#' @export
#'
#'


calcInd <- function(obs = NULL,
                    exp = NULL,
                    type = NULL,
                    region_short = NULL,
                    habitat = NULL,
                    year = NULL,
                    aWeight = getAmountWeights(),
                    dWeight = getDiffWeights(),
                    nIter = 9,
                    save.draws = T,
                    digits = 7) {
  ## Kommentera denna funktion bättre så man förstår vad den gör

  if (is.null(obs)) {
    obs <- getComplData(
      type = type,
      region_short = region_short,
      habitat = habitat,
      year = year
    )
  }

  if (is.null(exp)) {
    exp <- getExpValues(
      type = type,
      region_short = region_short,
      habitat = habitat
    )
  }

  ## check if all reported species names match with the expexted communities'
  # if(!all(colnames(obs)[3:length(obs)] %in% exp$species))
  # stop("Species names don't match") else

  ## check which expected species are not recorded and therefor not in the obs data set
  exp <- exp %>%
    filter(amount == "v" | amount == "m" | amount == "s")

  # missing <- exp$species[!(exp$species %in% colnames(obs))]
  missing <- exp %>%
    filter(!(species_latin %in% colnames(obs))) %>%
    select(species_latin)


  miss.add <- matrix(0,
    nrow = nrow(obs),
    ncol = nrow(missing),
    dimnames = list(
      1:nrow(obs),
      missing$species_latin
    )
  )
  obs <- cbind(obs, miss.add)

  ## Check which observed species are not in the expected communities
  # not_zeroes <- obs[3:length(obs)][colSums(obs[3:length(obs)])>0] # ,3:length(obs)]
  not_zeroes <- obs %>%
    select(-c(Flate, Transekt)) %>%
    select(which(colSums(.) > 0))

  not_in_ref_comm <- not_zeroes %>%
    select(which(!(colnames(not_zeroes) %in% exp$species_latin))) %>%
    colnames()

  ## Calculate the point estimate
  # sum.obs<-sort(colSums(obs[,3:length(obs)]>0),decreasing=T)
  sum.obs <- obs %>%
    select(-c(Flate, Transekt)) %>%
    mutate_all(~ (.) > 0) %>%
    colSums() %>%
    sort(decreasing = T)

  # sum.obs<-sum.obs[sum.obs>0] ## Loose the zeroes
  perc.obs <- sum.obs / nrow(obs)

  n_v <- perc.obs[names(perc.obs) %in% exp$species_latin[exp$amount == "v"]]
  n_m <- perc.obs[names(perc.obs) %in% exp$species_latin[exp$amount == "m"]]
  n_s <- perc.obs[names(perc.obs) %in% exp$species_latin[exp$amount == "s"]]

  n_vcopy <- n_v
  n_mcopy <- n_m
  n_scopy <- n_s

  # Register discrepancies from expected to observed frequencies
  n_vcopy[n_v >= 0.05] <- dWeight["v", "v"]
  n_vcopy[n_v < 0.05 & n_v >= 0.01] <- dWeight["m", "v"]
  n_vcopy[n_v < 0.01 & n_v > 0] <- dWeight["s", "v"]
  n_vcopy[n_v == 0] <- dWeight["t", "v"]

  n_mcopy[n_m >= 0.05] <- dWeight["v", "m"]
  n_mcopy[n_m < 0.05 & n_m >= 0.01] <- dWeight["m", "m"]
  n_mcopy[n_m < 0.01 & n_m > 0] <- dWeight["s", "m"]
  n_mcopy[n_m == 0] <- dWeight["t", "m"]

  n_scopy[n_s >= 0.05] <- dWeight["v", "s"]
  n_scopy[n_s < 0.05 & n_s >= 0.01] <- dWeight["m", "s"]
  n_scopy[n_s < 0.01 & n_s > 0] <- dWeight["s", "s"]
  n_scopy[n_s == 0] <- dWeight["t", "s"]

  n_vm <- n_v[n_v < 0.05 & n_v >= 0.01]
  n_vs <- n_v[n_v < 0.01 & n_v > 0]
  n_vt <- n_v[n_v == 0]
  n_ms <- n_m[n_m < 0.01 & n_m > 0]
  n_mt <- n_m[n_m == 0]
  n_st <- n_s[n_s == 0]


  RT <- length(n_v) * aWeight$v + length(n_m) * aWeight$m + length(n_s) * aWeight$s

  ET <- length(n_vm) * dWeight["m", "v"] + length(n_vs) * dWeight["s", "v"] + length(n_vt) * dWeight["t", "v"] +
    length(n_ms) * dWeight["s", "m"] + length(n_mt) * dWeight["t", "m"] +
    length(n_st) * dWeight["t", "s"]

  point.est <- (RT - ET) / RT

  v.miss <- 1 - n_vcopy / aWeight[[1]]
  m.miss <- 1 - n_mcopy / aWeight[[2]]
  s.miss <- 1 - n_scopy / aWeight[[3]]
  v.val <- v.miss * aWeight[[1]] / RT
  m.val <- m.miss * aWeight[[2]] / RT
  s.val <- s.miss * aWeight[[3]] / RT

  tt <- c(v.val, m.val, s.val)
  point.est.species <- tt[match(exp$species_latin, names(tt))]

  point.est.ind <- sum(point.est.species) ## Does work! Wohoo!

  if (!round(point.est.ind, 7) == round(point.est, 7)) {
    stop("Community and species calculations don't match!")
  } else

  ## Perform the randomizations

  if (!is.null(nIter) & nIter >= 0) {
    Spec.ind <- matrix(nrow = nrow(exp), ncol = nIter, dimnames = list(exp$species_latin, 1:nIter))
    SI.ind <- numeric()
    SI <- numeric()
    FL <- unique(obs$Flate)
    rand <- replicate(nIter, sample(FL, length(FL), replace = T))

    for (i in 1:nIter) {
      i.obs <- data.frame()
      for (j in 1:nrow(rand)) {
        i.obs <- rbind(i.obs, obs[obs$Flate == rand[j, i], ])
      }

      # sum.obs<-sort(colSums(i.obs[,3:length(i.obs)]>0),decreasing=T)
      sum.obs <- i.obs %>%
        select(-c(Flate, Transekt)) %>%
        mutate_all(~ (.) > 0) %>%
        colSums() %>%
        sort(decreasing = T)

      # sum.obs<-sum.obs[sum.obs>0] ## Loose the zeroes
      perc.obs <- sum.obs / nrow(i.obs)

      n_v <- perc.obs[names(perc.obs) %in% exp$species_latin[exp$amount == "v"]]
      n_m <- perc.obs[names(perc.obs) %in% exp$species_latin[exp$amount == "m"]]
      n_s <- perc.obs[names(perc.obs) %in% exp$species_latin[exp$amount == "s"]]

      n_vcopy <- n_v
      n_mcopy <- n_m
      n_scopy <- n_s

      n_vcopy[n_v >= 0.05] <- dWeight["v", "v"]
      n_vcopy[n_v < 0.05 & n_v >= 0.01] <- dWeight["m", "v"]
      n_vcopy[n_v < 0.01 & n_v > 0] <- dWeight["s", "v"]
      n_vcopy[n_v == 0] <- dWeight["s", "v"]

      n_mcopy[n_m >= 0.05] <- dWeight["v", "m"]
      n_mcopy[n_m < 0.05 & n_m >= 0.01] <- dWeight["m", "m"]
      n_mcopy[n_m < 0.01 & n_m > 0] <- dWeight["s", "m"]
      n_mcopy[n_m == 0] <- dWeight["t", "m"]

      n_scopy[n_s >= 0.05] <- dWeight["v", "s"]
      n_scopy[n_s < 0.05 & n_s >= 0.01] <- dWeight["m", "s"]
      n_scopy[n_s < 0.01 & n_s > 0] <- dWeight["s", "s"]
      n_scopy[n_s == 0] <- dWeight["t", "s"]

      n_vm <- n_v[n_v < 0.05 & n_v >= 0.01]
      n_vs <- n_v[n_v < 0.01 & n_v > 0]
      n_vt <- n_v[n_v == 0]
      n_ms <- n_m[n_m < 0.01 & n_m > 0]
      n_mt <- n_m[n_m == 0]
      n_st <- n_s[n_s == 0]


      RT <- length(n_v) * aWeight$v + length(n_m) * aWeight$m + length(n_s) * aWeight$s

      ET <- length(n_vm) * dWeight["m", "v"] + length(n_vs) * dWeight["s", "v"] + length(n_vt) * dWeight["t", "v"] +
        length(n_ms) * dWeight["s", "m"] + length(n_mt) * dWeight["t", "m"] +
        length(n_st) * dWeight["t", "s"]

      v.miss <- 1 - n_vcopy / aWeight[[1]]
      m.miss <- 1 - n_mcopy / aWeight[[2]]
      s.miss <- 1 - n_scopy / aWeight[[3]]
      v.val <- v.miss * aWeight[[1]] / RT
      m.val <- m.miss * aWeight[[2]] / RT
      s.val <- s.miss * aWeight[[3]] / RT

      tt <- c(v.val, m.val, s.val)

      Spec.ind[, i] <- tt[match(exp$species_latin, names(tt))]
      SI.ind[i] <- sum(v.val, m.val, s.val)
      SI[i] <- (RT - ET) / RT
    }


    SI.limits <- quantile(SI, c(0.025, 0.05, 0.95, 0.975))
    SI.ind.limits <- quantile(SI.ind, c(0.025, 0.05, 0.95, 0.975))
    Spec.ind.limits <- t(apply(Spec.ind, 1, function(x) quantile(x, c(0.025, 0.05, 0.95, 0.975))))
    SI.limits.spec.calc <- colSums(Spec.ind.limits)

    out.comm <- c(SI.limits[1:2], point.est, SI.limits[3:4])
    names(out.comm)[3] <- "Point estimate"

    # out.species<-cbind(Spec.ind.limits[,1:2],point.est.species,Spec.ind.limits[,3:4])
    # colnames(out.species)[3]<-"Point estimates"
    out.species <- as.data.frame(point.est.species)

    dist.cols <- c(0 / RT, 0.25 / RT, 0.5 / RT, 0.75 / RT, 1 / RT) ## This is all possible values


    dist.table <- matrix(
      ncol = length(dist.cols), nrow = nrow(Spec.ind),
      dimnames = list(rownames(Spec.ind), round(dist.cols, 5))
    )


    for (i in 1:nrow(Spec.ind)) {
      dist.table[i, ] <- sapply(1:length(dist.cols), function(x) sum(Spec.ind[i, ] == dist.cols[x]) / nIter)
    }
    # dist.table<-round(dist.table,digits)

    if (!all(round(rowSums(dist.table), 5) == 1)) stop("Distributions don't sum to 1. Increase digits")

    ## Make a list of the maximum values for each species index

    exp <- exp %>%
      mutate(max = 0)

    exp$max[exp$amount == "v"] <- 1 / RT
    exp$max[exp$amount == "m"] <- 0.75 / RT
    exp$max[exp$amount == "s"] <- 0.5 / RT

    if (save.draws == T) {
      out <- list(
        "Community indicator estimates, with percentiles" = out.comm,
        "Species indicator estimates. These sum to the community index" = out.species,
        "Distribution table of index values. Index values in column names,
          \n  percentage of estimates in rows" = dist.table,
        "Max values for species indices " = exp[c(1, 3)],
        "Number of randomizations" = nIter,
        "Observed species not in reference comunity" = not_in_ref_comm,
        "Randomization draws, community" = SI, "Randomization draws,
          species" = Spec.ind
      )
    } else {
      out <- list(
        "Community indicator estimates, with percentiles" = out.comm,
        "Species indicator estimates. These sum to the community index" = out.species,
        "Distribution table of index values. Index values in column names,
                 \n  percentage of estimates in rows" = dist.table,
        "Max values for species indices " = exp[c(1, 3)],
        "Number of randomizations" = nIter,
        "Observed species not in reference comunity" = not_in_ref_comm
      )
    }
  } else {
    out <- list(
      "Indicator value" = point.est, "Indicator value, spec" = point.est.species,
      "Observed species not in reference comunity" = not_in_ref_comm
    )
  }

  class(out) <- c("comm_index", class(out))

  return(out)
}
