#' Frequentist power analysis for 2 proportions
#'
#' Runs Frequentist power analysis for 2 proportions on multiple sample sizes
#'
#' @inheritParams runABbayesTest
#' @inheritParams tidyPwr2p2nTest
#' @export
#' @importFrom pwr ES.h
#' @importFrom dplyr any_of
#' @seealso \code{\link[pwr]{pwr.2p2n.test}}
runFrequentistPower <- function(topScorerHitRate,
                             topScorerSampleSize,
                             lowScorerhitRate,
                             lowScorerSampleSize,
                             sig.level = 0.05,
                             power = NULL,
                             alternative = c("two.sided", "less", "greater")) {
  hCohen <- ES.h(topScorerHitRate, lowScorerhitRate)

  pwrTestResults <- cross2(topScorerSampleSize, lowScorerSampleSize) |>
    map_dfr(~ {
      tidyPwr2p2nTest(hCohen,
        n1 = .[[1]],
        n2 = .[[2]],
        sig.level = sig.level,
        alternative = alternative,
        power = power
      )
    }) |>
    rename(effectSize = any_of("h"),
           topScorerSampleSize = any_of("n1"),
           lowScorerSampleSize = any_of("n2"),
           pvalue = any_of("sig.level")) |>
    data.table()

  setattr(pwrTestResults, "class", union("powerTestResults", class(pwrTestResults)))

  return(pwrTestResults)
}

#' pwr.2p2n.test tidy wrapper
#'
#' Wrapper for \code{\link[pwr]{pwr.2p2n.test}} which returns a tidy table instead of a messy list
#' @keywords internal
#' @inheritParams pwr::pwr.2p2n.test
#' @importFrom pwr pwr.2p2n.test
#' @importFrom dplyr rename
#' @importFrom tidyr unnest_wider
#' @seealso \code{\link[pwr]{pwr.2p2n.test}}
tidyPwr2p2nTest <- function(h = NULL, n1 = NULL, n2 = NULL, sig.level = 0.05, power = NULL,
                            alternative = c("two.sided", "less", "greater")) {
  powerResults <- pwr.2p2n.test(
    h = h, n1 = n1, n2 = n2, sig.level = sig.level, power = power,
    alternative = alternative
  )

  class(powerResults) <- "list"

  tidyPowerResults <-
    powerResults |>
    list() |>
    data.table() |>
    unnest_wider(any_of("V1"))

  return(tidyPowerResults)
}
