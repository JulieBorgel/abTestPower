#' bayesTest wrapper
#'
#' Wrapper to run multiple bayesian A/B tests on lists of A_data, B_data and priors.
#'

#' @returns Returns bayesTest results in a tidy table
#' @param topScorerHitRate Numeric, hit rate for the top scorer
#' @param topScorerSampleSize Named numeric vector, group size for the top scorer
#' @param lowScorerhitRate Numeric, hit rate for the low scorer
#' @param lowScorerSampleSize Named numeric vector, group size for the low scorer
#' @param nSimulatedData number of trials for the simulated data
#' @inheritParams tidyBayesTest
#' @export
#' @importFrom purrr map cross2 map_dfr set_names rerun map2_dfr
#' @importFrom tidyr unnest_wider
#' @importFrom dplyr across mutate summarise group_by
#' @importFrom data.table setattr
#' @importFrom rlang .data :=
#' @importFrom utils getS3method
runABbayesTest <- function(topScorerHitRate,
                          topScorerSampleSize,
                          lowScorerhitRate,
                          lowScorerSampleSize,
                          priors,
                          distribution = c(
                            "bernoulli", "normal", "lognormal", "poisson", "exponential",
                            "uniform", "bernoulliC", "poissonC"
                          ),
                          nSimulatedData = 10,
                          n_samples = 1e+05) {
  simulateData <- getS3method("randomSample", distribution)

  topScorerData <- rerun(nSimulatedData, map(topScorerSampleSize, ~ {
    simulateData(.x, size = 1, prob = topScorerHitRate)
  }))
  lowScorerData <- rerun(nSimulatedData, map(lowScorerSampleSize, ~ {
    simulateData(.x, size = 1, prob = lowScorerhitRate)
  }))

  abTestResults <- map2_dfr(topScorerData, lowScorerData, ~ {
    cross2(.x, .y) |>
      map_dfr(~ {
        abTestResults <- tidyBayesTest(
          .[[1]],
          .[[2]],
          priors = priors,
          n_samples = n_samples,
          distribution = distribution
        )
        return(abTestResults)
      }) |>
      mutate(sampleSize = cross2(topScorerSampleSize, lowScorerSampleSize)) |>
      mutate(across(sampleSize, ~ map(., .f = ~ set_names(.x, c("topScorerSampleSize", "lowScorerSampleSize"))))) |>
      unnest_wider(sampleSize)
  }) |>
    group_by(.data$topScorerSampleSize, .data$lowScorerSampleSize) |>
    summarise(
      probability = exp(mean(log(.data$probability))),
      posteriorAdata = list(unlist(.data$posteriorAdata, use.names = FALSE)),
      posteriorBdata = list(unlist(.data$posteriorBdata, use.names = FALSE)),
      .groups = "keep"
    ) |>
    data.table()

  setattr(abTestResults, "class", union("bayesTestResults", class(abTestResults)))
}

#' bayesTest wrapper
#'
#' Wrapper for \code{\link[bayesAB]{bayesTest}} which returns a tidy table instead of a messy list
#' @examples
#' A_binom <- rbinom(100, 1, .5)
#' B_binom <- rbinom(100, 1, .6)
#' tidyBayesTest(A_binom, B_binom,
#'   priors = c("alpha" = 1, "beta" = 1),
#'   distribution = "bernoulli"
#' )
#' @export
#' @inheritParams bayesAB::bayesTest
#' @importFrom bayesAB bayesTest
#' @importFrom data.table data.table
#' @importFrom glue glue
#' @seealso \code{\link[bayesAB]{bayesTest}}
tidyBayesTest <- function(A_data,
                       B_data,
                       priors,
                       n_samples = 1e+05,
                       distribution = c(
                         "bernoulli", "normal", "lognormal", "poisson", "exponential",
                         "uniform", "bernoulliC", "poissonC"
                       )) {
  abTest <- bayesTest(A_data = A_data, B_data = B_data, priors = priors, n_samples = n_samples, distribution = distribution)
  abTestSummary <- summary(abTest)
  abTestSummaryTable <- data.table(
    probability = abTestSummary$probability$Probability,
    interval = list(abTestSummary$interval$Probability),
    posteriorAdata = list(abTest$posteriors$Probability$A),
    posteriorBdata = list(abTest$posteriors$Probability$B)
  )
  return(abTestSummaryTable)
}
