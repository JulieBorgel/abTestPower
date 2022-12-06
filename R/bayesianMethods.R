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
#' @param distribution of underlying A/B test data. See more in details
#' @details
#' * Only \code{bernoulli} & \code{bernoulliC} distribution are supported for now but the user can easily add a \code{tidyBayesTest} S3 method for the distribution
#' that suits better their data. The distribution that can be implemented are the one available in the bayesAB package see \code{\link[bayesAB]{bayesTest}}.
#' * Bernoulli can be used if the data is binary, modeled by 1s and 0s, according to a specific probability p of a 1 occurring. The conjugate Beta distribution for the parameter p should to be used.
#' * \code{bernoulli} is using Monte Carlo sampling to estimate the posterior probability while \code{BernoulliC} is computing the posterior probability analytically (faster but doesn't report the distribution over the posterior)
#' @md
#' @returns
#' \describe{
  #' \item{topScorerSampleSize}{numeric - top-scorer sample size}
  #' \item{lowScorerSampleSize}{numeric - low-scorer sample size}
  #' \item{probability}{numeric - Probability of top-scorer hit rate to be better than low-scorer hit rate P(topScorerHitRate > lowScorerhitRate)}
  #' \item{posteriorTopScorerData}{list - distribution over the posterior for top-scorer (if available)}
  #' \item{posteriorLowScorerData}{list - distribution over the posterior for low-scorer (if available)}
  #' }
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
                          distribution = c("bernoulli", "bernoulliC"),
                          nSimulatedData = 10,
                          n_samples = 1e+05) {
  simulateData <- getS3method("simulateData", distribution)
  abTest <- getS3method("tidyBayesTest", distribution)

  topScorerData <- rerun(nSimulatedData, map(topScorerSampleSize, ~ {
    simulateData(.x, size = 1, prob = topScorerHitRate)
  }))
  lowScorerData <- rerun(nSimulatedData, map(lowScorerSampleSize, ~ {
    simulateData(.x, size = 1, prob = lowScorerhitRate)
  }))

  abTestResults <- map2_dfr(topScorerData, lowScorerData, ~ {
    cross2(.x, .y) |>
      map_dfr(~ {
        abTestResults <- abTest(
          .[[1]],
          .[[2]],
          priors = priors,
          n_samples = n_samples
        )
        return(abTestResults)
      }) |>
      mutate(sampleSize = cross2(topScorerSampleSize, lowScorerSampleSize)) |>
      mutate(across(sampleSize, ~ map(., .f = ~ set_names(.x, c("topScorerSampleSize", "lowScorerSampleSize"))))) |>
      unnest_wider(sampleSize)
  }) |>
    group_by(.data$topScorerSampleSize, .data$lowScorerSampleSize) |>
    mutate(check = ifelse(probability > 0.9, 1, 0)) |>
    summarise(
      probability = exp(mean(log(.data$probability))),
      power = mean(check),
      credibleIntervalQ5 = across(any_of("credibleIntervalQ5"), ~list(.x)),
      credibleIntervalQ90 = across(any_of("credibleIntervalQ90"), ~list(.x)),
      posteriorTopScorerData = across(any_of("posteriorAdata"), ~list(.x)),
      posteriorLowScorerData = across(any_of("posteriorBdata"), ~list(.x)),
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
#' getS3method("tidyBayesTest", "bernoulli")(A_binom, B_binom,
#'   priors = c("alpha" = 1, "beta" = 1)
#' )
#' @export
#' @param ... S3 method compatibility
tidyBayesTest = function(A_data, ...) {

  UseMethod("tidyBayesTest", A_data)

}

#' @rdname tidyBayesTest
#' @method tidyBayesTest bernoulli
#' @inheritParams bayesAB::bayesTest
#' @importFrom bayesAB bayesTest
#' @importFrom data.table data.table
#' @importFrom glue glue
#' @seealso \code{\link[bayesAB]{bayesTest}}
#' @export
tidyBayesTest.bernoulli <- function(A_data,
                       B_data,
                       priors,
                       n_samples = 1e+05,
                       ...) {
  abTest <- bayesTest(A_data = A_data, B_data = B_data, priors = priors, n_samples = n_samples, distribution = "bernoulli")
  class(abTest) <- union(class(abTest), "list")

  tidyabTest <-
  data.table(list(abTest)) |>
    unnest_wider("V1") |>
    unnest_wider(posteriors) |>
    mutate(across(Probability, ~ map(., .f = ~ set_names(.x, c("posteriorAdata", "posteriorBdata"))))) |>
    unnest_wider(Probability) |>
    unnest_wider(inputs) |>
    unnest_wider(col = c(A_data, B_data, priors)) |>
    data.table()

  abTestSummary <- summary(abTest)
  class(abTestSummary) <- union(class(abTestSummary), "list")

  tidyabTestSummary <-
    data.table(list(abTestSummary)) |>
    unnest_wider("V1") |>
    select(-any_of("posteriorSummary")) |>
    unnest_wider(col = c(probability)) |>
    rename(probability = any_of("Probability")) |>
    mutate(across(interval, ~ map(., .f = ~ set_names(.x, c("credibleInterval"))))) |>
    unnest_wider(col = c(interval)) |>
    mutate(across(credibleInterval, ~ map(., .f = ~ set_names(.x, c("credibleIntervalQ5", "credibleIntervalQ90"))))) |>
    unnest_wider(col = c(credibleInterval)) |>
    mutate(across(posteriorExpectedLoss, ~ map(., .f = ~ set_names(.x, c("posteriorExpectedLoss"))))) |>
    unnest_wider(col = c(posteriorExpectedLoss)) |>
    data.table()

  return(bind_cols(tidyabTestSummary, tidyabTest))
}

#' @rdname tidyBayesTest
#' @method tidyBayesTest bernoulliC
#' @examples
#' A_binom <- rbinom(100, 1, .5)
#' B_binom <- rbinom(100, 1, .6)
#' getS3method("tidyBayesTest", "bernoulliC")(A_binom, B_binom,
#'   priors = c("alpha" = 1, "beta" = 1)
#' )
#' @export
tidyBayesTest.bernoulliC <- function(A_data,
                                    B_data,
                                    priors,
                                    ...) {
  abTest <- bayesTest(A_data = A_data, B_data = B_data, priors = priors, distribution = "bernoulliC")
  abTestSummary <- summary(abTest)
  abTestSummaryTable <- data.table(
    probability = abTestSummary$probability$Probability
  )
  return(abTestSummaryTable)
}


#' @rdname tidyBayesTest
#' @method tidyBayesTest default
#' @importFrom rlang abort
#' @importFrom stringr str_remove
#' @importFrom utils methods
#' @export
tidyBayesTest.default = function(A_data, ...){

  availableDispatchTypes = methods('tidyBayesTest') |>
    str_remove('tidyBayesTest.') |>
    setdiff('default')

  abort(
    str_c(
      'No know method to dispatch class(es): ',
      paste0(class(A_data), collapse = '; '),
      ' Available distribution are: ',
      paste0( availableDispatchTypes, collapse = '; ')
    )
  )
}
