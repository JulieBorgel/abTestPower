hitRateA = 0.05
hitRateB = 0.01
sampleSizeA <- c(10, 80, 150)
sampleSizeB <- c(10, 100)
priors <- c("alpha" = 1, "beta" = 1)
nSimulatedData = 3
n_samples = 100
abTestResults <- runABbayesTest(hitRateA,
                                      sampleSizeA,
                                      hitRateB,
                                      sampleSizeB,
                                 priors = priors,
                                 distribution = "bernoulli",
                                 nSimulatedData = nSimulatedData,
                                n_samples = n_samples
)

abTestResultsClosed <- runABbayesTest(hitRateA,
                                sampleSizeA,
                                hitRateB,
                                sampleSizeB,
                                priors = priors,
                                distribution = "bernoulliC",
                                nSimulatedData = nSimulatedData)

test_that("runABbayesTest works as expected", {

  expect_s3_class(abTestResults, "data.table")

  expect_equal(nrow(abTestResults), length(sampleSizeA) * length(sampleSizeB))

  expect_equal(unique(sapply(abTestResults$posteriorTopScorerData, length)), nSimulatedData * n_samples)

  expect_s3_class(abTestResultsClosed, "data.table")

  expect_equal(nrow(abTestResultsClosed), length(sampleSizeA) * length(sampleSizeB))

})


test_that("plot works as expected", {
  expect_s3_class(plot(abTestResults, topScorerSampleSize, lowScorerSampleSize, probability), "ggplot")
  expect_s3_class(plot(abTestResultsClosed, topScorerSampleSize, lowScorerSampleSize, probability), "ggplot")
})
