hitRateA = 0.05
hitRateB = 0.01
pvalue = 0.05
sampleSizeA <- c(10, 80, 150)
sampleSizeB <- c(10, 100)

abTestPowerRes <- runFrequentistPower(hitRateA,
  sampleSizeA[[1]],
  hitRateB,
  sampleSizeB[[1]],
  sig.level = pvalue,
  alternative = "greater"
)

bayesABres <- pwr.2p2n.test(
  h = ES.h(hitRateA, hitRateB),
  n1 = sampleSizeA[[1]],
  n2 = sampleSizeB[[1]],
  sig.level = pvalue,
  alternative = "greater"
)

abTestPowerResMultiple <- runFrequentistPower(hitRateA,
  sampleSizeA,
  hitRateB,
  sampleSizeB,
  sig.level = pvalue,
  alternative = "greater"
)

test_that("abTestPowerRes works as expected", {
  expect_equal(abTestPowerRes$power, bayesABres$power)

  expect_s3_class(abTestPowerRes, "data.table")

  expect_equal(nrow(abTestPowerResMultiple), length(sampleSizeA) * length(sampleSizeB))
})


test_that("plot works as expected", {
  expect_s3_class(plot(abTestPowerResMultiple, topScorerSampleSize, lowScorerSampleSize, power), "ggplot")
})
