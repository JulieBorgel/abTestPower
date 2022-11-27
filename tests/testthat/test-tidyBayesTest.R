
A_binom <- rbinom(100, 1, .5)
B_binom <- rbinom(100, 1, .6)
priors <- c("alpha" = 1, "beta" = 1)
set.seed(1234)
tidyRes <- tidyBayesTest(A_binom, B_binom, priors, distribution = "bernoulli")
set.seed(1234)
res <- bayesTest(A_binom, B_binom, priors, distribution = "bernoulli") |>
  summary()

test_that("tidyBayesTest works as expected", {
  expect_equal(tidyRes$probability, res$probability$Probability)

  expect_equal(unlist(tidyRes$interval), res$interval$Probability)

  expect_equal(quantile(unlist(tidyRes$posteriorAdata)), res$posteriorSummary$Probability$A)

  expect_equal(quantile(unlist(tidyRes$posteriorBdata)), res$posteriorSummary$Probability$B)

  expect_s3_class(tidyRes, "data.table")
})
