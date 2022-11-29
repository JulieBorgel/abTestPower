
A_binom <- rbinom(100, 1, .5)
B_binom <- rbinom(100, 1, .6)
priors <- c("alpha" = 1, "beta" = 1)
distributionType = "bernoulli"
set.seed(1234)
tidyRes <- getS3method("tidyBayesTest", distributionType)(A_binom, B_binom, priors)
set.seed(1234)
res <- bayesTest(A_binom, B_binom, priors, distribution = "bernoulli") |>
  summary()

test_that("tidyBayesTest bernoulli works as expected", {
  expect_equal(tidyRes$probability, res$probability$Probability)

  expect_equal(unlist(tidyRes$interval), res$interval$Probability)

  expect_equal(quantile(unlist(tidyRes$posteriorAdata)), res$posteriorSummary$Probability$A)

  expect_equal(quantile(unlist(tidyRes$posteriorBdata)), res$posteriorSummary$Probability$B)

  expect_s3_class(tidyRes, "data.table")
})


distributionType = "bernoulliC"
tidyRes <- getS3method("tidyBayesTest", distributionType)(A_binom, B_binom, priors)
res <- bayesTest(A_binom, B_binom, priors, distribution = distributionType) |>
  summary()

test_that("tidyBayesTest bernoulliC works as expected", {
  expect_equal(tidyRes$probability, res$probability$Probability)

  expect_equal(unlist(tidyRes$interval), res$interval$Probability)

  expect_s3_class(tidyRes, "data.table")
})
