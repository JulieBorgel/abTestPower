tidyPowerRes <- tidyPwr2p2nTest(h = 0.30, n1 = 80, n2 = 245, sig.level = 0.05, alternative = "greater")

powerRes <- pwr.2p2n.test(h = 0.30, n1 = 80, n2 = 245, sig.level = 0.05, alternative = "greater")

class(powerRes) <- "list"

test_that("tidyPwr2p2nTest works as expected", {
  expect_equal(as.list(tidyPowerRes), powerRes)

  expect_s3_class(tidyPowerRes, "data.frame")
})


test_that("tidyPwr2p2nTest Fails", {
  expect_error(tidyPwr2p2nTest(h = 0.30, n1 = 80, n2 = 245, sig.level = 0.05, power = 0.5, alternative = "greater"))
})
