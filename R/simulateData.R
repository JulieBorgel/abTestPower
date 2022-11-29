#' Random generation for some data distribution
#' @param ... S3 method compatibility
simulateData = function(...) {

  UseMethod("simulateData", ...)

}

#' @rdname simulateData
#' @method simulateData bernoulli
#' @importFrom stats rbinom
#' @inheritParams stats::rbinom
simulateData.bernoulli = function (n, size, prob, ...){

rbinom(n = n, size = size, prob = prob)

}

#' @rdname simulateData
#' @method simulateData bernoulliC
#' @importFrom stats rbinom
#' @inheritParams stats::rbinom
simulateData.bernoulliC = function (n, size, prob, ...){

  simulateData.bernoulli(n = n, size = size, prob = prob)

}
