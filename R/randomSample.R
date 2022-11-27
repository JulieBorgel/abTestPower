#' Random generation for some data distribution
#' @param ... S3 method compatibility
randomSample = function(...) {

  UseMethod("randomSample", ...)

}

#' @rdname randomSample
#' @method randomSample bernoulli
#' @importFrom stats rbinom
#' @inheritParams stats::rbinom
randomSample.bernoulli = function (n, size, prob, ...){

rbinom(n = n, size = size, prob = prob)

}
