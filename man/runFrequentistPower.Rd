% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/frequentistMethods.R
\name{runFrequentistPower}
\alias{runFrequentistPower}
\title{Frequentist power analysis for 2 proportions}
\usage{
runFrequentistPower(
  topScorerHitRate,
  topScorerSampleSize,
  lowScorerhitRate,
  lowScorerSampleSize,
  sig.level = 0.05,
  power = NULL,
  alternative = c("two.sided", "less", "greater")
)
}
\arguments{
\item{topScorerHitRate}{Numeric, hit rate for the top scorer}

\item{topScorerSampleSize}{Named numeric vector, group size for the top scorer}

\item{lowScorerhitRate}{Numeric, hit rate for the low scorer}

\item{lowScorerSampleSize}{Named numeric vector, group size for the low scorer}

\item{sig.level}{Significance level (Type I error probability)}

\item{power}{Power of test (1 minus Type II error probability)}

\item{alternative}{a character string specifying the alternative hypothesis,
          must be one of "two.sided" (default), "greater" or
          "less"}
}
\value{
\describe{
\item{effectSize}{numeric - Cohen's index Effect size \code{\link[pwr]{ES.h}} }
\item{topScorerSampleSize}{numeric - top-scorer sample size}
\item{lowScorerSampleSize}{numeric - low-scorer sample size}
\item{pvalue}{numeric - Significance level (Type I error probability)}
\item{power}{numeric - Power of test (1 minus Type II error probability)}
\item{alternative}{character - alternative hypothesis}
\item{note}{character - Notes}
\item{alternative}{character - Method used to run the power analysis}
}
}
\description{
Runs Frequentist power analysis for 2 proportions on multiple sample sizes
}
\seealso{
\code{\link[pwr]{pwr.2p2n.test}}
}
