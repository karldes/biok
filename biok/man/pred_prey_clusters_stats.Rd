% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pred_prey_clusters_stats.R
\name{pred_prey_clusters_stats}
\alias{pred_prey_clusters_stats}
\title{pred_prey_clusters_stats Function}
\usage{
pred_prey_clusters_stats(from, to, MC = 1, mean = 0, sd = 0.1, C = 0.1,
  precision = 0.001)
}
\arguments{
\item{from}{the smallest number of species in the network (default = 2)}

\item{to}{the biggest number of species in the network (default = 70)}

\item{MC}{the number of Monte Carlo simulations (default = 10)}

\item{mean}{the mean (default = 0)}

\item{sd}{the standard deviation (default = 0.1)}

\item{C}{the connectance (default = 0.1)}

\item{precision}{the precision used to convert matrix from quantitative coefficients to binary coefficients (treshold size) (default = 0.001)}
}
\description{
This function returns 3 graphs presenting:
- The number of clusters for different sizes of population
- The average size of clusters for different sizes of population
- The stability of netowrks for different number of clusters
}
