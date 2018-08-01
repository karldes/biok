#' network_frame Function
#'
#' This function returns a tidyverse tibble containing networks of a certain type (random/predator-prey/mixture)
#' and statistics on them: type, network size, average degree, number of clusters, average size of clusters, 
#' stability of the network, connectance, mean and sd
#' @param type the type of networks we want to create (random, pred_prey, mixture)
#' @param from smallest size of population
#' @param to biggest size of population
#' @param mean the mean used in the gaussian distribution
#' @param sd the standard deviation used in the gaussian distribution
#' @param C the connectance
#' @param MC set it if MC simulations are desired
#' @param precision is the treshold size used to convert to binary networks
#' @export
network_frame = function (type, from = 2, to = 50, mean = 0, sd = 0.1, C = 0.1, MC = 1, precision = 0.01){
  rows = to-from+1
  types = rep(c(type),rows)
  sizes = c(from:to)
  avg_degrees = c()
  nb_of_clusters = c()
  avg_size_clusters = c()
  stability_of_networks = c()
  connectance = rep(c(C),rows)
  mean = rep(c(mean),rows)
  sd = rep(c(sd),rows)
  if (type == "random"){
    print(sd)
    result = random_clusters_stats(from,to,MC,mean,sd,C,precision)
    nb_of_clusters = result[1]
    avg_size_clusters = result[2]
    stability_of_networks = result[3]
    avg_degrees = result[4]
  }
  if (type == "pred_prey"){
    result = pred_prey_clusters_stats(from,to,MC,mean,sd,C,precision)
    nb_of_clusters = result[1]
    avg_size_clusters = result[2]
    stability_of_networks = result[3]
    avg_degrees = result[4]
  }
  if (type == "mixture"){
    result = random_clusters_stats(from,to,MC,mean,sd,C,precision)
    nb_of_clusters = result[1]
    avg_size_clusters = result[2]
    stability_of_networks = result[3]
    avg_degrees = result[4]
  }
  df = data.frame(types,sizes,avg_degrees,nb_of_clusters,avg_size_clusters,stability_of_networks,connectance,mean,sd)
  colnames(df)[3] <- "avg_degrees"
  colnames(df)[4] <- "nb_of_clusters"
  colnames(df)[5] <- "avg_size_clusters"
  colnames(df)[6] <- "stable ?"
  return (as.tibble(df))
}