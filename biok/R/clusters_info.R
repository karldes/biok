#'clusters_info Function
#'
#' This function returns the number of clusters and their average size
#' @param type the type of the network
#' @param S size of the population
#' @param precision the treshold size (default = 0.001)
#' @export
#' 
clusters_info = function(type,S,mean,sd,C,precision){
  vecteur_pop = c(1:S)
  actors = data.frame(vecteur_pop)
  relationship = relationships(type,S,mean,sd,C,precision)
  avg_deg = relationship[4]
  if (relationship[1] != 0){
    relationships <- data.frame(from = relationship[2],to = relationship[3] )
    g = graph_from_data_frame(relationships, directed=FALSE, vertices=actors)
    cluster_edge = cluster_edge_betweenness(g)
    n = length(cluster_edge)
    count = 0
    avg = 0
    for (i in 1:n){
      size = nrow(data.frame(cluster_edge[i]))
      if (size > 1){
        count = count + 1
        avg = avg + size
      }
    }
    if (count == 0) print("No clusters found")
    avg = avg/count
    return (list(count,avg,avg_deg,relationship[5]))
  }
  else {
    return (list(0,0,avg_deg,relationship[5]))
  }
}