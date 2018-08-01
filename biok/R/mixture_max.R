#' @export
mixture_max = function(x,d,sd){
  return (d**2/(x*sd**2*(1+(sd*sqrt(2/pi))**2/sd**2)**2))
}