#' dHondt function (Vector entry)
#'
#' This function automatically apportions seats proportional to the population.
#' @param n Number of seats to apportion.
#' @param x Vector of population inputs.
#' @param vis Default TRUE. Do you want to display the output or not?
#' @export
dhondtV<-function(n,x,vis=TRUE){
  if(length(x)>12){stop("Too long.")}
  V<-vector("numeric",12)
  for(i in 1:length(x)){V[i]<-x[i]}
  X<-dhondt(n,V[1],V[2],V[3],V[4],V[5],V[6],V[7],V[8],V[9],V[10],V[11],V[12],vis = FALSE)
  X<-X[1:length(x)];if(vis==TRUE){print(X)}
}