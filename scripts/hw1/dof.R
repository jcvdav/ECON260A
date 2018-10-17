dof <- function(H, X, a, b, delta, r, K, Xgrid, V, z = z) {
  Pi <- a * H - b * (H ^ 2)
  
  Xprime <- z*((X - H) + r * (X - H) * (1 - (X - H) / K))
  
  Vnext <- spline(Xgrid, V, xout = Xprime, method = "natural")
  
  negout<- -(Pi + delta * Vnext$y)
  
  return(negout)
}