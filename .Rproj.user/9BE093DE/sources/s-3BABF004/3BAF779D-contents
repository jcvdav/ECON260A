call_dof <- function(XL = 50, T_fin = 30, delta = 0.9, a = 20, b = 0.6, r = 0.3, K = 100, seed = set.seed(42), z = 1, returnHs = FALSE){
  #Grid over state space
  Xgrid <- seq(from = 0.1,
               to = K,
               length.out = XL)
  
  # Setting V = 0 at T_fin + 1
  Vmat <-  matrix(data = 0,
                  nrow = XL,
                  ncol = T_fin + 1)
  
  #Initialize the control vector
  Hstar <- matrix(data = NA,
                  nrow = XL,
                  ncol = T_fin)
  
  #Initialize the "new" value function
  Vnew <- rep(NA, XL)
  
  set.seed(seed)
  
  
  for (t in seq(T_fin , 1 , -1)){
    
    
    for (i in seq(1, XL, 1)){
      
      guess <- 0
      X <- Xgrid[i]
      
      #This finds optimal policy function
      Thing = optim(par = guess,
                    fn = dof,
                    gr = NULL,
                    lower = 0,
                    upper = X,
                    X = X,
                    a = a,
                    b = b,
                    delta = delta,
                    r = r,
                    K = K,
                    Xgrid = Xgrid,
                    V = Vmat[,t+1],
                    z = z,
                    method = "L-BFGS-B")
      
      #the optimal Harvest for each stock
      Hstar[i,t] <- Thing$par
      Vmat[i,t] <- -Thing$value
      
    }
  }
  
  xg <- data.frame(Xgrid)
  hdf <- data.frame(Hstar)
  vdf <- data.frame(Vmat[,1:T_fin])
  II <- data.frame(index=1:XL)
  
  hdf2 <- bind_cols(II,xg,hdf)
  vdf2 <- bind_cols(II,xg,vdf)
  
  hdf_new <- gather(data = hdf2,
                    key ="Xtime",
                    value = "harvest_opt",
                    num_range('X',1:T_fin)) %>%
    mutate(T_end = T_fin + 1 - as.numeric(str_replace(Xtime, 'X', ''))) %>%
    select(-Xtime)
  
  vdf_new = gather(data = vdf2,
                   key = "Xtime",
                   value = "value_function",
                   num_range('X', 1:T_fin)) %>%
    mutate(T_end = T_fin + 1 - as.numeric(str_replace(Xtime, 'X', ''))) %>%
    select(-Xtime)
  
  results <- left_join(hdf_new, vdf_new, by = c("index", "Xgrid", "T_end")) %>%
    filter(!T_end == T_fin)
  
  if(returnHs){
    return(Hstar)
  } else {
    return(results)}
}
