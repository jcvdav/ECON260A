sensitivity <- function(min, max, num, par = "r"){
  
  parameter <- seq(from = min, to = max, length.out = num)
  
  results <- data.frame()
  
  for(i in 1:num){
    run_i <- eval(parse(text = paste0("call_dof(", par, "=", parameter[i], ")"))) %>% 
      filter(T_end == 29) %>% 
      mutate(par = parameter[i])
    
    results <- rbind(results, run_i)
  }
  
  results
  
  return(results)
}
