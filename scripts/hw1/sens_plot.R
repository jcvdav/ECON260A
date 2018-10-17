sens_plot <- function(data){
  p1 <- ggplot(data = data,
               mapping = aes(x = Xgrid, y = par, z = harvest_opt, fill = harvest_opt)) +
    geom_raster(interpolate = T) +
    geom_contour(color = "black") +
    scale_fill_gradientn(colours = colorRamps::matlab.like(30)) +
    ggtheme_plot()
  
  p2 <- ggplot(data = data,
               mapping = aes(x = Xgrid, y = harvest_opt)) +
    geom_line(size = 1, aes(group = par, color = par)) +
    xlab("X") +
    ylab("h(X)") +
    scale_color_gradientn(colours = colorRamps::matlab.like(30)) +
    ggtheme_plot()
  
  plot <- cowplot::plot_grid(p1, p2,
                             labels = "AUTO",
                             ncol = 1,
                             rel_heights = c(2, 1))
  
  return(plot)
  
}

