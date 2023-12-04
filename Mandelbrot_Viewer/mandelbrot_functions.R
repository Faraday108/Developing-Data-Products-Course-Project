library(tidyverse)
library(RColorBrewer)
# Helper function to convert matrix to long format so colors can be interpolated easily by row
matrix_to_long <- function(m, x, y, max_iterations) {
  m %>% data.frame() %>% 
    # Add column names
    set_names(y) %>% 
    # Add row for row names
    cbind(x = x, .) %>% 
    # Pivot to long
    pivot_longer(!x, names_to = "y") %>% 
    mutate(y = as.numeric(y)) %>% 
    # Set unescaped points to 0
    mutate(value = ifelse(value == max_iterations, 0, value))
}


vector_mandelbrot2 <- function(xmin = -2, xmax = .5, xpixel = 500, 
                               ymin = -1.2, ymax = 1.2, ypixel = 500, 
                               max_iterations = 500, showimage = TRUE, 
                               plt_color = "RdBu", updateProgress = NULL) {
  if(is.function(updateProgress)) {
    updateProgress(detail = "Begin escape calculation")
  }
  
  # My initial naive mandelbrot implementation didn't take advantage of R's vectorized nature and was slow. 
  # The following version that calculates the whole matrix simultaneously was inspired by Myles Harrison at https://www.r-bloggers.com/2014/12/the-mandelbrot-set-in-r/
  x <- seq(xmin, xmax, length.out = xpixel)
  y <- seq(ymin, ymax, length.out = ypixel)
  # Produces matrix of complex numbers, note that 'x' changes across the columns and 'y' changes down the rows. 
  # C is the value added in each iteration of Mandelbrot set
  c <- outer(x, y * 1i, FUN = "+")
  # Initialize empty matrix to hold calculated z's
  z <- matrix(0, nrow = xpixel, ncol = ypixel)
  # Initialize empty matrix to hold iteration counts
  k <- matrix(0, nrow = xpixel, ncol = ypixel)
  
  # Calculate the complex numbers up till escape and escape iteration. 
  
  for (j in 1:max_iterations) {
    # Note escape value of 1000. If this is changed, make sure to alter in calculation of nu below. 
    index <- which(abs(z) < 1000)
    z[index] <- z[index]^2 + c[index]
    k[index] <- k[index] + 1
  }
  
  if(is.function(updateProgress)) {
    updateProgress(detail = "Computed escape")
  }
  
  # Note that the use of outer expands the first variable x along the column and y along the row. 
  # Convert escape iterations and complex numbers to long dataframe to aid in coloring. 
  mandelset <- matrix_to_long(k, x, y, max_iterations)
  z <- matrix_to_long(z, x, y, max_iterations) %>%
    rename("z_val" = "value")
  
  # Histogram binning of colors as described at Wikipedia: https://en.wikipedia.org/wiki/Plotting_algorithms_for_the_Mandelbrot_set
  # Changes iteration at escape to [0,1] scale with a more even weighting based on number of times an escape value shows up. 
  colors <- mandelset %>%
    group_by(value) %>%
    summarize(n = n()) %>%
    mutate(color = n/sum(n)) %>%
    mutate(color = cumsum(color)) %>%
    select(-n)
  
  if(is.function(updateProgress)) {
    updateProgress(detail = "Normalized colors")
  }
  
  # Add lag of color to aid in interpolation smoothing
  colors_lag <- colors %>%
    mutate(color2 = lag(color), .keep = "unused")
  
  # Combine mandelset of x, y, k with colors, color_lag, and z at escape. 
  mandelset <- left_join(mandelset, colors, by = "value") %>%
    left_join(colors_lag, by = "value") %>%
    left_join(z, join_by(x == x, y == y))
  
  mincol <- min(colors$color)
  # Smoothing algorithm "normalized iteration count" as described at Wikipedia above. 
  colors2 <- mandelset %>%
    # Calculate nu, smoothing value
    # Note, if escape value is changed, make sure to update log(1000)
    mutate(nu = ifelse(value == 0, 0,
                       log(log(abs(z_val))/log(1000))/log(2))) %>%
    rowwise() %>%
    # Interpolate color between intervals based on nu
    mutate(smth_col = color + (ifelse(is.na(color2),mincol,color2) - color)*nu)
  
  if(is.function(updateProgress)) {
    updateProgress(detail = "Colors Smoothed, plotting")
  }
  
  # Allows function to return image (TRUE) or dataframe (FALSE)
  if(showimage) {
    brew_max_col <- brewer.pal.info[plt_color, "maxcolors"]
    ggplot(colors2, aes(x = x, y = y, fill = smth_col)) + 
      geom_raster() + 
      scale_fill_gradientn(colours = 
                             c("black", brewer.pal(brew_max_col, plt_color))) +
      coord_fixed() + 
      theme_void() + 
      theme(plot.background = element_rect(fill='transparent')) + 
      theme(legend.position = "null")
    
  } else {
    select(colors2, c(x,y,color, smth_col))
  }
}
