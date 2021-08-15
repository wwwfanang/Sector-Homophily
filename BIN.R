bin <- function(x, y, n) {
  bin_edges <- seq(min(x), max(x), length.out = n + 1)       # divide index euqally
  xbin <- rep(NA, length(x))                                 # create data structure to assign trees to bins
  b <- bin_edges                                             # add a little to the biggest bin temporarily
  b[length(b)] <- b[length(b)] + 1                           # (so that the biggest single tree is put in a bin)
  for (i in 1:length(x)) {
    xbin[i] <- sum(x[i] >= b)                                # larger than how many edges to make sure in which bin 
  }
  bin_midpoints <- numeric(n)
  for (i in 1:n) {
    bin_midpoints[i] <- mean(bin_edges[i:(i+1)])             # backtransform bin edges to linear, and get midpoints
  }
  bin_widths <- diff(bin_edges)                              # get linear width of each bin
  bin_factor <- factor(xbin, levels=1:n)                     # convert bin to factor (required to deal with zeroes if present)
  bin_counts <- table(bin_factor)                            # find number of trees in each bin
  if (!is.null(y)) {
    rawy <- tapply(y, bin_factor, sum)                       # sum y value in each bin
    rawy[is.na(rawy)] <- 0                                   # add zeroes back in if present
    # bin_values <- as.numeric(rawy/bin_widths)                # divide production by width for each bin
    bin_values <- as.numeric(rawy)
  }
  else {
    # bin_values <- as.numeric(bin_counts/bin_widths)          # 1-dimensional case.
    bin_values <- as.numeric(bin_counts)
  }
  
  # return(data.frame(bin_midpoint = bin_midpoints,            # return result!
  #                   bin_value = bin_values,                  # also add bin min and max for bar plot purposes
  #                   bin_count = as.numeric(bin_counts),
  #                   bin_min = bin_edges[1:n],
  #                   bin_max = bin_edges[2:(n+1)]))
  return(data.frame(bin_midpoint = bin_midpoints,            # return result!
                    bin_value = bin_values))                # also add bin min and max for bar plot purposes
  
}
