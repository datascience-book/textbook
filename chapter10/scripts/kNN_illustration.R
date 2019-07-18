#Illustration of kNN

require(ggplot2)
  n <- 200
  k <- 4
  
  #Create training data
    set.seed(100)
    x <- runif(n)
    
    set.seed(44)
    y <- runif(n)
  
    z <- ifelse(y > x*1.2 , 1, 0)
    z1 <- ifelse(y < 0.8, 1,0)
    z <- z*z1
    
    df <- data.frame(x, y, z)
    
  #Create test
    df_test <- data.frame(id = 1:3,
                          x = c(0.33, 0.63, 0.6),
                          y = c(0.6, 0.7, 0.5))
  
    
  #Create segments  
    segs <- data.frame()
    segs_all <- data.frame()
    for(i in 1:nrow(df_test)){
      d <- sqrt((df_test$x[i] - df$x)^2 + (df_test$y[i] - df$y)^2)
      segs <- rbind(segs,
                data.frame(id = i, 
                           x0 = df_test$x[i], 
                           y0 = df_test$y[i],
                           df[rank(d, ties.method = "random") <=k,]))
      segs_all <- rbind(segs_all,
                    data.frame(id = i, 
                               x0 = df_test$x[i], 
                               y0 = df_test$y[i],
                               df))
    }
    
  #Plot
    
  par(mfrow = c(1,3), mar = rep(3,4))
  #First plot
    plot(df$x, df$y, col = rgb(df$z, 0.8, 1- df$z, 0.4),
         pch = 20, cex = 2, xlab = "", ylab = "", 
         yaxt = "n", xaxt = "n",
         xlim = c(0.2, 0.7), ylim = c(0.3, 0.8), asp = 1)
    points(df_test$x, df_test$y, col = "black", 
           cex = 3, pch = 18)
    title(ylab="X1", xlab = "X2", line=0, cex.lab=1.2)
    title(main = "(1) Set up", line=1, cex.lab=1.2)
    
  #Second plot
    plot(df$x, df$y, col = rgb(df$z, 0.8, 1- df$z, 0.4),
         pch = 20, cex = 2, xlab = "", ylab = "", 
         yaxt = "n", xaxt = "n",
         xlim = c(0.2, 0.7), ylim = c(0.3, 0.8), asp = 1)
    segments(x0 = segs_all$x0,
             y0 = segs_all$y0,
             x1 = segs_all$x,
             y1 = segs_all$y, 
             col = rgb(segs_all$z, 0.8, 1 - segs_all$z, 0.1), 
             lwd = 1)
    points(df_test$x, df_test$y, col = "black", 
           cex = 3, pch = 18)

    title(ylab="X1", xlab = "X2", line=0, cex.lab=1.2)
    title(main = "(2) Calculate distance", line=1, cex.lab=1.2)
    
    
    #Third plot
      scored <- c(1, 0, 0)
      plot(df$x, df$y, col = rgb(df$z, 0.8, 1- df$z, 0.4),
           pch = 20, cex = 2, xlab = "", ylab = "", 
           xlim = c(0.2, 0.7), ylim = c(0.3, 0.8), asp = 1,
           yaxt = "n", xaxt = "n")
      segments(x0 = segs$x0,
               y0 = segs$y0,
               x1 = segs$x,
               y1 = segs$y, 
               col = rgb(segs$z, 0.8, 1 - segs$z, 1), 
               lwd = 2)
      points(df_test$x, df_test$y, col = rgb(scored, 0.8, 1- scored, 1), 
             cex = 3, pch = 18)
       title(ylab="X1", xlab = "X2", line=0, cex.lab=1.2)
      title(main = "(3) Voting by neighbors", line=1, cex.lab=1.2)
      
      