#Prep diagrams

#Base data
  set.seed(234)
  examp <- data.frame(x1 = runif(3000), x2 = runif(3000))
  examp$linear <- (examp$x1 < examp$x2) * 1
  examp$nonlinear <- examp$x1 < ( examp$x2 * 4 - 8*examp$x2^2 + 2*examp$x2^3) -0.1
  part1 <- examp$x1 < ( examp$x2 * 4 - 8*examp$x2^2 + 2*examp$x2^3) +0.2
  part2 <- examp$x2 >  1.2- ( examp$x1*0.6)
  part3 <- examp$x2 >  0.2+ ( examp$x1*1.4)
  examp$discont <- (part1 == TRUE | part2 == TRUE )*1
  examp$varied <- (part1 == TRUE | part2 == TRUE | part3 == TRUE)*1

#Create grid
  grid <- expand.grid(x1= seq(0.001, 1, 0.001), x2 = seq(0.001, 1, 0.001))
  
###################
# CALIBRATE MODELS#
###################
  
#Logistic
  loglin <- glm(linear ~ x1 + x2, data = examp, family = binomial())
  logvaried <- glm(varied ~ x1 + x2, data = examp, family = binomial())
  logdiscont <- glm(discont ~ x1 + x2, data = examp, family = binomial())
  prob.grid.lin1 <- predict(loglin, grid, type = "response") 
  prob.grid.lin2 <- predict(logvaried, grid, type = "response") 
  prob.grid.lin3 <- predict(logdiscont, grid, type = "response") 
  
#RPART
  library(rpart)
  dectree <- rpart(linear ~ x1 + x2, data = examp, cp = 0)
  decvaried <- rpart(varied ~ x1 + x2, data = examp, cp = 0)
  decdiscont <- rpart(discont ~ x1 + x2, data = examp, cp = 0)
  prob.grid.rpart1 <- predict(dectree, grid)
  prob.grid.rpart2 <- predict(decvaried, grid)
  prob.grid.rpart3 <- predict(decdiscont, grid)
  
#Random Forest 
  library(randomForest)
   adatree <- randomForest(factor(linear) ~ x1 + x2, data = examp, mtry = 2)
   adavaried <- randomForest(factor(varied) ~ x1 + x2, data = examp, mtry = 2)
   adadiscont <- randomForest(factor(discont) ~ x1 + x2, data = examp, mtry = 2)
   prob.grid.gbm1 <- predict(adatree, grid, type = "response")
   prob.grid.gbm2 <- predict(adavaried, grid, type = "response")
   prob.grid.gbm3 <- predict(adadiscont, grid, type = "response")
   
#GAM
   library(gam)
   gammod1 <- gam(linear ~ s(x1) + s(x2), data = examp)
   gammod2 <- gam(varied ~ s(x1) + s(x2), data = examp)
   gammod3 <- gam(discont ~ s(x1) + s(x2), data = examp)
   prob.grid.gam1 <- predict(gammod1, grid, type = "response")
   prob.grid.gam2 <- predict(gammod2, grid, type = "response")
   prob.grid.gam3 <- predict(gammod3, grid, type = "response")

#####################   
# plot the boundary##
#####################
   
  par(mfrow = c(3,4), mar = c(0, 1.2, 0.9, 0))
   
  #Linear example
  plot(examp[,c("x1", "x2")], col= "lightblue", ylab = "", xlab ="",
       font.main = 1,  pch = 16, cex = 0.8, 
       xaxt = "n", yaxt = "n", asp = 1,bty="n")
  points(examp[examp$linear == 1, c("x1", "x2")], col= "purple", pch = 16, cex = 0.7)
  points(examp[examp$linear == 0, c("x1", "x2")], col= "lightblue", pch = 16, cex = 0.7)
  contour(x=seq(0.001, 1, 0.001), y = seq(0.001, 1, 0.001), 
          z=matrix(prob.grid.lin1, nrow=1000), levels=0.5,
          col="black", drawlabels=FALSE, lwd=3, add=T)
  title(main="GLM", line= 0, cex.main=1.2, font.main = 1)
  title(ylab="Linear", line= -0.2, cex.lab=1.3, font=3)

  
  plot(examp[,c("x1", "x2")], col= "lightblue", ylab = "", xlab ="",
       font.main = 1,  pch = 16, cex = 0.8, 
       xaxt = "n", yaxt = "n", asp = 1,bty="n")
  points(examp[examp$linear == 1, c("x1", "x2")], col= "purple", pch = 16, cex = 0.7)
  points(examp[examp$linear == 0, c("x1", "x2")], col= "lightblue", pch = 16, cex = 0.7)
  contour(x=seq(0.001, 1, 0.001), y = seq(0.001, 1, 0.001), 
          z=matrix(prob.grid.gam1, nrow=1000), levels=0.5,
          col="black", drawlabels=FALSE, lwd=3, add=T)
  title(main="GAM", line= 0, cex.main=1.2, font.main = 1)
  
  
  plot(examp[,c("x1", "x2")], col= "lightblue", 
       font.main = 1, cex.main = 0.9, pch = 16, cex = 0.8,  
       xlab = "", ylab = "",
       xaxt = "n", yaxt = "n", asp = 1,bty="n")
  points(examp[examp$linear == 1, c("x1", "x2")], col= "purple", pch = 16, cex = 0.7)
  points(examp[examp$linear == 0, c("x1", "x2")], col= "lightblue", pch = 16, cex = 0.7)
  contour(x=seq(0.001, 1, 0.001), y = seq(0.001, 1, 0.001), 
          z=matrix(prob.grid.rpart1, nrow=1000), levels=0.5,
          col="black", drawlabels=FALSE, lwd=3, add=T)
  title(main="Decision Tree", line= 0, cex.main=1.2, font.main = 1)
  
  plot(examp[,c("x1", "x2")], col= "lightblue", 
       font.main = 1, cex.main = 0.9, pch = 16, cex = 0.8, xlab = "", ylab = "",
       xaxt = "n", yaxt = "n", asp = 1,bty="n")
  points(examp[examp$linear == 1, c("x1", "x2")], col= "purple", pch = 16, cex = 0.7)
  points(examp[examp$linear == 0, c("x1", "x2")], col= "lightblue", pch = 16, cex = 0.7)
  contour(x=seq(0.001, 1, 0.001), y = seq(0.001, 1, 0.001), 
          z=matrix(prob.grid.gbm1, nrow=1000), levels=0.5,
          col="black", drawlabels=FALSE, lwd=3, add=T)
  title(main="Random Forest", line= 0, cex.main=1.2, font.main = 1)
  
  
  #Non-linear
  plot(examp[,c("x1", "x2")], col= "lightblue", ylab = "", xlab ="",
       font.main = 1,  pch = 16, cex = 0.8, 
       xaxt = "n", yaxt = "n", asp = 1,bty="n")
  points(examp[examp$varied == 1, c("x1", "x2")], col= "purple", pch = 16, cex = 0.7)
  points(examp[examp$varied == 0, c("x1", "x2")], col= "lightblue", pch = 16, cex = 0.7)
  contour(x=seq(0.001, 1, 0.001), y = seq(0.001, 1, 0.001), 
          z=matrix(prob.grid.lin2, nrow=1000), levels=0.5,
          col="black", drawlabels=FALSE, lwd=3, add=T)
  title(ylab="Non-Linear", line= -0.2, cex.lab=1.3, font=3)
  
  plot(examp[,c("x1", "x2")], col= "lightblue", ylab = "", xlab ="",
       font.main = 1,  pch = 16, cex = 0.8, 
       xaxt = "n", yaxt = "n", asp = 1,bty="n")
  points(examp[examp$varied == 1, c("x1", "x2")], col= "purple", pch = 16, cex = 0.7)
  points(examp[examp$varied == 0, c("x1", "x2")], col= "lightblue", pch = 16, cex = 0.7)
  contour(x=seq(0.001, 1, 0.001), y = seq(0.001, 1, 0.001), 
          z=matrix(prob.grid.gam2, nrow=1000), levels=0.5,
          col="black", drawlabels=FALSE, lwd=3, add=T)
  
  
  plot(examp[,c("x1", "x2")], col= "lightblue", 
       font.main = 1, cex.main = 0.9, pch = 16, cex = 0.8,  
       xlab = "", ylab = "",
       xaxt = "n", yaxt = "n", asp = 1,bty="n")
  points(examp[examp$varied == 1, c("x1", "x2")], col= "purple", pch = 16, cex = 0.7)
  points(examp[examp$varied == 0, c("x1", "x2")], col= "lightblue", pch = 16, cex = 0.7)
  contour(x=seq(0.001, 1, 0.001), y = seq(0.001, 1, 0.001), 
          z=matrix(prob.grid.rpart2, nrow=1000), levels=0.5,
          col="black", drawlabels=FALSE, lwd=3, add=T)
  
  plot(examp[,c("x1", "x2")], col= "lightblue", 
       font.main = 1, cex.main = 0.9, pch = 16, cex = 0.8, xlab = "", ylab = "",
       xaxt = "n", yaxt = "n", asp = 1,bty="n")
  points(examp[examp$varied == 1, c("x1", "x2")], col= "purple", pch = 16, cex = 0.7)
  points(examp[examp$varied == 0, c("x1", "x2")], col= "lightblue", pch = 16, cex = 0.7)
  contour(x=seq(0.001, 1, 0.001), y = seq(0.001, 1, 0.001), 
          z=matrix(prob.grid.gbm2, nrow=1000), levels=0.5,
          col="black", drawlabels=FALSE, lwd=3, add=T)
  
  #Discontinuous
  plot(examp[,c("x1", "x2")], col= "lightblue", ylab = "", xlab ="",
       font.main = 1,  pch = 16, cex = 0.8, 
       xaxt = "n", yaxt = "n", asp = 1,bty="n")
  points(examp[examp$discont == 1, c("x1", "x2")], col= "purple", pch = 16, cex = 0.7)
  points(examp[examp$discont == 0, c("x1", "x2")], col= "lightblue", pch = 16, cex = 0.7)
  contour(x=seq(0.001, 1, 0.001), y = seq(0.001, 1, 0.001), 
          z=matrix(prob.grid.lin3, nrow=1000), levels=0.5,
          col="black", drawlabels=FALSE, lwd=3, add=T)
  title(ylab="Discontinuous", line= -0.2, cex.lab=1.3, font=3)
  
  
  plot(examp[,c("x1", "x2")], col= "lightblue", ylab = "", xlab ="",
       font.main = 1,  pch = 16, cex = 0.8, 
       xaxt = "n", yaxt = "n", asp = 1,bty="n")
  points(examp[examp$discont == 1, c("x1", "x2")], col= "purple", pch = 16, cex = 0.7)
  points(examp[examp$discont == 0, c("x1", "x2")], col= "lightblue", pch = 16, cex = 0.7)
  contour(x=seq(0.001, 1, 0.001), y = seq(0.001, 1, 0.001), 
          z=matrix(prob.grid.gam3, nrow=1000), levels=0.5,
          col="black", drawlabels=FALSE, lwd=3, add=T)
  
  plot(examp[,c("x1", "x2")], col= "lightblue", 
       font.main = 1, cex.main = 0.9, pch = 16, cex = 0.8,  
       xlab = "", ylab = "",
       xaxt = "n", yaxt = "n", asp = 1,bty="n")
  points(examp[examp$discont == 1, c("x1", "x2")], col= "purple", pch = 16, cex = 0.7)
  points(examp[examp$discont == 0, c("x1", "x2")], col= "lightblue", pch = 16, cex = 0.7)
  contour(x=seq(0.001, 1, 0.001), y = seq(0.001, 1, 0.001), 
          z=matrix(prob.grid.rpart3, nrow=1000), levels=0.5,
          col="black", drawlabels=FALSE, lwd=3, add=T)

  plot(examp[,c("x1", "x2")], col= "lightblue", 
       font.main = 1, cex.main = 0.9, pch = 16, cex = 0.8, xlab = "", ylab = "",
       xaxt = "n", yaxt = "n", asp = 1,bty="n")
  points(examp[examp$discont == 1, c("x1", "x2")], col= "purple", pch = 16, cex = 0.7)
  points(examp[examp$discont == 0, c("x1", "x2")], col= "lightblue", pch = 16, cex = 0.7)
  contour(x=seq(0.001, 1, 0.001), y = seq(0.001, 1, 0.001), 
          z=matrix(prob.grid.gbm3, nrow=1000), levels=0.5,
          col="black", drawlabels=FALSE, lwd=3, add=T)

################
# PROBLEM SPACE#
################
  
  par(mfrow = c(1,3), mar = c(0, 1.2, 0.9, 0))
  
  #Linear example
  plot(examp[,c("x1", "x2")], col= "lightblue", ylab = "", xlab ="",
       font.main = 1,  pch = 16, cex = 0.8, 
       xaxt = "n", yaxt = "n", asp = 1,bty="n")
  points(examp[examp$linear == 1, c("x1", "x2")], col= "purple", pch = 16, cex = 0.7)
  points(examp[examp$linear == 0, c("x1", "x2")], col= "lightblue", pch = 16, cex = 0.7)
  title(main="(1) Linear", line= -0.2, cex.lab=1.3, font=3)
  
  
  #Non-linear
  plot(examp[,c("x1", "x2")], col= "lightblue", ylab = "", xlab ="",
       font.main = 1,  pch = 16, cex = 0.8, 
       xaxt = "n", yaxt = "n", asp = 1,bty="n")
  points(examp[examp$varied == 1, c("x1", "x2")], col= "purple", pch = 16, cex = 0.7)
  points(examp[examp$varied == 0, c("x1", "x2")], col= "lightblue", pch = 16, cex = 0.7)
    title(main="(2) Non-Linear", line= -0.2, cex.lab=1.3, font=3)
  
  #Discontinuous
  plot(examp[,c("x1", "x2")], col= "lightblue", ylab = "", xlab ="",
       font.main = 1,  pch = 16, cex = 0.8, 
       xaxt = "n", yaxt = "n", asp = 1,bty="n")
  points(examp[examp$discont == 1, c("x1", "x2")], col= "purple", pch = 16, cex = 0.7)
  points(examp[examp$discont == 0, c("x1", "x2")], col= "lightblue", pch = 16, cex = 0.7)
   title(main="(3) Discontinuous", line= -0.2, cex.lab=1.3, font=3)
  