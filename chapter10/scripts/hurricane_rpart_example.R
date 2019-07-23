
require(rpart)
require(rpart.plot)

train1 <- train
test1 <- test

train1$xcoord <- 100*(train1$xcoord - min(train1$xcoord ))/diff(range(train1$xcoord))
train1$ycoord <- 100*(train1$ycoord - min(train1$ycoord ))/diff(range(train1$ycoord))
test1$xcoord <- 100*(test1$xcoord - min(test1$xcoord ))/diff(range(test1$xcoord))
test1$ycoord <- 100*(test1$ycoord - min(test1$ycoord ))/diff(range(test1$ycoord))

mod <- rpart(tree.sandy~xcoord + ycoord, data = train1, cp = 0.0042272)
out <- predict(mod, test1)
test1$prob <- out

rpart.plot(mod, shadow.col="gray", nn=TRUE, 
           box.palette = "BuGn")



par(mfrow = c(1,3))

plot(train[,2:1], main = "(1) Calls during storm",
     col = rgb(train$tree.sandy , 0, 1- train$tree.sandy, 1), 
     cex = 0.4, pch = 16, asp = 1)

plot(test[,2:1], main =  "(2) Predicted probabilities",
     col = rgb(test$prob, 0, 1 - test$prob, 1), 
     cex = 0.4, pch = 16, asp = 1)

plot(test[,2:1], main =  "(3) Actual next 7 days",
     col = rgb(test$tree.next7, 0, 1 - test$tree.next7, 1), 
     cex = 0.4, pch = 16, asp = 1)
