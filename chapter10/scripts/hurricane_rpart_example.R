
require(rpart)
require(rpart.plot)
mod <- rpart(tree.sandy~xcoord + ycoord, data = train, cp = 0.0042272)
out <- predict(mod, test)
test$prob <- out

rpart.plot(mod, shadow.col="gray", nn=TRUE, 
           box.palette = "BuGn", 
           main = "Hurricane example")



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
