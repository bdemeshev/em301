h$brick <- as.factor(h$brick)

flats$brick <- as.factor(flats$brick)
levels(flats$brick) <- list(no = "0", yes = "1") 
flats$walk <- as.factor(flats$walk)


set.seed(777)

train_index <- createDataPartition(y = flats$brick, p = 0.75, list = FALSE)
train_flats <- flats[train_index, ]
test_flats <- flats[-train_index, ]


m1 <- ksvm(brick ~ price + totsp + livesp + kitsp + dist + metrdist + walk + floor + code, 
           data = train_flats, 
           kernel = "rbfdot", 
           kpar = list(sigma = 0.05), 
           C = 5)
		   
		   
m1_auto_sigma <- ksvm(brick ~ price + totsp + livesp + kitsp + dist + metrdist + walk + floor + code, 
           data = train_flats, 
           kernel = "rbfdot", 
           C = 5)
		   
kernelf(m1)

kpar(kernelf(m1))$sigma

test_flats$brick.pred <- predict(m1, test_flats)
predict(m1, test_flats, type = "probabilities")



   


tree1 <- getTree(model, 1, labelVar = TRUE)
head(tree1)

model <- randomForest(brick ~ price + totsp + kitsp + dist, 
                      data = h, importance = TRUE)


importance(model)

predict(model, new_data, type = "prob")





k1 <- vanilladot()
k2 <- rbfdot(sigma = 1)
k1(x, y)


# кросс валидация

m1 <- ksvm(brick ~ price + totsp + livesp + kitsp + dist + metrdist + walk + floor + code,
           data = flats,
           kernel = rbfdot,
           kpar = list(sigma = 1),
           C = 1,
           cross = 10)

cross(m1) # cross validation error



library(caret)
ctrl <- trainControl(classProbs = TRUE)
fit <- train(brick ~ price + totsp + livesp + kitsp + dist + metrdist + walk + floor + code,
             data = train_flats,
             method = "svmRadial",
             trControl = ctrl)
fit


predict(fit, test_flats , type = "prob")




