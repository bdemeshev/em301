library(caret) # model tuning
library(mlbench) # dataset
library(tidyverse) # data manipulation
library(skimr)

data("Sonar")
skim(Sonar)

in_train = createDataPartition(Sonar$Class, p = 0.8, list = FALSE)
in_train

sonar_train = Sonar[in_train, ]
sonar_test = Sonar[-in_train, ]

glimpse(sonar_train)

control = trainControl(method = "repeatedcv", 
                       number = 10, repeats = 3, classProbs = TRUE)

fit_ranger = train(Class ~ ., data = sonar_train, 
                   method = "ranger", trControl = control)
fit_ranger

fit_knn = train(Class ~ ., data = sonar_train, 
          method = "knn", preProc = c("scale", "center"), trControl = control)
fit_knn

fit_penlog = train(Class ~ ., data = sonar_train, 
        method = "plr", preProc = c("scale", "center"), trControl = control)
fit_penlog

results = resamples(list(fit_ranger, fit_knn, fit_penlog))
summary(results)

predictions = predict(fit_ranger, newdata = sonar_test)
predictions

confusionMatrix(predictions, sonar_test$Class)

probab_predictions = predict(fit_ranger, newdata = sonar_test, type = "prob")
probab_predictions




