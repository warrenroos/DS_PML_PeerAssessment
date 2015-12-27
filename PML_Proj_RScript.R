# Practical Machine Learning:  Peer Assessment 

# load libraries 
library(dplyr)
library(caret)
library(rpart)


# read in data from file 
pml.testing.orig <- read.table(
  file = "..\\..\\Data\\DS_PML_PeerAssessment\\pml-testing.csv", 
  header = TRUE, sep = ",", na.strings = c("NA", "#DIV/0!", ""), nrows = 2500000 # 250000
)

# read in data from file 
pml.training.orig <- read.table(
  file = "..\\..\\Data\\DS_PML_PeerAssessment\\pml-training.csv", 
  header = TRUE, sep = ",", na.strings = c("NA", "#DIV/0!", ""), nrows = 250000
)

# viewed training data - noticed lots of NAs 
# also noticed NA strings should be on 
# na.strings = c("NA", "#DIV/0!", "") and updated the read.table accordingly 

# # viewed # of NAs in one column 
# # 244 of 250 - NA 
# # 490 of 500 - NA 
# sum(is.na(pml.training.orig$max_yaw_dumbbell))
# 
# # next viewed NAs across all columns to determine the columns (predictors) with a 
# # high number of NAs 
# apply(pml.training.orig,2,function(x){sum(is.na(x))})
# 
# # converted to a percentage (technically a fraction) of the columns with NAs 
# # ~ 60 column had a low fraction of NAs, where ~ 100 columns had a high fraction of NAs 
# apply(pml.training.orig,2,function(x){sum(is.na(x))/length(x)})
# 
# # next, looked at how many predictors had less than 99% NAs... 60 of 160... 
# sum(apply(pml.training.orig,2,function(x){sum(is.na(x))/length(x) < 0.98}))
# 
# # or alternatively, how many predictors had greater than 99.9999% data values (i.e. !is.na())
# sum(apply(pml.training.orig,2,function(x){sum(!is.na(x))/length(x) > 0.999999}))

# vertically filter for columns that predominatly have data and are not NAs 
pml.training.vFilNA <- pml.training.orig[,
    (apply(pml.training.orig,2,function(x){sum(!is.na(x))/length(x)})) > 0.99]

# check dimensions... 
dim(pml.training.vFilNA)

# # horizontally filter just for carlitos for model build construction purposes 
# pml.training.vhFilNA = pml.training.vFilNA %>% 
#   select(., -X) %>% 
#   filter(user_name == "carlitos")
# 
# # vertically filter out non-relevant predictor columns 
# pml.training.vhvFilNA <- pml.training.vhFilNA %>% 
#   select(., -grep("time", colnames(pml.training.vhFilNA)), -grep("window", colnames(pml.training.vhFilNA))) 
# 
# # vertically filter out non-relevant predictor columns 
# pml.training.vvFilNA <- pml.training.vFilNA %>% 
#   select(., 
#          -X, 
#          -grep("time", colnames(pml.training.vFilNA)), 
#          -grep("window", colnames(pml.training.vFilNA))) 

# get list of unique user_name values 
unique(pml.training.orig$user_name)

# vertically filter out non-relevant predictor columns 
pml.training.all <- pml.training.vFilNA %>% 
  select(., 
         -X, 
         -grep("time", colnames(pml.training.vFilNA)), 
         -grep("window", colnames(pml.training.vFilNA))) 

# horizontally filter just for carlitos for model build construction purposes 
# vertically filter out non-relevant predictor columns 
pml.training.carlitos <- pml.training.vFilNA %>% 
  select(., 
         -X, 
         -grep("time", colnames(pml.training.vFilNA)), 
         -grep("window", colnames(pml.training.vFilNA))) %>% 
  filter(user_name == "carlitos")

# horizontally filter just for pedro for model build construction purposes 
# vertically filter out non-relevant predictor columns 
pml.training.pedro <- pml.training.vFilNA %>% 
  select(., 
         -X, 
         -grep("time", colnames(pml.training.vFilNA)), 
         -grep("window", colnames(pml.training.vFilNA))) %>% 
  filter(user_name == "pedro")

# horizontally filter just for adelmo for model build construction purposes 
# vertically filter out non-relevant predictor columns 
pml.training.adelmo <- pml.training.vFilNA %>% 
  select(., 
         -X, 
         -grep("time", colnames(pml.training.vFilNA)), 
         -grep("window", colnames(pml.training.vFilNA))) %>% 
  filter(user_name == "adelmo")

 
# horizontally filter just for charles for model build construction purposes 
# vertically filter out non-relevant predictor columns 
pml.training.charles <- pml.training.vFilNA %>% 
  select(., 
         -X, 
         -grep("time", colnames(pml.training.vFilNA)), 
         -grep("window", colnames(pml.training.vFilNA))) %>% 
  filter(user_name == "charles")


# horizontally filter just for eurico for model build construction purposes 
# vertically filter out non-relevant predictor columns 
pml.training.eurico <- pml.training.vFilNA %>% 
  select(., 
         -X, 
         -grep("time", colnames(pml.training.vFilNA)), 
         -grep("window", colnames(pml.training.vFilNA))) %>% 
  filter(user_name == "eurico")


# horizontally filter just for jeremy for model build construction purposes 
# vertically filter out non-relevant predictor columns 
pml.training.jeremy <- pml.training.vFilNA %>% 
  select(., 
         -X, 
         -grep("time", colnames(pml.training.vFilNA)), 
         -grep("window", colnames(pml.training.vFilNA))) %>% 
  filter(user_name == "jeremy")

# # read in data from file 
# pml.training.orig <- read.table(
#   file = "..\\..\\Data\\DS_PML_PeerAssessment\\pml-training.csv", 
#   header = TRUE, sep = ",", na.strings = "NA", nrows = 2500000 # 250000
# )

# inTrain = createDataPartition(pml.training.orig$classe, p = 1/20)[[1]]
# pml.training.subset.a = pml.training.orig[ inTrain,]
# #pml.testing.subset.a = pml.training.orig[-inTrain,]# read in data from file 
# pml.training.orig <- read.table(
#   file = "..\\..\\Data\\DS_PML_PeerAssessment\\pml-training.csv", 
#   header = TRUE, sep = ",", na.strings = "NA", nrows = 2500000 # 250000
# )
# 
# 
# # vertically filter columns for Result column and indicator columns 
# pml.training.subset.b = pml.training.subset.a %>% 
#   select(., -X)
# 
# training <- pml.training.vhvFilNA  # pml.training.subset.b # pml.training.orig 
# testing <- pml.testing.orig # pml.testing.orig ***** replace for submision ***** 
# dim(training) 
# dim(testing)
# colnames(training)
# 
# modelFit <- train(classe ~ ., method="rpart", data=training)
# print(modelFit$finalModel)
# plot(modelFit$finalModel, uniform=TRUE, main="Classification Tree")
# text(modelFit$finalModel, use.n = TRUE, all= TRUE, cex = 0.8)
# 
# # train the no PCA model... 
# modelFit.glm <- train(classe ~ ., data = training, method = "glm")
# 
# modelFit4<- train(classe~., data=training, method="rf")
# print(modelFit4$finalModel)
# plot(modelFit4$finalModel, uniform=TRUE, main="Classification Tree")
# text(modelFit4$finalModel, use.n = TRUE, all= TRUE, cex = 0.8)
# plot(modelFit4$finalModel)
# print(modelFit4$results)

# split 5%/95% training vs. testing, run model, and predict results 
inTrain.p5 = createDataPartition(pml.training.all$classe, p = 1/20)[[1]]
pml.training.p5.train = pml.training.all[ inTrain.p5,] 
pml.training.p5.test = pml.training.all[-inTrain.p5,] 
modelFit.p5 <- train(classe~., data=pml.training.p5.train, method="rf")
print(modelFit.p5$finalModel)
print(modelFit.p5$results)
modelFit.p5.predict.p5.test <- predict(modelFit.p5, pml.training.p5.test)
sum(modelFit.p5.predict.p5.test == pml.training.p5.test$classe)/length(pml.training.p5.test$classe)
modelFit.p5.predict.adelmo <- predict(modelFit.p5, pml.training.adelmo)
sum(modelFit.p5.predict.adelmo == pml.training.adelmo$classe)/length(pml.training.adelmo$classe)
modelFit.p5.predict.carlitos <- predict(modelFit.p5, pml.training.carlitos)
sum(modelFit.p5.predict.carlitos == pml.training.carlitos$classe)/length(pml.training.carlitos$classe)
modelFit.p5.predict.charles <- predict(modelFit.p5, pml.training.charles)
sum(modelFit.p5.predict.charles == pml.training.charles$classe)/length(pml.training.charles$classe)
modelFit.p5.predict.eurico <- predict(modelFit.p5, pml.training.eurico)
sum(modelFit.p5.predict.eurico == pml.training.eurico$classe)/length(pml.training.eurico$classe)
modelFit.p5.predict.jeremy <- predict(modelFit.p5, pml.training.jeremy)
sum(modelFit.p5.predict.jeremy == pml.training.jeremy$classe)/length(pml.training.jeremy$classe)
modelFit.p5.predict.pedro <- predict(modelFit.p5, pml.training.pedro)
sum(modelFit.p5.predict.pedro == pml.training.pedro$classe)/length(pml.training.pedro$classe)
modelFit.p5.predict.testing <- predict(modelFit.p5, pml.testing.orig)
modelFit.p5.predict.testing

# split 25%/75% training vs. testing, run model, and predict results 
inTrain.p25 = createDataPartition(pml.training.all$classe, p = 0.25)[[1]]
pml.training.p25.train = pml.training.all[ inTrain.p25,] 
pml.training.p25.test = pml.training.all[-inTrain.p25,] 
modelFit.p25 <- train(classe~., data=pml.training.p25.train, method="rf")
print(modelFit.p25$finalModel)
print(modelFit.p25$results)
modelFit.p25.predict.p25.test <- predict(modelFit.p25, pml.training.p25.test)
sum(modelFit.p25.predict.p25.test == pml.training.p25.test$classe)/length(pml.training.p25.test$classe)
modelFit.p25.predict.adelmo <- predict(modelFit.p25, pml.training.adelmo)
sum(modelFit.p25.predict.adelmo == pml.training.adelmo$classe)/length(pml.training.adelmo$classe)
modelFit.p25.predict.carlitos <- predict(modelFit.p25, pml.training.carlitos)
sum(modelFit.p25.predict.carlitos == pml.training.carlitos$classe)/length(pml.training.carlitos$classe)
modelFit.p25.predict.charles <- predict(modelFit.p25, pml.training.charles)
sum(modelFit.p25.predict.charles == pml.training.charles$classe)/length(pml.training.charles$classe)
modelFit.p25.predict.eurico <- predict(modelFit.p25, pml.training.eurico)
sum(modelFit.p25.predict.eurico == pml.training.eurico$classe)/length(pml.training.eurico$classe)
modelFit.p25.predict.jeremy <- predict(modelFit.p25, pml.training.jeremy)
sum(modelFit.p25.predict.jeremy == pml.training.jeremy$classe)/length(pml.training.jeremy$classe)
modelFit.p25.predict.pedro <- predict(modelFit.p25, pml.training.pedro)
sum(modelFit.p25.predict.pedro == pml.training.pedro$classe)/length(pml.training.pedro$classe)
modelFit.p25.predict.testing <- predict(modelFit.p25, pml.testing.orig)
modelFit.p25.predict.testing

# split 75%/25% training vs. testing, run model, and predict results 
inTrain.p75 = createDataPartition(pml.training.all$classe, p = 0.75)[[1]]
pml.training.p75.train = pml.training.all[ inTrain.p75,] 
pml.training.p75.test = pml.training.all[-inTrain.p75,] 
modelFit.p75 <- train(classe~., data=pml.training.p75.train, method="rf")
print(modelFit.p75$finalModel)
print(modelFit.p75$results)
modelFit.p75.predict.p75.test <- predict(modelFit.p75, pml.training.p75.test)
sum(modelFit.p75.predict.p75.test == pml.training.p75.test$classe)/length(pml.training.p75.test$classe)
modelFit.p75.predict.adelmo <- predict(modelFit.p75, pml.training.adelmo)
sum(modelFit.p75.predict.adelmo == pml.training.adelmo$classe)/length(pml.training.adelmo$classe)
modelFit.p75.predict.carlitos <- predict(modelFit.p75, pml.training.carlitos)
sum(modelFit.p75.predict.carlitos == pml.training.carlitos$classe)/length(pml.training.carlitos$classe)
modelFit.p75.predict.charles <- predict(modelFit.p75, pml.training.charles)
sum(modelFit.p75.predict.charles == pml.training.charles$classe)/length(pml.training.charles$classe)
modelFit.p75.predict.eurico <- predict(modelFit.p75, pml.training.eurico)
sum(modelFit.p75.predict.eurico == pml.training.eurico$classe)/length(pml.training.eurico$classe)
modelFit.p75.predict.jeremy <- predict(modelFit.p75, pml.training.jeremy)
sum(modelFit.p75.predict.jeremy == pml.training.jeremy$classe)/length(pml.training.jeremy$classe)
modelFit.p75.predict.pedro <- predict(modelFit.p75, pml.training.pedro)
sum(modelFit.p75.predict.pedro == pml.training.pedro$classe)/length(pml.training.pedro$classe)
modelFit.p75.predict.testing <- predict(modelFit.p75, pml.testing.orig)
modelFit.p75.predict.testing

# try all data, run model, and predict results 
modelFit.all <- train(classe~., data=pml.training.all, method="rf")
print(modelFit.all$finalModel)
print(modelFit.all$results)
modelFit.all
modelFit.all.predict.adelmo <- predict(modelFit.all, pml.training.adelmo)
sum(modelFit.all.predict.adelmo == pml.training.adelmo$classe)/length(pml.training.adelmo$classe)
modelFit.all.predict.carlitos <- predict(modelFit.all, pml.training.carlitos)
sum(modelFit.all.predict.carlitos == pml.training.carlitos$classe)/length(pml.training.carlitos$classe)
modelFit.all.predict.charles <- predict(modelFit.all, pml.training.charles)
sum(modelFit.all.predict.charles == pml.training.charles$classe)/length(pml.training.charles$classe)
modelFit.all.predict.eurico <- predict(modelFit.all, pml.training.eurico)
sum(modelFit.all.predict.eurico == pml.training.eurico$classe)/length(pml.training.eurico$classe)
modelFit.all.predict.jeremy <- predict(modelFit.all, pml.training.jeremy)
sum(modelFit.all.predict.jeremy == pml.training.jeremy$classe)/length(pml.training.jeremy$classe)
modelFit.all.predict.pedro <- predict(modelFit.all, pml.training.pedro)
sum(modelFit.all.predict.pedro == pml.training.pedro$classe)/length(pml.training.pedro$classe)
modelFit.all.predict.testing <- predict(modelFit.all, pml.testing.orig)
modelFit.all.predict.testing

# subset adelmo vs rest of subjects - training vs. testing, run model, and predict results 
modelFit.adelmo <- train(classe~., data=pml.training.adelmo, method="rf")
print(modelFit.adelmo$finalModel)
print(modelFit.adelmo$results)
modelFit.adelmo
modelFit.adelmo.predict.adelmo <- predict(modelFit.adelmo, pml.training.adelmo)
sum(modelFit.adelmo.predict.adelmo == pml.training.adelmo$classe)/length(pml.training.adelmo$classe)
modelFit.adelmo.predict.carlitos <- predict(modelFit.adelmo, pml.training.carlitos)
sum(modelFit.adelmo.predict.carlitos == pml.training.carlitos$classe)/length(pml.training.carlitos$classe)
modelFit.adelmo.predict.charles <- predict(modelFit.adelmo, pml.training.charles)
sum(modelFit.adelmo.predict.charles == pml.training.charles$classe)/length(pml.training.charles$classe)
modelFit.adelmo.predict.eurico <- predict(modelFit.adelmo, pml.training.eurico)
sum(modelFit.adelmo.predict.eurico == pml.training.eurico$classe)/length(pml.training.eurico$classe)
modelFit.adelmo.predict.jeremy <- predict(modelFit.adelmo, pml.training.jeremy)
sum(modelFit.adelmo.predict.jeremy == pml.training.jeremy$classe)/length(pml.training.jeremy$classe)
modelFit.adelmo.predict.pedro <- predict(modelFit.adelmo, pml.training.pedro)
sum(modelFit.adelmo.predict.pedro == pml.training.pedro$classe)/length(pml.training.pedro$classe)
modelFit.adelmo.predict.testing <- predict(modelFit.adelmo, pml.testing.orig)
modelFit.adelmo.predict.testing

# subset carlitos vs rest of subjects - training vs. testing, run model, and predict results 
modelFit.carlitos <- train(classe~., data=pml.training.carlitos, method="rf")
print(modelFit.carlitos$finalModel)
print(modelFit.carlitos$results)
modelFit.carlitos
modelFit.carlitos.predict.adelmo <- predict(modelFit.carlitos, pml.training.adelmo)
sum(modelFit.carlitos.predict.adelmo == pml.training.adelmo$classe)/length(pml.training.adelmo$classe)
modelFit.carlitos.predict.carlitos <- predict(modelFit.carlitos, pml.training.carlitos)
sum(modelFit.carlitos.predict.carlitos == pml.training.carlitos$classe)/length(pml.training.carlitos$classe)
modelFit.carlitos.predict.charles <- predict(modelFit.carlitos, pml.training.charles)
sum(modelFit.carlitos.predict.charles == pml.training.charles$classe)/length(pml.training.charles$classe)
modelFit.carlitos.predict.eurico <- predict(modelFit.carlitos, pml.training.eurico)
sum(modelFit.carlitos.predict.eurico == pml.training.eurico$classe)/length(pml.training.eurico$classe)
modelFit.carlitos.predict.jeremy <- predict(modelFit.carlitos, pml.training.jeremy)
sum(modelFit.carlitos.predict.jeremy == pml.training.jeremy$classe)/length(pml.training.jeremy$classe)
modelFit.carlitos.predict.pedro <- predict(modelFit.carlitos, pml.training.pedro)
sum(modelFit.carlitos.predict.pedro == pml.training.pedro$classe)/length(pml.training.pedro$classe)
modelFit.carlitos.predict.testing <- predict(modelFit.carlitos, pml.testing.orig)
modelFit.carlitos.predict.testing

# subset charles vs rest of subjects - training vs. testing, run model, and predict results 
modelFit.charles <- train(classe~., data=pml.training.charles, method="rf")
print(modelFit.charles$finalModel)
print(modelFit.charles$results)
modelFit.charles
modelFit.charles.predict.adelmo <- predict(modelFit.charles, pml.training.adelmo)
sum(modelFit.charles.predict.adelmo == pml.training.adelmo$classe)/length(pml.training.adelmo$classe)
modelFit.charles.predict.carlitos <- predict(modelFit.charles, pml.training.carlitos)
sum(modelFit.charles.predict.carlitos == pml.training.carlitos$classe)/length(pml.training.carlitos$classe)
modelFit.charles.predict.charles <- predict(modelFit.charles, pml.training.charles)
sum(modelFit.charles.predict.charles == pml.training.charles$classe)/length(pml.training.charles$classe)
modelFit.charles.predict.eurico <- predict(modelFit.charles, pml.training.eurico)
sum(modelFit.charles.predict.eurico == pml.training.eurico$classe)/length(pml.training.eurico$classe)
modelFit.charles.predict.jeremy <- predict(modelFit.charles, pml.training.jeremy)
sum(modelFit.charles.predict.jeremy == pml.training.jeremy$classe)/length(pml.training.jeremy$classe)
modelFit.charles.predict.pedro <- predict(modelFit.charles, pml.training.pedro)
sum(modelFit.charles.predict.pedro == pml.training.pedro$classe)/length(pml.training.pedro$classe)
modelFit.charles.predict.testing <- predict(modelFit.charles, pml.testing.orig)
modelFit.charles.predict.testing

# subset eurico vs rest of subjects - training vs. testing, run model, and predict results 
modelFit.eurico <- train(classe~., data=pml.training.eurico, method="rf")
print(modelFit.eurico$finalModel)
print(modelFit.eurico$results)
modelFit.eurico
modelFit.eurico.predict.adelmo <- predict(modelFit.eurico, pml.training.adelmo)
sum(modelFit.eurico.predict.adelmo == pml.training.adelmo$classe)/length(pml.training.adelmo$classe)
modelFit.eurico.predict.carlitos <- predict(modelFit.eurico, pml.training.carlitos)
sum(modelFit.eurico.predict.carlitos == pml.training.carlitos$classe)/length(pml.training.carlitos$classe)
modelFit.eurico.predict.charles <- predict(modelFit.eurico, pml.training.charles)
sum(modelFit.eurico.predict.charles == pml.training.charles$classe)/length(pml.training.charles$classe)
modelFit.eurico.predict.eurico <- predict(modelFit.eurico, pml.training.eurico)
sum(modelFit.eurico.predict.eurico == pml.training.eurico$classe)/length(pml.training.eurico$classe)
modelFit.eurico.predict.jeremy <- predict(modelFit.eurico, pml.training.jeremy)
sum(modelFit.eurico.predict.jeremy == pml.training.jeremy$classe)/length(pml.training.jeremy$classe)
modelFit.eurico.predict.pedro <- predict(modelFit.eurico, pml.training.pedro)
sum(modelFit.eurico.predict.pedro == pml.training.pedro$classe)/length(pml.training.pedro$classe)
modelFit.eurico.predict.testing <- predict(modelFit.eurico, pml.testing.orig)
modelFit.eurico.predict.testing

# subset jeremy vs rest of subjects - training vs. testing, run model, and predict results 
modelFit.jeremy <- train(classe~., data=pml.training.jeremy, method="rf")
print(modelFit.jeremy$finalModel)
print(modelFit.jeremy$results)
modelFit.jeremy
modelFit.jeremy.predict.adelmo <- predict(modelFit.jeremy, pml.training.adelmo)
sum(modelFit.jeremy.predict.adelmo == pml.training.adelmo$classe)/length(pml.training.adelmo$classe)
modelFit.jeremy.predict.carlitos <- predict(modelFit.jeremy, pml.training.carlitos)
sum(modelFit.jeremy.predict.carlitos == pml.training.carlitos$classe)/length(pml.training.carlitos$classe)
modelFit.jeremy.predict.charles <- predict(modelFit.jeremy, pml.training.charles)
sum(modelFit.jeremy.predict.charles == pml.training.charles$classe)/length(pml.training.charles$classe)
modelFit.jeremy.predict.eurico <- predict(modelFit.jeremy, pml.training.eurico)
sum(modelFit.jeremy.predict.eurico == pml.training.eurico$classe)/length(pml.training.eurico$classe)
modelFit.jeremy.predict.jeremy <- predict(modelFit.jeremy, pml.training.jeremy)
sum(modelFit.jeremy.predict.jeremy == pml.training.jeremy$classe)/length(pml.training.jeremy$classe)
modelFit.jeremy.predict.pedro <- predict(modelFit.jeremy, pml.training.pedro)
sum(modelFit.jeremy.predict.pedro == pml.training.pedro$classe)/length(pml.training.pedro$classe)
modelFit.jeremy.predict.testing <- predict(modelFit.jeremy, pml.testing.orig)
modelFit.jeremy.predict.testing

# subset pedro vs rest of subjects - training vs. testing, run model, and predict results 
modelFit.pedro <- train(classe~., data=pml.training.pedro, method="rf")
print(modelFit.pedro$finalModel)
print(modelFit.pedro$results)
modelFit.pedro
modelFit.pedro.predict.adelmo <- predict(modelFit.pedro, pml.training.adelmo)
sum(modelFit.pedro.predict.adelmo == pml.training.adelmo$classe)/length(pml.training.adelmo$classe)
modelFit.pedro.predict.carlitos <- predict(modelFit.pedro, pml.training.carlitos)
sum(modelFit.pedro.predict.carlitos == pml.training.carlitos$classe)/length(pml.training.carlitos$classe)
modelFit.pedro.predict.charles <- predict(modelFit.pedro, pml.training.charles)
sum(modelFit.pedro.predict.charles == pml.training.charles$classe)/length(pml.training.charles$classe)
modelFit.pedro.predict.eurico <- predict(modelFit.pedro, pml.training.eurico)
sum(modelFit.pedro.predict.eurico == pml.training.eurico$classe)/length(pml.training.eurico$classe)
modelFit.pedro.predict.jeremy <- predict(modelFit.pedro, pml.training.jeremy)
sum(modelFit.pedro.predict.jeremy == pml.training.jeremy$classe)/length(pml.training.jeremy$classe)
modelFit.pedro.predict.pedro <- predict(modelFit.pedro, pml.training.pedro)
sum(modelFit.pedro.predict.pedro == pml.training.pedro$classe)/length(pml.training.pedro$classe)
modelFit.pedro.predict.testing <- predict(modelFit.pedro, pml.testing.orig)
modelFit.pedro.predict.testing


# combine predictors side by side for the split by subjects 
modelFit.predict.bySubjects <- cbind(
  adelmo = modelFit.adelmo.predict.testing, 
  carlos = modelFit.carlitos.predict.testing, 
  charles = modelFit.charles.predict.testing, 
  eurico = modelFit.eurico.predict.testing, 
  jeremy = modelFit.jeremy.predict.testing, 
  pedro = modelFit.pedro.predict.testing 
  ) 

# combine predictors side by side for the split by percent 
modelFit.predict.byPercents <- cbind(
  p5 = modelFit.p5.predict.testing, 
  p25 = modelFit.p25.predict.testing, 
  p75 = modelFit.p75.predict.testing
  )

# combine predictors side by side for all splits 
modelFit.predict.byAll <- cbind(
  modelFit.predict.bySubjects
  , all = modelFit.all.predict.testing 
  , modelFit.predict.byPercents
)

# combine predictors side by side for the split by subjects 
modelFit.predict.bySubjects
modelFit.predict.byPercents
modelFit.predict.byAll

modelFit.p75.predict.testing
modelFit.p75.predict.submission = as.character(modelFit.p75.predict.testing)
# print final results 
print(modelFit.p75$finalModel)
print(modelFit.p75$results)
plot(modelFit.p75$finalModel)
modelFit.p75.predict.testing

# prepare for submission 

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(modelFit.p75.predict.submission) 
