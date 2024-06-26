# Script Settings and Resources
library(tidyverse)
library(haven)
library(caret)
library(stats)
library(parallel)
library(doParallel)
library(tictoc)

# Data Import and Cleaning
gss_import_tbl <- read_spss("/home/lande065/nickl103/psy8712-week11/data/GSS2016.sav") %>% #I know this isn't reproducible but I couldn't get it to work any other way. 
  filter(!is.na(MOSTHRS)) %>%
  rename(`work hours`= MOSTHRS) %>%
  select(-HRS1, -HRS2)

gss_tbl <- gss_import_tbl[, colSums(is.na(gss_import_tbl)) < .75 * nrow(gss_import_tbl)] %>%
  mutate(across(everything(), as.numeric))


# Analysis
set.seed(1234)

holdout_indices <- createDataPartition(gss_tbl$`work hours`,
                                       p = .25,
                                       list = T)$Resample1
test_tbl <- gss_tbl[holdout_indices,]
training_tbl <- gss_tbl[-holdout_indices,]

training_folds <- createFolds(training_tbl$`work hours`)

tic() #start time
OLS_model <- train(
  `work hours` ~ .,
  training_tbl,
  method="lm",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = training_folds)
)
OLS_toc <- toc() #end time 
OLS_time <- OLS_toc$toc - OLS_toc$tic
OLS_model
cv_OLS <- OLS_model$results$Rsquared
holdout_OLS <- cor(
  predict(OLS_model, test_tbl, na.action = na.pass),
  test_tbl$`work hours`
)^2


tic() #start time
EN_model <- train(
  `work hours` ~ .,
  training_tbl,
  method="glmnet",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = training_folds)
)
EN_toc <- toc() #end time 
EN_time <- EN_toc$toc - EN_toc$tic
EN_model
cv_EN <- max(EN_model$results$Rsquared)
holdout_EN <- cor(
  predict(EN_model, test_tbl, na.action = na.pass),
  test_tbl$`work hours`
)^2




tic() #start time
RF_model <- train(
  `work hours` ~ .,
  training_tbl,
  method="ranger",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = training_folds)
)
RF_toc <- toc() #end time
RF_time <- RF_toc$toc - RF_toc$tic
RF_model
cv_RF <- max(RF_model$results$Rsquared)
holdout_RF <- cor(
  predict(RF_model, test_tbl, na.action = na.pass),
  test_tbl$`work hours`
)^2



tic() #start time
XGB_model <- train(
  `work hours` ~ .,
  training_tbl,
  method="xgbLinear",
  na.action = na.pass,
  tuneLength = 1,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = training_folds)
)
XGB_toc <- toc() #end time  
XGB_time <- XGB_toc$toc - XGB_toc$tic
XGB_model
cv_XGB <- max(XGB_model$results$Rsquared)
holdout_XGB <- cor(
  predict(XGB_model, test_tbl, na.action = na.pass),
  test_tbl$`work hours`
)^2



summary(resamples(list(OLS_model, EN_model, RF_model, XGB_model)), metric="Rsquared")
dotplot(resamples(list(OLS_model, EN_model, RF_model, XGB_model)), metric="Rsquared")


###PARALLEL MODELS 
#I just copied the code from the original models and added a p for parallel

local_cluster <- makeCluster(40) #making the local cluster based on demonstration, chose random number because why not. 
registerDoParallel(local_cluster) #telling R to parallelize it based on lecture

#OLS parallel model, this doesn't parallelize according to lecture 
tic() #start time for benchmarking for models 
OLS_p_model <- train(
  `work hours` ~ .,
  training_tbl,
  method="lm",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = training_folds)
)
OLS_p_toc <- toc() #end time 
OLS_p_time <- OLS_p_toc$toc - OLS_p_toc$tic #calculating time elapsed based on lecture for Table later
OLS_p_model
cv_p_OLS <- OLS_p_model$results$Rsquared
holdout_p_OLS <- cor(
  predict(OLS_p_model, test_tbl, na.action = na.pass),
  test_tbl$`work hours`
)^2

#Elastic Net Parallel Model
tic() #start time
EN_p_model <- train(
  `work hours` ~ .,
  training_tbl,
  method="glmnet",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = training_folds)
)
EN_p_toc <- toc() #end time  
EN_p_time <- EN_p_toc$toc - EN_p_toc$tic #calculating time elapsed based on lecture for Table later
EN_p_model
cv_p_EN <- max(EN_p_model$results$Rsquared)
holdout_p_EN <- cor(
  predict(EN_p_model, test_tbl, na.action = na.pass),
  test_tbl$`work hours`
)^2

#Random Forest parallel model
tic() #start time
RF_p_model <- train(
  `work hours` ~ .,
  training_tbl,
  method="ranger",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = training_folds)
)
RF_p_toc <- toc() #end time
RF_p_time <- RF_p_toc$toc - RF_p_toc$tic #calculating time elapsed for Table later
RF_p_model
cv_p_RF <- max(RF_p_model$results$Rsquared)
holdout_p_RF <- cor(
  predict(RF_p_model, test_tbl, na.action = na.pass),
  test_tbl$`work hours`
)^2

###eXtreme gradient boosting parallel model 
tic() #start time
XGB_p_model <- train(
  `work hours` ~ .,
  training_tbl,
  method="xgbLinear",
  na.action = na.pass,
  tuneLength = 1,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = training_folds)
)
XGB_p_toc <- toc() #end time . 
XGB_p_time <- XGB_p_toc$toc - XGB_p_toc$tic #calculating time elapsed for table later.
XGB_p_model
cv_p_XGB <- max(XGB_p_model$results$Rsquared)
holdout_p_XGB <- cor(
  predict(XGB_p_model, test_tbl, na.action = na.pass),
  test_tbl$`work hours`
)^2



summary(resamples(list(OLS_p_model, EN_p_model, RF_p_model, XGB_p_model)), metric="Rsquared")
dotplot(resamples(list(OLS_p_model, EN_p_model, RF_p_model, XGB_p_model)), metric="Rsquared")

stopCluster(local_cluster)
registerDoSEQ()



# Publication
make_it_pretty <- function (formatme) {
  formatme <- formatC(formatme, format="f", digits=2)
  formatme <- str_remove(formatme, "^0")
  return(formatme)
}

`Table 3` <- tibble(
  algo = c("regression","elastic net","random forests","xgboost"),
  cv_rqs = c(
    make_it_pretty(cv_OLS),
    make_it_pretty(cv_EN),
    make_it_pretty(cv_RF),
    make_it_pretty(cv_XGB)
  ),
  ho_rqs = c(
    make_it_pretty(holdout_OLS),
    make_it_pretty(holdout_EN),
    make_it_pretty(holdout_RF),
    make_it_pretty(holdout_XGB)
  )
)

`Table 4` <- tibble(
  algo= c("regression", "elastic net", "random forests", "xgboost"),
  supercomputer = c(OLS_time, EN_time, RF_time, XGB_time),
  supercomputer_40 = c(OLS_p_time, EN_p_time, RF_p_time, XGB_p_time)
    
)


write.csv(`Table 3`, "table3.csv") 
write.csv(`Table 4`, "table4.csv")

#1.Elastic Net, Random Forest, and eXtreme Gradiant Boosting all benefited from the supercomputer, but Random Forest benefited the most going from 52.389 to 11.643. The supercomputer was able to run through the more hyperparameters a lot quicker than my local computer. 
#2.The number of cores used lessens the amount of time needed to run the models. 
#3.For the models we used for this assignment, I probably would not recommend using the supercomputer because even the longest model only took slightly over a minute at the longest for only a .67 Rsquared at the highest (random forest model), while the work it took to get the files in order to use the supercomputer took hours to do. The benefit of using the supercomputer (less time to run each model) was outweighed by the cons in my opinion. Since I'm recommending agaisnt using the supercomputer, like with the local file, I would choose the XGB model since it had the best R squared compared to the time it took for the model to work. 