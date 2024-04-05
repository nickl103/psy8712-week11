# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)

# Data Import and Cleaning
gss_import_tbl <- read_spss("../data/GSS2016.sav") %>%
  filter(!is.na(MOSTHRS)) %>%
  rename(`work hours`= MOSTHRS) %>%
  select(-HRS1, -HRS2)

gss_tbl <- gss_import_tbl[, colSums(is.na(gss_import_tbl)) < .75 * nrow(gss_import_tbl)] %>%
  mutate(across(everything(), as.numeric))

# Visualization
ggplot(gss_tbl,
       aes(x=`work hours`)) +
  geom_histogram()

# Analysis
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
OLS_time <- toc() #end time and 7.209 seconds elapsed
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
EN_time <- toc() #end time and 9.087 seconds elapsed. 
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
RF_time <- toc() #end time() and 72.519 seconds
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
XGB_time <- toc() #end time and 9.757 seconds elapsed. 
XGB_model
cv_XGB <- max(XGB_model$results$Rsquared)
holdout_XGB <- cor(
  predict(XGB_model, test_tbl, na.action = na.pass),
  test_tbl$`work hours`
)^2

summary(resamples(list(OLS_model, EN_model, RF_model, XGB_model)), metric="Rsquared")
dotplot(resamples(list(OLS_model, EN_model, RF_model, XGB_model)), metric="Rsquared")

# Publication
make_it_pretty <- function (formatme) {
  formatme <- formatC(formatme, format="f", digits=2)
  formatme <- str_remove(formatme, "^0")
  return(formatme)
}

table1_tbl <- tibble(
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