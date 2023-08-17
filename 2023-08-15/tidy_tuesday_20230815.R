## -------------------------------------------------
##
## Script name: Tidy Tuesday (Spam)
##
## Purpose of script: Analyze spam dataset
##
## Author: Daniel Dorfsman
##
## Date Created: 2023-08-15
##
## -------------------------------------------------
##
## Notes:
##   
##
## -------------------------------------------------


## load packages

library(tidyverse)
library(here)
library(DescTools)
library(MASS)
library(pROC)

# MASS select function conflicts with dplyr select function

select <- dplyr::select

## -------------------------------------------------

## load data

spam <- read_csv("spam.csv", col_names = TRUE)

## -------------------------------------------------

## summarize

# check for missing data
sapply(spam, anyNA)

# overall 

# mean of each column 
spam %>% 
  summarize(across(-yesno, ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"))

# mean of each column, grouped by spam status (y/n)
spam %>% 
  group_by(yesno) %>% 
  summarize(across(everything(), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"))

# mean and standard deviation, grouped by spam status (y/n)
spam %>% 
  group_by(yesno) %>% 
  summarize(across(everything(), list(mean = mean, sd = sd), .names = "{.col}_{.fn}"))


## -------------------------------------------------

## visualize

# all variables
spam_vars <- grep("yesno", names(spam), invert = TRUE, value = TRUE)

# spam_vars %>%
#   map(~ ggplot(spam, aes(x = .data[[.x]])) +
#         geom_histogram(fill = "dodgerblue4", color = "black", alpha = 0.5) +
#         theme_classic()
#   )

# all variables, stratified by spam y/n status
spam_vars %>%
  map(~ggplot(spam, aes(x = .data[[.x]])) +
        geom_histogram(fill = "dodgerblue4", color = "black", alpha = 0.5) +
        facet_wrap(~ yesno) +
        theme_classic()
  )

# add winsorized columns for each variable
spam2 <- spam %>% 
  mutate(across(-yesno, ~ Winsorize(.x, probs = c(0.01, 0.99)), .names = "{.col}_winsorized"))

select(spam2, contains("winsorized")) %>%
  names() %>%
  map(~ ggplot(spam2, aes(x = .data[[.x]])) +
      geom_histogram(fill = "dodgerblue4", color = "black", alpha = 0.5) +
      facet_wrap(~ yesno) +
      theme_classic()
)

## -------------------------------------------------

## model

spam2$yesno <- factor(spam2$yesno, levels = c("n", "y"))

# split sample
set.seed(123)
train <- sample(c(TRUE, FALSE), size = nrow(spam2), replace = TRUE, prob = c(0.7, 0.3))

spam_train <- spam2[train, grepl("yesno|winsor", names(spam2))]
spam_test <- spam2[!train, grepl("yesno|winsor", names(spam2))]

# train
spam_glm <- glm(yesno ~ ., data = spam_train, family = binomial)

# step AIC
spam_glm_step <- stepAIC(spam_glm, direction = "both")

# predict testing data
spam_test_probs <- predict(spam_glm_step, spam_test, type = "response")
spam_test_pred <- ifelse(spam_test_probs > 0.5, "y", "n")

table(spam_test_pred, spam_test$yesno)

# at a > 50% classification cutoff, the model accuracy is 83.8%
mean(spam_test_pred == spam_test$yesno)

# plot ROC
par(pty = "s")
roc(spam_test$yesno, spam_test_probs, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab = "False Positive Percentage", ylab = "True Positive Percentage", 
    col = "dodgerblue4", lwd = 4, print.auc = TRUE)

roc_info <- roc(spam_test$yesno, spam_test_probs, legacy.axes = TRUE)

roc_df <- data.frame(true_positive_pct = roc_info$sensitivities*100,
                     false_positive_pct = (1 - roc_info$specificities)*100,
                     classification_threshold = roc_info$thresholds)


