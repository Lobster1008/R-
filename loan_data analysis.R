#################### 前置設定 ####################
# 載入資料
setwd('/Users/thomas/Documents/個人研究')
loan_data = read.csv('Loan_payments_data.csv', header = T)
attach(loan_data)

# function setting
# download package
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# table create
count_percentage <- function(col) {
  loan_data %>%  
    group_by(.data[[col]]) %>%
    dplyr::summarise(count = n(), .groups = "drop") %>%
    mutate(percentage = round(count / sum(count) * 100, 1))
}

# packages
packages <- c("ggplot2", "plyr", "tidyr", "gridExtra", "corrplot", "readr", "dplyr", "data.table", "glm", "randomForest", "e1071", "caret", "pROC")
ipak(packages)

#################### EDA ####################
# Data structure
str(loan_data)
sapply(loan_data, class)
summary(loan_data)

# Missing/Duplicate value
colSums(is.na(loan_data)) #past_due_days 的300筆NA為準時還款用戶，因此無逾期天數
sum(duplicated(loan_data))

loan_data <- loan_data %>%
  mutate(
    past_due_days = ifelse(is.na(past_due_days), 0, past_due_days)
  )

# Data str transform
loan_data$effective_date <- as.Date(loan_data$effective_date)
loan_data$due_date <- as.Date(loan_data$due_date)

loan_data$gender <- as.factor(loan_data$Gender)
loan_data$education <- as.factor(loan_data$education)
loan_data$loan_status <- as.factor(loan_data$loan_status)

# Data analysis
count_percentage("Gender")
count_percentage("education")
count_percentage("Principal")

# Loan Characteristics Distribution
summary(Principal)
summary(past_due_days)

loan_data$delay_group <- cut(past_due_days,
                             breaks = c(-1, 0, 7, 30, Inf),
                             labels = c("準時", "1-7天", "8-30天", ">30天"))
count_percentage("delay_group")

# Gender delay
gender_delay <- aggregate(past_due_days ~ Gender, data = loan_data, mean)
t.test(past_due_days ~ Gender, data = loan_data)

# Education delay
edu_delay <- aggregate(past_due_days ~ education, data = loan_data, mean)
kruskal.test(past_due_days ~ education, data = loan_data)


# Age delay
loan_data$age_group <- cut(loan_data$age,
                          breaks = c(17, 30, 45, 60),
                          labels = c("18-30", "31-45", "46-60"))
count_percentage("age_group")
anova_result <- aov(past_due_days ~ age_group, data = loan_data)
summary(anova_result)

#################### Modeling ####################
# Binary feature
loan_data$is_late <- ifelse(loan_data$past_due_days > 0, 1, 0)

# Feature selection
features <- c("age", "Gender", "education", "Principal", "terms")
X <- loan_data[, features]
y <- loan_data$is_late

# 70% training set, 30% testing set
set.seed(611650127)
train_idx <- sample(nrow(loan_data), 0.7 * nrow(loan_data))
train_data <- loan_data[train_idx, ]
test_data <- loan_data[-train_idx, ]

# Logistic model
logit_model <- glm(is_late ~ age + Gender + education + Principal,
                   data = train_data, family = binomial)
summary(logit_model)
logit_pred <- predict(logit_model, test_data, type = "response")
logit_pred_class <- ifelse(logit_pred > 0.5, 1, 0)
confusionMatrix(as.factor(logit_pred_class), as.factor(test_data$is_late))

# RF
rf_model <- randomForest(as.factor(is_late) ~ age + Gender + education + Principal,
                         data = train_data, ntree = 500)
rf_pred <- predict(rf_model, test_data)
confusionMatrix(rf_pred, as.factor(test_data$is_late))

#SVM
svm_model <- svm(as.factor(is_late) ~ age + Gender + education + Principal,
                 data = train_data, kernel = "radial")
svm_pred <- predict(svm_model, test_data)
confusionMatrix(svm_pred, as.factor(test_data$is_late))

# ROC curve
rf_roc <- roc(test_data$is_late, as.numeric(rf_pred))
plot(rf_roc, main = "ROC Curves Comparison")
