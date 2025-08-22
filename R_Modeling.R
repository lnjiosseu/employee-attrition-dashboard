# ---- Clear environment ----
rm(list = ls())

# ---- Libraries ----
library(tidyverse)
library(caret)
library(xgboost)
library(pROC)
library(ggplot2)
library(corrplot)   # for heatmap

# ---- Paths ----
data_path <- "/Users/caliboi/Desktop/Resumes/Github/Project 3/ibm_hr_attrition.csv"
base_dir  <- dirname(data_path)                       # same folder as the CSV
out_dir   <- file.path(base_dir, "dashboards")        # dashboards subfolder
dir.create(out_dir, showWarnings = FALSE)             # create if missing

# ---- Load data ----
hr <- read.csv(data_path)
cat("✅ Data loaded. Rows:", nrow(hr), "Columns:", ncol(hr), "\n")

# ---- Preprocess ----
hr$Attrition <- as.factor(hr$Attrition)

# Remove zero-variance predictors
nzv <- nearZeroVar(hr)
if (length(nzv) > 0) {
  hr <- hr[, -nzv]
  cat("✅ Removed", length(nzv), "zero-variance columns\n")
}

# ---- 📊 Visual 1: Class balance ----
p1 <- ggplot(hr, aes(x = Attrition, fill = Attrition)) +
  geom_bar() +
  labs(title = "Attrition Distribution", x = "Attrition", y = "Count") +
  theme_minimal() +
  theme(panel.background = element_rect(fill="white", color=NA))

print(p1) # show in RStudio
ggsave(filename = file.path(out_dir, "attrition_distribution.png"), plot = p1,
       width = 8, height = 5, dpi = 300)
cat("📊 Saved →", file.path(out_dir, "attrition_distribution.png"), "\n")

# ---- Train-test split ----
set.seed(123)
trainIndex <- createDataPartition(hr$Attrition, p=0.8, list=FALSE)
train <- hr[trainIndex, ]
test  <- hr[-trainIndex, ]

# ---- Logistic Regression ----
logit_model <- train(
  Attrition ~ ., 
  data = train, 
  method = "glm", 
  family = "binomial"
)

# ---- XGBoost ----
xgb_model <- train(
  Attrition ~ ., 
  data = train,
  method = "xgbTree",
  trControl = trainControl(
    method = "cv", 
    number = 5, 
    classProbs = TRUE, 
    summaryFunction = twoClassSummary,
    savePredictions = "final"
  ),
  metric = "ROC"
)

# ---- Predictions ----
logit_pred <- predict(logit_model, newdata = test, type = "prob")[,2]
xgb_pred   <- predict(xgb_model, newdata = test, type = "prob")[,2]

roc_logit <- roc(test$Attrition, logit_pred)
roc_xgb   <- roc(test$Attrition, xgb_pred)

auc_logit <- roc_logit$auc
auc_xgb   <- roc_xgb$auc

cat("✅ Logistic Regression AUC:", round(auc_logit, 3), "\n")
cat("✅ XGBoost AUC:", round(auc_xgb, 3), "\n")

# ---- 📊 Visual 2: ROC Curve ----
# Show in RStudio Viewer
plot(roc_logit, col="blue", main="ROC Curve - Logistic vs XGBoost")
plot(roc_xgb, col="red", add=TRUE)
legend("bottomright", legend=c("Logistic Regression", "XGBoost"),
       col=c("blue","red"), lwd=2)

# Save as PNG
png(file.path(out_dir, "roc_comparison.png"), width = 800, height = 500)
plot(roc_logit, col="blue", main="ROC Curve - Logistic vs XGBoost")
plot(roc_xgb, col="red", add=TRUE)
legend("bottomright", legend=c("Logistic Regression", "XGBoost"),
       col=c("blue","red"), lwd=2)
dev.off()
cat("📊 Saved →", file.path(out_dir, "roc_comparison.png"), "\n")

# ---- 📊 Visual 3: Feature Importance (XGBoost) ----
xgb_importance <- varImp(xgb_model)

p3 <- ggplot(xgb_importance, aes(x = reorder(rownames(xgb_importance$importance), Overall), y = Overall)) +
  geom_bar(stat="identity", fill="steelblue") +
  coord_flip() +
  labs(title="Feature Importance (XGBoost)", x="Features", y="Importance") +
  theme_minimal() +
  theme(panel.background = element_rect(fill="white", color=NA))

print(p3) # show in RStudio
ggsave(filename = file.path(out_dir, "feature_importance.png"), plot = p3,
       width = 8, height = 6, dpi = 300)
cat("📊 Saved →", file.path(out_dir, "feature_importance.png"), "\n")

# ---- 📊 Visual 4: Correlation Heatmap ----
# Convert Attrition to numeric for correlation
hr_corr <- hr
hr_corr$Attrition <- ifelse(hr_corr$Attrition == "Yes", 1, 0)

# Select only numeric columns
numeric_vars <- hr_corr %>% select(where(is.numeric))

# Compute correlations
corr_matrix <- cor(numeric_vars)

# Show in RStudio
corrplot(corr_matrix, method="color", type="upper", 
         tl.cex=0.7, tl.col="black",
         title="Correlation Heatmap (Numeric Features & Attrition)",
         mar=c(0,0,2,0))

# Save as PNG
png(file.path(out_dir, "correlation_heatmap.png"), width = 900, height = 700)
corrplot(corr_matrix, method="color", type="upper", 
         tl.cex=0.7, tl.col="black",
         title="Correlation Heatmap (Numeric Features & Attrition)",
         mar=c(0,0,2,0))
dev.off()
cat("📊 Saved →", file.path(out_dir, "correlation_heatmap.png"), "\n")

# ---- Summary Report ----
# Extract top 10 features
top10_feats <- xgb_importance$importance %>%
  tibble::rownames_to_column("Feature") %>%
  arrange(desc(Overall)) %>%
  head(10)

summary_txt <- paste0(
  "IBM Attrition Models Summary\n",
  "=============================\n",
  "Logistic Regression AUC: ", round(auc_logit, 3), "\n",
  "XGBoost AUC: ", round(auc_xgb, 3), "\n",
  "Rows: ", nrow(hr), " | Columns: ", ncol(hr), "\n\n",
  "Top 10 Features (XGBoost):\n",
  paste0(sprintf("%2d. %s — %.4f", 1:nrow(top10_feats), 
                 top10_feats$Feature, top10_feats$Overall), collapse = "\n")
)

writeLines(summary_txt, file.path(out_dir, "model_summary.txt"))
cat("📄 Saved summary report →", file.path(out_dir, "model_summary.txt"), "\n")

cat("✅ All dashboard plots and summary saved in:", out_dir, "\n")
