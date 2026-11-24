install.packages("glmnet")

#PART A 
##############
# TASK 1
#############
# Load data
df <- read.csv("Data/firm_signals.csv")

# Select only y_A and x01..x80 (in case other columns like IDs exist)
predictors <- sprintf("x%02d", 1:80)
fit <- lm(y_A ~ ., data = df[, c("y_A", predictors)])

# Inspect R-squared, Adjusted R-squared, and overall F-test
s <- summary(fit)
cat("R-squared:         ", s$r.squared, "\n")
cat("Adjusted R-squared:", s$adj.r.squared, "\n")
cat("F-stat p-value:    ", pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE), "\n")

# Quick 10-fold cross-validation check (requires caret)
install.packages("caret")
library(caret)
set.seed(42)
cv_fit <- train(
  y_A ~ .,
  data = df[, c("y_A", predictors)],
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)
cv_fit$results$Rsquared


#Check test set.
te <- read.csv("Data/firm_signals_test.csv")
X <- as.matrix(df[,paste0("x",sprintf("%02d",1:80))])
summary(lm(y_A~X,data=df))$r.squared
test_mse_mean <-mean((te$y_A-mean(df$y_A))^2)

#test MSE for OLS-modellen på testdata (2.000 observation):
pred_te <- predict(fit, newdata=te)
test_mse_ols <- mean((te$y_A-pred_te)^2)
#udskriver alle tallene:
cat("R-squared:         ", s$r.squared, "\n")
cat("Adjusted R-squared:", s$adj.r.squared, "\n")
cat("test MSE(OLS):",round(test_mse_ols,4),"\n")
cat("test MSE (predict mean):", round(test_mse_mean,4),"\n")



###########
# TASK 2
###########
library(glmnet)
df <- read.csv("Data/firm_signals.csv")
predictors <- sprintf("x%02d", 1:80)

X <- as.matrix(df[, predictors])
y <- df$y_A

# 10-fold CV Lasso (alpha = 1; standardizes predictors by default)
set.seed(42)
cv_lasso <- cv.glmnet(X, y, alpha = 1, nfolds = 10)

# Helper to extract non-zero signals (excluding intercept)
get_selected <- function(fit, s) {
  coefs <- coef(fit, s = s)
  idx <- which(coefs[-1] != 0)
  data.frame(signal = rownames(coefs)[-1][idx], coef = coefs[-1][idx])
}

# Compare the two standard penalty choices
selected_1se <- get_selected(cv_lasso, "lambda.1se") # sparser, fewer false positives
selected_min <- get_selected(cv_lasso, "lambda.min") # lowest CV error, keeps more signals

print(selected_1se)
print(selected_min)

#check
te <- read.csv("Data/firm_signals_test.csv")
Xte <- as.matrix(te[, predictors])
yte <- te$y_A
# 2. Score Lasso på testdata

pred_lasso_min <- predict(cv_lasso, newx=Xte, s="lambda.min")
pred_lasso_1se <- predict(cv_lasso, newx =Xte, s ='lambda.1se')
test_mse_lasso_min <- mean((yte - pred_lasso_min)^2)
test_mse_lasso_1se <- mean((yte-pred_lasso_1se)^2)

#3 fit Ridge (alpha =0 ) på samme træningsdata
set.seed(42)
cv_ridge <- cv.glmnet(X,y, alpha =0, nfolds =10)
#4 score ridge på testdata
pred_ridge_min <-predict(cv_ridge, newx=Xte, s='lambda.min')
test_mse_ridge <- mean((yte-pred_ridge_min)^2)
#hvor mange koefficienter sætter ridge til 0?
n_ridge_zeros<- sum(coef(cv_ridge, s='lambda.min')[-1]==0)

#5 udskriver overblik
cat('=== Task 2 sammmenligning på testdata === \n')
cat("Ridge Test MSE (lambda.min):",round(test_mse_ridge,4),"\n")
cat("Lasso Test MSE (lambda.min): ", round(test_lasso_min <- test_mse_lasso_min, 4), "\n")
cat("Lasso Test MSE (lambda.1se): ", round(test_mse_lasso_1se, 4), "\n")
cat("Antal Ridge-koefficienter = 0:", n_ridge_zeros, "\n")
cat("Antal Lasso overlevende (min):", nrow(selected_min), "\n")
cat("Antal Lasso overlevende (1se):", nrow(selected_1se), "\n")




##############
# Task 3 
#############
library(glmnet)

df <- read.csv("Data/firm_signals.csv")
predictors <- sprintf("x%02d", 1:80)

X <- as.matrix(df[, predictors])
y <- df$y_B

# Shared 10-fold split for an apples-to-apples comparison
set.seed(42)
foldid <- sample(rep(1:10, length.out = nrow(X)))

cv_ridge <- cv.glmnet(X, y, alpha = 0, foldid = foldid)
cv_lasso <- cv.glmnet(X, y, alpha = 1, foldid = foldid)

# Helper to extract CV MSE, RMSE, R^2, and non-zero count
eval_cv <- function(cv_fit, s) {
  idx <- which(cv_fit$lambda == cv_fit[[s]])
  mse <- cv_fit$cvm[idx]
  r2  <- 1 - mse / var(y)
  nz  <- sum(coef(cv_fit, s = s)[-1] != 0)
  c(MSE = mse, RMSE = sqrt(mse), CV_R2 = r2, NonZero = nz)
}

results <- rbind(
  Ridge_min = eval_cv(cv_ridge, "lambda.min"),
  Lasso_min = eval_cv(cv_lasso, "lambda.min"),
  Ridge_1se = eval_cv(cv_ridge, "lambda.1se"),
  Lasso_1se = eval_cv(cv_lasso, "lambda.1se")
)
print(round(results, 4))


#check på test

pred_ridge_B <- predict(cv_ridge,newx=Xte, s="lambda.min")
pred_lasso_B <- predict(cv_lasso,newx=Xte, s='lambda.min')
test_mse_ridge_B <- mean((te$y_B-pred_ridge_B)^2)
test_mse_lasso_B <- mean((te$y_B-pred_lasso_B)^2)

#hvilke varaible overlever i Lasso ved lambda.1se
coef_1se <- coef(cv_lasso, s='lambda.1se')
surviving_1se <- rownames(coef_1se)[which(coef_1se[-1]!=0)+1]

#udskriv resultater
cat("=== TASK 3 TEST MSE PÅ y_B ===\n")
cat("Ridge Test MSE (lambda.min):", round(test_mse_ridge_B, 4), "\n")
cat("Lasso Test MSE (lambda.min):", round(test_mse_lasso_B, 4), "\n")

cat("Overlevende variable ved lambda.1se:\n")
print(surviving_1se)

#2x2 tabel:
mse_df <- data.frame(
  Model = c("Ridge","lasso"),
  y_A =c(test_mse_ridge,test_mse_lasso_min),
  y_B =c(test_mse_ridge_B,test_mse_lasso_B)
)
print(mse_df)

#Part B
##############
# task 4
#############
#Standard 10 fold cv for choosing lambda
library(glmnet)

df <- read.csv("Data/firm_signals.csv")
X <- as.matrix(df[, sprintf("x%02d", 1:80)])
y <- df$y_B

set.seed(42)
cv_lasso <- cv.glmnet(X, y, alpha = 1, nfolds = 10)

# CV MSE at lambda.min and lambda.1se
mse_min <- cv_lasso$cvm[cv_lasso$lambda == cv_lasso$lambda.min]
mse_1se <- cv_lasso$cvm[cv_lasso$lambda == cv_lasso$lambda.1se]

cat("Chosen lambda.min:", cv_lasso$lambda.min, "| CV MSE:", round(mse_min, 4), "\n")
cat("Chosen lambda.1se:", cv_lasso$lambda.1se, "| CV MSE:", round(mse_1se, 4), "\n")

#check
#test MSE på 2000 ubesete test-virksomheder ved lambda min
pred_te <- predict(cv_lasso,newx=Xte,s ='lambda.min')
test_mse <- mean((te$y_B- pred_te)^2)
#procentuel forskel
pct_diff <- ((test_mse-mse_min)/mse_min)*100
cat("=== TASK 4 TAL ===\n")
cat("1. CV MSE ved lambda.min: ", round(mse_min, 4), "\n")
cat("2. Test MSE (2.000 obs): ", round(test_mse, 4), "\n")
cat("3. Forskel i procent: ", round(pct_diff, 2), "%\n")

########
#task 5
#######
library(glmnet)

df <- read.csv("Data/firm_signals.csv")
X <- as.matrix(df[, sprintf("x%02d", 1:80)])
y <- df$y_B

set.seed(42)
cv_lasso <- cv.glmnet(X, y, alpha = 1, nfolds = 10)

# Optimal lambdas
cat("lambda.min:", cv_lasso$lambda.min, "\n")
cat("lambda.1se:", cv_lasso$lambda.1se, "\n\n")

# Extract non-zero signals (excluding intercept), sorted by |coef|
get_signals <- function(fit, s) {
  b <- as.matrix(coef(fit, s = s))[-1, 1]
  b <- b[b != 0]
  data.frame(signal = names(b), coef = unname(b))[order(-abs(b)), ]
}

cat("--- Selected at lambda.min ---\n")
print(get_signals(cv_lasso, "lambda.min"), row.names = FALSE)

cat("\n--- Selected at lambda.1se ---\n")
print(get_signals(cv_lasso, "lambda.1se"), row.names = FALSE)

#checktest

res <- t(sapply(1:10, function(s) {
  set.seed(s); cv <- cv.glmnet(X, df$y_B, alpha = 1)
  c(lmin = cv$lambda.min, l1se = cv$lambda.1se,
    n_min = sum(coef(cv, s = "lambda.min")[-1] != 0),
    n_1se = sum(coef(cv, s = "lambda.1se")[-1] != 0))
}))
round(res, 4); max(res[,"lmin"]) / min(res[,"lmin"])
max(res[,"lmin"])-min(res[,"lmin"])
max(res[,"l1se"])-min(res[,"l1se"])

# Test MSE for f.eks. seed 1, 2 og 4 (hvor lambda.min varierede mest)
te <- read.csv("Data/firm_signals_test.csv")
Xte <- as.matrix(te[,sprintf("x%02d",1:80)])
yBte <- te$y_B

test_mses <- sapply(c(1,2,4),function(s){
  set.seed(s)
  cv <-cv.glmnet(X,y,alpha=1)
  pred<-predict(cv,newx=Xte, s="lambda.min")
  mean((yBte-pred)^2)
})
cat("Test MSEs for de tre seeds:\n")
print(round(test_mses, 4))
cat("Interval (range):", round(min(test_mses), 3), "-", round(max(test_mses), 3), "\n")



#########
#task 6
########
df <- read.csv("Data/firm_signals.csv")
X <- as.matrix(df[, sprintf("x%02d", 1:80)])

# Shared 10-fold split for a fair comparison
set.seed(42)
foldid <- sample(rep(1:10, length.out = nrow(X)))

compare_methods <- function(y) {
  cv_ridge <- cv.glmnet(X, y, alpha = 0, foldid = foldid)
  cv_lasso <- cv.glmnet(X, y, alpha = 1, foldid = foldid)
  
  ridge_mse <- min(cv_ridge$cvm) # MSE at lambda.min
  lasso_mse <- min(cv_lasso$cvm) # MSE at lambda.min
  
  data.frame(
    Ridge_MSE = round(ridge_mse, 4),
    Lasso_MSE = round(lasso_mse, 4),
    Winner    = ifelse(ridge_mse < lasso_mse, "Ridge", "Lasso")
  )
}

results <- rbind(
  y_A = compare_methods(df$y_A),
  y_B = compare_methods(df$y_B)
)

print(results)


#Part C
#task 7
library(glmnet)

# 1. Load data and prepare matrix/vector
df <- read.csv("Data/firm_signals_corr.csv")
X <- as.matrix(df[, sprintf("x%02d", 1:40)])
y <- df$y_quiet

# 2. Fit 10-fold CV Lasso (alpha = 1)
set.seed(123)
cv_lasso <- cv.glmnet(X, y, alpha = 1, nfolds = 10)
plot(cv_lasso)

# 3. Extract non-zero predictors at lambda.min and lambda.1se
coef_min <- coef(cv_lasso, s = "lambda.min")
coef_1se <- coef(cv_lasso, s = "lambda.1se")

sel_min <- rownames(coef_min)[which(coef_min != 0)][-1] # drop intercept
sel_1se <- rownames(coef_1se)[which(coef_1se != 0)][-1]

cat("Selected at lambda.min:", sel_min, "\n")
cat("Selected at lambda.1se:", sel_1se, "\n")

# 4. Check CV MSE vs. null model (intercept-only)
null_mse <- mean((y - mean(y))^2)
min_mse  <- min(cv_lasso$cvm)
cat(sprintf("Null MSE: %.4f | Best Lasso CV MSE: %.4f\n", null_mse, min_mse))


#check
Xc <- as.matrix(df[, paste0("x", sprintf("%02d", 1:40))])
f <- glmnet(Xc, df$y_quiet, alpha = 1)
ord <- unique(unlist(apply(coef(f)[-1, ] != 0, 2, which)))
head(colnames(Xc)[ord], 8)

#correlation matrix
round(cor(Xc)[c(5,12,18,23,19,40),
              c(5,12,18,23,19,40)], 3)


########
#TASK 8
########



#check
sel <- matrix(0, 200, 40)
set.seed(1)
for (b in 1:200) {
  i <- sample(nrow(Xc), floor(0.8 * nrow(Xc)))
  cv <- cv.glmnet(Xc[i, ], df$y_noisy[i], alpha = 1)
  sel[b, ] <- as.numeric(coef(cv, s = "lambda.1se")[-1] != 0)
}
round(colMeans(sel), 3)[c(5, 12, 18, 23, 19, 40)]
mean(colMeans(sel)[-c(5, 12, 18, 23, 19, 40)])
