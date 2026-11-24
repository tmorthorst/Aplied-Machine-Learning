# ==========================================
# Task 1: First contact
# ==========================================
df <- read.csv("Data/telco_churn.csv")

# Set reference levels explicitly so the coefficient reflects
# Premium vs. Basic for Churn = "Yes"
df$churn <- factor(df$churn, levels = c("No", "Yes"))
df$plan  <- factor(df$plan, levels = c("Basic", "Premium"))

model <- glm(churn ~ plan, data = df, family = binomial(link = "logit"))
summary(model)

# Odds ratio and 95% confidence interval
exp(cbind(OR = coef(model), confint(model)))

#Check
tapply(df$churn=="Yes",df$plan, mean)


# ==========================================
# Task 2: The sign that flips
# ==========================================
df <- read.csv("Data/telco_churn.csv")
df$churn <- factor(df$churn, levels = c("No", "Yes"))
df$plan  <- factor(df$plan, levels = c("Basic", "Premium"))

# Model 1: plan alone
m1 <- glm(churn ~ plan, data = df, family = binomial(link = "logit"))

# Model 2: plan + monthly_fee_dkk
m2 <- glm(churn ~ plan + monthly_fee_dkk, data = df, family = binomial(link = "logit"))

summary(m1)
summary(m2)

# Side-by-side comparison of the planPremium coefficient (log-odds)
c(m1 = coef(m1)["planPremium"], m2 = coef(m2)["planPremium"])

#check
cor(df$monthly_fee_dkk,df$plan=='Premium')
tapply(df$monthly_fee_dkk,df$plan,mean)

# ==========================================
# Task 2: The accuracy paradox
# ==========================================
set.seed(42)
df <- read.csv("Data/telco_churn.csv")

# Ensure categorical variables are factors with explicit reference for churn
df$churn         <- factor(df$churn, levels = c("No", "Yes"))
df$plan          <- factor(df$plan)
df$autopay       <- factor(df$autopay)
df$contract_type <- factor(df$contract_type)

# 80/20 train-test split to avoid optimistic in-sample accuracy
train_idx <- sample(seq_len(nrow(df)), size = floor(0.8 * nrow(df)))
train <- df[train_idx, ]
test  <- df[-train_idx, ]

model <- glm(
  churn ~ plan + monthly_fee_dkk + tenure_months + support_calls + autopay + contract_type,
  data = train,
  family = binomial(link = "logit")
)
summary(model)

# Out-of-sample predictions at the default 0.5 threshold
probs <- predict(model, newdata = test, type = "response")
preds <- factor(ifelse(probs >= 0.5, "Yes", "No"), levels = c("No", "Yes"))

cm <- table(Predicted = preds, Actual = test$churn)
cm

accuracy <- mean(preds == test$churn, na.rm=T)
recall   <- cm["Yes", "Yes"] / sum(cm[, "Yes"])
precision <- cm["Yes", "Yes"] / sum(cm["Yes", ])

c(Accuracy = accuracy, Recall = recall, Precision = precision)


#Check
table(df$churn)
(no_information <- (410)/(410+190))

#using the whole dataset instead of the training data AI made:
df <- read.csv("Data/telco_churn.csv")
model1<- glm(
  churn ~ plan + monthly_fee_dkk + tenure_months + support_calls + autopay + contract_type,
  data = df,
  family = binomial(link = "logit")
)
p<-predict(model1,type='response')
table(p>0.5, df$churn)
length(p)
length(df$churn)

# Forvekslingsmatrix
cm1 <- table(Predicted = p > 0.5, Actual = model1$model$churn)
cm1

# No-information rate på de 583 analyserede kunder
prop.table(table(model1$model$churn))

# Nøjagtighed (Accuracy)
mean((p > 0.5) == (model1$model$churn == "Yes"))


##############
# part B
##############

#############
# Task 4 #
#############
rm(list=ls())
set.seed(42)

# 1. Load data
df <- read.csv("Data/biomarker_panel.csv")
y <- ifelse(df$relapse == "Yes", 1, 0)
gene_cols <- grep("^gene_", names(df), value = TRUE)
X <- as.matrix(df[, gene_cols])

# 2. Create 10 stratified folds
k <- 10
folds <- integer(length(y))
folds[y == 1] <- sample(rep(1:k, length.out = sum(y == 1)))
folds[y == 0] <- sample(rep(1:k, length.out = sum(y == 0)))

# 3. 10-fold CV with IN-FOLD feature selection
oof_probs <- numeric(length(y))

for (i in 1:k) {
  train_idx <- which(folds != i)
  test_idx  <- which(folds == i)
  
  # Select top 15 genes using ONLY training fold
  cors <- abs(apply(X[train_idx, ], 2, cor, y = y[train_idx]))
  top15 <- names(sort(cors, decreasing = TRUE))[1:15]
  
  # Fit logistic regression classifier
  train_df <- data.frame(y = y[train_idx], X[train_idx, top15])
  test_df  <- data.frame(X[test_idx, top15])
  
  fit <- glm(y ~ ., data = train_df, family = binomial)
  oof_probs[test_idx] <- predict(fit, newdata = test_df, type = "response")
}

# 4. Evaluate performance against majority-class baseline (NIR)
oof_preds <- ifelse(oof_probs >= 0.5, 1, 0)
cv_acc <- mean(oof_preds == y)
nir <- max(mean(y), 1 - mean(y))

test_res <- binom.test(sum(oof_preds == y), length(y), p = nir, alternative = "greater")

cat(sprintf("10-Fold CV Accuracy: %.3f\n", cv_acc))
cat(sprintf("Baseline (NIR):      %.3f\n", nir))
cat(sprintf("p-value (Acc > NIR): %.4f\n", test_res$p.value))

# 5. Final top 15 genes on full dataset (for reporting only, not accuracy estimation)
full_cors <- abs(apply(X, 2, cor, y = y))
final_top15 <- names(sort(full_cors, decreasing = TRUE))[1:15]
print(final_top15)


#check
#indlæser datasæt:
g<-read.csv("Data/biomarker_panel.csv")
# Find alle gen-kolonnerne (gene_001 til gene_200)
gcols <- grep("^gene_",names(g))
# Udregn korrelationen for hvert eneste gen med relapse == "Yes"
cors <- sapply(gcols,function(j) cor(g[[j]], g$relapse == "Yes"))
#finder AI's tærskel ( den 15. største absolutte korrelation)
top15_cutoff<- sort(abs(cors),decreasing =T)[15]
cat("15. højeste absolutte korrelation:",round(top15_cutoff,3),"\n")
#hvor mange gener når f.eks. over 0.25
cat("Antal gener med cor > 0.25:", sum(abs(cors)>0.25),"\n")
sum(abs(cors)>0.25)
#no-information rate for datasættet
table(g$relapse)
prop.table(table(g$relapse))
View(g)
top15_cutoff


###################
# TASK 6
##################
set.seed(42)

df <- read.csv("Data/biomarker_panel.csv")
y <- ifelse(df$relapse == "Yes", 1, 0)
X <- as.matrix(df[, grep("^gene_", names(df), value = TRUE)])

# Helper: compute nearest-centroid score (d0 - d1; higher = closer to relapse centroid)
nc_score <- function(X_train, y_train, X_eval) {
  top15 <- names(sort(abs(apply(X_train, 2, cor, y = y_train)), decreasing = TRUE))[1:15]
  mu1 <- colMeans(X_train[y_train == 1, top15, drop = FALSE])
  mu0 <- colMeans(X_train[y_train == 0, top15, drop = FALSE])
  
  X_sub <- X_eval[, top15, drop = FALSE]
  d1 <- sqrt(rowSums(sweep(X_sub, 2, mu1, "-")^2))
  d0 <- sqrt(rowSums(sweep(X_sub, 2, mu0, "-")^2))
  return(d0 - d1)
}

# 1. Naive (Leaky) Gene Risk Score using all 90 patients
df$gene_score_leaky <- nc_score(X, y, X)
fit_leaky <- glm(y ~ gene_score_leaky + tumor_size_cm, data = df, family = binomial)

cat("=== NAIVE (LEAKY) COEFFICIENT TABLE ===\n")
print(summary(fit_leaky)$coefficients)

# 2. Corrected (Out-of-Fold) Gene Risk Score via 10-fold stratified CV
k <- 10
folds <- integer(length(y))
folds[y == 1] <- sample(rep(1:k, length.out = sum(y == 1)))
folds[y == 0] <- sample(rep(1:k, length.out = sum(y == 0)))

oof_score <- numeric(length(y))
for (i in 1:k) {
  tr <- which(folds != i)
  te <- which(folds == i)
  oof_score[te] <- nc_score(X[tr, ], y[tr], X[te, ])
}

df$gene_score_oof <- oof_score
fit_oof <- glm(y ~ gene_score_oof + tumor_size_cm, data = df, family = binomial)

cat("\n=== CORRECTED (OUT-OF-FOLD) COEFFICIENT TABLE ===\n")
print(summary(fit_oof)$coefficients)



#Check

gcols <- grep("^gene_", names(g))
score_fold <- function(train, test) {
  cors <- sapply(gcols, function(j) cor(g[[j]][train], g$relapse[train]=="Yes"))
  sel <- gcols[order(-abs(cors))][1:15]
  mu1 <- colMeans(g[train, ][g$relapse[train]=="Yes", sel])
  mu0 <- colMeans(g[train, ][g$relapse[train]=="No", sel])
  Xt <- as.matrix(g[test, sel])
  rowSums(sweep(Xt, 2, mu0)^2) - rowSums(sweep(Xt, 2, mu1)^2)
}
set.seed(1) ; folds <- sample(rep(1:9, length.out = nrow(g)))
z_tilde <- numeric(nrow(g))
for (k in 1:9) {
  test <- which(folds == k) ; train <- which(folds != k)
  z_tilde[test] <- score_fold(train, test)
}
fit2 <- glm(relapse=="Yes" ~ scale(z_tilde) + tumor_size_cm,
            data = g, family = binomial)
summary(fit2)



################
# Task 7
################
set.seed(42)

df <- read.csv("Data/biomarker_panel.csv")
df$y <- ifelse(df$relapse == "Yes", 1, 0)

# 1. Model-based standard error
fit <- glm(y ~ tumor_size_cm, data = df, family = binomial)
se_model <- summary(fit)$coefficients["tumor_size_cm", "Std. Error"]

# 2. Bootstrap standard error (2,000 resamples)
B <- 2000
n <- nrow(df)
boot_coefs <- numeric(B)

for (b in 1:B) {
  idx <- sample.int(n, size = n, replace = TRUE)
  boot_fit <- glm(y ~ tumor_size_cm, data = df[idx, ], family = binomial)
  boot_coefs[b] <- coef(boot_fit)["tumor_size_cm"]
}

se_boot <- sd(boot_coefs)

cat(sprintf("Model-based SE:     %.4f\n", se_model))
cat(sprintf("Bootstrap SE:       %.4f\n", se_boot))
cat(sprintf("Ratio (Boot/Model): %.3f\n", se_boot / se_model))
