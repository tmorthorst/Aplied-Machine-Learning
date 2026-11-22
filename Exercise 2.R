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
