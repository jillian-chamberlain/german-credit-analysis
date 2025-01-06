
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r - step 1 and 2}
# read in data
germancredit.df <- read.csv("C://Users//jillc//OneDrive//Desktop//Portfolio//german-credit-analysis//data//GermanCredit.csv")
head(germancredit.df)

# run at least 5 data exploration functions
data.frame(sapply(germancredit.df, mean), +
             sapply(germancredit.df, sd), +
             sapply(germancredit.df, min), +
             sapply(germancredit.df, max), +
             sapply(germancredit.df, median))
Sys.time()

```
```{r - step 3}
# Partition into training and validation sets.
set.seed(1)
train.rows <- sample(rownames(germancredit.df), dim(germancredit.df)[1]*0.6)
valid.rows <- sample(setdiff(rownames(germancredit.df), train.rows), dim(germancredit.df)[1]*0.4)
train.data <- germancredit.df[train.rows, ]
valid.data <- germancredit.df[valid.rows, ]
Sys.time()

```
```{r - Logistic Regression}
# run logistic regression
# use glm() (general linear model) with family = "binomial" to fit a logistic regression.
logit.reg <- glm(OWN_RES ~ ., data = train.data, family = "binomial") 
options(scipen=999)
summary(logit.reg)
#Show how the prediction and the prediction error
data.frame("Predicted" = pred[1], "Actual" = valid.data$OWN_RES[1],
    "Residual" = resid)
library(ggplot2)
library(lattice)
library(caret)
library(gains)
pred <- predict(logit.reg, germancredit.df)
gain <- gains(germancredit.df$OWN_RES, pred, groups=100)

plot(c(0,gain$cume.pct.of.total*sum(germancredit.df$OWN_RES))~
     c(0,gain$cume.obs), 
     xlab="OWN_RES", ylab="Cumulative", main="", type="l")
lines(c(0,sum(germancredit.df$OWN_RES))~c(0, dim(germancredit.df)[1]), lty=2)

conf_matrix_table <- table(Actual = germancredit.df$OWN_RES, Predicted = pred)
print(conf_matrix_table)
Sys.time()
```
```{r - Classification}

# classification tree
library(rpart)
library(rpart.plot)
default.ct <- rpart(OWN_RES ~ ., data = train.data, method = "class")
# plot tree
prp(default.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)
Sys.time()
```