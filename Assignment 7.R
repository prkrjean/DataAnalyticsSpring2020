### Parker Jean
### Data Analytics 
### Assignment 7

### Bank Marketing Data Set

#1 - age (numeric) 

#2 - job : type of job (categorical: "admin.","unknown","unemployed","management","housemaid","entrepreneur","student",
                      # "blue-collar","self-employed","retired","technician","services") 

#3 - marital : marital status (categorical: 
              #"married","divorced","single"; note: "divorced" means divorced or widowed)

#4 - education (categorical: "unknown","secondary","primary","tertiary")

#5 - default: has credit in default? (binary: "yes","no")

#6 - balance: average yearly balance, in euros (numeric) 

#7 - housing: has housing loan? (binary: "yes","no")

#8 - loan: has personal loan? (binary: "yes","no")
    # related with the last contact of the current campaign:

#9 - contact: contact communication type (categorical: "unknown","telephone","cellular") 

#10 - day: last contact day of the month (numeric)

#11 - month: last contact month of year (categorical: "jan", "feb", "mar", ..., "nov", "dec")

#12 - duration: last contact duration, in seconds (numeric)

# other attributes:
#13 - campaign: number of contacts performed during this campaign 
        #and for this client (numeric, includes last contact)

#14 - pdays: number of days that passed by after the client was last 
        #contacted from a previous campaign (numeric, -1 means client 
        #was not previously contacted)

#15 - previous: number of contacts performed before this campaign and 
        #for this client (numeric)

#16 - poutcome: outcome of the previous marketing campaign 
        #(categorical: "unknown","other","failure","success")



# import data
df <- read.csv('C:/Users/prkrj/Downloads/bank-full.csv', header = TRUE,sep = ';')
View(df)

boxplot(df, las = 2, main = "Bank Boxplot")
boxplot(df$balance, xlab = 'Balance') 
## averge yearly balance in euros --> standardize
dim(df)
summary(df)

hist(df$age)
hist(df$balance)

## remove balance outliers 
bal.sum <- summary(df$balance)
IQR <- bal.sum[5] - bal.sum[2]
lower <- bal.sum[2] - (1.5*IQR)
upper <- bal.sum[5] - (1.5*IQR)

boxplot(df$balance)$out 
# 3729 outliers 
outliers <- boxplot(df$balance, plot = FALSE)$out
df <- df[-which(df$balance %in% outliers),]
boxplot(df$balance)
dim(df)
# 40482 rows

plot(df$marital)
plot(df$job, las = 2)
plot(df$education)

plot(df$pday)
boxplot(df$pdays)

table(df$contact)

plot(df$balance, df$duration)

na.omit(df)
dim(df)
# no na in set row 40482

sapply(df, class)
## correlation matrix 
num <- unlist(lapply(df, is.numeric))
num.cor <- df[,num]
round(cor(num.cor),2)

col <- colorRampPalette(c('green','blue','red'))(20)
heatmap(x = cor(num.cor), col = col, symm = TRUE, main = 'Bank Correlation Matrix')
# low correlation no need to omit column 

## train and test split 70/30

View(df)
df$y <- 1*(df$y == "yes")


train.size <- floor(.70 *nrow(df))

set.seed(00)
train.ind <- sample(seq_len(nrow(df)), size = train.size)

train <- df[train.ind,]
dim(train)
test <- df[-train.ind,]
dim(test)


## Logistic regression 

lreg <- glm(train$y ~., data = train, family = 'binomial')
summary(lreg)

pred <- predict(lreg, test, type = 'response')

library('caret')
library('e1071')
cfm1 <- confusionMatrix(as.factor(ifelse(pred > .39, 1,0)), as.factor(test$y))
## Accuracy .9056

# drop month
lreg.2 <- glm(train$y ~ age + job + marital + education + default + contact + pdays + poutcome, data = train, family = 'binomial' )
pred.2 <- predict(lreg.2, test, type = 'response')

cfm2 <- confusionMatrix(as.factor(ifelse(pred > .39,1,0)), as.factor(test$y))
cfm2
## same outcome
# adjusted cut off for 0,1 splt to .39 for best result of .9107 or 91.07%

## Classification Tree
library('rpart')
library('rpart.plot')


# base
base.tree <- rpart(y~., data = train, method = 'class')
prp(base.tree, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, main = 'Base Tree')
summary(base.tree)

library('caret')
library('e1071')
base.tree.pred.train <- predict(base.tree, train, type = 'class')
confusionMatrix(base.tree.pred.train, as.factor(train$y))
# accuracy .9036
# validate base tree
base.tree.pred.test <- predict(base.tree, test, type = 'class')
confusionMatrix(base.tree.pred.test, as.factor(test$y))
# acc .9068

# deeper
tree.deeper <- rpart(y~., data = train, method = 'class', cp = 0, minsplit = 1 ) 
prp(tree.deeper, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)

tree.deeper.pred.train <- predict(tree.deeper, train, type = 'class')
confusionMatrix(tree.deeper.pred.train, as.factor(train$y))
# acc 1.0

# validate deeper tree
tree.deeper.pred.test <- predict(tree.deeper, test, type = 'class')
confusionMatrix(tree.deeper.pred.test, as.factor(test$y))
# acc .879

# prune tree
tree.pre.prune <- rpart(y~., data = train, method = 'class', cp = .0001, minsplit = 5, xval = 5)
printcp(tree.pre.prune)
pruned <- prune(tree.pre.prune, cp = tree.pre.prune$cptable[which.min(tree.pre.prune$cptable[,'xerror']),'CP'])
prp(pruned, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(pruned$frame$var == "<leaf>", 'gray', 'white'))

# prune based on cp = .01397516
pruned.2 <- prune(tree.pre.prune, cp = 0.01397516)
prp(pruned.2, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, main = 'Pruned Tree' )

pred.pruned.2 <- predict(pruned.2, test, type = 'class')
confusionMatrix(pred.pruned.2, as.factor(test$y))
# acc .9054

## Random Forest
library(randomForest)
 
# default rf
rf.1 <- randomForest(y ~., data = train, importance = TRUE)
rf.1  
  
rf.2 <- randomForest(y ~., data = train, ntree = 500, mtry = 6, importance = TRUE)
rf.2  

rf.1.pred <- pred(rf.1)  
  
## KNN
df$default <- 1*(df$default == "yes")
df$housing <- 1*(df$housing == "yes")
df$loan <- 1*(df$loan == "yes")

sapply(train, class)
train.y <- train[,17]
train.knn <- train[,-c(2,3,4,9,11,16,17)]

test.y <- test[,17]
test.knn <- test[,-c(2,3,4,9,11,16,17)]

library(class)
set.seed(00)
knn1 <- knn(train.knn, test.knn, cl = train.y, k = 5, prob = TRUE)
head(knn1)

missClassError <- mean(test.y != knn1)
print(missClassError)

# choosing k value 
knn1 <- NULL
error_rate <- NULL

for (i in 1:20) {
  set.seed(00)
  knn1 <- knn(train.knn, test.knn, cl = train.y, k = i)
  error_rate[i] <- mean(test.y != knn1) 
  }

print(error_rate)

# plot k val in a graph
library(ggplot2)
k.val <- 1:20

error_df <- data.frame(error_rate, k_values)

ggplot(error_df, aes(k_values, error_rate)) + geom_point() + geom_line(lty = 'dotted', color = 'blue') + ggtitle("k-plot Optimizaiton")

knn20 <- knn(train.knn, test.knn, cl = train.y, k = 20)
mean(test.y != knn20)
# err rate .1039111
1-.1039111

#### Begin data set 2 ####

bank <- read.csv('C:/Users/prkrj/Downloads/Absenteeism_at_work.csv', header = TRUE,sep = ';')
View(bank)

dim(bank)
sapply(bank, class)

sum(is.na(bank))
# no na

boxplot(bank[,1:10], las = 2, main = 'col 1-10')
boxplot(bank[,11:21], las = 2, main = 'col 11-21')


round(cor(bank),2)

col <- colorRampPalette(c('green','blue','red'))(20)
heatmap(x = cor(bank), col = col, symm = TRUE, main = 'Absenteeism Correlation Matrix')

library('caret')

df.2 <- cor(bank)
hc = findCorrelation(df.2, cutoff = .7)
hc <- sort(hc)
reduced.data <- bank[,-c(hc)]
dim(bank)
### no correlation over 70%

# look at transporation expense, average day, height
boxplot(bank$Height)$out
boxplot(bank$Work.load.Average.day)$out
boxplot(bank$Transportation.expense)$out

# varible importance 

reg.def <- lm(bank$Absenteeism.time.in.hours ~., data = bank)
summary(reg.def)

# low significance - Reason.for.absence(2), Disciplinary.failure (12) signif = 0

lite.bank <- bank[,-c(2,12)]
dim(lite.bank)


## Train test split
train.size <- floor(.70 *nrow(lite.bank))

set.seed(00)
train.ind <- sample(seq_len(nrow(lite.bank)), size = train.size)

train <- lite.bank[train.ind,]
dim(train)
test <- lite.bank[-train.ind,]
dim(test)

## modeling 
# linear regression

reg.1 <- lm(train$Absenteeism.time.in.hours~., data = train)
summary(reg.1)

plot(reg.1)

pred.hrs <- predict(reg.1, test)

actuals_pred <- data.frame(cbind(actuals = test$Absenteeism.time.in.hours, predicteds = pred.hrs))
cor_acc <- cor(actuals_pred)
head(actuals_pred)
actuals_pred[1]

min.max.accuracy <- mean(apply(actuals_pred,1,min)/ apply(actuals_pred,1, max))

min.max.accuracy
# .477


# create class
b <- c(-Inf, 10,50, Inf)
names <- c('Low', 'Medium', 'High')
bank$Absent.class <- cut(bank$Absenteeism.time.in.hours, breaks = b, labels = names)

train <- bank[train.ind,]
dim(train)
train <- train[,-21]
test <- bank[-train.ind,]
test <- test[,-21]
dim(test)


# tree model

bank.tree <- rpart(train$Absent.class~., data = train, method = 'class', cp = 0, minsplit = 1 )
prp(bank.tree, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)

# validate 
bank.tree.pred <- predict(bank.tree, test, type = 'class')
confusionMatrix(bank.tree.pred, as.factor(test$Absent.class))
# acc .8784

printcp(bank.tree)

# tree 2
bank.tree.2<- rpart(train$Absent.class~., data = train, method = 'class', cp = .019, minsplit = 1 )
prp(bank.tree.2, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, main = 'Pruned Tree Absenteeism')

# validate
bank.tree.pred.2 <- predict(bank.tree.prune, test, type = 'class')
confusionMatrix(bank.tree.pred.2, as.factor(test$Absent.class))
# .8874


## random forest 

library(randomForest)
set.seed(00)
bank.rf.1 <- randomForest(Absent.class ~., data = train, importance = T)
bank.rf.1

bank.rf.2 <- randomForest(Absent.class ~., data = train, importance = T, ntree = 250 )
bank.rf.2
# error 8.69%
1- .0888
# acc .9131