library(rpart)
library(rpart.plot)

iris
dim(iris)
s_iris <- sample(150, 100)
s_iris

iris_train <- iris[s_iris,]
iris_test <- iris[-s_iris,]

dim(iris_test)
dim(iris_train)

decisionTreeModel <- rpart(Species ~., iris_train, method = 'class')
decisionTreeModel

rpart.plot(decisionTreeModel)

# abalone
abalone <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"), header = FALSE,
                    sep = ",")
# Column names
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight',
                       'rings' )
# summary on abalone
summary(abalone)
# structure of the abalone data
str(abalone)
# summary of the abalone rings column
summary(abalone$rings)
# As shown above, the "rings" variable has a range between 1-29.
# This is the variable that we want to predict, and predicting this many levels
# might not give us the insight we're looking for.
# For now, we'll break the rings variable
# into 3 levels" "young" for abalones less than 8, "adult" for abalones between 8-11,
# and "old" for abalones older than 11.
abalone$rings <- as.numeric(abalone$rings)
abalone$rings <- cut(abalone$rings, br=c(-1,8,11,35), labels = c("young", 'adult', 'old'))
abalone$rings <- as.factor(abalone$rings)
summary(abalone$rings)
# remove the "sex" varialbe in abalone, because KNN requires all numeric variables for prediction
# z <- abalone
aba <- abalone
aba$sex <- NULL

normalize <- function(x) {
  return (( x - min(x))/ (max(x) - min(x)))
}
aba[1:7] <- as.data.frame(lapply(aba[1:7], normalize))
summary(aba$shucked_wieght)
# After Normalization, each variable has a min of 0 and a max of 1.
# in otherwords, values are in the range from 0 to 1.
# We'll now split the data into training and testing sets.
ind <- sample(2, nrow(aba), replace = T, prob = c(.7,.3))
KNNtrain <- aba[ind == 1,]
KNNtest <- aba[ind == 2,]
sqrt(2918)
library(class)
KNNpred <- knn(train = KNNtrain[1:7], test = KNNtest[1:7], cl = KNNtrain$rings, k = 55)
KNNpred
table(KNNpred)

library(ggplot2)
head(iris)
str(iris)
summary(iris)
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) + geom_point()
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, col = Species)) + geom_point()
set.seed(300)
k.max <- 12
wss <- sapply(1:k.max, function(k){kmeans(iris[,3:4],k,nstart = 20, iter.max = 20)$tot.withinss})
plot(1:k.max, wss, type = b, xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")
icluster <- kmeans(iris[,3:4],3, nstart = 20 )
table(icluster$cluster, iris$Species)

# inclass exercise 
library(rpart)
library(rpart.plot)
iris
dim(iris)

s_iris <- sample(150,100)
s_iris

iris_train <- iris[s_iris,]
iris_test <- irist[-s_iris]
dim(iris_test)
dim(iris_train)

dectionTreeModel <- rpart(Species ~., iris_train, method = "class")
dectionTreeModel

rpart.plot(dectionTreeModel)
