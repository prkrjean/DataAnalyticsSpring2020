#### Parker Jean
#### Data Analytics 
#### Assignment B
#### Due: 5/4/2020


## Import Data
data <- read.csv("C:/Users/prkrj/OneDrive/Data Analytics/Monthly_Table_Data.csv")
View(data)
head(data)
dim(data)
sapply(data, class)
class_names <- names(data)
names(data)[1] <- "Year"

### EDA
## uniqe by column

library(ggplot2)
# year
# no na
unique(data$Year)
table(data$Year)
hist(data$Year, main = "Historgram of Year")
sum(is.na(data$Year))

# port
# no na
unique(data$Port)
port_counts <- table(data$Port)
sort(port_counts)
sum(is.na(data$Port))

# number of records
# all 1 -- DROP
unique(data$Number.of.Records)
data <- subset(data, select = -c(Number.of.Records))

# rows with 0 value got removed
is.na(data) <- !data
sum(is.na(data))
data <- na.omit(data)
dim(data)

# Border 
unique(data$Border)
table(data$Border)
barplot(table(data$Border), main = "Border Counts")


# break date down
library(lubridate)

data$Date <- as.Date(data$Date, format = '%m/%d/%Y')
class(data$Date)

# port
unique(data$State)
plot(x = data$State, y = data$Value, las = 2, main = "Volume by State")

# stacked bar state and port
barplot(table(data$State), las = 2)  

state_port <- ggplot(data, aes(State))
state_port+
  geom_bar(aes(fill = Measure))+ 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Measure by State")


# create low medium high for value 
ts.data <-  data
sapply(data,class)
View(data)
b <- c(-Inf, 113,9000, Inf)
names <- c('Low', 'Medium', 'High')
data$Value <- as.numeric(data$Value)
data$Value <- cut(data$Value, breaks = b, labels = names)

sapply(data,class)
data.lite <- data[,-c(1,2,3,4,7)]
ts.lite <- ts.data[,-c(1,2,3,4,7)]
table(data.lite$Value)
sapply(data.lite,class)
# group measures
library(tidyverse)
data.lite$Measure <- fct_collapse(data.lite$Measure,
             people = c('Bus Passengers', 'Pedestrians', 'Personal Vehicle Passengers', 'Train Passengers'),
             vehicle = c('Buses', 'Personal Vehicles', 'Trains', 'Trucks'),
             container.empty = c('Rail Containers Empty', 'Truck Containers Empty'),
             container.full = c('Rail Containers Full', 'Truck Containers Full'))
table(data.lite$Measure)

ts.lite$Measure <- fct_collapse(ts.lite$Measure,
                                people = c('Bus Passengers', 'Pedestrians', 'Personal Vehicle Passengers', 'Train Passengers'),
                                vehicle = c('Buses', 'Personal Vehicles', 'Trains', 'Trucks'),
                                container.empty = c('Rail Containers Empty', 'Truck Containers Empty'),
                                container.full = c('Rail Containers Full', 'Truck Containers Full'))


# create dummies 
library(dummies)
names(data.lite)
y <- data.lite[,5]
Date <- data.lite[,1]
table(y)
df.dummy <- data.lite[,-c(1,5)]
names(df.dummy)


df.dummy <- dummy.data.frame(df.dummy, sep = ".")
names(df.dummy)


model.df <- cbind(Date, df.dummy, y)

## modeling 
sapply(model.df, class)

# train test split - with dummy set
dim(model.df)
train.size <- floor(.70 *nrow(model.df))

set.seed(00)
train.ind <- sample(seq_len(nrow(model.df)), size = train.size)

train <- model.df[train.ind,]
dim(train)
View(train)
test <- model.df[-train.ind,]
dim(test)

# train test split no dummy
dim(data.lite)
set.seed(00)
train.size.no <- floor(.70 *nrow(data.lite))

train.ind.no <- sample(seq_len(nrow(data.lite)), size = train.size.no)

train.nd <- data.lite[train.ind.no,]
dim(train.nd)
test.nd <- data.lite[-train.ind.no,]
dim(test.nd)


### Modeling

## Classificaiton tree
library('rpart')
library('rpart.plot')


base.tree <- rpart(y~., data = train, method = 'class')
prp(base.tree, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, main = 'Base Tree')
summary(base.tree)

library('caret')
library('e1071')
base.tree.pred.train <- predict(base.tree, train, type = 'class')
confusionMatrix(base.tree.pred.train, as.factor(train$y))
# .5376

# validate base tree
base.tree.pred.test <- predict(base.tree, test, type = 'class')
confusionMatrix(base.tree.pred.test, as.factor(test$y))
# .5394

#deeper
tree.deeper <- rpart(y~., data = train, method = 'class', cp = 0, minsplit = 1 ) 
prp(tree.deeper, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, main = 'deeper tree')

tree.deeper.pred.train <- predict(tree.deeper, train, type = 'class')
confusionMatrix(tree.deeper.pred.train, as.factor(train$y))
#.7248

#prune tree
tree.pre.prune <- rpart(y~., data = train, method = 'class', cp = .0001, minsplit = 5, xval = 5)
printcp(tree.pre.prune)
pruned <- prune(tree.pre.prune, cp = tree.pre.prune$cptable[which.min(tree.pre.prune$cptable[,'xerror']),'CP'])
prp(pruned, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(pruned$frame$var == "<leaf>", 'gray', 'white'), main = "Pruned Tree")
# cp @ 0.00114583
pruned.2 <- prune(tree.pre.prune, cp = 0.00111)
prp(pruned.2, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, main = 'Pruned Tree' )

pred.pruned.2 <- predict(pruned.2, test, type = 'class')
confusionMatrix(pred.pruned.2, as.factor(test$y))
# acc .6223

# cp @ 0.00204524
pruned.3 <- prune(tree.pre.prune, cp = 0.0001)
prp(pruned.3, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, main = 'Pruned Tree 3' )

pred.pruned.3 <- predict(pruned.3, test, type = 'class')
confusionMatrix(pred.pruned.3, as.factor(test$y))
# .6521



## Time series
library(ggplot2)
library(dplyr)
library(lubridate)
sapply(ts.lite, class)

# Whole data 
ts.lite %>% 
  ggplot(aes(x = Date, y = Value)) +
  geom_point(color = "darkorchid4") +
  labs(title = 'Border Crossing count')

# by year
ts.lite$Julian <- format(as.Date(ts.lite$Date, format = '%m/%d/%Y' ), "%j")
ts.lite$Month <-  format(as.Date(ts.lite$Date, format = '%m/%d/%Y' ), "%m")
ts.lite$Year <-  format(as.Date(ts.lite$Date, format = '%m/%d/%Y' ), "%y")
View(ts.lite)

# broken by month
ts.lite %>%
  ggplot(aes(x = Date, y = Value)) +
  geom_point(color = 'darkorchid4') +
  facet_wrap(~ Month) + 
  labs(title = 'Monthly Border Crossing Count')

# Monthly Border Crossing Count all years
ts.lite %>%
  ggplot(aes(x = Month  , y = Value)) +
  geom_point(color = 'darkorchid4') +
  labs(title = 'Monthly Border Crossing Count all years')


ts.lite %>%
  ggplot(aes(x = Month  , y = Value)) +
  geom_point(color = 'darkorchid4') +
  labs(title = 'Monthly Border Crossing Count by Year') +
  facet_wrap(~ Year)

ts.lite %>% 
  ggplot(aes(x = Date, y = Value)) +
  geom_point(aes(fill = Measure, color = Measure)) +
  labs(title = 'Border Crossing count')

# break by measure 
people <- ts.lite[ts.lite$Measure == 'people',]
vehicle <- ts.lite[ts.lite$Measure == 'vehicle',]
container <- ts.lite[ts.lite$Measure == "container.empty" | ts.lite$Measure == "container.full",]

## people
people %>% 
  ggplot(aes(x = Date, y = Value)) +
  geom_point(color = 'red') +
  labs(title = 'People Border Crossing count')

people %>% 
  ggplot(aes(x = Month, y = Value)) +
  geom_point(color = 'red') +
  labs(title = 'People Border Crossing Monthly')

people %>% 
  ggplot(aes(x = Month, y = Value)) +
  geom_point(color = 'red') +
  labs(title = 'State by month - People') + 
  facet_wrap(~ State)

library(forecast)
people.ts[people.ts == 0] <- .9
table(people.ts)
people.ts <- ts(people$Value, frequency = 12, start = c(1996,1))  

plot(people.ts, main = "Time Series - People")

length(people.ts)*.7 # 54014
nValid <- 54014

ntrain <- length(people.ts) - nValid

Ptrain <- window(people.ts, start = c(1996,1), end = c(1996, ntrain))
ptest <- window(people.ts, start = c(1996, ntrain +1), end = c(1996, ntrain+nValid) )

ptrain.trend.sea <- tslm(Ptrain ~ trend + I(trend^2) + season)
summary(ptrain.trend.sea)
 
Acf(ptrain.trend.sea$residuals, lag.max = 20)

ptrain.res.arima <- Arima(ptrain.trend.sea$residuals, order = c(1,0,0))
ptrain.res.arima.pred <- forecast(ptrain.res.arima, h = nValid)

summary(ptrain.res.arima)

plot(ptrain.trend.sea$residuals, ylim = c(0, 4e+06))
lines(ptrain.res.arima.pred$fitted, lwd = 2, col = 'blue')

ptrain.trend.sea.pred <- forecast(ptrain.trend.sea, h = nValid, level = 0)
pred.p <- ptrain.trend.sea.pred$mean + ptrain.res.arima.pred$mean

accuracy(pred.p, ptest)
## horrible 


library(zoo)

p.centered <- ma(people.ts, order = 12)
p.trailing <- rollmean(people.ts, k = 12, align = 'right')

plot(people.ts, ylim = c(0,4e+06))
lines(p.centered, lwd = 2)
lines(p.trailing, lwd = 2, lty = 2)

## every thing it too tight to make accurate



## vehicle 
vehicle %>% 
  ggplot(aes(x = Date, y = Value)) +
  geom_point(color = "blue") +
  labs(title = 'Vehicle Border Crossing count')


vehicle %>% 
  ggplot(aes(x = Month, y = Value)) +
  geom_point(color = 'blue') +
  labs(title = 'Vehicle Border Crossing Monthly')

vehicle %>% 
  ggplot(aes(x = Month, y = Value)) +
  geom_point(color = 'blue') +
  labs(title = 'State by month - vehicle') + 
  facet_wrap(~ State)

library(forecast)
vehicle.ts <- ts(vehicle$Value, frequency = 12, start = c(1996,1))  

plot(vehicle.ts, main = "Time Series - Vehicle")

length(vehicle.ts)*.7 # 62245
nValid <- 62245

ntrain <- length(vehicle.ts) - nValid

vtrain <- window(vehicle.ts, start = c(1996,1), end = c(1996, ntrain))
vtest <- window(vehicle.ts, start = c(1996, ntrain +1), end = c(1996, ntrain+nValid) )

vtrain.trend.sea <- tslm(vtrain ~ trend + I(trend^2) + season)
summary(vtrain.trend.sea)

Acf(vtrain.trend.sea$residuals, lag.max = 20)

vtrain.res.arima <- Arima(vtrain.trend.sea$residuals, order = c(1,0,0))
vtrain.res.arima.pred <- forecast(vtrain.res.arima, h = nValid)

summary(vtrain.res.arima)

plot(vtrain.trend.sea$residuals, ylim = c(0, 1e+06))
lines(vtrain.res.arima.pred$fitted, lwd = 2, col = 'blue')

vtrain.trend.sea.pred <- forecast(vtrain.trend.sea, h = nValid, level = 0)
pred.v <- vtrain.trend.sea.pred$mean + vtrain.res.arima.pred$mean

accuracy(pred.v, vtest)
## horrible 

library(zoo)

v.centered <- ma(vehicle.ts, order = 12)
v.trailing <- rollmean(vehicle.ts, k = 12, align = 'right')

plot(vehicle.ts, ylim = c(0,2e+06))
lines(v.centered, lwd = 2, col = 'blue')
lines(v.trailing, lwd = 2, lty = 2)

## every thing it too tight to make accurate


## Containter
container %>% 
  ggplot(aes(x = Date, y = Value)) +
  geom_point(aes(fill = Measure, color = Measure)) +
  labs(title = 'Containter Crossing count')

container %>% 
  ggplot(aes(x = Month, y = Value)) +
  geom_point(aes(fill = Measure, color = Measure)) +
  labs(title = 'Containter Crossing Monthly')

container %>% 
  ggplot(aes(x = Month, y = Value)) +
  geom_point(aes(fill = Measure, color = Measure)) +
  labs(title = 'State by month - Container') + 
  facet_wrap(~ State)

library(forecast)
container.ts <- ts(container$Value, frequency = 12, start = c(1996,1))  

plot(container.ts, main = "Time Series - Container")

length(container.ts)*.7 # 45842
nValid <- 45842

ntrain <- length(container.ts) - nValid

ctrain <- window(container.ts, start = c(1996,1), end = c(1996, ntrain))
ctest <- window(container.ts, start = c(1996, ntrain +1), end = c(1996, ntrain+nValid) )

ctrain.trend.sea <- tslm(ctrain ~ trend + I(trend^2) + season)
summary(ctrain.trend.sea)

Acf(ctrain.trend.sea$residuals, lag.max = 20)

ctrain.res.arima <- Arima(ctrain.trend.sea$residuals, order = c(1,0,0))
ctrain.res.arima.pred <- forecast(ctrain.res.arima, h = nValid)

summary(ctrain.res.arima)

plot(ctrain.trend.sea$residuals, ylim = c(0, 150000))
lines(ctrain.res.arima.pred$fitted, lwd = 2, col = 'blue')

ctrain.trend.sea.pred <- forecast(ctrain.trend.sea, h = nValid, level = 0)
pred.c <- ctrain.trend.sea.pred$mean + ctrain.res.arima.pred$mean

accuracy(pred.c, ctest)
## horrible 

library(zoo)

c.centered <- ma(container.ts, order = 12)
c.trailing <- rollmean(container.ts, k = 12, align = 'right')

plot(container.ts, ylim = c(0,160000))
lines(c.centered, lwd = 2, col = 'blue')
lines(c.trailing, lwd = 2, lty = 2)

## every thing it too tight to make accurate
