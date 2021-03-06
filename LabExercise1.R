days <- c('Mon', 'Tue', 'wed', 'Thur', 'Fri', 'Sat', 'Sun')
temp <- c(28,30.5,32,31.2,29.3,27.9,26.4)
snowed <- c('T', 'T', 'F', 'F', 'T', 'T', 'F')
help("data.frame")
RPI_Weather_Week <- data.frame(days, temp, snowed)

RPI_Weather_Week
head(RPI_Weather_Week)

str(RPI_Weather_Week)

summary(RPI_Weather_Week)

RPI_Weather_Week[1,]
RPI_Weather_Week[,1]

RPI_Weather_Week[,'snowed'] #column selection
RPI_Weather_Week[,'days'] 
RPI_Weather_Week[,'temp']
RPI_Weather_Week[1:5, c("days", "temp")]
RPI_Weather_Week$temp
subset(RPI_Weather_Week, subset = snowed == "T")

sorted.snowed <- order(RPI_Weather_Week['snowed']) #returns index in order F first
sorted.snowed
RPI_Weather_Week[sorted.snowed,] 

dec.snow <- order(-RPI_Weather_Week$temp) #decending snowed
dec.snow

empty.Dataframe <- data.frame()
v1 <- 1:10
v1
letters
v2 <- letters[1:10]
df <- data.frame(col.name.1 = v1, col.name.2 = v2)
df

write.csv(df, file = 'saved_df1.csv')
df2 <- read.csv('saved_df1.csv')
df2

#exercise 1
install.packages("readxl")
library(readxl)
GPW3 <- read.csv(file.choose(), header = TRUE)
EPI_data <- read.csv(file.choose(), header = TRUE, skip = 1)

stem(GPW3$Continent)
hist(GPW3$LevelUsed)
# the EPI2010 data set only contains one column with many nulls

data()
help(data)

View(EPI_data)
attach(EPI_data)
fix(EPI_data) #launches data editor

EPI_data$EPI
tf <- is.na(EPI_data$EPI)
E <- EPI_data$EPI[!tf]

summary(EPI_data$EPI)

fivenum(EPI_data$EPI, na.rm = TRUE)
stem(EPI_data$EPI)
hist(EPI_data$EPI)
hist(EPI_data$EPI, seq(30.,95.,1.0), prob = TRUE)
lines(density(EPI_data$EPI, na.rm = TRUE, bw = "SJ")) 

rug(EPI_data$EPI)

###

#CDF 
plot(ecdf(EPI_data$EPI), do.points = FALSE, verticals = TRUE) 

#Quartile - Quartile
par(pty = "s")
qqnorm(EPI_data$EPI); qqline(EPI_data$EPI)
x <- seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

#daly
par(pty = "s")
qqnorm(EPI_data$DALY); qqline(EPI_data$DALY)
x <- seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

#Water_h
par(pty = "s")
qqnorm(EPI_data$WATER_H); qqline(EPI_data$WATER_H)
x <- seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

#compare dist
boxplot(EPI_data$EPI, EPI_data$DALY)
qqplot(EPI_data$EPI, EPI_data$DALY)

#EPI V AIR_H
boxplot(EPI_data$EPI, EPI_data$AIR_H)
qqplot(EPI_data$EPI, EPI_data$AIR_H)

#EPI V WATER_H
boxplot(EPI_data$EPI, EPI_data$WATER_H)
qqplot(EPI_data$EPI, EPI_data$WATER_H)

## Exercise 2
EPILand <- EPI[!Landlock]
Eland <- EPILand[!is.na(EPILand)]
hist(Eland)
hist(Eland, seq(30., 95., 1.0), prob = TRUE)

tf <- is.na(Eland)
E <- Eland[!tf]

summary(Eland)

fivenum(Eland, na.rm = TRUE)
stem(Eland)
hist(Eland)
hist(Eland, seq(30.,95.,1.0), prob = TRUE)
lines(density(Eland, na.rm = TRUE, bw = "SJ")) 

rug(Eland)

#CDF 
plot(ecdf(Eland), do.points = FALSE, verticals = TRUE) 

#Quartile - Quartile
par(pty = "s")
qqnorm(Eland); qqline(Eland)
x <- seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

# No Surface Water 
EPInosurfacewater <- EPI[!No_surface_water]
ENowater <- EPInosurfacewater[!is.na(EPInosurfacewater)]
hist(ENowater)
hist(ENowater, seq(30., 95., 1.0), prob = TRUE)

tf <- is.na(ENowater)
E <- ENowater[!tf]

summary(ENowater)

fivenum(ENowater, na.rm = TRUE)
stem(ENowater)
hist(ENowater)
hist(ENowater, seq(30.,95.,1.0), prob = TRUE)
lines(density(ENowater, na.rm = TRUE, bw = "SJ")) 

rug(ENowater)

#CDF 
plot(ecdf(ENowater), do.points = FALSE, verticals = TRUE) 

#Quartile - Quartile
par(pty = "s")
qqnorm(ENowater); qqline(ENowater)
x <- seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

#Desert
EPIDesert <- EPI[!Desert]
EDesert <- EPIDesert[!is.na(EPIDesert)]
hist(EDesert)
hist(EDesert, seq(30., 95., 1.0), prob = TRUE)

tf <- is.na(EDesert)
E <- EDesert[!tf]

summary(EDesert)

fivenum(EDesert, na.rm = TRUE)
stem(EDesert)
hist(EDesert)
hist(EDesert, seq(30.,95.,1.0), prob = TRUE)
lines(density(EDesert, na.rm = TRUE, bw = "SJ")) 

rug(EDesert)

#CDF 
plot(ecdf(EDesert), do.points = FALSE, verticals = TRUE) 

#Quartile - Quartile
par(pty = "s")
qqnorm(EDesert); qqline(EDesert)
x <- seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")

# High Population Density
EPIHighPopulationDensity <- EPI[!High_Population_Density]
Epopdensity <- EPIHighPopulationDensity[!is.na(EPIHighPopulationDensity)]
hist(Epopdensity)
hist(Epopdensity, seq(30., 95., 1.0), prob = TRUE)

tf <- is.na(Epopdensity)
E <- Epopdensity[!tf]

summary(Epopdensity)

fivenum(Epopdensity, na.rm = TRUE)
stem(Epopdensity)
hist(Epopdensity)
hist(Epopdensity, seq(30.,95.,1.0), prob = TRUE)
lines(density(Epopdensity, na.rm = TRUE, bw = "SJ")) 

rug(Epopdensity)

#CDF 
plot(ecdf(Epopdensity), do.points = FALSE, verticals = TRUE) 

#Quartile - Quartile
par(pty = "s")
qqnorm(Epopdensity); qqline(Epopdensity)
x <- seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
