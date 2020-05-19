library(ggplot2)
library(zoo)
library(forecast)
library(tidyverse)
library(data.table)
library(skimr)

test = read.csv("C:/Users/dubey/OneDrive/Desktop/IIT COURSES/DATA-ANYLITCS/PROJECT/rossmann-store-sales/original data/test.csv", header = T)
train = read.csv("C:/Users/dubey/OneDrive/Desktop/IIT COURSES/DATA-ANYLITCS/PROJECT/rossmann-store-sales/original data/train.csv", header = T)
store = read.csv("C:/Users/dubey/OneDrive/Desktop/IIT COURSES/DATA-ANYLITCS/PROJECT/rossmann-store-sales/original data/store.csv", header = T)

#No. of missing values in each dataframe
sum(is.na(test));sum(is.na(train));sum(is.na(store))

#To view top 5 rows in each dataframe
head(train, 5);head(store, 5);head(test, 5)

#To know more about columns in each dataframe
str(train);str(test);str(store)

#Summary Statistics
summary(train);summary(test);summary(store)
skim(train);skim(test);skim(store)

#No. of rows and cols in each dataframe
dim(train);dim(test);dim(store)

#vertical - columns, horizontal - data
glimpse(train);glimpse(test);glimpse(store)

#date function  
data_function <- function(f){
  dataframe <- fread(f)   
  dataframe[, Date:= as.Date(Date)]
  return(dataframe)
}

train = data_function("C:/Users/dubey/OneDrive/Desktop/IIT COURSES/DATA-ANYLITCS/PROJECT/rossmann-store-sales/original data/train.csv")
test = data_function("C:/Users/dubey/OneDrive/Desktop/IIT COURSES/DATA-ANYLITCS/PROJECT/rossmann-store-sales/original data/test.csv")

#check number of unique values in each column
train %>% summarise_all(n_distinct)
test %>% summarise_all(n_distinct)



# count of test stores are also in the train data
c <- unique(c(test$Store, train$Store))
sum(unique(test$Store) %in% unique(train$Store)) 

# count of train stores that are not in the test data
sum(!(unique(train$Store) %in% unique(test$Store))) 

#unique values of Open column
table(train$Open)

#percentage of stores closed and opened in train csv
sum(train$Open %in% 0 ) / nrow(train);sum(train$Open %in% 1 ) / nrow(train)

#percentage of stores closed and opened in test csv
sum(test$Open %in% 0 ) / nrow(test);sum(test$Open %in% 1 ) / nrow(test)

#unique values of Promo column
table(train$Promo)

#percentage of stores displayed promos in train csv
sum(train$Promo %in% 1 ) / nrow(train);sum(train$Promo %in% 0 ) / nrow(train)

#percentage of stores displayed promos in test csv
sum(test$Promo %in% 1 ) / nrow(test);sum(test$Promo %in% 0 ) / nrow(test)

#unique values of StateHoliday column in train csv
table(train$StateHoliday)

#percentage of no holidays in train csv
sum(train$StateHoliday %in% 0 ) / nrow(train)

#unique values of StateHoliday column in test csv
table(test$StateHoliday)

#percentage of no holidays in test csv
sum(test$StateHoliday %in% 0 ) / nrow(test); sum(test$StateHoliday %in% 1 ) / nrow(test)

#percentage of no SchoolHoliday in train csv
sum(train$SchoolHoliday %in% 0 ) / nrow(train); sum(train$SchoolHoliday %in% 1 ) / nrow(train)

#percentage of no SchoolHoliday in train_csv
sum(test$SchoolHoliday %in% 0 ) / nrow(test); sum(test$SchoolHoliday %in% 1 ) / nrow(test)

#Visualization of Date Column
plot(train$Date, type = "p")
plot(test$Date, type = "p")

#In test data we need to predict all 856 stores daily
all(table(test$Date) == 856) 

#fill all missing values 
sum(is.na(train))
train[is.na(train)] <- 1
sum(is.na(train))

#histogram for number of sales
hist(train$Sales,main="Histogram for sales count in stores",xlab="stores" ,border="black",col="green", breaks=10, ylab = "count of sales")

#histogram for Average sales per store when opened
hist(aggregate(train[Sales != 0]$Sales, 
               by = list(train[Sales != 0]$Store), mean)$x, xlab="per store" ,border="black",col="green", breaks=100, ylab = "Avg sales",
     main = "Average sales per store when opened")

#histogram for count of customers
hist(train$Customers,main="Histogram for customers count in stores",xlab="customers" ,border="black",col="green", breaks=100, ylab = "count of customers")

#histogram for Avg customers per store when opened
hist(aggregate(train[Sales != 0]$Customers, 
               by = list(train[Sales != 0]$Store), mean)$x, breaks = 100, xlab="customers" ,ylab = "Average", border="black",col="green", 
     main = "Avg customers per store when opened")

#How Schoolholiday is affecting sales
ggplot(train[Sales != 0], aes(x = factor(SchoolHoliday), y = Sales)) +
    geom_jitter(alpha = 0.1) +
    geom_boxplot(color = "red", outlier.colour = NA, fill = NA, width = 0.4)

#How number of customers are affecting sales
ggplot(train[train$Sales != 0 & train$Customers != 0],
       aes(x = log(Customers), y = log(Sales))) + 
    geom_point(alpha = 0.2) + geom_smooth()

#How promo is affecting customers there by sales
ggplot(train[train$Sales != 0 & train$Customers != 0],
       aes(x = factor(Promo), y = Sales)) + 
    geom_jitter(alpha = 0.1) +
    geom_boxplot(color = "red", outlier.colour = NA, fill = NA)

#How Promo is influencing customers
ggplot(train[train$Sales != 0 & train$Customers != 0],
       aes(x = factor(Promo), y = Customers)) + 
    geom_jitter(alpha = 0.1) +
    geom_boxplot(color = "red", outlier.colour = NA, fill = NA)

#How promo is affecting sales
with(train[train$Sales != 0 & train$Promo == 0], mean(Sales / Customers)) 
with(train[train$Sales != 0 & train$Promo == 1], mean(Sales / Customers))

#How are Sales w.r.t. Promo
table(ifelse(train$Promo, "Promo", "No promo"),
      ifelse(train$Sales != 0, "Sales > 0", "Sales = 0")
      )

#How are Sales w.r.t. Opening of a store
table(ifelse(train$Open == 1, "Opened", "Closed"),
      ifelse(train$Sales > 0, "Sales > 0", "Sales = 0"))

#which stores have 0 sales when store is opened
train[Open == 1 & Sales == 0]

#Count of zero sales per store
df1 <- sort(tapply(train$Sales, list(train$Store), function(y) sum(y == 0)))
hist(df1,100)

#dimension of dataframe
dim(df1)

head(df1, 5); tail(df1, 5)

#How sales in stores are changing day by day which has zero sales atleast once
plot(train[Store == 349, Sales], ylab = "Sales Count", xlab = "Days", main = "Store 349")
plot(train[Store == 674, Sales], ylab = "Sales Count", xlab = "Days", main = "Store 674")
plot(train[Store == 103, Sales], ylab = "Sales Count", xlab = "Days", main = "Store 103")

ggplot(train[Store == 335], 
       aes(x = Date, y = Sales, 
           color = factor(DayOfWeek == 7), shape = factor(DayOfWeek == 7))) + 
    geom_point(size = 3) + ggtitle("Sales of store 335 (True if sunday)")

ggplot(train[Store == 423], 
       aes(x = Date, y = Sales, 
           color = factor(DayOfWeek == 7), shape = factor(DayOfWeek == 7))) + 
    geom_point(size = 3) + ggtitle("Sales of store 423 (True if sunday)")

ggplot(train[Sales != 0],
       aes(x = factor(DayOfWeek), y = Sales)) + 
    geom_jitter(alpha = 0.1) + 
    geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA)

summary(store)
table(store$StoreType)
table(store$Assortment)

table(data.frame(Assortment = store$Assortment, StoreType = store$StoreType))

hist(store$CompetitionDistance, 100)

store$CompetitionOpenSince <- as.yearmon(paste(store$CompetitionOpenSinceYear, 
                                               store$CompetitionOpenSinceMonth, sep = "-"))

hist(as.yearmon("2015-10") - store$CompetitionOpenSince, 100, 
     main = "Years since opening of nearest competition")

store$Promo2Since <- as.POSIXct(paste(store$Promo2SinceYear, 
                                   store$Promo2SinceWeek, 1, sep = "-"),
                             format = "%Y-%U-%u")

hist(as.numeric(as.POSIXct("2015-10-01", format = "%Y-%m-%d") - store$Promo2Since), 
     100, main = "Days since start of promo2")

table(store$PromoInterval)

train_store <- merge(train, store, by = "Store")

ggplot(train_store[Sales != 0], aes(x = factor(PromoInterval), y = Sales)) + 
    geom_jitter(alpha = 0.1) + 
    geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA)

ggplot(train_store[Sales != 0], 
       aes(x = as.Date(Date), y = Sales, color = factor(StoreType))) + 
    geom_smooth(size = 2)

ggplot(train_store[Customers != 0], 
       aes(x = as.Date(Date), y = Customers, color = factor(StoreType))) + 
    geom_smooth(size = 2)

ggplot(train_store[Sales != 0], 
       aes(x = as.Date(Date), y = Sales, color = factor(Assortment))) + 
    geom_smooth(size = 2)

ggplot(train_store[Sales != 0], 
       aes(x = as.Date(Date), y = Customers, color = factor(Assortment))) + 
    geom_smooth(size = 2)

salesByDist <- aggregate(train_store[Sales != 0 & !is.na(CompetitionDistance)]$Sales, 
               by = list(train_store[Sales != 0 & !is.na(CompetitionDistance)]$CompetitionDistance), mean)

colnames(salesByDist) <- c("CompetitionDistance", "MeanSales")

ggplot(salesByDist, aes(x = log(CompetitionDistance), y = log(MeanSales))) + 
    geom_point() + geom_smooth()

ggplot(train_store[Sales != 0],
       aes(x = factor(!is.na(CompetitionOpenSinceYear)), y = Sales)) +
    geom_jitter(alpha = 0.1) +
    geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA) +
    ggtitle("Any competition?")

train_store$DateYearmon <- as.yearmon(train_store$Date)

train_store <- train_store[order(Date)]

timespan <- 100

beforeAndAfterComp <- function(s) {
    x <- train_store[Store == s]
    daysWithComp <- x$CompetitionOpenSince >= x$DateYearmon
    if (any(!daysWithComp)) {
        compOpening <- head(which(!daysWithComp), 1) - 1
        if (compOpening > timespan & compOpening < (nrow(x) - timespan)) {
           x <- x[(compOpening - timespan):(compOpening + timespan), ] 
            x$Day <- 1:nrow(x)
            return(x)
        }
    }
}

temp <- lapply(unique(train_store[!is.na(CompetitionOpenSince)]$Store), beforeAndAfterComp)

temp <- do.call(rbind, temp)

length(unique(temp$Store))

ggplot(temp[Sales != 0], aes(x = Day, y = Sales)) + 
    geom_smooth() + 
    ggtitle(paste("Competition opening around day", timespan))

temp <- train
temp$year <- format(temp$Date, "%Y")
temp$month <- format(temp$Date, "%m")
temp[, StoreMean := mean(Sales), by = Store]
temp <- temp[, .(MonthlySalesMean = mean(Sales / (StoreMean)) * 100), 
             by = .(year, month)]
temp <- as.data.frame(temp)

SalesTS <- ts(temp$MonthlySalesMean, start=2013, frequency=12)
col = rainbow(3)
seasonplot(SalesTS, col=col, year.labels.left = TRUE, pch=19, las=1)

