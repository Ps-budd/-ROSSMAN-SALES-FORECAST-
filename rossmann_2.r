#*****************1. MultiLinear Regression Model********************

store_data <- read.csv("C:/Users/dubey/OneDrive/Desktop/IIT COURSES/DATA-ANYLITCS/PROJECT/rossmann-store-sales/original data/store.csv", header = T)
train_data <- read.csv("C:/Users/dubey/OneDrive/Desktop/IIT COURSES/DATA-ANYLITCS/PROJECT/rossmann-store-sales/original data/train.csv")


test_data <- read.csv("C:/Users/dubey/OneDrive/Desktop/IIT COURSES/DATA-ANYLITCS/PROJECT/rossmann-store-sales/original data/test.csv")

test_df <- merge(test_data,store_data)

train_df <- merge(train_data,store_data)

remodelling_func <- function(input_df){
input_df$CompetitionDistance[is.na(input_df$CompetitionDistance)] = round(mean(input_df$CompetitionDistance, na.rm = T))
input_df$CompetitionOpenSinceMonth[is.na(input_df$CompetitionOpenSinceMonth)] = round(mean(input_df$CompetitionOpenSinceMonth, na.rm = T))
input_df$CompetitionOpenSinceYear[is.na(input_df$CompetitionOpenSinceYear)] = round(mean(input_df$CompetitionOpenSinceYear, na.rm = T))
input_df$Promo2SinceWeek[is.na(input_df$Promo2SinceWeek)] = round(mean(input_df$Promo2SinceWeek, na.rm = T))
input_df$Promo2SinceYear[is.na(input_df$Promo2SinceYear)] = round(mean(input_df$Promo2SinceYear, na.rm = T))
input_df$Open[is.na(input_df$Open)] = round(mean(input_df$Open, na.rm = T))
input_df$StateHoliday = as.numeric(input_df$StateHoliday)
input_df$StoreType = as.numeric(input_df$StoreType)
input_df$Assortment = as.numeric(input_df$Assortment)
input_df$PromoInterval = as.numeric(input_df$PromoInterval)
input_df$Date = as.Date(input_df$Date, format = "%Y-%m-%d")
input_df$month <- as.integer(format(input_df$Date, "%m"))
input_df$year <- as.integer(format(input_df$Date, "%y"))
input_df$day <- as.integer(format(input_df$Date, "%d"))
input_df$Date = NULL
input_df$Customers = NULL
input_df$CompetitionOpenSinceYear = NULL

return(input_df)
}

final_train = remodelling_func(train_df)
final_test = remodelling_func(test_df)

#Correlation of each column w.r.t. other columns; corr closer to 1 is better
cor(final_train)

ml_model = lm(Sales ~ ., data = final_train)

summary(ml_model)

predictions = predict(ml_model, newdata = final_test)

head(final_test)

predictions_df <-  data.frame(Id=final_test$Id, Sales=predictions)

write.csv(predictions_df, "C:/Users/dubey/OneDrive/Desktop/IIT COURSES/DATA-ANYLITCS/PROJECT/rossmann-store-sales/original data/linear_model.csv", row.names = F, quote = F)

#***********************2. Hypothesis************************************
head(train)

hypo_data <- aggregate(Sales~Promo,train,length)
names(hypo_data)[2] <- 'num'

hypo_data_1 <- aggregate(Sales~Promo,train,sum)
names(hypo_data_1)[2] <- 'Total_Sales'

merge(hypo_data,hypo_data_1)

table(train$Promo)

#We can see it clearly that When Promo is there Number of Sales took place is low, but Sales amount is high,
#When Promo is not there Number of Sales are high, but Sales amount is less.
#Therefore our hypothesis is Correct

#***********************3. Annova************************************
table(train$StoreType)
table(train$StateHoliday)

annova_data <- aggregate(Sales ~ StoreType,train,length)
names(annova_data)[2] <- 'num'

annova_data_1 <- aggregate(Sales~StoreType,train,sum)
names(annova_data_1)[2] <- 'Total_Sales'

merge(annova_data,annova_data_1)

new_df <- train %>%
  select(StoreType,StateHoliday,Sales)

head(new_df)

table(new_df$StoreType, new_df$StateHoliday)

my_anova <- aov(Sales ~ StoreType * StateHoliday, data = train)
Anova(my_anova, type = "III")

#This is an unbalanced design
"Conclusion: StoreType is not significant, StateHoliday is significant as p-value is less than 0.05(significant level) "