# Market Basket Analysis - arules

install.packages("data.table")
library(data.table)

expedia_train <- fread('C:/sanju previous files/uconn/Data science/R studio directory/1Project/expedia/expediaFinalSample.csv', header=TRUE)

head(expedia_train)
null


install.packages("arules")
install.packages("arulesViz")
install.packages("datasets")
library(arules)
library(arulesViz)
library(datasets)

str(expedia_train)
summary(expedia_train$hotel_country)

expedia_train$posa_continent = as.factor(expedia_train$posa_continent)
expedia_train$user_location_country = as.factor(expedia_train$user_location_country)
expedia_train$user_location_region = as.factor(expedia_train$user_location_region)
expedia_train$user_location_city = as.factor(expedia_train$user_location_city)
expedia_train$user_id = as.factor(expedia_train$user_id)
expedia_train$channel = as.factor(expedia_train$channel)
expedia_train$srch_destination_id = as.factor(expedia_train$srch_destination_id)
expedia_train$srch_destination_type_id = as.factor(expedia_train$srch_destination_type_id)
expedia_train$hotel_continent = as.factor(expedia_train$hotel_continent)
expedia_train$hotel_country = as.factor(expedia_train$hotel_country)
expedia_train$hotel_market = as.factor(expedia_train$hotel_market)
expedia_train$hotel_cluster = as.factor(expedia_train$hotel_cluster)
expedia_train$site_name = as.factor(expedia_train$site_name)
expedia_train$is_mobile = as.factor(expedia_train$is_mobile)
expedia_train$srch_adults_cnt = as.factor(expedia_train$srch_adults_cnt)
expedia_train$srch_children_cnt = as.factor(expedia_train$srch_children_cnt)
expedia_train$srch_rm_cnt = as.factor(expedia_train$srch_rm_cnt)
expedia_train$is_booking = as.factor(expedia_train$is_booking)
expedia_train$cnt = as.factor(expedia_train$cnt)
expedia_train$is_package = as.factor(expedia_train$is_package)

# Rules
rules <- apriori(expedia_train, parameter = list(supp = 0.001, conf = 0.8))


# Show the top 5 rules, but only 2 digits
options(digits=2)
inspect(rules[1:10])

# Sorting rules
rules<-sort(rules, by="confidence", decreasing=TRUE)

# Can use this for Market Basket Analysis
options(digits=2)
inspect(rules[1:10])



