
#Load some packages to get started
##################################
if(!require('readr')){
  install.packages('readr')
  library(readr)
}

if(!require('tidyverse')){
  install.packages('tidyverse')
  library(tidyverse)
}

if(!require('caret')){
  install.packages('caret')
  library(caret)
}

if(!require('ggpubr')){
  install.packages('ggpubr')
  library(ggpubr)
}






###################
#Downloading the Data
###################

if(!file.exists("kc_house_data.csv")){
url <- "https://github.com/katebw/Captstone_-Real-Estate/raw/master/kc_house_data.csv.zip"

download.file(url, "kc_house_data.csv.zip")
}


dl <- unzip("kc_house_data.csv.zip")

dl <- read_csv(dl)

set.seed(1,sample.kind = "Rounding") # this is for R 3.6.3

test_index <- createDataPartition(dl$price, times = 1, p = 0.1, list = FALSE)
dl <- dl %>% slice(-test_index)
validation <- dl %>% slice(test_index)

####################
#exploring the data
####################

#head(dl)

#dim(dl)

glimpse(dl)

#dl$zipcode<- as.factor(dl$zipcode)  #turning the zipcode from numeric to factor for later use
#class(dl$zipcode)

dl%>%summarize(avg_price = mean(price), median_price= median(price))

#############################################
#Sales over the year
#############################################
if(!require('lubridate')){
  install.packages('lubridate')
  library(lubridate)
} # install package to handle date/time class

time_sales<- dl%>%group_by(month = cut(date, "month"))%>%summarise(n_sales = n()) %>% mutate( type = ifelse(month(month)=="1", "highlight", "not"))


time_sales%>%ggplot(aes(month, n_sales), fill = type)+
  geom_col(aes(fill=type), show.legend = FALSE)+
  scale_fill_manual(values = c("black", "grey"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "Month", y = "Number of Sales", title = "Seasonal Pattern of Number of Sales in King County")
 
detach(package:lubridate) # detach package 

########################################
# graphing data: location vs. avg. price
########################################

top_1<-dl%>%group_by(zipcode)%>%summarise(avg_price = mean(price))%>%arrange(desc(avg_price))%>%top_n(1)

dl%>%group_by(zipcode)%>%summarise(avg_price = mean(price))%>%ggplot()+
  geom_count(aes(x = zipcode, y= avg_price), alpha = 0.2, size = 2)+
  geom_count(data = top_1, aes(x= zipcode, y= avg_price), color = "purple", size = 3)+
  geom_text(data = top_1, aes(x=zipcode, y= avg_price, label=zipcode), hjust = -0.2, vjust = 1, size =4)+
  geom_hline(yintercept = 540088, size = 1)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x = "Zipcode", y = "Average Price ($)", title = "Average Sale Price of Houses in Various Zip Codes of King County", caption = "Horizontal line indicates county average sale price. Sale prices in zipcode 98039 (highlighted in purple) are substantially higher than in any other zip code.")

########################################
# mutating the data to account for price/sqft
########################################



dl <-dl %>% mutate(ppSFt_living = price/sqft_living) # calculate and add column for price per square foot of living space

top_1<-dl%>%group_by(zipcode)%>%summarise(avg_price = mean(ppSFt_living))%>%arrange(desc(avg_price))%>%top_n(1)


dl %>% mutate(ppSFt_living = price/sqft_living)%>%group_by(zipcode)%>%summarise(avg_ppSqft = mean(ppSFt_living))%>% ggplot()+
  geom_count(aes(x=zipcode, y= avg_ppSqft), alpha =0.3, size = 2)+
  geom_count( data = top_1, aes(x=zipcode, y= avg_price), color = "purple", size = 3)+
  geom_text(data = top_1, aes(x= zipcode, y= avg_price, label = zipcode), hjust = -0.2, vjust = 1, size = 5)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x = "Zipcode", y = "Avegrage Price($) per Square Foot", title = "Average Price per square foot of living space for various zip codes in King County")

#######################################################################################
#variation in price per square foot within zip codes (top 10 average price as example)
#######################################################################################
dl_temp <- dl  # create a temporary version for converting zipcode to factor for boxplot

dl_temp$zipcode <- as.factor(dl_temp$zipcode) # convert zipcode to factor for boxplot graphing

top_10 <- dl_temp%>% group_by(zipcode)%>%summarise(avg_price = mean(ppSFt_living)) %>% arrange(desc(avg_price))%>% top_n(10, avg_price)

top_10_names <- top_10$zipcode

dl_temp%>%filter(zipcode %in% top_10$zipcode)%>%mutate(type = ifelse(zipcode == top_10$zipcode[1], "highlight","not"))%>%select(zipcode, ppSFt_living, type)%>%ggplot(aes(x= zipcode, y = ppSFt_living, fill= type))+
  geom_boxplot(show.legend = FALSE)+
  scale_fill_manual(values = c("purple", "grey"))+
  theme_light()+
  labs(x = "Zip Code", y = "Average Price($) per Square Foot", title = "Variation in Price per Square Foot in Top 10 Priciest Zip Codes in King County")

############################################################################
# partitioning data into train and test data within the greater dl train set
############################################################################
dl <-dl%>%select(-ppSFt_living) # remove created column of price per square foot
#$zipcode <- as.numeric(dl$zipcode) # return zipcode to class numeric 
set.seed(1, sample.kind = "Rounding")
test_index2 <- createDataPartition(dl$price, times = 1, p =0.1, list = FALSE)
dl_train <- dl %>% slice(-test_index2)
dl_test <- dl %>% slice(test_index2)

glimpse(dl_train)
###################################################
#basic linear regression between sqft and price
##################################################

fit <- lm(price~ sqft_living, data = dl_train)
summary(fit)


equation <- function(x){fit$coefficients[1]+ fit$coefficients[2]*x}

top_10 <- dl%>% group_by(zipcode)%>%summarise(avg_price = mean(price)) %>% arrange(desc(avg_price))%>% top_n(10, avg_price)

dl_train%>%mutate(type = ifelse(zipcode == top_10$zipcode[1], "highlight","not"))%>%ggplot(aes(x= sqft_living, y = price))+
  geom_point(aes(color = type, alpha = type), show.legend = FALSE)+
  scale_alpha_manual(values = c(0.9, 0.3))+
  scale_color_manual(values = c("purple", "grey"))+
  stat_function(fun = equation, geom = "line", show.legend = FALSE, color = "black", size= 1)+
  theme_light()+
  labs( x = "Sq. Ft. of Living Space", y = "Average Sale Price($)", title = "Correlation between Sq. Ft. of Living space and Sale Price in King County")


par(mfrow = c(2,2))
plot(fit) # plotting residuals from linear regression

y_hat_lm <- predict(fit, dl_test) # predicting values against test set

RMSE_lm <-sqrt(mean((y_hat_lm - dl_test$price)^2)) # calculating the RMSE of the predicted values vs. actual values

RMSE_lm
####################################################################
#basic linear regression across the predictors
##########################################################

foo <- apply(dl_train[4:21], 2, function(x) {
  fit_foo <- lm(price~x, data = dl_train)
  summary(fit_foo)$r.squared
})

par(mfrow = c(1,1))
par(las =2)
barplot(foo, cex.names = 1, col = ifelse(foo >=0.3,"black", "grey"))
############################################################################
#multivariate linear regression between sqft and price, but also including zip code
#############################################################################

fit_mv <- lm(price~ sqft_living+
               grade+
               sqft_above+
               sqft_living15, data = dl_train )
summary(fit_mv)
par(mfrow = c(2,2))
plot(fit_mv)

y_hat_mv <- predict(fit_mv, dl_test) # prediction values

RMSE_mv <- sqrt(mean((y_hat_mv - dl_test$price)^2)) #RMSE of predicted values

RMSE_mv

equation <- function(x){fit$coefficients[1]+ fit$coefficients[2]*x}

top_10 <- dl%>% group_by(zipcode)%>%summarise(avg_price = mean(price)) %>% arrange(desc(avg_price))%>% top_n(10, avg_price)

dl_train%>%mutate(type = ifelse(zipcode == top_10$zipcode[1], "highlight","not"))%>%ggplot(aes(x= sqft_living, y = price))+
  geom_point(aes(color = type, alpha = type), show.legend = FALSE)+
  scale_alpha_manual(values = c(0.9, 0.3))+
  scale_color_manual(values = c("purple", "grey"))+
  stat_function(fun = equation, geom = "line", show.legend = FALSE, color = "black", size= 1)+
  theme_light()+
  labs( x = "Sq. Ft. of Living Space", y = "Average Sale Price($)", title = "Correlation between Sq. Ft. of Living space and Sale Price in King County", caption = "Purple points represent sales in zip code 98039")



###################################################################################
#back to multivariate linear regression analyses-  which regression model is best? Using stepwise regression
###################################################################################

dl_numerics <- dl %>% select(-c(id, date)) #trimming the tibble to focus on numeric predictors



if(!require('leaps')){
  install.packages('leaps')
  library(leaps)
}
if(!require('MASS')){
  install.packages('MASS')
  library(MASS)
} # need to load libraries here due to masking conflicts with previous libraries, specifically 'select' from dplyr



set.seed(123, sample.kind = "Rounding") # for repeatability, R 3.6.3

train_ctrl <- trainControl(method = "cv", number = 10) # set-up for 10-fold cross-validation

step_model <- train(price~., data = dl_numerics, method = "leapBackward", tuneGrid = data.frame(nvmax=1:18), trControl = train_ctrl) # using up to 17 variables (n-1) columns, in this case the 18th column is price, the dependent variable

step_model$results

step_model$bestTune

summary(step_model$finalModel)

coef(step_model$finalModel, step_model$bestTune[,1])

detach(package:MASS)

#################################################################################
#fit with predictive values from step model
##################################################################
#dl$zipcode <- as.numeric(dl$zipcode) # need to return zipcode to it's orignial numeric class


fit_step <- lm(price~ bedrooms+
            bathrooms+
            sqft_living+
            sqft_lot+
            floors+
            waterfront+
            view+
            condition+
            grade+
            sqft_above+
            yr_renovated+
            zipcode+
            lat+
            long+
            sqft_living15+
            sqft_lot15+
            sqft_basement, data = dl_train)


summary(fit_step)
par(mfrow = c(2,2))
plot(fit_step)

y_hat_mv2 <- predict(fit_step, dl_test) # predicted values

RMSE_mv2 <- sqrt(mean((y_hat_mv2 - dl_test$price)^2)) # RMSE of predicted values

RMSE_mv2

#################################################################
# how does the trained algorithm fare on the validation set? 
################################################################
#validation$zipcode <- as.numeric(validation$zipcode)# need to turn zipcode into a numeric for the predictor to work
#class(validation$zipcode)



fit_step_dl <- lm(price~ bedrooms+
                 bathrooms+
                 sqft_living+
                 sqft_lot+
                 floors+
                 waterfront+
                 view+
                 condition+
                 grade+
                 sqft_above+
                 yr_renovated+
                 zipcode+
                 lat+
                 long+
                 sqft_living15+
                 sqft_lot15+
                 sqft_basement, data = dl)


summary(fit_step)
par(mfrow = c(2,2))
plot(fit_step_dl)

y_hat_final <- predict(fit_step_dl, validation) # final predicted values

RMSE_final <- sqrt(mean((y_hat_final - validation$price)^2)) # final RMSE

RMSE_final






