# Import data
basic <- read.csv("C:/Users/Ramona/Google Drive/RPI/Spring 2016/Data Analytics/project/age_gender_bkts.csv/age_gender_bkts.csv")
countries <- read.csv("C:/Users/Ramona/Google Drive/RPI/Spring 2016/Data Analytics/project/countries.csv/countries.csv")
test <- read.csv("C:/Users/Ramona/Google Drive/RPI/Spring 2016/Data Analytics/project/test_users.csv/test_users.csv")
train <- read.csv("C:/Users/Ramona/Google Drive/RPI/Spring 2016/Data Analytics/project/train_users_2.csv/train_users_2.csv")
sessions <- read.csv("C:/Users/Ramona/Google Drive/RPI/Spring 2016/Data Analytics/sessions.csv")

# Age
summary(train$age)
hist(train$age,breaks=1000,xlim = c(0,150),main="Hsitogram of Users'Age in Train Set")
CleanAge <- function(x,minAge,maxAge)
