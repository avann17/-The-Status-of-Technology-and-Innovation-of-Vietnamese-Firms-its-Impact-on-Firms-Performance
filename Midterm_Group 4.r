rm(list = ls())
library(haven)
library(plyr)
library(dplyr)
library(tidyr)
library(ggridges)
library(ggplot2)
library(hrbrthemes)
library(ggthemes)
library(viridis)
library(ggrepel)
install.packages("Hmisc")
install.packages("PerformanceAnalytics")
install.packages(c("corrplot","lmtest","car","ggplot2"))
install.packages("")
library("PerformanceAnalytics")
library("Hmisc")
library("corrplot")
library(MASS)
data2023 <- read_dta("C:/Users/nguye/AppData/Local/Temp/Rar$DRa18932.40576/Data for Assignment No.1/Viet-Nam-2023-full-data.dta")
View(data2023)
# Chọn giá trị liên quan đến I&T -> bỏ h2 thì có nhiều NA
newdata2023<- data2023[,c("d2","f1", "k4","b8","e6","c22b","h1","h5","h8","l1","l4b","b4a","a2","a4a","a6a", "c36","c39")]
View(newdata2023)

# Quy đổi các giá trị -6,-9 về NA
data_updated2023 <- replace(newdata2023, is.na(newdata2023) | newdata2023 == -9 | newdata2023 == -6| newdata2023 == -7, NA)
View(data_updated2023)
#Loại bỏ giá trị NA
data2023_filtered <- na.omit(data_updated2023)
View(data2023_filtered)

#Description
##Distribution of d2
ggplot(data2023_filtered, aes(x= d2)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.4, fill="blue") +
  labs(title= "Distribution of total annual sales", x="sales")
##Relationship between d2 and b8
ggplot(data2023_filtered, aes(x = b8, y = d2)) +
  geom_point() +
  geom_smooth(method=lm, color="red", se=FALSE)+
  labs(title = "Relationship between sales and international quality cert",
       y = "Annual sales",
       x = "Internationally-recognized quality certification")
##Relationship between d2 and e6
ggplot(data2023_filtered, aes(x = e6, y = d2)) +
  geom_point() +
  geom_smooth(method=lm, color="red", se=FALSE)+
  labs(title = "Relationship between sales and foreign technology lisence",
       y = "Annual sales",
       x = "Foreign technology lisence")
##Relationship between d2 and c22b
ggplot(data2023_filtered, aes(x = c22b, y = d2)) +
  geom_point() +
  geom_smooth(method=lm, color="red", se=FALSE)+
  labs(title = "Relationship between sales and own website",
       y = "Annual sales",
       x = "Own website")
##Relationship between d2 and h1
ggplot(data2023_filtered, aes(x = h1, y = d2)) +
  geom_point() +
  geom_smooth(method=lm, color="red", se=FALSE)+
  labs(title = "Relationship between sales and new products/services",
       y = "Annual sales",
       x = "New products/services")
##Relationship between d2 and h5
ggplot(data2023_filtered, aes(x = h5, y = d2)) +
  geom_point() +
  geom_smooth(method=lm, color="red", se=FALSE)+
  labs(title = "Relationship between sales and new improved process",
       y = "Annual sales",
       x = "new improved process")
### Không có significant relationship
##Relationship between d2 and h8
ggplot(data2023_filtered, aes(x = h8, y = d2)) +
  geom_point() +
  geom_smooth(method=lm, color="red", se=FALSE)+
  labs(title = "Relationship between sales and R&D",
       y = "Annual sales",
       x = "Spent on R&D")
##Relationship between d2 and f1
ggplot(data2023_filtered, aes(x = f1, y = d2)) +
  geom_point() +
  geom_smooth(method=lm, color="red", se=FALSE)+
  labs(title = "Relationship between sales and capacity utilization",
       y = "Annual sales",
       x = "Capacity utilization")
##Relationship between d2 and k4
ggplot(data2023_filtered, aes(x = k4, y = d2)) +
  geom_point() +
  geom_smooth(method=lm, color="red", se=FALSE)+
  labs(title = "Relationship between sales and new fixed asset",
       y = "Annual sales",
       x = "Spent on buying fixed asset")
##Relationship between d2 and c36
ggplot(data2023_filtered, aes(x = c36, y = d2)) +
  geom_point() +
  geom_smooth(method=lm, color="red", se=FALSE)+
  labs(title = "Relationship between sales and internet connection",
       y = "Annual sales",
       x = "Internet connection")
##Relationship between d2 and c39
ggplot(data2023_filtered, aes(x = c39, y = d2)) +
  geom_point() +
  geom_smooth(method=lm, color="red", se=FALSE)+
  labs(title = "Relationship between sales and internet disruption",
       y = "Annual sales",
       x = "Internet disruption")

#full model
data1<-data2023_filtered[,c("k4","b8","e6","c22b","h1","h5","h8","l1","b4a","l4b","a2","f1","a4a","a6a", "c36", "c39")]
PerformanceAnalytics::chart.Correlation(data1, histogram = TRUE, pch = 19)
a <- cor(data1, method = "pearson")
corrplot::corrplot(a,method = "shade")
data2023_filtered$d2_log<-log(data2023_filtered$d2)
model1 <- lm(formula = d2_log ~ b8 + e6 + c22b + h1 +l4b + l1 + h8 + a6a + f1 + h5+ k4+ b4a + a2 + a4a + c36 + c39, data=data2023_filtered)
summary(model1)



#Model rút ngọn
data2<-data2023_filtered[,c("b8","e6","c22b", "h1","h8","l4b","l1", "f1", "a6a", "c36","c39")]
PerformanceAnalytics::chart.Correlation(data2, histogram = TRUE, pch = 19)
a <- cor(data2, method = "pearson")
corrplot::corrplot(a,method = "shade")
data2023_filtered$d2_log<-log(data2023_filtered$d2)
model2 <- lm(formula = d2_log ~ b8 + e6 + c22b + h1 +l4b + l1 + h8 + a6a + f1 + c36 + c39, data=data2023_filtered)
summary(model2)
par(mfrow = c(2,2))
plot(model2) 

#Kiểm tra assumption
#Linear Relationship
plot(model2,1)
#Normal Residuals
res <- resid(model2)
plot(model2,2)
hist(res)
shapiro.test(res)

#Residual = 0
t.test(res, mu=0)
#   Ho: mean(res) = 0
#   Ha: mean(res) =! 0
#   p-value high -> not reject Ho

#Homoskedasticity
plot(model2,3)
library(lmtest)
bptest(model2)

#Autocorrelation
#load car package
library(car)
#perform Durbin-Watson test
durbinWatsonTest(model2) #check the autocorrelation (p-value>0,05 -> no autocorrelation)
plot(res)
vif(model2)

step.model1 <- stepAIC(model2,direction = "both")
step.model2 <- stepAIC(model2,direction = "backward")
summary(step.model1)
par(mfrow = c(2,2))
plot(step.model1)
#Linear Relationship
plot(model2,1)
#Normal Residuals
res1 <- resid(step.model1)
plot(step.model1,2)
hist(res1)
shapiro.test(res1)
#Residual = 0
t.test(res1, mu=0)
#Homoskedasticity
plot(step.model1,3)
library(lmtest)
bptest(step.model1)
#Autocorrelation
#load car package
library(car)
#perform Durbin-Watson test
durbinWatsonTest(step.model1)
plot(res)
vif(model1)

# kết luận: mô hình ngon là bỏ c22b, c39 đi


#năm 2015
data2015<- read_dta("C:/Users/nguye/AppData/Local/Temp/Rar$DRa6752.28300/Data for Assignment No.1/Vietnam-2015-full-data.dta")
View(data2015)
newdata2015<- data2015[,c("d2", "k4","b8","e6","c22b","h1","h5","h8","l1","b4a","l4b","a2","f1","a4a","a6a")]
View(newdata2023)

data_updated2015 <- replace(newdata2015, is.na(newdata2015)| newdata2015 == -9 | newdata2015 == -6| newdata2015 == -7, NA)
View(data_updated2015)
data2015_filtered <- na.omit(data_updated2015)
View(data2015_filtered)

#Full model
data3<-data2015_filtered[,c("k4","b8","e6","c22b","h1","h5","h8","l1","b4a","l4b","a2","f1","a4a","a6a")]
PerformanceAnalytics::chart.Correlation(data3, histogram = TRUE, pch = 19)
a <- cor(data3, method = "pearson")
corrplot::corrplot(a,method = "shade")

data2015_filtered$d2_log<-log(data2015_filtered$d2)
model3 <- lm(formula = d2_log ~ b8 + e6 + c22b + h1 +l4b + l1 + h8 + a6a + f1 + h5+ k4+ b4a + a2 + a4a, data=data2015_filtered)
summary(model3)

#Model rút ngắn
data4<-data2015_filtered[,c("b8","e6","c22b", "h1","l4b","l1", "f1", "a6a")]
PerformanceAnalytics::chart.Correlation(data4, histogram = TRUE, pch = 19)
a <- cor(data4, method = "pearson")
corrplot::corrplot(a,method = "shade")
data2015_filtered$d2_log<-log(data2015_filtered$d2)
model4 <- lm(formula = d2_log ~ b8 + e6 + c22b + h1 +l4b + l1  + a6a + f1, data=data2015_filtered)
summary(model4) # không có tech variables nào ảnh hưởng

data2015_filtered$d2_log<-log(data2015_filtered$d2)
model5 <- lm(formula = d2_log ~ b8 + e6 + c22b + h1 + h5  + a6a + a2, data=data2015_filtered)
summary(model5) #biến e6 liên quan
 
#Kiểm tra assumption
res <- resid(model4)
plot(model4,2)
hist(res)
shapiro.test(res)

#Linear Relationship
plot(model4,1)
#Normal Residuals
res <- resid(model4)
plot(model4,2)
hist(res)
shapiro.test(res)

#Residual = 0
t.test(res, mu=0)
#   Ho: mean(res) = 0
#   Ha: mean(res) =! 0
#   p-value high -> not reject Ho

#Homoskedasticity
plot(model4,3)
library(lmtest)
bptest(model4)

plot(model4,3)
library(lmtest)
bptest(model4)

step.model1 <- stepAIC(model4,direction = "both")
step.model2 <- stepAIC(model4,direction = "backward")
summary(step.model1)
par(mfrow = c(2,2))
plot(step.model1)
#Linear Relationship
plot(model2,1)
#Normal Residuals
res1 <- resid(step.model1)
plot(step.model1,2)
hist(res1)
shapiro.test(res1)
#Residual = 0
t.test(res1, mu=0)
#Homoskedasticity
plot(step.model1,3)
library(lmtest)
bptest(step.model1)
#Autocorrelation
#load car package
library(car)
#perform Durbin-Watson test
durbinWatsonTest(step.model1)
plot(res)
vif(model1)

