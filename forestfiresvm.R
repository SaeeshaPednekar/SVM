forestfire<-read.csv("F:/Excelr/Assignments/dataset/SVM/forestfires.csv")
View(forestfire)
str(forestfire)
attach(forestfire)
#exploratory data analysis

boxplot(FFMC,horizontal = T)
boxplot(DMC,horizontal = T)
boxplot(DC,horizontal = T)
boxplot(ISI,horizontal = T)
boxplot(temp,horizontal = T)
boxplot(RH,horizontal = T)
boxplot(wind,horizontal = T)
boxplot(rain,horizontal = T)
library(moments)

skewness(FFMC)
skewness(DMC)
skewness(DC)
skewness(ISI)
skewness(temp)
skewness(RH)
skewness(wind)
skewness(rain)

kurtosis(FFMC)
kurtosis(DMC)
kurtosis(DC)
kurtosis(ISI)
kurtosis(temp)
kurtosis(RH)
kurtosis(wind)
kurtosis(rain)

plot(FFMC,RH)
cor(FFMC,RH)
plot(DMC,RH)
cor(DMC,RH)
plot(DC,ISI)
cor(DC,ISI)
plot(ISI,wind)
cor(ISI,wind)
plot(FFMC,DC)
cor(FFMC,DC)
plot(FFMC,DMC)
cor(FFMC,DMC)
plot(ISI,FFMC)
cor(ISI,FFMC)


hist(area,probability = T,breaks = 30)
lines(density(area))
hist(FFMC,probability = T,breaks = 30)
lines(density(FFMC))
hist(DMC,probability = T,breaks = 30)
lines(density(DMC))
hist(DC,probability = T,breaks = 30)
lines(density(DC))
hist(ISI,probability = T,breaks = 30)
lines(density(ISI))
hist(temp,probability = T,breaks = 30)
lines(density(temp))
hist(RH,probability = T,breaks = 30)
lines(density(RH))
hist(wind,probability = T,breaks = 30)
lines(density(wind))


normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}


attach(forestfire)
View(forestfire)
norm<-as.data.frame(lapply(forestfire[3:10],FUN = normalize))
View(forestfire)

forest_norm<-cbind(month,day,norm,size_category)

forest_train<-forest_norm[1:362,]
forest_test<-forest_norm[363:517,]


?ksvm
library(kernlab)

forest_model_1<-ksvm(size_category~.,data=forest_train,kernel = "vanilladot")
predtest_vanilladot <-predict(forest_model_1,newdata=forest_test)
mean(forest_test$size_category==predtest_vanilladot)
table(predtest_vanilladot,forest_test$size_category)


forest_model_2<-ksvm(size_category~.,data=forest_train,kernel = "rbfdot")
predtest_rbfdot <-predict(forest_model_1,newdata=forest_test)
mean(forest_test$size_category==predtest_rbfdot)
table(predtest_rbfdot,forest_test$size_category)


forest_model_3<-ksvm(size_category~.,data=forest_train,kernel = "polydot")
predtest_polydot <-predict(forest_model_3,newdata=forest_test)
mean(forest_test$size_category==predtest_polydot)
table(predtest_polydot,forest_test$size_category)


forest_model_4<-ksvm(size_category~.,data=forest_train,kernel = "besseldot")
predtest_besseldot <-predict(forest_model_4,newdata=forest_test)
mean(forest_test$size_category==predtest_besseldot)
table(predtest_besseldot,forest_test$size_category)

