install.packages("corrplot")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("psych")
install.packages("relaimpo")
install.packages("Rtools")
install.packages("magrittr")
install.packages("dplyr")
install.packages("olsrr")

library(olsrr)
library(corrplot)
library(Hmisc)
library(ggplot2)
library(ggthemes)
library(psych)
library(relaimpo)
library(car)
library(magrittr)
library(dplyr)



getwd()
setwd("C:/Users/pc/Desktop")
veriset=read.csv("insurance.csv",header = T,sep = ";")
attach(veriset)


str(veriset)

veriset$sex=as.factor(veriset$sex)
veriset$smoker=as.factor(veriset$smoker)
veriset$region=as.factor(veriset$region)
str(veriset)
names(veriset)
dim(veriset)
sum(is.na(veriset))
summary(veriset)

par(mfrow=c(1,2))
hist(veriset$charges,main = "Charges Histogram",col="red")

boxplot(veriset$charges,main="Charges Boxplot",col="red")



pairs(veriset[,1:7], pch = 19, col='blue', lower.panel = NULL)


num_veriler=veriset[,1:4]

par(mfrow=c(1,1))
corrplot(cor(num_veriler))


ggplot(data = veriset,mapping = aes(x = as.factor(children),y = charges,color = children))+geom_boxplot()

ggplot(data = veriset,mapping = aes(x = as.factor(region),y = charges,color = region))+geom_boxplot()

ggplot(data = veriset, aes(x = as.factor(smoker),y = charges,color = smoker))+geom_boxplot()

ggplot(data = veriset,mapping = aes(x = as.factor(sex),y = charges,color = sex))+geom_boxplot()



pairs.panels(veriset[c("age", "bmi", "children", "charges")])

# model kurulumu

model=lm(charges~.,data=veriset)
summary(model)

vif(model) 

par(mfrow=c(2,2))
plot(model)


#bmi degiskeninin gruplandirilmasi
veriset %<>% mutate(bmi_cat = cut(bmi,
                                  breaks = c(0, 18.5, 25, 30, 60),
                                  labels = c("Under Weight", "Normal Weight", "Overweight", "Obese")
))
# health degiskeninin olusturulmasi (**)
veriset %<>%
  mutate(
    health = case_when(
      smoker == "yes" & bmi_cat == "Obese" ~ "Obese and smoker",
      smoker == "yes" & bmi_cat != "Obese" ~ "Non obese and smoker",
      smoker == "no" ~ "Non smoker"
    )
  )
veriset$health %<>% as.factor()




# Yeni deðiþkenler ile model kurulumu
yeni_model=lm(charges~.,data = veriset)

summary(yeni_model)



par(mfrow=c(2,2))
plot(yeni_model)

#1 ALL POSSIBLE BEST SUBSETS
library(olsrr)
a=ols_step_all_possible(yeni_model)
plot(a)

#ALTERNATÝF MODELLER
model1=lm(charges~age+health,data =veriset) # 9. Model
model2=lm(charges~age+children+health,data = veriset) # 37. Model
model3=lm(charges~age+children+region+health,data = veriset) # 93. Model 
model4=lm(charges~age+children+sex+health,data = veriset)  # 165. Model 


yenimodel2=lm(charges~age+I(age^2)+children+region+bmi+health,data = veriset)
model5=lm(charges~age+I(age^2)+children+region+bmi+health,data = veriset) # 163. Model
summary(model5)

par(mfrow=c(2,2))
plot(model5)

##PRESS

ols_press(model1)
ols_press(model2)
ols_press(model3)
ols_press(model4)
ols_press(model5)



summary(model5)


#STEPWISE REGRESSION

s=ols_step_both_p(yeni_model)
s

model_s=lm(charges~age+children+region+bmi+health+sex,data = veriset)
summary(model_s)


par(mfrow=c(2,2))
plot(model5)

m5_residuals=residuals(model5)
hist(m5_residuals)




#HATA NORMALLÝÐÝ
shapiro.test(m5_residuals)

#HATALARIN HOMOJEN VARYANSLILIÐI
library(lmtest)
bptest(model5)



#LEVAREGE KALDIRAÇ NOKTASI
hatvalues(model5)
hatvalues(model5)>2*mean(hatvalues(model5))
which(hatvalues(model5)>2*mean(hatvalues(model5)))

anova_m5=anova(model5)

st.res=model5$residuals/sqrt(anova_m5$`Mean Sq`[2])
student.res=model5$residuals/sqrt((anova_m5$`Mean Sq`[2]*(1-hatvalues(model5)))) 
cbind(st.res,student.res) 
par(1,1)
plot(hatvalues(model5),st.res)  
abline(h=c(-3,3),v=2*mean(hatvalues(model5)))
identify(hatvalues(model5),st.res)



cleaned_veri=veriset[-c(12,33,93,95,104,167,245,251,252,329,420,421,439,495,665,678,891,985,1048,1086,1187,1205,1242,1266,1302,1318),]
model5_cleaned=lm(charges~age+I(age^2)+children+region+bmi+health,data =cleaned_veri)

summary(model5_cleaned)
summary(model5)
par(mfrow=c(2,2))
plot(model5_cleaned)
plot(model5)



par(mfrow=c(1,1))
plot(charges,cooks.distance(model5))
abline(h=4/(length(charges)-2))



library("car")
vif(model5_cleaned) #çoklu doðrusal baðlantý sorunu yok

predictions3<- predict(model3,veriset)
predictions4 <- predict(model4,veriset)
predictions5 <- predict(model5,veriset)
predictions5_cleaned <- predict(model5_cleaned,veriset)

RMSE1 <-  RMSE(predictions3,veriset$charges)
RMSE2 <- RMSE(predictions4, veriset$charges)
RMSE3 <- RMSE(predictions5, veriset$charges)
RMSE4 <- RMSE(predictions5_cleaned,veriset$charges)

cbind(RMSE1,RMSE2,RMSE3,RMSE4)




final_model <- model5
summary(final_model)

confint(final_model)

