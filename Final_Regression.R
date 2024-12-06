install.packages('corrgram')
install.packages("fastDummies")
library(fastDummies)
library(dplyr) 
library(ggplot2) 
library(corrgram)

data<- read.csv('Transactions_Customer.csv')
head(data)
summary(data)
sum(complete.cases(data))
sum(!complete.cases(data))

cor(data)

ggplot(newdata)+geom_histogram(aes(Estimated_Age),binwidth = 5,fill='#cba667')+
  labs(title="Histogram of Estimated Age",x="Estimated Age",y="# of transactions", tag= "Figure 9")+
  theme(legend.position="none")

ggplot(newdata)+geom_histogram(aes(Estimated_Income),binwidth = 500,fill='#894867')+
  labs(title="Histogram of Estimated Income",x="Estimated Income",y="# of transactions", tag= "Figure 10")+
  theme(legend.position="none")

ggplot(newdata)+geom_histogram(aes(Time_On_Site),binwidth = 5,fill='#003643')+
  labs(title="Histogram of Time on site",x="Time on Site",y="# of transactions", tag= "Figure 11")+
  theme(legend.position="none")

ggplot(newdata)+geom_histogram(aes(Revenue),binwidth = 5,fill='#114687')+
  labs(title="Histogram of Revenue",x="Revenue",y="# of transactions", tag= "Figure 12")+
  theme(legend.position="none")


data$Seen_Voucher<- as.factor(data$Seen_Voucher)

ggplot(data) + geom_point(aes(Estimated_Age,Revenue,color=Seen_Voucher))+
  labs(title = "Estimated Age and Revenue Plot with Seen Voucher Information", caption = "Data from Customer Transaction",
       x = "Estimated Age",y = "Revenue", tag= "Figure 13")

ggplot(data) + geom_point(aes(Time_On_Site,Revenue,color=Seen_Voucher))+
  labs(title = "Time on site and Revenue Plot with Seen Voucher Information", caption = "Data from Customer Transaction",
       x = "Time on site",y = "Revenue", tag= "Figure 14") 

ggplot(data) + geom_point(aes(Estimated_Income,Revenue,color=Seen_Voucher))+
  labs(title = "Estimated Income and Revenue Plot with Seen Voucher Information", caption = "Data from Customer Transaction",
       x = "Estimated Income",y = "Revenue", tag= "Figure 15")

ggplot(data) + geom_point(aes(Estimated_Income,Revenue,color=Seen_Voucher))+
  labs(title = "Estimated Income and Revenue Plot with Seen Voucher Information", caption = "Data from Customer Transaction",
       x = "Estimated Income",y = "Revenue", tag= "Figure 16")+geom_abline(slope=0.003, intercept=13.49)



ggplot(data)+geom_boxplot(aes(as.factor(Advertisement_Channel),Revenue),fill='#bbf587')+
  labs(title="Boxplpot of Advertisement channel over Revenue",x="Advertisement Channel",y="Revenue", tag= "Figure 17")

ggplot(data)+geom_boxplot(aes(as.factor(Seen_Voucher),Revenue),fill='#cb0987')+
  labs(title="Boxplpot of Seen Voucher over Revenue",x="Seen Voucher",y="Revenue", tag= "Figure 18")

ggplot(data) + geom_point(aes(Estimated_Income,Estimated_Age))+
  labs(title = "Estimated Income and Estimated Age Plot", caption = "Data from Customer Transaction",
       tag = "Figure 1",x = "Estimated Income",y = "Estimated Age", tag= "Figure 19")

ggplot(data) + geom_point(aes(Estimated_Income,Time_On_Site))+
  labs(title = "Estimated Income and Time On Site Plot", caption = "Data from Customer Transaction",
       tag = "Figure 1",x = "Estimated Income",y = "Time on site", tag= "Figure 20")

ggplot(data) + geom_point(aes(Time_On_Site,Estimated_Age))+
  labs(title = "Time On Site and Estimated Age Plot", caption = "Data from Customer Transaction",
       tag = "Figure 1",x = "Time on site",y = "Estimated Age", tag= "Figure 21")


table(data$Advertisement_Channel)
data$Advertisement_Channel<- as.factor(data$Advertisement_Channel)
newdata<- data
newdata<- dummy_cols(data,select_columns='Advertisement_Channel',remove_first_dummy=TRUE)

newdata$Advertisement_Channel_2 <- ifelse(newdata$Advertisement_Channel==2,1,0)
newdata$Advertisement_Channel_3 <- ifelse(newdata$Advertisement_Channel==3,1,0)
newdata$Advertisement_Channel_4 <- ifelse(newdata$Advertisement_Channel==4,1,0)

model<- lm(Revenue~Advertisement_Channel_3+Advertisement_Channel_2+Advertisement_Channel_4+Estimated_Income+Seen_Voucher,newdata)
head(newdata)
summary(model)



model1<-lm(Revenue~Estimated_Income+Seen_Voucher+Advertisement_Channel,data)
summary(model1)

ggplot(data)+geom_point(aes(model1,Revenue),fill='#bbf587')+
  labs(title="Boxplpot of Advertisement channel over Revenue",x="Advertisement Channel",y="Revenue")

ggplot(data) + geom_point(aes(Estimated_Income,Revenue,color=Seen_Voucher))+
  labs(title = "Estimated Income and Revenue Plot with Seen Voucher Information", caption = "Data from Customer Transaction",
       tag = "Figure 1",x = "Estimated Income",y = "Revenue")+geom_abline(slope=0.003,intercept=13)




#task2
newdata<- dummy_cols(data,select_columns='Advertisement_Channel',remove_first_dummy=FALSE)
data$Seen_Voucher<- as.factor(data$Seen_Voucher)
head(newdata)
model2<-lm(Revenue~Advertisement_Channel_2,newdata)
summary(model2)
model4<-lm(Revenue~Estimated_Income+Seen_Voucher+Advertisement_Channel_4,newdata)

summary(model4)
model5<-lm(Revenue~Advertisement_Channel_3,newdata)

summary(model5)
model6<-lm(Revenue~Advertisement_Channel_4,newdata)
summary(model6)



model3<- lm(Revenue~Seen_Voucher+Estimated_Income+Advertisement_Channel,data)
summary(model3)





