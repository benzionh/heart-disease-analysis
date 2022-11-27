setwd("~/Heart disease analysis")
library("tidyverse")
data<-read.csv("heart.csv")
head(data)
glimpse(data)
summary(data)
colnames(data)
# Data transformation
data2<-data %>%
  mutate(sex = if_else(sex==1,"MALE","FEMALE"),
         fbs = if_else(fbs==1,">120","<=120"),
         exang = if_else(exang==1,"YES","NO"),
         cp = if_else(cp==1,"ATYPICAL ANGINA",
                      if_else(cp==2,"NON-ANGINAL PAIN","ASYMPTOMATIC")),
         restecg = if_else(restecg==0,"NORMAL",
                           if_else(restecg==1,"ABNORMALITY","PROBABLE OR DEFINITE")),
         slope = as.factor(slope),
         ca = as.factor(ca),
         thal = as.factor(thal),
         target = if_else(target==1,"YES","NO")
         ) %>%
  mutate_if(is.character,as.factor) %>%
  dplyr::select(target,sex,fbs,exang,cp,restecg,slope,ca,thal,everything())
# Data visualization
#Bar plot for target
ggplot(data2,aes(x=target,fill=target))+
  geom_bar()+
  xlab("Heart disease")+
  ylab("count")+
  ggtitle("Presence & Abscence of Heart disese")+
  scale_fill_discrete(name="Heart disease", labels = c("Abscence","Presence"))
prop.table(table(data2$target))
#count the frequency of age
data2%>%
  group_by(ï..age) %>%
  count() %>%
  filter(n>10) %>%
  ggplot()+
  geom_col(aes(ï..age,n),fill="green")+
  ggtitle("Age analysis")+
  xlab("Age")+
  ylab("Age count")
#Compare blood pressure across chest pain
data2 %>%
  ggplot(aes(x=sex,y=trestbps))+
  geom_boxplot(fill = "Purple")+
  xlab("sex")+
  ylab("BP")+
  facet_grid(~cp)

data2 %>%
  ggplot(aes(x=sex,y=chol))+
  geom_boxplot(fill = "orange")+
  xlab("sex")+
  ylab("BP")+
  facet_grid(~cp)

#Correlation
library(corrplot)
library(ggplot2)
cor_heart<-cor(data2[,10:14])
cor_heart

corrplot(cor_heart,method = "square",type = "upper")
