
install.packages("RODBC")
install.packages("tcltk")
library(sjPlot)
library(gridExtra)
library(tidyverse)
library(plyr)
library(ggplot2)
library(dplyr)
library(ztable)
library(scales)
library(tidyr)



setwd("/shared/0895 - Educational attainment of children with epilepsy in Wales/Meghna/")
source("sail_login.r")

# pulling all the values from dataset.

test2 <- sqlQuery(channel, "select * FROM sailw0895v.EPI_EDU_DEP_DRU")

#test3 <- sqlQuery(channel, "select * FROM sailw0895v.EPI_EDU_DEP_DRU")

# To determine the data type of the  Variable 

str(test2)

#encoding CSI1 

test2 <- test2 %>% mutate(CSI1=ifelse(CSI1=="Y",1,0))

#encoding school year. 

test2 <- test2%>% mutate(SCHOOL_YEAR=ifelse(SCHOOL_YEAR==2005,5,
         ifelse(SCHOOL_YEAR==2006,6,
         ifelse(SCHOOL_YEAR== 2007,7,
         ifelse(SCHOOL_YEAR==2008,8,
         ifelse(SCHOOL_YEAR==2009,9,
         ifelse(SCHOOL_YEAR==2010,10,
         ifelse(SCHOOL_YEAR==2011,11,
         ifelse(SCHOOL_YEAR==2012,12,
         ifelse(SCHOOL_YEAR==2014,14,
         ifelse(SCHOOL_YEAR==2015,15,NA)))))))))))
         
test2$SCHOOL_YEAR <- as.factor(test2$SCHOOL_YEAR)

test2$GNDR_CD <- as.factor(test2$GNDR_CD)

test2$CSI1 <- as.numeric(test2$CSI1)

test2$QUINTILE <- as.factor(test2$QUINTILE)


#encoding for drug code. 

test2 <- test2%>% mutate(DRUG_NAME=ifelse(DRUG_NAME=="LOMOTRIGINE",26,
         ifelse(DRUG_NAME=="LEVETRICETAM",2,
         ifelse(DRUG_NAME=="VALPROIC ACID",3,
         ifelse(DRUG_NAME=="ARBIL",4,
         ifelse(DRUG_NAME=="TOPIRAMATE",5,
         ifelse(DRUG_NAME=="SODIUM VALPROATE",6,
         ifelse(DRUG_NAME=="ZONISAMIDE",7,
         ifelse(DRUG_NAME=="ETHOSUXIMIDE",8,
         ifelse(DRUG_NAME=="PENTRAN",9,
         ifelse(DRUG_NAME=="RUFINAMIDE",10,
         ifelse(DRUG_NAME=="LACOSAMIDE",11,
         ifelse(DRUG_NAME=="VIGABATRIN",12,
         ifelse(DRUG_NAME=="GABAPENTINE",13,
         ifelse(DRUG_NAME=="PHENYTOIN",14,
         ifelse(DRUG_NAME=="OXCARBAZEPINE",15,
         ifelse(DRUG_NAME=="DIAZEPAM",16,
         ifelse(DRUG_NAME=="PHENOBARBITAL",17,
         ifelse(DRUG_NAME=="PIRACETAM",18,
         ifelse(DRUG_NAME=="ESLICARBAZEPINE_ACETATE",19,
         ifelse(DRUG_NAME=="GABITRIL",20,
         ifelse(DRUG_NAME=="RETIGABINE",21,
         ifelse(DRUG_NAME=="PREGABALINE",22,
         ifelse(DRUG_NAME=="STIRIPENTOL",23,
         ifelse(DRUG_NAME=="EMESIDE",24,
         ifelse(DRUG_NAME=="PRIMIDONE",25,
         ifelse(DRUG_NAME=="NO_DRUG",1,NA)))))))))))))))))))))))))))

test2$KEYSTAGE <-as.factor(test2$KEYSTAGE)

test2$DRUG_NAME <- as.factor(test2$DRUG_NAME)

str(test2)
summary(test2)

# glm for epilesy student , with all the keystages in them.
 
m1 <- glm(CSI1~ GNDR_CD+QUINTILE+ KEYSTAGE+SCHOOL_YEAR+DRUG_NAME, data=test2,family=binomial)
summary(m1)

plot_model(m7,type='pred', terms='QUINTILE')

plot_model(m8,type='pred', terms='SCHOOL_YEAR')

p = plot_model(m1,type='pred')
plot_grid(p)


p1 = plot_model(m2,type='pred')
plot_grid(p1)

draft1 = plot_model(m1,type='pred', terms='DRUG_NAME')
plot(draft1)

draft2 = plot_model(m1,type='pred', terms='KEYSTAGE')
plot(draft2)

draft3 = plot_model(m1,type='pred', terms='QUINTILE')
plot(draft3)

draft4= plot_model(m1,type='pred', terms='GNDR_CD')
plot(draft4)

draft5= plot_model(m1,type='pred', terms='SCHOOL_YEAR')
plot(draft5)


library(ggplot2)

ggplot(test2, aes(x = QUINTILE, fill = CSI1)) + 
    geom_bar(position = "fill") + theme_classic()

#ggplot()


control_group <- sqlQuery(channel, "select * FROM sailw0895v.MEGNHA_PUL_edu_keystage edu")


#################################Keystage##########################################

# Logistic regression for only children with keystage 2 having epilepsy .

Keystage2 <- sqlQuery(channel, "select CSI1,Quintile,School_year,GNDR_CD,DRUG_NAME from  Sailw0895v.epi_Keysatge_2")
summary(Keystage2)
str(Keystage2)


Keystage2 <- Keystage2 %>% mutate(DRUG_CODE=ifelse(DRUG_NAME=="LOMOTRIGINE",26,
          ifelse(DRUG_NAME=="LEVETRICETAM",2,
          ifelse(DRUG_NAME=="VALPROIC ACID",3,
          ifelse(DRUG_NAME=="ARBIL",4,
          ifelse(DRUG_NAME=="TOPIRAMATE",5,
          ifelse(DRUG_NAME=="SODIUM VALPROATE",6,
          ifelse(DRUG_NAME=="ZONISAMIDE",7,
          ifelse(DRUG_NAME=="ETHOSUXIMIDE",8,
          ifelse(DRUG_NAME=="PENTRAN",9,
          ifelse(DRUG_NAME=="RUFINAMIDE",10,
          ifelse(DRUG_NAME=="LACOSAMIDE",11,
          ifelse(DRUG_NAME=="VIGABATRIN",12,
          ifelse(DRUG_NAME=="GABAPENTINE",13,
          ifelse(DRUG_NAME=="PHENYTOIN",14,
          ifelse(DRUG_NAME=="OXCARBAZEPINE",15,
          ifelse(DRUG_NAME=="DIAZEPAM",16,
          ifelse(DRUG_NAME=="PHENOBARBITAL",17,
          ifelse(DRUG_NAME=="PIRACETAM",18,
          ifelse(DRUG_NAME=="ESLICARBAZEPINE_ACETATE",19,
          ifelse(DRUG_NAME=="GABITRIL",20,
          ifelse(DRUG_NAME=="RETIGABINE",21,
          ifelse(DRUG_NAME=="PREGABALINE",22,
          ifelse(DRUG_NAME=="STIRIPENTOL",23,
          ifelse(DRUG_NAME=="EMESIDE",24,
          ifelse(DRUG_NAME=="PRIMIDONE",25,
          ifelse(DRUG_NAME=="NO_DRUG",1,NA)))))))))))))))))))))))))))

                                                                       


Keystage2$DRUG_CODE <- as.factor(Keystage2$DRUG_CODE)

Keystage2 <- Keystage2 %>% mutate(CSI1=ifelse(CSI1=="Y",1,0))

Keystage2$SCHOOL_YEAR <- as.factor(Keystage2$SCHOOL_YEAR)

Keystage2$GNDR_CD <- as.factor(Keystage2$GNDR_CD)

Keystage2$CSI1 <- as.factor(Keystage2$CSI1)

Keystage2$QUINTILE <- as.factor(Keystage2$QUINTILE)

str(Keystage2)

K2 <- glm(CSI1~ QUINTILE + SCHOOL_YEAR +GNDR_CD + DRUG_CODE ,data=Keystage2, family=binomial(link="logit"),
          na.action=na.exclude)
summary(K2)


# Pulling all the data , similar to above model in order to make predicted probability graph.

keystage_pred <- Keystage2 <- sqlQuery(channel, "select CSI1,Quintile,School_year,GNDR_CD,DRUG_NAME from  Sailw0895v.epi_Keysatge_2")
str(keystage_pred)

# converting all the variable as per requirement of the model.

keystage_pred <- keystage_pred %>% mutate(CSI1=ifelse(CSI1=="Y",1,0))

keystage_pred <- keystage_pred %>% mutate(DRUG_NAME=ifelse(DRUG_NAME=="LOMOTRIGINE",26,
                              ifelse(DRUG_NAME=="LEVETRICETAM",2,
                              ifelse(DRUG_NAME=="VALPROIC ACID",3,
                              ifelse(DRUG_NAME=="ARBIL",4,
                              ifelse(DRUG_NAME=="TOPIRAMATE",5,
                              ifelse(DRUG_NAME=="SODIUM VALPROATE",6,
                              ifelse(DRUG_NAME=="ZONISAMIDE",7,
                              ifelse(DRUG_NAME=="ETHOSUXIMIDE",8,
                              ifelse(DRUG_NAME=="PENTRAN",9,
                              ifelse(DRUG_NAME=="RUFINAMIDE",10,
                              ifelse(DRUG_NAME=="LACOSAMIDE",11,
                              ifelse(DRUG_NAME=="VIGABATRIN",12,
                              ifelse(DRUG_NAME=="GABAPENTINE",13,
                              ifelse(DRUG_NAME=="PHENYTOIN",14,
                              ifelse(DRUG_NAME=="OXCARBAZEPINE",15,
                              ifelse(DRUG_NAME=="DIAZEPAM",16,
                              ifelse(DRUG_NAME=="PHENOBARBITAL",17,
                              ifelse(DRUG_NAME=="PIRACETAM",18,
                              ifelse(DRUG_NAME=="ESLICARBAZEPINE_ACETATE",19,
                              ifelse(DRUG_NAME=="GABITRIL",20,
                              ifelse(DRUG_NAME=="RETIGABINE",21,
                              ifelse(DRUG_NAME=="PREGABALINE",22,
                              ifelse(DRUG_NAME=="STIRIPENTOL",23,
                              ifelse(DRUG_NAME=="EMESIDE",24,
                              ifelse(DRUG_NAME=="PRIMIDONE",25,
                              ifelse(DRUG_NAME=="NO_DRUG",1,NA)))))))))))))))))))))))))))




keystage_pred$CSI1 <- as.factor(keystage_pred$CSI1)

keystage_pred$QUINTILE <-as.integer(keystage_pred$QUINTILE)

keystage_pred$DRUG_NAME<- as.numeric(keystage_pred$DRUG_NAME)

keystage_pred$GNDR_CD <- as.factor(keystage_pred$GNDR_CD)

str(keystage_pred)

summary(keystage_pred)


# replicating the above model keeping all the variable as numeric or integer.

K2_predict  <- glm(CSI1~ QUINTILE + SCHOOL_YEAR +GNDR_CD + DRUG_NAME ,data=keystage_pred , family=binomial(link="logit"),
          na.action=na.exclude)


# Plotting a graph of predictive probability
{
data2 <- with(keystage_pred, data.frame(QUINTILE= rep(seq(from = 1, to= 5,
                                                          length.out=4260),2),
                                        SCHOOL_YEAR=mean(SCHOOL_YEAR,na.rm=TRUE),
                                        DRUG_NAME=mean(DRUG_NAME,na.rm=TRUE),
                                        GNDR_CD=factor(rep(1:2,each=4260))))

# predicting the value of Quintile as per the model.

data3 <- cbind(data2,predict(K2_predict,data2,type="link",se=TRUE))

data3$CSI <- (predict(K2_predict,data2,type="response"))

#Predicted probability.

data3 <-within(data3,{
  PredictedProb <-plogis(fit)
})

# determining the confidence interval

data3 <- within(data3,{PredictedProb<-plogis(fit)
LL<-plogis(fit-(1.96*se.fit))
UL<- plogis(fit+(1.96*se.fit))
})
# plotting graph with CI
ggplot(data=new_1,aes(x=QUINTILE,y=n))+
geom_point(mapping=aes(colour=GNDR_CD),size=1)+
scale_y_continuous(labels=scales::percent)
ggplot(data=Keystage2, aes(x=GNDR_CD,fill=CSI1))+ facet_wrap(QUINTILE~., ncol=5)+
geom_bar(position = "fill") + 
theme_classic() +
labs(y="Percentage-Pass/Fail",x="Gender",title=" Epileptic Keystage2- Pass Percentage")+
scale_x_discrete(limit=c("1","2"),labels=c("Male","Female"))+
scale_y_continuous(labels=scales::percent)+
scale_fill_discrete(labels=c("Fail","Pass"))
ggplot(data=new_1,aes(x=QUINTILE))+
  geom_line(mapping=aes(fill=..count..),stat="bin",binwidth=2)
  scale_y_continuous(labels=scales::percent)
ggplot(data=new_1,aes(x=QUINTILE,y=CSI1))+
  geom_line(mapping=aes(colour=GNDR_CD),size=1)+
  geom_ribbon(mapping=aes(ymin=LL,ymax=UL,fill=GNDR_CD),alpha=.2)
qunit1<-Keystage2 %>% filter(QUINTILE==1)
x1 <- NROW(qunit1)
CS11 <-qunit1%>% filter(CSI1=='Y')
y1<-NROW(CS11)
percen1 <- y1/x1*100
qunit2<-Keystage2 %>% filter(QUINTILE==2)
x2 <- NROW(qunit2)
CS12 <-qunit2%>% filter(CSI1=='Y')
y2<-NROW(CS12)
percen2 <- y2/x2*100
qunit3<-Keystage2 %>% filter(QUINTILE==3)
x3 <- NROW(qunit3)
CS13 <-qunit3%>% filter(CSI1=='Y')
y3<-NROW(CS13)
percen3 <- y3/x3*100
qunit4<-Keystage2 %>% filter(QUINTILE==4)
x4 <- NROW(qunit4)
CS14 <-qunit4%>% filter(CSI1=='Y')
y4<-NROW(CS14)
percen4 <- y4/x4*100
percen4[[1]]
qunit5<-Keystage2 %>% filter(QUINTILE==5)
x5 <- NROW(qunit5)
CS15 <-qunit5%>% filter(CSI1=='Y')
y5<-NROW(CS15)
percen5 <- y5/x5*100

percen5[[1]]
new_t <- Keystage2 %>%
count(QUINTILE,CSI1)%>%
mutate(per=prop.table(n))
str(Keystage2)
 }
################### Function-Plots #############

plot_res_stage <- function(stage_num) {
 
#1) Plot Quintile vs Pass % 
  
  ##get pass count for each Quintile
  K2_Quintile <- stage_num%>% group_by(QUINTILE,CSI1)%>%
    dplyr::summarise(pass_count=n())%>%filter(CSI1=="1")
  ##get total count per Quintile
  K2_Quintile_1<- stage_num%>% group_by(QUINTILE)%>% dplyr::summarise(total=n())
  ##calculate proportion
  K2_Quintile$Pass_Percent <- K2_Quintile$pass_count/K2_Quintile_1$total
  ##plot Quintile vs Pass % for both genders
  print(ggplot(data=K2_Quintile,aes(x=QUINTILE,y=Pass_Percent,group=1))+
          geom_line(aes(group=1),colour="#9933FF")+geom_point(aes(group=1),colour="#9933FF")+
          labs(title="Keystage2- Quintile vs Pass Percentage")+
          scale_y_continuous(labels=scales::percent)+
          theme(legend.position="none",axis.text=element_text(face="bold"))
)  

#2) Plot Quintile vs Pass % 
  
##get pass count for each Quintile
K2_Quintile <- stage_num%>% group_by(GNDR_CD,QUINTILE,CSI1)%>%
  dplyr::summarise(pass_count=n())%>%filter(CSI1=="1")
##get total count per Quintile
K2_Quintile_1<- stage_num%>% group_by(GNDR_CD,QUINTILE)%>% dplyr::summarise(total=n())
##calculate proportion
K2_Quintile$Pass_Percent <- K2_Quintile$pass_count/K2_Quintile_1$total
##plot Quintile vs Pass % for both genders
print(ggplot(data=K2_Quintile,aes(x=QUINTILE,y=Pass_Percent,colour=as.factor(GNDR_CD),group=GNDR_CD))+
  geom_line()+geom_point()+
  labs(title="Keystage2- Quintile vs Pass Percentage")+
  scale_y_continuous(labels=scales::percent)+
  scale_colour_discrete(name="Gender",labels=c("Male","Female"))+
  theme(axis.text=element_text(face="bold")))

#2) Bar Plot- Pass ratio with respect to gender
K2_Gender <- stage_num%>% group_by(GNDR_CD,CSI1)%>%
  dplyr::summarise(pass_count=n())%>%filter(CSI1=="1")
##get total count per Quintile
K2_Gender_1<- stage_num%>% group_by(GNDR_CD)%>% dplyr::summarise(total=n())
##calculate proportion
K2_Gender$Pass_Percent <- K2_Gender$pass_count/K2_Gender_1$total

print(ggplot(data=K2_Gender, aes(x=GNDR_CD,y=Pass_Percent))+
        geom_bar(stat='identity',width=0.3,fill="#9933FF") + 
        theme_classic() +
        labs(x="Gender",title="Keystage2- Gender vs Pass Percentage")+
        scale_x_discrete(limit=c("1","2"),labels=c("Male","Female"))+
        scale_y_continuous(labels=scales::percent)+
        scale_fill_discrete(labels=c("Fail","Pass"))+
        theme(legend.position="none",axis.text=element_text(face="bold")))


#3) Plot School_Year vs Pass %
##get pass count for each School_Year
K2_Schoolyear <- stage_num%>% group_by(SCHOOL_YEAR,CSI1)%>%
  dplyr::summarise(pass_count=n())%>%filter(CSI1=="1")
##get total count for School_Year
K2_Schoolyear_1<- stage_num%>% group_by(SCHOOL_YEAR)%>% dplyr::summarise(total=n())
##calculate proportion of pass
K2_Schoolyear$Pass_Percent <- K2_Schoolyear$pass_count/K2_Schoolyear_1$total
##plot School_Year vs Pass %
print(ggplot(data=K2_Schoolyear,aes(x=SCHOOL_YEAR,y=Pass_Percent,group=1,colour="green"))+
        geom_line(aes(group=1),colour="#9933FF")+geom_point(aes(group=1),colour="#9933FF")+
        labs(title="Keystage2- School_Year vs Pass Percentage")+
        scale_y_continuous(labels=scales::percent)+
        theme(legend.position="none",axis.text=element_text(face="bold")))

#3) Plot School_Year vs Pass %
##get pass count for each School_Year
K2_Schoolyear <- stage_num%>% group_by(SCHOOL_YEAR,QUINTILE,CSI1,.drop=FALSE)%>%
  dplyr::summarise(pass_count=n())%>%filter(CSI1=="1")
##get total count for School_Year
K2_Schoolyear_1<- stage_num%>% group_by(SCHOOL_YEAR,QUINTILE,.drop=FALSE)%>% dplyr::summarise(total=n())
##calculate proportion of pass
K2_Schoolyear$Pass_Percent <- K2_Schoolyear$pass_count/K2_Schoolyear_1$total
##plot School_Year vs Pass %
print(ggplot(data=K2_Schoolyear,aes(x=SCHOOL_YEAR,y=Pass_Percent,group=QUINTILE,colour=QUINTILE))+
  geom_line()+geom_point()+labs(title="Keystage2- School_Year vs Pass Percentage")+
  scale_y_continuous(labels=scales::percent)+
    theme(axis.text=element_text(face="bold")))

#4)Drug name 
##get pass count for each School_Year
K2_Drug_Name <- stage_num%>% group_by(DRUG_NAME,CSI1,.drop=FALSE)%>%
  dplyr::summarise(pass_count=n())%>%filter(CSI1=="1")
##get total count for School_Year
K2_Drug_Name_1<- stage_num%>% group_by(DRUG_NAME,.drop=FALSE)%>% dplyr::summarise(total=n())
##calculate proportion of pass
K2_Drug_Name$Pass_Percent <- K2_Drug_Name$pass_count/K2_Drug_Name_1$total
##plot School_Year vs Pass %
print(ggplot(data=K2_Drug_Name,aes(y=reorder(DRUG_NAME,-Pass_Percent),x=Pass_Percent))+
  geom_bar(stat='identity',width=0.5,fill="#9933FF")+labs(y="DRUG_CODE",title="Keystage2- Drug_Name vs Pass Percentage")+
  scale_x_continuous(labels=scales::percent)+
  theme(legend.position="none",axis.text=element_text(face="bold")))

} 

plot_res_stage(Keystage2)
plot_res_stage(Keystage3)
plot_res_stage(Keystage4)

 print(ggplot(data=K2_Drug_Name,aes(x=DRUG_NAME,y=Pass_Percent,group=1))+
        geom_line()+geom_point()+
        scale_y_continuous(labels=scales::percent)+
        theme(legend.position="none",axis.text=element_text(face="bold")))



SK2_Drug_Name <- Keystage2%>% group_by(DRUG_NAME,CSI1,.drop=FALSE)%>%
  dplyr::summarise(pass_count=n())%>%filter(CSI1=="Y")
##get total count for School_Year
K2_Drug_Name_1<- Keystage2%>% group_by(DRUG_NAME,.drop =FALSE )%>% dplyr::summarise(total=n())
##calculate proportion of pass
K2_Drug_Name$Pass_Percent <- K2_Drug_Name$pass_count/K2_Drug_Name_1$total

##plot School_Year vs Pass %
ggplot(data=K2_Drug_Name,aes(x=Pass_Percent,y=DRUG_NAME))+
        geom_point()+
        scale_x_continuous(labels=scales::percent)

ggplot(data=Keystage2, aes(y=DRUG_NAME,fill=CSI1))+
  geom_bar(width=0.5,position = "fill") 

ggplot(data=new_1,aes(x=QUINTILE,y=percent))+
  geom_line(mapping=aes(colour=GNDR_CD),size=1)
  geom_ribbon(mapping=aes(ymin=LL,ymax=UL,fill=GNDR_CD),alpha=.2) 

ggplot(data=new_1,aes(x=QUINTILE,y=percent,group=1))+
geom_point(stat='summary')+stat_summary(geom="line")+
scale_y_continuous(labels=scales::percent)



Keystage2_pass_perc <- data.frame(QUINTILE=c(1,2,3,4,5),
                                  pass_perc=c(percen1[[1]],percen2[[1]],
                    percen3[[1]],percen4[[1]],percen5[[1]]))


# plotting graph with CI
ggplot(data=Keystage2_pass_perc,aes(x=QUINTILE,y=pass_perc))+
  geom_line()
  




 # Logistic regression for only children with keystage 3 having epilepsy .


Keystage3 <- sqlQuery(channel, "select CSI1,Quintile,School_year,GNDR_CD,dRUG_NAME from  Sailw0895v.epi_Keysatge_3")
summary(Keystage3)
str(Keystage3)


Keystage3 <- Keystage3%>% mutate(DRUG_CODE=ifelse(DRUG_NAME=="LOMOTRIGINE",26,
                          ifelse(DRUG_NAME=="LEVETRICETAM",2,
                          ifelse(DRUG_NAME=="VALPROIC ACID",3,
                          ifelse(DRUG_NAME=="ARBIL",4,
                          ifelse(DRUG_NAME=="TOPIRAMATE",5,
                          ifelse(DRUG_NAME=="SODIUM VALPROATE",6,
                          ifelse(DRUG_NAME=="ZONISAMIDE",7,
                          ifelse(DRUG_NAME=="ETHOSUXIMIDE",8,
                          ifelse(DRUG_NAME=="PENTRAN",9,
                          ifelse(DRUG_NAME=="RUFINAMIDE",10,
                          ifelse(DRUG_NAME=="LACOSAMIDE",11,
                          ifelse(DRUG_NAME=="VIGABATRIN",12,
                          ifelse(DRUG_NAME=="GABAPENTINE",13,
                          ifelse(DRUG_NAME=="PHENYTOIN",14,
                          ifelse(DRUG_NAME=="OXCARBAZEPINE",15,
                          ifelse(DRUG_NAME=="DIAZEPAM",16,
                          ifelse(DRUG_NAME=="PHENOBARBITAL",17,
                          ifelse(DRUG_NAME=="PIRACETAM",18,
                          ifelse(DRUG_NAME=="ESLICARBAZEPINE_ACETATE",19,
                          ifelse(DRUG_NAME=="GABITRIL",20,
                          ifelse(DRUG_NAME=="RETIGABINE",21,
                          ifelse(DRUG_NAME=="PREGABALINE",22,
                          ifelse(DRUG_NAME=="STIRIPENTOL",23,
                          ifelse(DRUG_NAME=="EMESIDE",24,
                          ifelse(DRUG_NAME=="PRIMIDONE",25,
                          ifelse(DRUG_NAME=="NO_DRUG",1,NA)))))))))))))))))))))))))))                                                                                    



Keystage3$DRUG_CODE <- as.factor(Keystage3$DRUG_CODE)

Keystage3 <- Keystage3 %>% mutate(CSI1=ifelse(CSI1=="Y",1,0))

Keystage3$SCHOOL_YEAR <- as.factor(Keystage3$SCHOOL_YEAR)

Keystage3$GNDR_CD <- as.factor(Keystage3$GNDR_CD)

Keystage3$CSI1 <- as.factor(Keystage3$CSI1)

Keystage3$QUINTILE <- as.factor(Keystage3$QUINTILE)

str(Keystage3)

K3 <- glm(CSI1~ QUINTILE + SCHOOL_YEAR +GNDR_CD+DRUG_CODE , data=Keystage3, family=binomial)
summary(K3)

# Logistic regression for only children with keystage 4 having epilepsy .


Keystage4 <- sqlQuery(channel, "select  CSI1,Keystage,Quintile,School_year,drug_nAME,GNDR_CD from  Sailw0895v.epi_Keysatge_4")

Keystage4 <- Keystage4%>% mutate(DRUG_CODE=ifelse(DRUG_NAME=="LOMOTRIGINE",26,
                                ifelse(DRUG_NAME=="LEVETRICETAM",2,
                                ifelse(DRUG_NAME=="VALPROIC ACID",3,
                                ifelse(DRUG_NAME=="ARBIL",4,
                                ifelse(DRUG_NAME=="TOPIRAMATE",5,
                                ifelse(DRUG_NAME=="SODIUM VALPROATE",6,
                                ifelse(DRUG_NAME=="ZONISAMIDE",7,
                                ifelse(DRUG_NAME=="ETHOSUXIMIDE",8,
                                ifelse(DRUG_NAME=="PENTRAN",9,
                                ifelse(DRUG_NAME=="RUFINAMIDE",10,
                                ifelse(DRUG_NAME=="LACOSAMIDE",11,
                                ifelse(DRUG_NAME=="GABAPENTINE",13,
                                ifelse(DRUG_NAME=="PHENYTOIN",14,
                                ifelse(DRUG_NAME=="OXCARBAZEPINE",15,
                                ifelse(DRUG_NAME=="DIAZEPAM",16,
                                ifelse(DRUG_NAME=="PHENOBARBITAL",17,
                                ifelse(DRUG_NAME=="PIRACETAM",18,
                                ifelse(DRUG_NAME=="ESLICARBAZEPINE_ACETATE",19,
                                ifelse(DRUG_NAME=="GABITRIL",20,
                                ifelse(DRUG_NAME=="RETIGABINE",21,
                                ifelse(DRUG_NAME=="PREGABALINE",22,
                                ifelse(DRUG_NAME=="STIRIPENTOL",23,
                                ifelse(DRUG_NAME=="EMESIDE",24,
                                ifelse(DRUG_NAME=="PRIMIDONE",25,
                                ifelse(DRUG_NAME=="NO_DRUG",1,NA))))))))))))))))))))))))))                                                                                  



Keystage4$DRUG_NAME <- as.factor(Keystage4$DRUG_NAME)                                                                                                                                          

Keystage4 <- Keystage4 %>% mutate(CSI1=ifelse(CSI1=="Y",1,0))

Keystage4$SCHOOL_YEAR <- as.factor(Keystage4$SCHOOL_YEAR)

Keystage4$GNDR_CD <- as.factor(Keystage4$GNDR_CD)

Keystage4$CSI1 <- as.factor(Keystage4$CSI1)

Keystage4$QUINTILE <- as.factor(Keystage4$QUINTILE)

str(Keystage4)

K4 <- glm(CSI1~ QUINTILE + SCHOOL_YEAR +GNDR_CD+DRUG_NAME , data=Keystage4, family=binomial)
summary(K4)






#Glm for case control with combined keystage, not to be used for analysis.

result_logis <- sqlQuery(channel, "select * FROM Sailw0895v.combined_group ")
summary(result_logis)


str(result_logis)


result_logis <- result_logis %>% mutate(CSI1=ifelse(CSI1=="Y",1,0))
result_logis <- result_logis %>% mutate(STATUS=ifelse(STATUS=="EPILEPSY",1,0))


result_logis$CSI1 <- as.factor(result_logis$CSI1)
result_logis$STATUS <- as.factor(result_logis$STATUS)
result_logis$KEYSTAGE <- as.factor(result_logis$KEYSTAGE)
result_logis$QUINTILE <- as.factor(result_logis$QUINTILE)
result_logis$SCHOOL_YEAR <- as.factor(result_logis$SCHOOL_YEAR)

str(result_logis)

result <- glm(CSI1 ~ STATUS+ QUINTILE + SCHOOL_YEAR +GNDR_CD, data=result_logis, family=binomial)
summary(result)
ggplot(result_logis, aes(fill=CSI1 , x=STATUS)) + geom_bar()



# case_ control with  keystage 2 glm.
Case_control_key2 <- sqlQuery(channel, "select * FROM Sailw0895v.control_case_Key2 ")
summary(Case_control_key2)

str(Case_control_key2)

Case_control_key2 <- Case_control_key2 %>% mutate(CSI1=ifelse(CSI1=="Y",1,0))
Case_control_key2<- Case_control_key2 %>% mutate(STATUS=ifelse(STATUS=="EPILEPSY",1,0))

Case_control_key2$CSI1 <- as.factor(Case_control_key2$CSI1)
Case_control_key2$STATUS <- as.factor(Case_control_key2$STATUS)
Case_control_key2$KEYSTAGE <- as.factor(Case_control_key2$KEYSTAGE)
Case_control_key2$QUINTILE <- as.factor(Case_control_key2$QUINTILE)
Case_control_key2$SCHOOL_YEAR <- as.factor(Case_control_key2$SCHOOL_YEAR)

result_KS2 <- glm(CSI1 ~ STATUS+ QUINTILE + SCHOOL_YEAR +GNDR_CD, data=
                Case_control_key2, family=binomial)
summary(result_KS2)

# case_ control with  keystage 3 glm
Case_control_key3 <- sqlQuery(channel, "select * FROM Sailw0895v.control_case_Key3 ")
summary(Case_control_key3)

str(Case_control_key3)

Case_control_key3 <- Case_control_key3 %>% mutate(CSI1=ifelse(CSI1=="Y",1,0))
Case_control_key3<- Case_control_key3 %>% mutate(STATUS=ifelse(STATUS=="EPILEPSY",1,0))

Case_control_key3$CSI1 <- as.factor(Case_control_key3$CSI1)
Case_control_key3$STATUS <- as.factor(Case_control_key3$STATUS)
Case_control_key3$KEYSTAGE <- as.factor(Case_control_key3$KEYSTAGE)
Case_control_key3$QUINTILE <- as.factor(Case_control_key3$QUINTILE)
Case_control_key3$SCHOOL_YEAR <- as.factor(Case_control_key3$SCHOOL_YEAR)

result_KS3 <- glm(CSI1 ~ STATUS+ QUINTILE + SCHOOL_YEAR +GNDR_CD, data=
                    Case_control_key3, family=binomial)
summary(result_KS3)


# case_ control with  keystage 4 glm
Case_control_key4 <- sqlQuery(channel, "select * FROM Sailw0895v.control_case_Key4 ")
summary(Case_control_key4)

str(Case_control_key4)

Case_control_key4 <- Case_control_key4 %>% mutate(CSI1=ifelse(CSI1=="Y",1,0))
Case_control_key4<- Case_control_key4 %>% mutate(STATUS=ifelse(STATUS=="EPILEPSY",1,0))

Case_control_key4$CSI1 <- as.factor(Case_control_key4$CSI1)
Case_control_key4$STATUS <- as.factor(Case_control_key4$STATUS)
Case_control_key4$KEYSTAGE <- as.factor(Case_control_key4$KEYSTAGE)
Case_control_key4$QUINTILE <- as.factor(Case_control_key4$QUINTILE)
Case_control_key4$SCHOOL_YEAR <- as.factor(Case_control_key4$SCHOOL_YEAR)

result_KS4 <- glm(CSI1 ~ STATUS+ QUINTILE + SCHOOL_YEAR +GNDR_CD, data=
                    Case_control_key4, family=binomial)
summary(result_KS4)


plot_case_stage <- function(stage_num) {
#1) Plot Quintile vs Pass % 
  ##get pass count for each Quintile
  K2_Quintile <- stage_num%>% group_by(STATUS,QUINTILE,CSI1)%>%
    dplyr::summarise(pass_count=n())%>%filter(CSI1=="1")
  ##get total count per Quintile
  K2_Quintile_1<- stage_num%>% group_by(STATUS,QUINTILE)%>% dplyr::summarise(total=n())
  ##calculate proportion
  K2_Quintile$Pass_Percent <- K2_Quintile$pass_count/K2_Quintile_1$total
  ##plot Quintile vs Pass % for
  print(ggplot(data=K2_Quintile,aes(x=QUINTILE,y=Pass_Percent,colour=as.factor(STATUS),group=STATUS))+
          geom_line()+geom_point()+
          labs(title="Keystage4 Case control- Quintile vs Pass Percentage")+
          scale_y_continuous(labels=scales::percent)+
          scale_colour_discrete(name="Epilepsy",labels=c("No","Yes")))
  
#2) Bar Plot- Pass ratio with respect to gender
  K2_Status <- stage_num%>% group_by(GNDR_CD,STATUS,CSI1,.drop=FALSE)%>%
    dplyr::summarise(pass_count=n())%>%filter(CSI1=="1")
  ##get total count per Quintile
  K2_Status_1<- stage_num%>% group_by(GNDR_CD,STATUS,.drop=FALSE)%>% dplyr::summarise(total=n())
  ##calculate proportion
  K2_Status$Pass_Percent <- K2_Status$pass_count/K2_Status_1$total
  
  print(ggplot(data=K2_Status, aes(x=GNDR_CD,y=Pass_Percent,fill=STATUS))+
          geom_bar(stat='identity',width=0.3,position="fill") + 
          theme_classic() +
          labs(y="Percentage-Pass",x="Gender",title="Keystage4 Case control-Gender vs Pass Percentage")+
          scale_x_discrete(limit=c("1","2"),labels=c("Male","Female"))+
          scale_y_continuous(labels=scales::percent)+
          scale_fill_discrete(name="Epilepsy",labels=c("No","Yes")))
          #scale_colour_discrete(name="Epilepsy",labels=c("No","Yes")))
          #theme(legend.position="none",axis.text=element_text(face="bold")))

  
#3) Plot Quintile vs Pass % 
  ##get pass count for each Quintile
  K2_Schoolyear <- stage_num%>% group_by(STATUS,SCHOOL_YEAR,CSI1,.drop=FALSE)%>%
    dplyr::summarise(pass_count=n())%>%filter(CSI1=="1")
  ##get total count per Quintile
  K2_Schoolyear_1<- stage_num%>% group_by(STATUS,SCHOOL_YEAR,.drop=FALSE)%>% dplyr::summarise(total=n())
  ##calculate proportion
  K2_Schoolyear$Pass_Percent <- K2_Schoolyear$pass_count/K2_Schoolyear_1$total
  ##plot Quintile vs Pass % for
  print(ggplot(data=K2_Schoolyear,aes(x=SCHOOL_YEAR,y=Pass_Percent,colour=as.factor(STATUS),group=STATUS))+
          geom_line()+geom_point()+
        labs(title="Keystage Case control- School year vs Pass Percentage")+
          scale_y_continuous(labels=scales::percent)+ 
          scale_colour_discrete(name="Epilepsy",labels=c("No","Yes")))
  
#pie(K2_Status$Pass_Percent,labels=c("Non-epilepsy","Epilepsy"),col=c("purple",""))
  
} 

plot_case_stage(Case_control_key2)
plot_case_stage(Case_control_key3)
plot_case_stage(Case_control_key4)

library(RColor)

Case_control_key3 <-

  
  
  K2_Status <- Case_control_key2%>% group_by(STATUS,CSI1,.drop=FALSE)%>%
  dplyr::summarise(pass_count=n())%>%filter(CSI1=="1")
##get total count per Quintile
K2_Status_1<- Case_control_key2%>% group_by(STATUS,.drop=FALSE)%>% dplyr::summarise(total=n())
##calculate proportion
K2_Status$Pass_Percent <- K2_Status$pass_count/K2_Status_1$total  
  
K3_Quintile <- Case_control_key4%>% group_by(CSI1,QUINTILE,STATUS)%>%
  dplyr::summarise(pass_count=n())%>%filter(CSI1=="1")
##get total count per Quintile
K3_Quintile_1<- Case_control_key4%>% group_by(QUINTILE,STATUS)%>% dplyr::summarise(total=n())
##calculate proportion
K3_Quintile$total <- K3_Quintile_1$total
K3_Quintile$Pass_Percent <- K3_Quintile$pass_count/K3_Quintile_1$total
##plot Quintile vs Pass % for both genders
print(ggplot(data=K3_Quintile,aes(x=QUINTILE,y=Pass_Percent,colour=as.factor(STATUS),group=STATUS))+
        geom_line()+
        labs(title="Epileptic - Non Epileptic comparision")+
        scale_y_continuous(labels=scales::percent)+
        scale_colour_discrete(name="Epileptic"))


#3) Plot School_Year vs Pass %
##get pass count for each School_Year
K2_Schoolyear <- stage_num%>% group_by(SCHOOL_YEAR,QUINTILE,CSI1)%>%
  dplyr::summarise(pass_count=n())%>%filter(CSI1=="1")
##get total count for School_Year
K2_Schoolyear_1<- stage_num%>% group_by(SCHOOL_YEAR,QUINTILE)%>% dplyr::summarise(total=n())
##calculate proportion of pass
K2_Schoolyear$Pass_Percent <- K2_Schoolyear$pass_count/K2_Schoolyear_1$total
##plot School_Year vs Pass %
print(ggplot(data=K2_Schoolyear,aes(x=SCHOOL_YEAR,y=Pass_Percent,group=QUINTILE,colour=QUINTILE))+
        geom_line()+geom_point()+labs(title=" Epileptic- School_Year vs Pass Percentage")+
        scale_y_continuous(labels=scales::percent))

##get pass count for each School_Year
K2_Drug_Name <- stage_num%>% group_by(DRUG_NAME,CSI1,.drop=FALSE)%>%
  dplyr::summarise(pass_count=n())%>%filter(CSI1=="1")
##get total count for School_Year
K2_Drug_Name_1<- stage_num%>% group_by(DRUG_NAME,.drop=FALSE)%>% dplyr::summarise(total=n())
##calculate proportion of pass
K2_Drug_Name$Pass_Percent <- K2_Drug_Name$pass_count/K2_Drug_Name_1$total

##plot School_Year vs Pass %
print(ggplot(data=K2_Drug_Name,aes(x=DRUG_NAME,y=Pass_Percent,group=1))+
        geom_line()+geom_point()+
        scale_y_continuous(labels=scales::percent))


# plotting predicted probabilty grpah.
#######nn



plot_case_stage <- function(stage_num) {
  #1) Plot Quintile vs Pass % 
  ##get pass count for each Quintile
  K2_Quintile <- stage_num%>% group_by(STATUS,QUINTILE,CSI1)%>%
    dplyr::summarise(pass_count=n())%>%filter(CSI1=="1")
  ##get total count per Quintile
  K2_Quintile_1<- stage_num%>% group_by(STATUS,QUINTILE)%>% dplyr::summarise(total=n())
  ##calculate proportion
  K2_Quintile$Pass_Percent <- K2_Quintile$pass_count/K2_Quintile_1$total
  ##plot Quintile vs Pass % for
  print(ggplot(data=K2_Quintile,aes(x=QUINTILE,y=Pass_Percent,colour=as.factor(STATUS),group=STATUS))+
          geom_line()+geom_point()+
          labs(title=" Case control- Quintile")+
          scale_y_continuous(labels=scales::percent)+
          scale_colour_discrete(name="Epileptic",labels=c("No","Yes")))
  
  #2) Bar Plot- Pass ratio with respect to gender
  K2_Status <- stage_num%>% group_by(STATUS,CSI1,.drop=FALSE)%>%
    dplyr::summarise(pass_count=n())%>%filter(CSI1=="1")
  ##get total count per Quintile
  K2_Status_1<- stage_num%>% group_by(STATUS,.drop=FALSE)%>% dplyr::summarise(total=n())
  ##calculate proportion
  K2_Status$Pass_Percent <- K2_Status$pass_count/K2_Status_1$total
  
  print(ggplot(data=K2_Status, aes(x=STATUS,y=Pass_Percent,fill="red"))+
          geom_bar(stat='identity',width=0.3) + 
          theme_classic() +
          labs(y="Percentage-Pass",x="Epileptic",title="Case control")+
          scale_x_discrete(limit=c("0","1"),labels=c("No","Yes"))+
          scale_y_continuous(labels=scales::percent)+
          theme(legend.position="none",axis.text=element_text(face="bold")))
  
  
  #3) Plot Quintile vs Pass % 
  ##get pass count for each Quintile
  K2_Schoolyear <- stage_num%>% group_by(STATUS,SCHOOL_YEAR,CSI1,.drop=FALSE)%>%
    dplyr::summarise(pass_count=n())%>%filter(CSI1=="1")
  ##get total count per Quintile
  K2_Schoolyear_1<- stage_num%>% group_by(STATUS,SCHOOL_YEAR,.drop=FALSE)%>% dplyr::summarise(total=n())
  ##calculate proportion
  K2_Schoolyear$Pass_Percent <- K2_Schoolyear$pass_count/K2_Schoolyear_1$total
  ##plot Quintile vs Pass % for
  print(ggplot(data=K2_Schoolyear,aes(x=SCHOOL_YEAR,y=Pass_Percent,colour=as.factor(STATUS),group=STATUS))+
          geom_line()+geom_point()+
          labs(title=" Case control- School year")+
          scale_y_continuous(labels=scales::percent)+
          scale_colour_discrete(name="Epileptic",labels=c("No","Yes")))
  
  


