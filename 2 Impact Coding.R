#Do impact coding on subtrain1
#Clickthru REcodes
rm(list=ls())
setwd("/Users/ben/Statistics/CTP II")

library(data.table)
library(dplyr)
library(glmnet) 
library(xlsx)
library(operator.tools)
library(stringr) 
library(caret)

#training data
orig<-fread("./data/2 training samples/sample1" 
            ,header=T 
            ,colClasses="character") #import as character so it doesn't mess up the values

#subset to only the variables that have too many categories
set1<-
transmute(orig
        ,click = as.numeric(click)
        ,site_id
        ,site_domain
        ,app_id
        ,app_domain
        ,device_id
        ,device_ip
        ,device_model
        ,C14
        ,C17
        ,C19
        ,C20
        ,C21
       ,count=1)
head(set1)


#the next 12 lines or so gives each category a click probability
ic1<-set1[,list(ic1=round(sum(click)/sum(count),1)),by="site_id"] 
ic2<-set1[,list(ic2=round(sum(click)/sum(count),1)),by="site_domain"]
ic3<-set1[,list(ic3=round(sum(click)/sum(count),1)),by="app_id"]
ic4<-set1[,list(ic4=round(sum(click)/sum(count),1)),by="app_domain"]
ic5<-set1[,list(ic5=round(sum(click)/sum(count),1)),by="device_id"]
ic6<-set1[,list(ic6=round(sum(click)/sum(count),1)),by="device_ip"]
ic7<-set1[,list(ic7=round(sum(click)/sum(count),1)),by="device_model"]
ic8<-set1[,list(ic8=round(sum(click)/sum(count),1)),by="C14"]
ic9<-set1[,list(ic9=round(sum(click)/sum(count),1)),by="C17"]
ic10<-set1[,list(ic10=round(sum(click)/sum(count),1)),by="C19"]
ic11<-set1[,list(ic11=round(sum(click)/sum(count),1)),by="C20"]
ic12<-set1[,list(ic12=round(sum(click)/sum(count),1)),by="C21"]

#this piece actually writes each the raw, individual impact codes to the directory
setwd("/Users/ben/Statistics/CTP II/data/3 impact coding")
f<-function(x){
write.table(x,deparse(substitute(x)),sep=",",append = F,quote = F,na = "",row.names = F,col.names = T)
}
f(ic1);f(ic2);f(ic3);f(ic4);f(ic5);f(ic6);
f(ic7);f(ic8);f(ic9);f(ic10);f(ic11);f(ic12);

f2<-function(x){
fread(deparse(substitute(x)),sep=",",header=T,colClasses="character")
}
ic1<-f2(ic1) 
ic2<-f2(ic2)
ic3<-f2(ic3)
ic4<-f2(ic4)
ic5<-f2(ic5)
ic6<-f2(ic6)
ic7<-f2(ic7)
ic8<-f2(ic8)
ic9<-f2(ic9)
ic10<-f2(ic10)
ic11<-f2(ic11)
ic12<-f2(ic12) 



##############################
#here is where we actually combine the impact codes to TRAIN set 
#training data
setwd("/Users/ben/Statistics/CTP II")
origdos<-fread("./data/2 training samples/sample2"
               ,sep=","
            ,header=T 
            ,colClasses="character" ) #import as character so it doesn't mess up the values

f3<-function(x,y){
setkeyv(origdos,deparse(substitute(x))) 
setkeyv(y,deparse(substitute(x))) 
z<-merge(origdos,y,by=deparse(substitute(x)),all.x=T, all.y=F)
i<-which(colnames(z)==deparse(substitute(x)))
select(z,-i)
}

origdos<-f3(site_id,ic1)
origdos<-f3(site_domain,ic2)
origdos<-f3(app_id,ic3)
origdos<-f3(app_domain,ic4)
origdos<-f3(device_id,ic5)
origdos<-f3(device_ip,ic6)
origdos<-f3(device_model,ic7)
origdos<-f3(C14,ic8)
origdos<-f3(C17,ic9)
origdos<-f3(C19,ic10)
origdos<-f3(C20,ic11)
origdos<-f3(C21,ic12)

origdos[is.na(origdos)==T]<-"m"
 
write.table(origdos,"./data/3 impact coding/sample2_ic",sep=",",append = F,quote = F,na = "",row.names = F,col.names = T)



##############################
#add impact codes to TEST set
#import test data
origtres<-fread("./data/1 original/test"
               ,sep=","
               ,header=T 
               ,colClasses="character" ) #import as character so it doesn't mess up the values

f3<-function(x,y){
  setkeyv(origtres,deparse(substitute(x))) 
  setkeyv(y,deparse(substitute(x))) 
  z<-merge(origtres,y,by=deparse(substitute(x)),all.x=T, all.y=F)
  i<-which(colnames(z)==deparse(substitute(x)))
  select(z,-i)
}

origtres<-f3(site_id,ic1)
origtres<-f3(site_domain,ic2)
origtres<-f3(app_id,ic3)
origtres<-f3(app_domain,ic4)
origtres<-f3(device_id,ic5)
origtres<-f3(device_ip,ic6)
origtres<-f3(device_model,ic7)
origtres<-f3(C14,ic8)
origtres<-f3(C17,ic9)
origtres<-f3(C19,ic10)
origtres<-f3(C20,ic11)
origtres<-f3(C21,ic12)

origtres[is.na(origtres)==T]<-"m"

write.table(origtres,"./data/3 impact coding/test_ic",sep=",",append = F,quote = F,na = "",row.names = F,col.names = T)


  


