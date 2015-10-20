#Clickthru REcodes
rm(list=ls())
setwd("/Users/ben/Statistics/CTP II/data/1 original")

library(data.table)
library(dplyr) 
 
train<-fread("train"
             #,nrows=50
             ,header=T 
            , sep=","
             ,colClasses="character")
head(train)
 
i<-runif(nrow(train),1,40)

#give myself three 1-million row datasets
train1<-train[i<2,]
train2<-train[i<3 & i >2,]
train3<-train[i<4 & i >3,]


setwd("/Users/ben/Statistics/CTP II/data/2 training samples")
write.table(train1,"sample1",quote = F,sep=",",append = F,na = "",col.names = T,row.names = F)
write.table(train2,"sample2",quote = F,sep=",",append = F,na = "",col.names = T,row.names = F)
write.table(train3,"sample3",quote = F,sep=",",append = F,na = "",col.names = T,row.names = F)
