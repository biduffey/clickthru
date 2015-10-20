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

# combine training and test data so I only have to make modifications once
train<-fread("./data/3 impact coding/sample2_ic"
            ,sep=","
            ,header=T 
            ,colClasses="character" #import as character so it doesn't mess up the values
            #,nrows = 20000 
            #,drop="id" 
            )  
train$train<-1 

test<-fread("./data/3 impact coding/test_ic"
            ,sep=","
            ,header=T 
            ,colClasses="character"
            #,nrows = 5000
)  
#ids<-test[,id]
#test<-test[,!"id",with=FALSE]
click<-test[,click := 0]
setcolorder(test,c(24,1:23)) 
test$train<-0
names(test)
names(train)    

#combine train and test (helps keep things straight)
orig<-rbind(train,test)
trn<-orig$train==1
tst<-(!trn)
 
#sample
dat=orig[runif(nrow(orig),1,10)<=2 | orig$train == 0 ]
#dat$hour =as.numeric(dat$hour)
#dat[]<-lapply(dat,as.factor) 
 

#find variables with near-zero variance and get them out
#nzv <- nearZeroVar(dat,saveMetrics = T)
#nzv 
#c16 does not look like it contains much info
dat<-select(dat,-C16) 

#find linear dependencies
comboInfo <- findLinearCombos(dat[,1:ncol(dat)])
comboInfo

dat<-mutate(dat
              ,day = as.factor(substr(hour,5,6))
              ,hour = as.factor(substr(hour,7,8))) 
head(dat)

#try making one model with day as a category, while the other one has date as a fold

write.table(dat,"./data/4 analysis/model_data",append = F,sep = ",",na = "",row.names = F,col.names = T,quote = F)
