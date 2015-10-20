rm(list=ls())
setwd("/Users/ben/Statistics/CTP II")

library(data.table)
library(dplyr)
library(glmnet) 
library(splines)
library(xlsx)
library(operator.tools)
library(stringr)   

set1<-fread("./data/4 analysis/model_data"
            ,sep=","
            ,header=T 
            #,nrows=10000
            ,colClasses="character")

set1<-set1[set1$day != 31,]

set1[]<-lapply(set1,as.factor) 
set1[,click:=as.numeric(click)-1]
names(set1)
ids<-select(set1,id)
set1<-select(set1,-train,-id,-(starts_with("ic")))
set1<-select(set1,-train,-id)
   
#foldids=as.numeric(as.factor(set1$day))


trn<-!(set1$day %in% c(30,31))
tst<-set1$day == 30

x<-sparse.model.matrix(click~.,set1)[,-1]
y<-as.matrix(set1$click)

cv.lasso<-cv.glmnet(x[trn,]
                    ,y[trn,]
                    ,alpha=1
                    ,family="binomial"
                    #,foldid = foldids[trn]
                    )
coef(cv.lasso)
plot(cv.lasso)
  

pred_comp<-as.data.frame(matrix(nrow=100,ncol=2))
for (i in 1:100){
j=2*i/(i+20)
preds=as.numeric(predict(cv.lasso, newx=x[tst,], s=cv.lasso$lambda.min*j, type = "response" )) 
actual<-as.numeric(set1$click[tst])
  
  llfun <- function(actual, prediction) {
      epsilon <- .000000000000001
      yhat <- pmin(pmax(prediction, epsilon), 1-epsilon)
      logloss <- -mean(actual*log(yhat)
                       + (1-actual)*log(1 - yhat))
      return(logloss)
  } 
  llfun(actual,preds)
  #response loss 
  pred_comp[i,1]<-cv.lasso$lambda.min*j
  pred_comp[i,2]<-llfun(actual,preds)
}
plot(pred_comp$V1,pred_comp$V2)

min(pred_comp$V2)
 

 
###############################################
############################################
###############################################
#predict on submission set 
test1<-filter(test,a18==pppp)
ids<-test1$id
test1<-transmute(test1
                 ,click
                 ,day=as.numeric(day)
                 ,hour=as.numeric(hour)
                 ,a1=as.factor(a1)
                 ,a2=as.factor(a2)
                 ,a3=as.factor(a3)
                 ,a4=as.factor(a4)
                 ,a5=as.factor(a5)
                 ,a6=as.factor(a6)
                 ,a7=as.factor(a7)
                 ,a8=as.factor(a8)
                 ,a9=as.factor(a9)     
                 ,a10=as.factor(a10)
                 ,a11=as.factor(a11)
                 ,a12=as.factor(a12) 
                 ,a13=as.factor(a13) 
                 ,a14=as.factor(a14) 
                 ,a15=as.factor(a15)
                 ,a16=as.factor(a16)
                 ,a17=as.factor(a17) 
                 #,a18=as.factor(a18) 
                 ,a19=as.factor(a19) 
                 ,a20=as.factor(a20) 
                 ,a21=as.factor(a21)                
                 ,bb1=as.factor(paste(hour,a1,sep=""))
                 ,bb2=as.factor(paste(hour,a12,sep=""))      
                 
                 ,b1=as.factor(paste(a1,a2,sep=""))
                 
                 ,c1=as.factor(paste(a1,a6,sep=""))
                 ,c2=as.factor(paste(a2,a6,sep=""))
                 
                 ,d1=as.factor(paste(a1,a7,sep=""))
                 ,d2=as.factor(paste(a2,a7,sep=""))
                 ,d3=as.factor(paste(a6,a7,sep=""))
                 
                 ,e1=as.factor(paste(a1,a8,sep=""))
                 ,e2=as.factor(paste(a2,a8,sep=""))
                 ,e3=as.factor(paste(a6,a8,sep=""))
                 ,e4=as.factor(paste(a7,a8,sep=""))
                 
                 ,f1=as.factor(paste(a1,a9,sep=""))
                 ,f2=as.factor(paste(a2,a9,sep=""))
                 ,f3=as.factor(paste(a6,a9,sep=""))
                 ,f4=as.factor(paste(a7,a9,sep=""))
                 ,f5=as.factor(paste(a8,a9,sep=""))
                 
                 ,g1=as.factor(paste(a1,a11,sep=""))
                 ,g2=as.factor(paste(a2,a11,sep=""))
                 ,g3=as.factor(paste(a6,a11,sep=""))
                 ,g4=as.factor(paste(a7,a11,sep=""))
                 ,g5=as.factor(paste(a8,a11,sep=""))
                 ,g6=as.factor(paste(a9,a11,sep=""))
                 
                 ,h1=as.factor(paste(a1,a12,sep=""))
                 ,h2=as.factor(paste(a2,a12,sep=""))
                 ,h3=as.factor(paste(a6,a12,sep=""))
                 ,h4=as.factor(paste(a7,a12,sep=""))
                 ,h5=as.factor(paste(a8,a12,sep=""))
                 ,h6=as.factor(paste(a9,a12,sep=""))
                 ,h7=as.factor(paste(a11,a12,sep=""))
                 
                 
                 ,z1=as.factor(paste(a1,a19,sep=""))
                 ,z2=as.factor(paste(a1,a20,sep=""))
                 ,z3=as.factor(paste(a1,a21,sep=""))
                 ,z4=as.factor(paste(a13,a19,sep=""))
                 
)
                  

test1$click<-as.factor("s")
model<-rbind(train2,test1)
foldids=as.numeric(as.factor(model$day)) 
trn<-model$day!=31 
tst=(!trn)

x<-sparse.model.matrix(click~.+ns(hour,df=3),model)[,-1]
y<-as.matrix(model$click)

cv.lasso<-cv.glmnet(x[trn,],y[trn,],alpha=1,family="binomial",foldid = foldids[trn])

plot(cv.lasso)  
preds<-as.numeric(predict(cv.lasso,newx=x[tst,],s=cv.lasso$lambda.1se/k,type="response")) 

out<-data.table(cbind(as.character(ids),as.matrix(preds)))
names(out)<-c("id","click") 
out
list(out,k,ld)
}

n=151000
a<-f("0") 
a[[3]] 
b<-f("1")
b[[2]]
c<-f("2")
c[[2]]
d<-f("3")
d[[3]]

a1<-data.table(a[[1]])
a2<-data.table(b[[1]])
a3<-data.table(c[[1]])
a4<-data.table(d[[1]])

out<-rbind(a1,a2,a3,a4) 
setkey(out,id)
look<-distinct(out,id)
nrow(out)
date=Sys.Date()
write.csv(out,paste("sub_",date,".csv",sep=""),row.names=F,append=F,quote=F)
 
coef(cv.lasso)
################
endtime=Sys.time()
endtime-starttime
###############
 

