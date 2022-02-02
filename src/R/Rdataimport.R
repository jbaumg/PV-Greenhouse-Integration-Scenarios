library(tidyverse)
library(feather)
library(dplyr)
library(ggplot2)
library(lubridate)
library(arrow)
library(suncalc)
library(qmap)

rundats<-lapply(1:9,function(i)
{
  flslist<-list.files("path/demand/",pattern=paste("name",i-1,"run",sep=""))
  
  date<-read_feather(paste("path/demand/",flslist[1],sep=""))["date"]
  rundat<-sapply(1:length(flslist),function(j){
    rundat<-read_feather(paste("path/demand/",flslist[j],sep=""))["pred"]
   
    })
  rundat<-data.frame(date,rowMeans(data.frame(rundat)))
  names(rundat)<-c("date","pred")
  return(rundat)
}
  )

sites<-c("pvhopf","pvschneider")

rundatspv<-lapply(1:length(sites),function(i)
{
  flslist<-list.files("path/pv/",pattern=paste(sites[i],"run",sep=""))
  
  date<-arrow::read_feather(paste("path/pv/",flslist[1],sep=""))["date"]
  rundat<-sapply(1:length(flslist),function(j){
    rundat<-arrow::read_feather(paste("path/pv/",flslist[j],sep=""))["pred"]
    })
  rundat<-data.frame(date,rowMeans(data.frame(rundat)))
  names(rundat)<-c("date","pred")
  return(rundat)
}
  )


pv<-rundatspv[[2]]

####### pv correction based on sunrise/sunset

sunyes<-getSunlightTimes(date = as.Date(pvschneider$date), keep = c("sunrise", "sunset"), lat = 48.172857, lon = 16.439628, tz = "UTC")

pvschneider$pred[!pvschneider$date%within%as.interval(sunyes$sunrise, sunyes$sunset)]<-0

sunyes<-getSunlightTimes(date = as.Date(pvhopf$date), keep = c("sunrise", "sunset"), lat = 48.172857, lon = 16.439628, tz = "UTC")

pvhopf$pred[!pvhopf$date%within%as.interval(sunyes$sunrise, sunyes$sunset)]<-0


write_feather(pv,"path/pv")


############## sum demand predictions

demand<-list()

demand<-rundats[c(1:5)]

demanddf<-do.call("cbind", demand)

demanddf<-demanddf[,-c(3,5,7,9)]

demanddf[,2:6][demanddf[,2:6]<0]<-0

demanddf["preds"]<-rowSums(demanddf[,2:6])

names(demanddf)[c(2,3,4,5,6)]<-c("pred.1", "pred.2", "pred.3", "pred.4", "pred.5")

######### read demand observations

glcobs<-read_feather("path/glc")
names(glcobs)[6]<-"grouping"

demandobs<-glcobs%>%filter(Site==0)

demandobsmeans<-demandobs%>%group_by(date,grouping)%>%summarize(power_abs_=sum(power_abs_))

demandobsmeans<-demandobsmeans%>%mutate(ymdh = format(date, '%Y-%m-%d-%H')) %>% group_by(grouping, ymdh) %>% summarize(means=mean(power_abs_,na.rm = TRUE))

demandcomp<-demanddf%>%filter(date >= min(demandobs$date) & date <= max(demandobs$date))

demandcomp<-demandcomp%>%gather(pred, predictions, pred.1:pred.5, factor_key=TRUE)
names(demandcomp)[3]<-"grouping"

demandcomp$type<-rep("model",NROW(demandcomp))

demandobsmeans$type<-rep("actual",NROW(demandobsmeans))

demandact<-demandobsmeans[,c(2,1,4,3)]
names(demandact)<-c("date","grouping","type","demand")

demandmod<-demandcomp[,c(1,3,5,4)]
names(demandmod)<-c("date","grouping","type","demand")

demandact$date<-as.POSIXct(demandact$date, format = "%Y-%m-%d-%H", tz="UTC")

demandfull<-bind_rows(demandact,
                      demandmod)

demandfull<-demandfull%>%group_by(date,grouping)%>%filter(n()>1)

####### demand mean bias correction and quantile mapping 

groupsh<-unique(demandfull$grouping)

demanddf_long <- gather(demanddf, grouping, demand, pred.1:pred.5, factor_key=TRUE)

meanbiascordemandafter <- lapply(1:length(groupsh),function(i){
  
  dd <- demandfull[demandfull$grouping==groupsh[i],]
  
  diffs <- mean(dd$demand[dd$type=="actual"]-dd$demand[dd$type=="model"])
  
  demanddf[,i+1] <- demanddf[,i+1] + diffs
  
  return(diffs)
  
})



qmapbiascordemand <- lapply(1:length(groupsh),function(i){
  
  dd <- demandfull[demandfull$grouping==groupsh[i],]
  
  qmap <- fitQmap(dd$demand[dd$type=="actual"],dd$demand[dd$type=="model"])
  
  demanddf[demanddf$date%in%dd$date,i+1] <- doQmap(demanddf[demanddf$date%in%dd$date,i+1],qmap)
  
  qmap2 <- fitQmap(dd$demand[dd$type=="actual"],demanddf[!demanddf$date%in%dd$date,i+1])
  
  demanddf[!demanddf$date%in%dd$date,i+1]<- doQmap(demanddf[!demanddf$date%in%dd$date,i+1],qmap2)
  
  return(qmap)
})

