library(tidyverse)
library(gdxtools)
library(arrow)
library("hydroGOF")
library("DTWBI")
library(feather)

source("Generate Scenario Inputs.R")
source("src/R/functions.R")

igdx(dirname(Sys.which('gams')))
setwd(paste0(dirname(rstudioapi::getActiveDocumentContext()$path),
             "/../../")
)


############# input data folder
input_dir <- "data/input/"
output_dir <- "data/output/"



##################### custom colour palette

rfCOLORS3<-c("#C72321", "#0D8085", "#EFC220")
rfCOLORS5<-c("#C62220","#FBD7A8","#7A6952","#0D8085","#F0C220")
rfCOLORS10<-c("#C72321","#861719","#FBD7A9","#BA9F7C","#7A6952","#6E9B9E","#0D8085","#19484C","#F0C320","#AF8F19")

####################

#define scenario ranges

gridcost_seq <- seq(0.1,0.18,0.02)

ts_seq <- c(2012, 2016, 2019)

feed_in_seq <- seq(0,0.08,0.02)

storage_cost_seq <- seq(50,550,100)

pv_cost_seq <- seq(200,1000,200)

interest_rate_seq <- seq(0,0.1,0.02)

scenarios <- expand.grid(gridcost_seq, ts_seq, feed_in_seq, storage_cost_seq, pv_cost_seq, interest_rate_seq)

names(scenarios)<-c("gridcost","year","feed_in_tariff","storage_cost","pv_cost","interest_rate")

#calculate scenarios; plyr package must not be loaded when running this command
detach("package:plyr", unload=TRUE)
library(dplyr)
library(lubridate)

system.time(scens<-lapply(1:NROW(scenarios),function(i){

generate_scenarios(site = "site", yt = scenarios$year[i], controllable_demands = c("all controllable demands; e.g. 2"), gridcost = (scenarios$gridcost[i])/1000, feed_in = (scenarios$feed_in_tariff[i])/1000, pvcost = (scenarios$pv_cost[i])/1000, storagecost = (scenarios$storage_cost[i])/1000, interest_rate = scenarios$interest_rate[i], pv_runtime = 20, storage_runtime = 10, efficiency_storage = 0.9, name = i)
  
}))

# convert results list to dataframe and save

library(plyr)

scensd <- ldply(scens,data.frame)

write_feather(scensdno,"path/scensdno") # every load shifting configuration is saved separately

######### compare scenario results 

scensdno<-arrow::read_feather("path/scensdno")
scensdmid<-arrow::read_feather("path/scensdmid")
scensdhigh<-arrow::read_feather("path/scensdhigh")

diffnomid<-scensdno[,14:17]-scensdmid[,14:17]
names(diffnomid)<-c("pv_cap_nomid","stor_cap_nomid","cost_nomid","dem_disp_nomid")
diffnohigh<-scensdno[,14:17]-scensdhigh[,14:17]
names(diffnohigh)<-c("pv_cap_nohigh","stor_cap_nohigh","cost_nohigh","dem_disp_nohigh")
diffmidhigh<-scensdmid[,14:17]-scensdhigh[,14:17]
names(diffmidhigh)<-c("pv_cap_midhigh","stor_cap_midhigh","cost_midhigh","dem_disp_midhigh")

scensdcomp<-cbind(scensdno,diffnomid,diffnohigh,diffmidhigh,scensdhigh$installed_pv_capacity,scensdmid$installed_pv_capacity)

scensdcomp2<- scensdcomp%>%
  group_by(feed_in,pvcost,storagecost)%>%
  {mutate(ungroup(.), g = group_indices(.))}%>%
  arrange(g)

scensdcomp2[,3:6]<-scensdcomp2[,3:6]*1000

scensdcomp2$feed_in<-as.character(scensdcomp2$feed_in)
scensdcomp2$year<-as.character(scensdcomp2$year)
scensdcomp2$storagecost<-as.character(scensdcomp2$storagecost)
scensdcomp2$storagecost<-factor(scensdcomp2$storagecost,levels=c("50","150","250","350","450","550"))

scensdcomp2%>%filter(installed_pv_capacity<1000000)%>%filter(installed_pv_capacity<=maxpvcucumbers)%>%filter(highpv<=maxpvcucumbers)%>%filter(midpv<=maxpvcucumbers)%>%filter(!storagecost%in%c(250,450))%>%
  ggplot()+
  geom_line(aes(x=gridcost,y=cost_nohigh,color=feed_in,linetype=year,linewidth=storagecost))+
  facet_grid(pvcost~storagecost,scales="free_y")+
  scale_y_continuous(
                     sec.axis = dup_axis(breaks=NULL,labels=NULL,
                                         name = "PV Investment Cost (EUR/kWp)"
                     ))+
  scale_x_continuous(sec.axis = dup_axis(breaks=NULL,labels=NULL,
                                         name = "Storage Investment Cost (EUR/kWh)"))+
  theme_classic(base_size = 16)+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)+
  labs(x="Grid Electricity Cost (EUR/kWh)", y="Electricity Cost Savings (EUR)", linetype="Year", 
       color="Feed In Tariff (EUR/kWh)")

ggsave("costnohigh.png",width = 15,height = 10)

scensdcomp2%>%filter(installed_pv_capacity<1000000)%>%filter(installed_pv_capacity<=maxpvtomatoes)%>%filter(highpv<=maxpvtomatoes)%>%filter(midpv<=maxpvtomatoes)%>%filter(!storagecost%in%c(250,450))%>%
ggplot()+
  geom_line(aes(x=gridcost,y=pv_cap_nohigh/1000,color=feed_in,linetype=year,linewidth=storagecost))+
  facet_grid(pvcost~storagecost,scales="free_y")+
  scale_y_continuous(sec.axis = dup_axis(breaks=NULL,labels=NULL,
                                                              name = "PV Investment Cost Scenario (EUR/kWp)"
  ))+
  scale_x_continuous(breaks=seq(0.1,0.18,0.04),sec.axis = dup_axis(breaks=NULL,labels=NULL,
                                                                   name = "Storage Investment Cost Scenario (EUR/kWh)"))+
  scale_color_manual(values = rfCOLORS5)+
  theme_classic(base_size = 24)+
  theme(panel.spacing = unit(2, "lines"))+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)+
  labs(x="Grid Electricity Cost Scenario (EUR/kWh)", y="Difference Installed PV Capacity (kWp)", linetype="Year", 
       color="Feed In Tariff (EUR/kWh)")

ggsave("pvnohigh.png",width = 15,height = 10)


########### fixed y-scale between sites

scensdcomp2%>%filter(installed_pv_capacity<1000000)%>%filter(installed_pv_capacity<=maxpvcucumbers)%>%filter(highpv<=maxpvcucumbers)%>%filter(midpv<=maxpvcucumbers)%>%filter(!storagecost%in%c(250,450))%>%
  ggplot()+
  geom_line(aes(x=gridcost,y=cost_nohigh,color=feed_in,linetype=year,linewidth=storagecost))+
  facet_grid(pvcost~storagecost,scales="fixed")+
  scale_y_continuous(limits=c(-1,1000),sec.axis = dup_axis(breaks=NULL,labels=NULL,
                                                              name = "PV Investment Cost Scenario (EUR/kWp)"
  ))+
  scale_x_continuous(breaks=seq(0.1,0.18,0.04),sec.axis = dup_axis(breaks=NULL,labels=NULL,
                                                                   name = "Storage Investment Cost Scenario (EUR/kWh)"))+
  scale_color_manual(values = rfCOLORS5)+
  theme_classic(base_size = 24)+
  theme(panel.spacing = unit(2, "lines"))+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)+
  labs(x="Grid Electricity Cost Scenario (EUR/kWh)", y="Difference Total Electricity Cost (EUR)", linetype="Year", 
       color="Feed In Tariff (EUR/kWh)")

ggsave("costnohighfixed.png",width = 15,height = 10)

scensdcomp2%>%filter(installed_pv_capacity<1000000)%>%filter(!storagecost%in%c(250,450))%>%
  ggplot()+
  geom_line(aes(x=gridcost,y=pv_cap_nohigh,color=feed_in,linetype=year,linewidth=storagecost))+
  facet_grid(pvcost~storagecost,scales="fixed")+
  
  scale_y_continuous(limits = c(-20000, 40000),
                     sec.axis = dup_axis(breaks=NULL,labels=NULL,
                                         name = "PV Investment Cost (EUR/kWp)"
                     ))+
  scale_x_continuous(sec.axis = dup_axis(breaks=NULL,labels=NULL,
                                         name = "Storage Investment Cost (EUR/kWh)")+ color="Feed In Tariff (EUR/kWh)")+
  theme_classic(base_size = 16)+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)+
  labs(x="Grid Electricity Cost (EUR/kWh)", y="Difference Installed PV Capacity (W)", linetype="Year", 
       color="Feed In Tariff (EUR/kWh)")

ggsave("pvnohighfixed.png",width = 15,height = 10)

#################demand shift figure

demanddf2019<-demanddf[year(demanddf$date)==2019,]

demandshiftin<-demanddf2019%>%
  gather(Demand_type, Demand, pred.1:pred.4)%>%
  group_by(Demand_type) %>%
  mutate(demtype="Demand-original")%>%
  mutate(t=1:n())%>%
  ungroup()%>%
  select(c(Demand_type,Demand,demtype,t))%>%
  filter(Demand_type!="pred.1")

demandshiftin[demandshiftin$Demand_type=="pred.2",]$Demand_type<-"Charging Devices"
demandshiftin[demandshiftin$Demand_type=="pred.3",]$Demand_type<-"Irrigation Pump"
demandshiftin[demandshiftin$Demand_type=="pred.4",]$Demand_type<-"Heating Pump"

mygdx<-gdx("path/scenario1767out.gdx")

demandshiftout<-mygdx["x_control_demand"] %>%
  mutate(Demand_type=V1) %>%
  group_by(V1) %>%
  mutate(t=1:n()) %>%
  mutate(demtype="Demand-shifted")%>%
  ungroup()%>%
  select(c(Demand_type,value,demtype,t))%>%
  rename(Demand=value)

demandshiftout[demandshiftout$Demand_type=="p2",]$Demand_type<-"Charging Devices"
demandshiftout[demandshiftout$Demand_type=="p3",]$Demand_type<-"Irrigation Pump"
demandshiftout[demandshiftout$Demand_type=="p4",]$Demand_type<-"Heating Pump"

demandshift<-bind_rows(demandshiftin,demandshiftout)

demandshift$Demand_type[demandshift$Demand_type=="Charging Devices"]<-"Charging\nDevices"
demandshift$Demand_type[demandshift$Demand_type=="Heating Pump"]<-"Heating\nPump"
demandshift$Demand_type[demandshift$Demand_type=="Irrigation Pump"]<-"Irrigation\nPump"

demandshift$Demand<-demandshift$Demand/1000  

demandshift %>%
  group_by(Demand_type,demtype)%>%
  slice(0:240)%>%
  ggplot(mapping=aes(x=t,y=Demand)) +
  geom_line(aes(colour=Demand_type,linetype=demtype)) +
  facet_grid(Demand_type~demtype,scales="free_y")+
  scale_colour_manual(values = rfCOLORS3)+
  theme_classic(base_size = 18)+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)+
  theme(legend.position = "none")+
  xlab("Hour") +
  ylab("Electricity Demand (kW)")

ggsave("demshift.png",width = 10,height = 5)

demandshift %>%
  group_by(Demand_type,demtype)%>%
  slice(0:240)%>%
  summarise(sums=sum(Demand))

########## summary figure

scensdno <- scensdno %>% mutate(shift=rep("no_shift",NROW(scensdno))) %>% mutate(maxpv=rep(maxpvtomatoes,NROW(scensdno)))
scensdmid <-  scensdmid %>% mutate(shift=rep("mid_shift",NROW(scensdmid))) %>% mutate(maxpv=rep(maxpvtomatoes,NROW(scensdmid)))
scensdhigh <-  scensdhigh %>% mutate(shift=rep("high_shift",NROW(scensdhigh))) %>% mutate(maxpv=rep(maxpvtomatoes,NROW(scensdhigh)))

scensdall <- bind_rows(scensdno,scensdmid,scensdhigh)

scensdall_long <- gather(scensdall, variable, result, installed_pv_capacity:x_cost, factor_key=TRUE)

scensdall_long<-scensdall%>%filter(installed_pv_capacity<1000000)%>%gather(variable, result, installed_pv_capacity:x_cost, factor_key=TRUE)

outfiles <- list.files("path",pattern = ".gdx",full.names = TRUE,recursive = TRUE)

fun_insert <- function(x, pos, insert) {
  gsub(paste0("^(.{", pos, "})(.*)$"),
       paste0("\\1", insert, "\\2"),
       x)
}

xot<-lapply(1:length(outfiles),function(i){
  mygdx <- gdx(outfiles[i])
  
  xpv <- mygdx["x_pv"]
  
  t<-data.frame(as.numeric(gsub("t","",mygdx["x_buy_from_grid"]$V1)))
  names(t)<-"t"
  
  pvpr<-mygdx["pv_production"]
  pvpr$t<-as.numeric(gsub("t","",pvpr$t))
  
  pvpr<-merge(t,pvpr,by=c("t"),all.x = TRUE)
  pvpr$value[is.na(pvpr$value)]<-0
  
  pvpr$value<-pvpr$value * xpv$value
  
  cdems<-mygdx["x_control_demand"]%>%group_by(as.numeric(gsub("t","",V2)))%>%summarize(sums=sum(value))
  names(cdems)<-c("t","value")
  
  cdems$value<-cdems$value+mygdx["demand"]$value
  
  sell <- pvpr$value - cdems$value - mygdx["x_in"]$value
  sell[sell<0]<-0
  
  xdirect <- mygdx["x_direct_use"]
  xdirect$value<-xdirect$value - sell
  
  xgrid <- mygdx["x_buy_from_grid"]

  
  xstor <- mygdx["x_out"]
  xstor$value<-mygdx["x_out"]$value - (mygdx["x_sell_to_grid"]$value-sell)
  
  
  dsum <- sum(xdirect$value)+sum(xgrid$value)+sum(xstor$value)
  site <- as.numeric(unlist(regmatches(outfiles[i], gregexpr("[[:digit:]]+", outfiles[i]))))[1]
  name <- as.numeric(unlist(regmatches(outfiles[i], gregexpr("[[:digit:]]+", outfiles[i]))))[2]
  
  stri<-unlist(str_split(outfiles[i],"/"))[8]
  
  idx <- unlist(gregexpr(pattern ='shift',stri))
  
  shift<-fun_insert(stri, (idx-1), "_")
  
  xdir<-sum(xdirect$value)/sum(cdems$value)
  xgr<-sum(xgrid$value)/sum(cdems$value)
  xstr<-sum(xstor$value)/sum(cdems$value)  
  
  ch<-xdir+xgr+xstr
  
  xot<-data.frame(xdir,xgr,xstr,site,shift,name,ch)
  return(xot)
  
  })

library(plyr)

xotd <-ldply(xot,data.frame)
View(xotd)

detach("package:plyr", unload=TRUE)
library(dplyr)

names(xotd)[1:3]<-c("x_direct","x_grid","x_storage")

scensdext<-merge(scensdall, xotd, by=c("site","shift","name"))

scensdext_long<-scensdext%>%filter(installed_pv_capacity<1000000)%>%gather(variable, result, installed_pv_capacity:x_storage, factor_key=TRUE)

scensdall_long$variable<-as.character(scensdall_long$variable)

scensdext_long$variable<-as.character(scensdext_long$variable)

scensdall_long$variable[scensdall_long$variable=="installed_pv_capacity"]<-"PV capacity (kWp)"
scensdall_long$variable[scensdall_long$variable=="installed_storage_capacity"]<-"Storage capacity (kWh)"
scensdall_long$variable[scensdall_long$variable=="x_cost"]<-"Total electricity\n cost (EUR)"
scensdall_long$variable<-factor(scensdall_long$variable,levels=c("PV capacity (kWp)","Storage capacity (kWh)","Total electricity\n cost (EUR)","Demand covered by\n PV electricity (fraction)"))

scensdext_long$variable[scensdext_long$variable=="x_direct"]<-"Demand covered by\n PV electricity (fraction)"
scensdext_long$variable<-factor(scensdext_long$variable,levels=c("PV capacity (kWp)","Storage capacity (kWh)","Total electricity\n cost (EUR)","Demand covered by\n PV electricity (fraction)"))

scensdext_long$site[scensdext_long$site==0]<-"Tomatoes"
scensdall_long$site[scensdall_long$site==0]<-"Tomatoes"
scensdext_long$site[scensdext_long$site==1]<-"Cucumbers"
scensdall_long$site[scensdall_long$site==1]<-"Cucumbers"


calc_boxplot_stat <- function(x) {
  coef <- 1.5
  n <- sum(!is.na(x))
  # calculate quantiles
  stats <- quantile(x, probs = c(0.0, 0.25, 0.5, 0.75, 1.0))
  names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
  iqr <- diff(stats[c(2, 4)])
  # set whiskers
  outliers <- x < (stats[2] - coef * iqr) | x > (stats[4] + coef * iqr)
  if (any(outliers)) {
    stats[c(1, 5)] <- range(c(stats[2:4], x[!outliers]), na.rm = TRUE)
  }
  return(stats)
}

maxpvtomatoes<-46968/1000
maxpvcucumbers<-25924.29/1000

scensdall_long$maxpv<-NA

scensdall_long$maxpv[scensdall_long$site=="Tomatoes" & scensdall_long$variable=="PV capacity (kWp)"]<-maxpvtomatoes
scensdall_long$maxpv[scensdall_long$site=="Cucumbers"& scensdall_long$variable=="PV capacity (kWp)"]<-maxpvcucumbers

scensdall_long$result[scensdall_long$variable=="PV capacity (kWp)" | scensdall_long$variable=="Storage capacity (kWh)"]<-scensdall_long$result[scensdall_long$variable=="PV capacity (kWp)" | scensdall_long$variable=="Storage capacity (kWh)"]*1000

scensdall_long$shift[scensdall_long$shift=="no_shift"]<-"no\nshift"
scensdall_long$shift[scensdall_long$shift=="mid_shift"]<-"mid\nshift"
scensdall_long$shift[scensdall_long$shift=="high_shift"]<-"high\nshift"

scensdext_long$shift[scensdext_long$shift=="no_shift"]<-"no\nshift"
scensdext_long$shift[scensdext_long$shift=="mid_shift"]<-"mid\nshift"
scensdext_long$shift[scensdext_long$shift=="high_shift"]<-"high\nshift"

scensdall_long%>%
  ggplot()+
  stat_summary(mapping=aes(x=shift,y=result,fill=shift),fun.data = calc_boxplot_stat, geom="boxplot") + 
  stat_summary(fun = "mean", aes(x = 1,y=maxpv, yintercept = maxpv, group = site), geom = "hline",size=1.2, linetype="dotdash")+
  stat_summary(scensdext_long%>%filter(variable%in%c("Demand covered by\n PV electricity (fraction)")),mapping=aes(x=shift,y=result,fill=shift),fun.data = calc_boxplot_stat, geom="boxplot")+ 
  facet_wrap(nrow=2,ncol=4,as.factor(site)~variable,scales="free_y")+
  scale_fill_manual(values = rfCOLORS3)+
  theme_classic(base_size = 16)+
  theme(legend.position = "none")+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)

ggsave("totcomp.png",width = 10,height = 5)

####### average diurnal demand figure

xcontrd<-lapply(1:length(outfiles),function(i){
  mygdx <- gdx(outfiles[i])
  xcontr <- mygdx["x_control_demand"]
  
  xcontr$season[(as.numeric(gsub("t","",xcontr$V2))>2190)&(as.numeric(gsub("t","",xcontr$V2))<6570)]<-"summer"
  
  xcontr$season[which(is.na(xcontr$season))]<-"winter"
  
  xcontr$hour<-rep(1:24,NROW(xcontr)/24)
  
  site <- rep(as.numeric(unlist(regmatches(outfiles[i], gregexpr("[[:digit:]]+", outfiles[i]))))[1],NROW(xcontr))
  
  stri<-unlist(str_split(outfiles[i],"/"))[8]
  
  idx <- unlist(gregexpr(pattern ='shift',stri))
  
  shift<-fun_insert(stri, (idx-1), "_")
  shift<-rep(shift,NROW(xcontr))
  
  xcontr<-data.frame(site,shift,xcontr[,c(1,3,4,5)])
  
  return(xcontr)
  
})


library(plyr)
xcontrd <-ldply(xcontrd,data.frame)

detach("package:plyr", unload=TRUE)
library(dplyr)

xcontrd2 <-xcontrd2%>%group_by(season,V1,hour,shift,site)%>%summarize(meanv=mean(value))

xcontrd2[xcontrd2$V1=="p1"& xcontrd2$site==0,]$V1<-"Miscellaneous"
xcontrd2[xcontrd2$V1=="p2"& xcontrd2$site==0,]$V1<-"Charging Devices"
xcontrd2[xcontrd2$V1=="p3"& xcontrd2$site==0,]$V1<-"Heating Pump"
xcontrd2[xcontrd2$V1=="p4"& xcontrd2$site==0,]$V1<-"Heating Converter"
xcontrd2[xcontrd2$V1=="p5"& xcontrd2$site==0,]$V1<-"Irrigation Pump"

xcontrd2[xcontrd2$V1=="p1"& xcontrd2$site==1,]$V1<-"Miscellaneous"
xcontrd2[xcontrd2$V1=="p2"& xcontrd2$site==1,]$V1<-"Charging Devices"
xcontrd2[xcontrd2$V1=="p3"& xcontrd2$site==1,]$V1<-"Irrigation Pump"
xcontrd2[xcontrd2$V1=="p4"& xcontrd2$site==1,]$V1<-"Heating Pump"

xcontrd2$site[xcontrd2$site==0]<-"Tomatoes"
xcontrd2$site[xcontrd2$site==1]<-"Cucumbers"

xcontrd2$season[xcontrd2$season=="summer"]<-"Summer"
xcontrd2$season[xcontrd2$season=="winter"]<-"Winter"

names(xcontrd2)[2]<-"Demand_type"

demanddf2012 <- demanddf[year(demanddf$date)==2012,]
demanddf2016 <- demanddf[year(demanddf$date)==2016,]
demanddf2019 <- demanddf[year(demanddf$date)==2019,]

demandshiftinh2012<-demanddf2012%>%gather(Demand_type, Demand, pred.1:pred.5)%>%
  mutate(demtype="origdem")%>%
  select(c(Demand_type,Demand,demtype))%>%
  #filter(Demand_type!="pred.1")%>%
  group_by(Demand_type) %>%
  mutate(t=1:n())%>%
  mutate(site=rep(0,n()))%>%
  mutate(year=rep(2012,n()))%>%
  ungroup()

demandshiftinh2016<-demanddf2016%>%gather(Demand_type, Demand, pred.1:pred.5)%>%
  mutate(demtype="origdem")%>%
  select(c(Demand_type,Demand,demtype))%>%
  #filter(Demand_type!="pred.1")%>%
  group_by(Demand_type) %>%
  mutate(t=1:n())%>%
  mutate(site=rep(0,n()))%>%
  mutate(year=rep(2016,n()))%>%
  ungroup()

demandshiftinh2019<-demanddf2019%>%gather(Demand_type, Demand, pred.1:pred.5)%>%
  mutate(demtype="origdem")%>%
  select(c(Demand_type,Demand,demtype))%>%
  #filter(Demand_type!="pred.1")%>%
  group_by(Demand_type) %>%
  mutate(t=1:n())%>%
  mutate(site=rep(0,n()))%>%
  mutate(year=rep(2019,n()))%>%
  ungroup()

demandshiftinh2012[demandshiftinh2012$Demand_type=="pred.1",]$Demand_type<-"Miscellaneous"
demandshiftinh2012[demandshiftinh2012$Demand_type=="pred.2",]$Demand_type<-"Charging Devices"
demandshiftinh2012[demandshiftinh2012$Demand_type=="pred.3",]$Demand_type<-"Heating Pump"
demandshiftinh2012[demandshiftinh2012$Demand_type=="pred.4",]$Demand_type<-"Heating Converter"
demandshiftinh2012[demandshiftinh2012$Demand_type=="pred.5",]$Demand_type<-"Irrigation Pump"

demandshiftinh2016[demandshiftinh2016$Demand_type=="pred.1",]$Demand_type<-"Miscellaneous"
demandshiftinh2016[demandshiftinh2016$Demand_type=="pred.2",]$Demand_type<-"Charging Devices"
demandshiftinh2016[demandshiftinh2016$Demand_type=="pred.3",]$Demand_type<-"Heating Pump"
demandshiftinh2016[demandshiftinh2016$Demand_type=="pred.4",]$Demand_type<-"Heating Converter"
demandshiftinh2016[demandshiftinh2016$Demand_type=="pred.5",]$Demand_type<-"Irrigation Pump"

demandshiftinh2019[demandshiftinh2019$Demand_type=="pred.1",]$Demand_type<-"Miscellaneous"
demandshiftinh2019[demandshiftinh2019$Demand_type=="pred.2",]$Demand_type<-"Charging Devices"
demandshiftinh2019[demandshiftinh2019$Demand_type=="pred.3",]$Demand_type<-"Heating Pump"
demandshiftinh2019[demandshiftinh2019$Demand_type=="pred.4",]$Demand_type<-"Heating Converter"
demandshiftinh2019[demandshiftinh2019$Demand_type=="pred.5",]$Demand_type<-"Irrigation Pump"

demandshiftintot <- bind_rows(demandshiftinh2012,demandshiftinh2016,demandshiftinh2019)

demandshiftintot$season <- NA

demandshiftintot$season[(demandshiftintot$t>2190)&(demandshiftintot$t<6570)]<-"summer"

demandshiftintot$season[which(is.na(demandshiftintot$season))]<-"winter"

demandshiftintot$hour<-rep(1:24,NROW(demandshiftintot)/24)

dmshftt2<-demandshiftintot%>%group_by(season,Demand_type,hour,site)%>%summarize(meanv=mean(Demand))

dmshftt2$site[dmshftt2$site==0]<-"Tomatoes"

dmshftt2$site[dmshftt2$site==1]<-"Cucumbers"

dmshftt2$season[dmshftt2$season=="summer"]<-"Summer"

dmshftt2$season[dmshftt2$season=="winter"]<-"Winter"

dmshftt2$shift<-rep("no_shift",NROW(dmshftt2))

dmshftt2<-dmshftt2[,c(1,2,3,6,4,5)]

xcontrd3<-xcontrd2%>%filter(shift!="no_shift")

dmshftt3 <- dmshftt2%>%filter(site!="Tomatoes")

dmshftt3 <- dmshftt3%>%filter(Demand_type!="Miscellaneous")

dems<-bind_rows(dmshftt3,xcontrd3)

dems$meanv<-dems$meanv/1000

dems%>%
  ggplot()+
  geom_area(aes(x=hour,y=meanv,fill=as.factor(Demand_type)))+
  facet_grid(as.factor(shift)~season,scales="free_y")+
  scale_x_continuous(breaks=seq(1,24,2))+
  scale_fill_manual(values = rfCOLORS3)+
  ylab("Electricity Demand (kW)")+
  xlab("Hour")+
  labs(fill = "Demand Group")+
  theme_classic(base_size = 20)

ggsave("demshiftx.png",width = 15,height = 10)


######## average diurnal electricity flow graph

outfiles <- list.files("path/",pattern = ".gdx",full.names = TRUE,recursive = TRUE)

outfiles2<-outfiles

xsrchno<-lapply(1:length(outfiles2),function(i){
  mygdx <- gdx(outfiles2[i])
  
  xpv <- mygdx["x_pv"]
  
  t<-data.frame(as.numeric(gsub("t","",mygdx["x_buy_from_grid"]$V1)))
  names(t)<-"t"
  
  pvpr<-mygdx["pv_production"]
  pvpr$t<-as.numeric(gsub("t","",pvpr$t))
  
  pvpr<-merge(t,pvpr,by=c("t"),all.x = TRUE)
  pvpr$value[is.na(pvpr$value)]<-0
  
  pvpr$value<-pvpr$value * xpv$value
  
  cdems<-mygdx["x_control_demand"]%>%group_by(as.numeric(gsub("t","",V2)))%>%summarize(sums=sum(value))
  names(cdems)<-c("t","value")
  
  cdems$value<-cdems$value+mygdx["demand"]$value
  
  sell <- pvpr$value - cdems$value - mygdx["x_in"]$value
  sell[sell<0]<-0
  
  xdir <- mygdx["x_direct_use"]
  xdir$value<-xdir$value - sell
  xdir <- data.frame(xdir,rep("x_direct",NROW(xdir)))
  names(xdir)[3]<-"source"
  
  xgr <- mygdx["x_buy_from_grid"]
  xgr <- data.frame(xgr,rep("x_grid",NROW(xgr)))
  names(xgr)[3]<-"source"
  
  xstor <- mygdx["x_out"]
  xstor$value<-mygdx["x_out"]$value - (mygdx["x_sell_to_grid"]$value-sell)
  xstor <- data.frame(xstor,rep("x_storage",NROW(xstor)))
  names(xstor)[3]<-"source"
  
  xsell <- mygdx["x_sell_to_grid"]
  xsell <- data.frame(xsell,rep("x_sell",NROW(xsell)))
  names(xsell)[3]<-"source"
  
  if(xpv<1000000){
  
  xsrc <- bind_rows(xdir,xgr,xstor,xsell)

  xsrc$season[(as.numeric(gsub("t","",xsrc$V1))>2190)&(as.numeric(gsub("t","",xsrc$V1))<6570)]<-"summer"
  
  xsrc$season[(as.numeric(gsub("t","",xsrc$V2))<=2190)/(as.numeric(gsub("t","",xsrc$V2))>=6570)]<-"winter"
  
  xsrc$season[which(is.na(xsrc$season))]<-"winter"
  
  xsrc$hour<-rep(1:24,NROW(xsrc)/24)
  
  site <- rep(as.numeric(unlist(regmatches(outfiles2[i], gregexpr("[[:digit:]]+", outfiles2[i]))))[1],NROW(xsrc))
  
  stri<-unlist(str_split(outfiles2[i],"/"))[8]
  
  idx <- unlist(gregexpr(pattern ='shift',stri))
  
  shift<-fun_insert(stri, (idx-1), "_")
  shift<-rep(shift,NROW(xsrc))
  
  xsrc<-data.frame(site,shift,xsrc,xpv)
  
  return(xsrc)}else{return()}
  
})%>%bind_rows()

xsrchno2 <-xsrchno%>%group_by(season,hour,shift,site,source)%>%summarize(meanv=mean(value))

xsrchno2$site[xsrchno2$site==0]<-"Tomatoes"

xsrchno2$site[xsrchno2$site==1]<-"Cucumbers"

xsrchno2$season[xsrchno2$season=="summer"]<-"Summer"

xsrchno2$season[xsrchno2$season=="winter"]<-"Winter"

xsrchno2$source[xsrchno2$source=="x_direct"]<-"PV electricity_direct"

xsrchno2$source[xsrchno2$source=="x_storage"]<-"PV electricity_storage"

xsrchno2$source[xsrchno2$source=="x_grid"]<-"Grid electricity"

xsrchno2$source[xsrchno2$source=="x_sell"]<-"PV electricity_feed-in"

xsrchno2$meanv[xsrchno2$source=="PV electricity_feed-in"]<-xsrchno2$meanv[xsrchno2$source=="PV electricity_feed-in"]*-1

xsrchno2$meanv<-xsrchno2$meanv/1000

xsrchno2%>%group_by(shift,season,source)%>%summarize(ssun=sum(meanv))

xsrchno2%>%
  ggplot()+
  geom_area(aes(x=hour,y=meanv,fill=as.factor(source)))+
  facet_grid(as.factor(shift)~season,scales="fixed")+
  scale_x_continuous(breaks=seq(1,24,2))+
  scale_fill_manual(values = rfCOLORS5)+
  ylab("Average Electricity Flow (kW)")+
  xlab("Hour")+
  labs(fill = "Electricity Flow")+
  theme_classic(base_size = 20)

ggsave("avgflows.png",width = 15,height = 10)


############# Model quality analysis and figure

#demand
mperffiles <- list.files("path/",pattern = "mperf",full.names = TRUE,recursive = TRUE)

mperfs<-lapply(1:length(mperffiles),function(i){

  mperf <- arrow::read_feather(mperffiles[i])
  group <- mperffiles[i]
  group <- as.numeric(str_extract(group, "[0-9]+"))
  group <- rep(group,NROW(mperf))  
  
  mperfs<-data.frame(mperf,group)
})

library(plyr)

mperfsd <- ldply(mperfs,data.frame)

detach("package:plyr", unload=TRUE)

library(dplyr)

mperfsd[mperfsd$power_abs_<0,]$power_abs_<-0
mperfsd[mperfsd$pred<0,]$pred<-0

mperfsd%>%group_by(group)%>%summarize(corr=cor(power_abs_,pred))

mperfsd%>%group_by(group)%>%summarize(nrmses=nrmse(pred,power_abs_))

mperfsd%>%group_by(group)%>%summarize(nmae=compute.nmae(pred,power_abs_))

mperfsd$site[mperfsd$group%in%c(0,1,2,3,4)]<-"Tomatoes"
mperfsd$site[!mperfsd$group%in%c(0,1,2,3,4)]<-"Cucumbers"

mperfsd[mperfsd$group==0,]$group<-"Miscellaneous"
mperfsd[mperfsd$group==1,]$group<-"Charging Devices"
mperfsd[mperfsd$group==2,]$group<-"Heating Pump"
mperfsd[mperfsd$group==3,]$group<-"Heating Converter"
mperfsd[mperfsd$group==4,]$group<-"Irrigation Pump"

mperfsd[mperfsd$group==5,]$group<-"Miscellaneous"
mperfsd[mperfsd$group==6,]$group<-"Charging Devices"
mperfsd[mperfsd$group==7,]$group<-"Irrigation Pump"
mperfsd[mperfsd$group==8,]$group<-"Heating Pump"

mperfsd_long<-mperfsd%>%gather(type, value, power_abs_:pred)
mperfsd_long[mperfsd_long$type=="power_abs_",]$type<-"Observed"
mperfsd_long[mperfsd_long$type=="pred",]$type<-"Simulated"

mperfsd_long[mperfsd_long$value<0,]$value<-0

mperfsd_long%>%
  ggplot()+
  stat_summary(mapping=aes(x=type,y=value,fill=type),fun.data = calc_boxplot_stat, geom="boxplot") + 
  facet_grid(group~site,scales="free")+
  theme_classic(base_size = 16)+
  theme(legend.title = element_blank())+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)+
  scale_fill_manual(values = rfCOLORS3)+
  ylab("Electricity Demand (W)")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("boxplotsdem.png",width = 15,height = 10)

# pv

mperfpvfiles <- list.files("path/",pattern = "mperfpv2",full.names = TRUE,recursive = TRUE)

mperfpvs<-lapply(1:length(mperfpvfiles),function(i){
  
  mperfpv <- arrow::read_feather(mperfpvfiles[i])
  
  mperfpvs<-data.frame(mperfpv)
})

library(plyr)

mperfpvsd <- ldply(mperfpvs,data.frame)

detach("package:plyr", unload=TRUE)
library(dplyr)

mperfpvsd%>%group_by(site)%>%summarize(corr=cor(act,pred))

mperfpvsd%>%group_by(site)%>%summarize(nrmses=nrmse(pred,act))

mperfpvsd%>%group_by(site)%>%summarize(nmae=compute.nmae(pred,act))

mperfpvsd$site[mperfpvsd$site==0]<-"Tomatoes"
mperfpvsd$site[!mperfpvsd$site==1]<-"Cucumbers"

mperfpvsd_long<-mperfpvsd%>%gather(type, value, act:pred)
mperfpvsd_long[mperfpvsd_long$type=="act",]$type<-"Observed"
mperfpvsd_long[mperfpvsd_long$type=="pred",]$type<-"Simulated"

####################### pv production and consumption

xotcp<-lapply(1:length(outfiles),function(i){
  
  mygdx <- gdx(outfiles[i])
  
  xpv <- mygdx["x_pv"]
  
  t<-data.frame(as.numeric(gsub("t","",mygdx["x_buy_from_grid"]$V1)))
  names(t)<-"t"
  
  pvpr<-mygdx["pv_production"]
  pvpr$t<-as.numeric(gsub("t","",pvpr$t))
  
  pvpr<-merge(t,pvpr,by=c("t"),all.x = TRUE)
  pvpr$value[is.na(pvpr$value)]<-0
  
  pvpr$value<-pvpr$value * xpv$value
  
  pvprsum<-sum(pvpr$value)
  
  cdems<-mygdx["x_control_demand"]%>%group_by(as.numeric(gsub("t","",V2)))%>%summarize(sums=sum(value))
  names(cdems)<-c("t","value")
  
  cdems$value<-cdems$value+mygdx["demand"]$value
  
  sell <- pvpr$value - cdems$value - mygdx["x_in"]$value
  sell[sell<0]<-0
  
  xdirect <- mygdx["x_direct_use"]
  xdirect$value<-xdirect$value - sell
  
  xdirectsum<-sum(xdirect$value)
  
  xsellsum<-sum(mygdx["x_sell_to_grid"]$value)
  
  site <- as.numeric(unlist(regmatches(outfiles[i], gregexpr("[[:digit:]]+", outfiles[i]))))[1]
  name <- as.numeric(unlist(regmatches(outfiles[i], gregexpr("[[:digit:]]+", outfiles[i]))))[2]
  
  stri<-unlist(str_split(outfiles[i],"/"))[8]
  
  idx <- unlist(gregexpr(pattern ='shift',stri))
  
  shift<-fun_insert(stri, (idx-1), "_")
  
  xotcp<-data.frame(mygdx["x_pv"]$value,pvprsum,xdirectsum,xsellsum,sum(cdems$value),site,shift,name)
  return(xotcp)
  
})

library(plyr)

xotcpd <-ldply(xotcp,data.frame)


detach("package:plyr", unload=TRUE)
library(dplyr)

names(xotcpd)[1]<-c("installed_pv_capacity")

bsnames<-scensdall_long%>%filter(feed_in==0.00004)%>%filter(pvcost==0.8)%>%filter(storagecost==0.55)%>%filter(gridcost==0.00012)%>%select(name)%>%unique()

xotcpd_red<-xotcpd%>%filter(name%in%bsnames$name)

xotcpd_red%>%group_by(site,shift)%>%summarise_at(.vars = names(.)[1:4],.funs = c(mean="mean"))

xotcpd%>%group_by(shift,site)%>%filter(installed_pv_capacity<1000000)%>%
  summarise_at(.vars = names(.)[1:5],
             .funs = c(mean="mean"))

scensdext_longsum <- scensdext_long%>%group_by(site,shift)%>%filter(!is.na(variable))%>%
  summarize(dirdem=mean(result))

############# peak demand comparison

xcontrdx<-lapply(1:length(outfiles),function(i){
  mygdx <- gdx(outfiles[i])
  xcontr <- mygdx["x_control_demand"]
  
  xcontrs<-xcontr%>%group_by(V2)%>%summarize(ds=sum(value))
  
  xcontrs$ds<-xcontrs$ds + mygdx["demand"]$value
  
  xcontrs$hour<-rep(1:24,NROW(xcontrs)/24)
  
  xcontr<-data.frame(xcontrs)
  
  xcontr<-xcontr%>%group_by(hour,season)%>%summarize(peak=max(ds))
  
  site <- rep(as.numeric(unlist(regmatches(outfiles[i], gregexpr("[[:digit:]]+", outfiles[i]))))[1],NROW(xcontr))
  
  name <- rep(as.numeric(unlist(regmatches(outfiles[i], gregexpr("[[:digit:]]+", outfiles[i]))))[2],NROW(xcontr))
  
  stri<-unlist(str_split(outfiles[i],"/"))[8]
  
  idx <- unlist(gregexpr(pattern ='shift',stri))
  
  shift<-fun_insert(stri, (idx-1), "_")
  
  shift<-rep(shift,NROW(xcontr))
  
  xcontr<-data.frame(site,shift,name,xcontr)
  
  return(xcontr)
  
})

library(plyr)

xcontrdx <-ldply(xcontrdx,data.frame)

detach("package:plyr", unload=TRUE)
library(dplyr)

xcontrdx%>%group_by(shift,site)%>%summarize(meanv=mean(peak))

########### baseline scenario results and time series figure

electricitycostno <- sum(demanddf2019$preds)*0.00012

scensdall_long_red<-scensdall_long%>%filter(feed_in==0.00004)%>%filter(pvcost==0.8)%>%filter(storagecost==0.55)%>%filter(gridcost==0.00012)%>%group_by(site,shift,variable)%>%summarize(sres=mean(result))

outx<-c("path/scenario1767out.gdx")#paths to different shift and site output files go here

basex<-lapply(1:length(outx),function(i){
  mygdx <- gdx(outx[i])
  
  xgrid <- mygdx["x_buy_from_grid"]
  
  
  xsell <- mygdx["x_sell_to_grid"]
  xsell$t<-rep(seq(1,365,1),each=24)
  
  cdems<-mygdx["x_control_demand"]%>%group_by(as.numeric(gsub("t","",V2)))%>%summarize(sums=sum(value))
  names(cdems)<-c("t","value")
  
  cdems$value<-cdems$value+mygdx["demand"]$value
  
  stri<-unlist(str_split(outx[i],"/"))[8]
  
  idx <- unlist(gregexpr(pattern ='shift',stri))
  
  shift<-fun_insert(stri, (idx-1), "_")
  
  shift<-rep(shift,NROW(xsell))
  
  xts<-data.frame(shift,xsell,cdems$value,xgrid$value)
  
  return(xts)
  
})

library(plyr)

basex <-ldply(basex,data.frame)

detach("package:plyr", unload=TRUE)
library(dplyr)

names(basex)<-c("shift","V1","PV_electricity_sell","t","Demand","Grid_electricity")

basex<-basex[,c(1,2,4,3,5,6)]

basex_long <- gather(basex, variable, result, PV_electricity_sell:Grid_electricity, factor_key=TRUE)

basex_long2<-basex_long%>%group_by(shift,variable,t)%>%summarize(vsum=sum(result))
names(basex_long2)[3]<-"doy"

basex_long2$vsum[basex_long2$variable=="PV_electricity_sell"]<-basex_long2$vsum[basex_long2$variable=="PV_electricity_sell"]*-1

basex_long2%>%
ggplot()+
  geom_line(aes(x=doy,y=vsum/1000,color=variable))+
  facet_grid(shift~.,scales="fixed")+
  theme_classic(base_size = 24)+
  scale_color_manual(values = rfCOLORS3)+
  theme(legend.title = element_blank())+
  ylab("Electricity Flow (kWh)")+
  xlab("Day of year")

ggsave("baselinets.png",width = 15,height = 10)

############ extrapolation to total greenhouse area

xotexp<-lapply(1:length(outfiles),function(i){
  mygdx <- gdx(outfiles[i])
  
  xpv <- mygdx["x_pv"]
  
  t<-data.frame(as.numeric(gsub("t","",mygdx["x_buy_from_grid"]$V1)))
  names(t)<-"t"
  
  pvpr<-mygdx["pv_production"]
  pvpr$t<-as.numeric(gsub("t","",pvpr$t))
  
  pvpr<-merge(t,pvpr,by=c("t"),all.x = TRUE)
  pvpr$value[is.na(pvpr$value)]<-0
  
  pvpr$value<-pvpr$value * xpv$value
  
  cdems<-mygdx["x_control_demand"]%>%group_by(as.numeric(gsub("t","",V2)))%>%summarize(sums=sum(value))
  names(cdems)<-c("t","value")
  
  cdems$value<-cdems$value+mygdx["demand"]$value
  
  sell <- pvpr$value - cdems$value - mygdx["x_in"]$value
  sell[sell<0]<-0
  
  xdirect <- mygdx["x_direct_use"]
  xdirect$value<-xdirect$value - sell
  
  xgrid <- mygdx["x_buy_from_grid"]
  
  
  xstor <- mygdx["x_out"]
  xstor$value<-mygdx["x_out"]$value - (mygdx["x_sell_to_grid"]$value-sell)
  
  
  dsum <- sum(xdirect$value)+sum(xgrid$value)+sum(xstor$value)
  site <- as.numeric(unlist(regmatches(outfiles[i], gregexpr("[[:digit:]]+", outfiles[i]))))[1]
  name <- as.numeric(unlist(regmatches(outfiles[i], gregexpr("[[:digit:]]+", outfiles[i]))))[2]
  
  stri<-unlist(str_split(outfiles[i],"/"))[8]
  
  idx <- unlist(gregexpr(pattern ='shift',stri))
  
  shift<-fun_insert(stri, (idx-1), "_")
  
  xdir<-sum(xdirect$value)/sum(cdems$value)
  xgr<-sum(xgrid$value)/sum(cdems$value)
  xstr<-sum(xstor$value)/sum(cdems$value) 
  
  xtpv <- xdir + xstr
  
  if(site==1){texp<-((sum(cdems$value)/3900)*5052400)*xtpv}
  
  if(site==0){texp<-((sum(cdems$value)/6700)*5052400)*xtpv}
  
  if(site==1){pvexp<-((xpv$value/3900)*5052400)}
  
  if(site==0){pvexp<-((xpv$value/6700)*5052400)}
  
  if(site==1){stexp<-((mygdx["x_storage"]$value/3900)*5052400)}
  
  if(site==0){stexp<-((mygdx["x_storage"]$value/6700)*5052400)}
  
  if(site==1){pvprexp<-((sum(pvpr$value)/3900)*5052400)}
  
  if(site==0){pvprexp<-((sum(pvpr$value)/6700)*5052400)}
  
  xot<-data.frame(site,shift,name,xpv,texp,pvexp,stexp,pvprexp)
  return(xot)
  
})

library(plyr)

xotexpd <-ldply(xotexp,data.frame)

detach("package:plyr", unload=TRUE)
library(dplyr)

xotexpd<-xotexpd%>%filter(value<1000000)

names(xotexpd)[4:8]<-c("PV","Demand","PVexp","Stexp","PVprexp")

xotexpd_long <- gather(xotexpd, variable, result, PV:PVprexp, factor_key=TRUE)

xotexpd_long$variable<-as.character(xotexpd_long$variable)

xotexpd_long$variable[xotexpd_long$variable=="PV"]<-"PV (kWp)"
xotexpd_long$variable[xotexpd_long$variable=="PVexp"]<-"PV capacity (MWp)"
xotexpd_long$variable[xotexpd_long$variable=="Demand"]<-"Demand covered by PV (GWh)"
xotexpd_long$variable[xotexpd_long$variable=="Stexp"]<-"Storage capacity (MWh)"
xotexpd_long$variable[xotexpd_long$variable=="PVprexp"]<-"PV electricity (GWh)"

xotexpd_long$result[xotexpd_long$variable=="PV (kWp)"]<-xotexpd_long$result[xotexpd_long$variable=="PV (kWp)"]/1000
xotexpd_long$result[xotexpd_long$variable=="PV capacity (MWp)"]<-xotexpd_long$result[xotexpd_long$variable=="PV capacity (MWp)"]/1000000
xotexpd_long$result[xotexpd_long$variable=="Demand covered by PV (GWh)"]<-xotexpd_long$result[xotexpd_long$variable=="Demand covered by PV (GWh)"]/1000000000
xotexpd_long$result[xotexpd_long$variable=="Storage capacity (MWh)"]<-xotexpd_long$result[xotexpd_long$variable=="Storage capacity (MWh)"]/1000000
xotexpd_long$result[xotexpd_long$variable=="PV electricity (GWh)"]<-xotexpd_long$result[xotexpd_long$variable=="PV electricity (GWh)"]/1000000000

xotexpd_long$site[xotexpd_long$site==0]<-"Tomatoes"
xotexpd_long$site[xotexpd_long$site==1]<-"Cucumbers"

xotexpd_long%>%filter(!variable%in%c("PV (kWp)","PV capacity (MWp)","Storage capacity (MWh)"))%>%
  ggplot()+
  stat_summary(mapping=aes(x=shift,y=result,fill=shift),fun.data = calc_boxplot_stat, geom="boxplot") +
  facet_wrap(as.factor(site)~variable,scales="free_y")+
  theme_classic(base_size = 20)+
  theme(legend.position = "none")+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)+
  scale_fill_manual(values = rfCOLORS3)

ggsave("extrapolation.png",width = 15,height = 10)  

xotexpd%>%filter(PV<1000000)%>%group_by(shift)%>%summarise_at(.vars = names(.)[4:7],.funs = c(mean="mean"))

################# Cucumber harvest evaluation figure

area<-c("Test","Reference")
hfm<-c(61.59,58.75)
names(hfm)<-area
hq<-c(130.49,122.36)
names(hq)<-area

har<-bind_rows(hfm,hq)

har_long <- har%>%gather(Area,Result,Test:Reference,factor_key = TRUE)

har_long$Test<-c("Fresh matter (kg/m²)", "Quantity (qt/m²)","Fresh matter (kg/m²)", "Quantity (qt/m²)")

names(har_long)<-c("Area","Result","Test")

har_long$Area[har_long$Area=="Test"]<-"Test Area"

har_long$Area<-c("Test Area", "Test Area", "Reference Area", "Reference Area")

har_long%>%
  ggplot(aes(x=Area,y=Result,fill=Area))+
  geom_bar(stat="identity")+
  facet_grid(Test~.,scales="free_y")+
  theme_classic(base_size = 20)+
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())+
  theme(legend.title = element_blank())+
  scale_fill_manual(values = rfCOLORS3)
  
ggsave("cucumberharvest.png",width = 15,height = 10)  
