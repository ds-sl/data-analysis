
rm(list = ls())

library(pacman)
library("maps")
library(ggthemes)
p_load(magrittr)
p_load(dplyr)
p_load(readr)
p_load(haven)
p_load(tidyr)
p_load(gpinter)
p_load(purrr)
p_load(stringr)
p_load(ggplot2)
p_load(glue)
p_load(progress)
p_load(zoo)
p_load(ggrepel)
p_load(countrycode)
options(dplyr.summarise.inform = FALSE)


setwd("~/Dropbox/WIR2022/Data/work-data")
income<-read_dta("income_map.dta")
Ptax2<-read_dta("di_2018.dta")
Ptax2$share<-as.numeric(Ptax2$share)
colnames(Ptax2)<-c("iso","value","p")
wealth2<-read_dta("wealth_map.dta")
index_region2<-read_dta("index_region.dta")
#PRE-TAX ####
###Maps### 

map<-map_data("world")
map$region[map$region=="Democratic Republic of the Congo"]<-"DR Congo"
map$region[map$region=="Republic of Congo"]<-"Congo"
map$region[map$region=="Ivory Coast"]<-"Cote dIvoire"
map$region[map$region=="Vietnam"]<-"Viet Nam"
map$region[map$region=="United Arab Emirates"]<-"UAE"
map<-left_join(map,index_region2,by=c("region"="name_region"))
map$ISO[map$region=="Greenland"]<-"GL"
map$ISO[map$region=="UAE"]<-"AE"
map$ISO[map$region=="Brunei"]<-"BR"
map$ISO[map$region=="Antigua"]<-"AG"
map$ISO[map$region=="Cape Verde"]<-"CV"
map$ISO[map$region=="Cote dIvoire"]<-"CI"
map$ISO[map$region=="UK"]<-"GB"
map$ISO[map$region=="Canary Islands"]<-"ES"
map$ISO[map$region=="French Guiana"]<-"FR"
map$ISO[map$region=="Saint Kitts"]<-"KN"
map$ISO[map$region=="South Korea"]<-"KR"
map$ISO[map$region=="Saint Martin"]<-"MF"
map$ISO[map$region=="Macedonia"]<-"MK"
map$ISO[map$region=="Russia"]<-"RU"
map$ISO[map$region=="Bonaire"]<-"BQ"
map$ISO[map$region=="Sint Eustatius"]<-"BQ"
map$ISO[map$region=="Saba"]<-"BQ"
map$ISO[map$region=="Laos"]<-"LA"
map$ISO[map$region=="Sint Maarten"]<-"SX"
map$ISO[map$region=="Syria"]<-"SY"
map$ISO[map$region=="Trinidad"]<-"TT"
map$ISO[map$region=="Tobago"]<-"TT"
map$ISO[map$region=="Virgin Islands"]<-"VI"
map$ISO[map$region=="Saint Vincent"]<-"VC"
map$ISO[map$region=="Grenadines"]<-"VC"
map$ISO[map$region=="French Southern and Antarctic Lands"]<-"FR"
map$ISO[map$region=="Western Sahara"]<-"WS"

test <- map[is.na(map$ISO),]
test2 <- unique(test$region)
test2

#Bot 50% income

map50<- left_join(map,income[income$p=="p0p50"&income$widcode=="sptinc992j",c(1,5)],by=c("ISO"="iso"))
# map50$value<-cut(map50$value,breaks=c(0,0.12,0.14,0.16,0.19,0.26),include.lowest = T)
quantiles<-quantile(map50$value, probs = c(0,0.2,0.4,0.6,0.8,1),na.rm="TRUE")
quantiles[]<-lapply(quantiles*100,round,0)
map50$value<-cut(map50$value,breaks=c(0,quantiles[[2]]/100,quantiles[[3]]/100,quantiles[[4]]/100,quantiles[[5]]/100,1),include.lowest = T)

map50$value[map50$region=="Western Sahara"]<-map50$value[map50$region=="Morocco"][1]
p<- ggplot() +
  theme_map() +
  borders(colour="black") +
  geom_polygon(data = map50, aes(
    x = long,
    y = lat,
    group = group,
    
    fill = value),colour="white",size=0.125)+
  coord_fixed(xlim = c(-130, 160), ylim = c(-50, 75), ratio = 1.3) +
  # scale_fill_gradient(low="yellow",high="red")+
  scale_fill_manual(
    values = c( "#fcee21","#edbc24", "#df8b27","#d0592a","#a51a21"),na.value="white", labels = c(paste(toString(quantiles[[1]]),"-",toString(quantiles[[2]]),"%",sep=""),paste(toString(quantiles[[2]]),"-",toString(quantiles[[3]]),"%",sep=""),paste(toString(quantiles[[3]]),"-",toString(quantiles[[4]]),"%",sep=""),paste(toString(quantiles[[4]]),"-",toString(quantiles[[5]]),"%",sep=""),paste(toString(quantiles[[5]]),"-",toString(quantiles[[6]]),"%",sep="")),na.translate=FALSE) +
  labs(fill="Share") 
ggsave(filename="Maps/map_b50_sptinc.png",p,width = 12,height=6,device = 'png')

# Mid40% income


map40<- left_join(map,income[income$p=="p50p90"&income$widcode=="sptinc992j",c(1,5)],by=c("ISO"="iso"))
# map40$value<-cut(map40$value,breaks=c(0.14,0.36,0.39,0.41,0.45,0.51),include.lowest = T)
quantiles<-quantile(map40$value, probs = c(0,0.2,0.4,0.6,0.8,1),na.rm="TRUE")
quantiles[]<-lapply(quantiles*100,round,0)
map40$value<-cut(map40$value,breaks=c(0,quantiles[[2]]/100,quantiles[[3]]/100,quantiles[[4]]/100,quantiles[[5]]/100,1),include.lowest = T)

map40$value[map40$region=="Western Sahara"]<-map40$value[map40$region=="Morocco"][1]
p <- ggplot() +
  theme_map() +
  borders(colour="black") +
  geom_polygon(data = map40, aes(
    x = long,
    y = lat,
    group = group,
   fill = value),colour="white",size=0.125)+
  coord_fixed(xlim = c(-130, 160), ylim = c(-50, 75), ratio = 1.3) +
  # scale_fill_gradient(low="yellow",high="green4")+
  scale_fill_manual(
    values = c( "palegreen","springgreen1","springgreen3","springgreen4","darkslategrey"),na.value="white", labels = c(paste(toString(quantiles[[1]]),"-",toString(quantiles[[2]]),"%",sep=""),paste(toString(quantiles[[2]]),"-",toString(quantiles[[3]]),"%",sep=""),paste(toString(quantiles[[3]]),"-",toString(quantiles[[4]]),"%",sep=""),paste(toString(quantiles[[4]]),"-",toString(quantiles[[5]]),"%",sep=""),paste(toString(quantiles[[5]]),"-",toString(quantiles[[6]]),"%",sep="")),na.translate=FALSE) +
  labs(fill="Share") 

ggsave(filename="Maps/map_m40_sptinc.png",p,width = 12,height=6,device = 'png')

# Top10% income

map10<- left_join(map,income[income$p=="p90p100"&income$widcode=="sptinc992j",c(1,5)],by=c("ISO"="iso"))
# map10$value<-cut(map10$value,breaks=c(0.22,0.35,0.43,0.49,0.51,0.90),include.lowest = T)
quantiles<-quantile(map10$value, probs = c(0,0.2,0.4,0.6,0.8,1),na.rm="TRUE")
quantiles[]<-lapply(quantiles*100,round,0)
map10$value<-cut(map10$value,breaks=c(0,quantiles[[2]]/100,quantiles[[3]]/100,quantiles[[4]]/100,quantiles[[5]]/100,1),include.lowest = T)

map10$value[map10$region=="Western Sahara"]<-map10$value[map10$region=="Morocco"][1]
p <- ggplot() +
  theme_map() +
  borders(colour="black") +
  geom_polygon(data = map10, aes(
    x = long,
    y = lat,
    group = group,
    
    fill = value),colour="white",size=0.125)+
  coord_fixed(xlim = c(-130, 160), ylim = c(-50, 75), ratio = 1.3) +
  # scale_fill_gradient(low="lightblue1",high="blue4")+
  scale_fill_manual(
    values = c( "azure2", "skyblue","deepskyblue1","royalblue","navy"),na.value="white", labels = c(paste(toString(quantiles[[1]]),"-",toString(quantiles[[2]]),"%",sep=""),paste(toString(quantiles[[2]]),"-",toString(quantiles[[3]]),"%",sep=""),paste(toString(quantiles[[3]]),"-",toString(quantiles[[4]]),"%",sep=""),paste(toString(quantiles[[4]]),"-",toString(quantiles[[5]]),"%",sep=""),paste(toString(quantiles[[5]]),"-",toString(quantiles[[6]]),"%",sep="")),na.translate=FALSE) +
  labs(fill="Share") 

ggsave(filename="Maps/map_m10_sptinc.png",p,width = 12,height=6,device = 'png')

# Top10/Bottom 50 income 
income50<-income[income$p=="p0p50"&income$widcode=="aptinc992j",]
income1050<-income[income$p=="p90p100"&income$widcode=="aptinc992j",]
income1050<-income1050[income1050$iso%in%levels(as.factor(income50$iso)),]
income1050$value<-income1050$value/income50$value
map50<- left_join(map,income1050[,c(1,5)],by=c("ISO"="iso"))
# map50$value<-cut(map50$value,breaks=c(5,8,11,15,20,150),include.lowest = T)
quantiles<-quantile(map50$value, probs = c(0,0.2,0.4,0.6,0.8,1),na.rm="TRUE")
quantiles[]<-lapply(quantiles,round,0)
map50$value<-cut(map50$value,breaks=c(0,quantiles[[2]],quantiles[[3]],quantiles[[4]],quantiles[[5]],quantiles[[6]]),include.lowest = T)

map50$value[map50$region=="Western Sahara"]<-map50$value[map50$region=="Morocco"][1]

p<- ggplot() +
  theme_map() +
  borders(colour="black") +
  geom_polygon(data = map50, aes(
    x = long,
    y = lat,
    group = group,
    
    fill = value),colour="white",size=0.125)+
  coord_fixed(xlim = c(-130, 160), ylim = c(-50, 75), ratio = 1.3) +
  # scale_fill_gradient(low="yellow",high="red")+
  scale_fill_manual(
    values = c( "#fcee21","#edbc24", "#df8b27","#d0592a","#a51a21"),na.value="white", labels = c(paste(toString(quantiles[[1]]),"-",toString(quantiles[[2]]),sep=""),paste(toString(quantiles[[2]]),"-",toString(quantiles[[3]]),sep=""),paste(toString(quantiles[[3]]),"-",toString(quantiles[[4]]),sep=""),paste(toString(quantiles[[4]]),"-",toString(quantiles[[5]]),sep=""),paste(toString(quantiles[[5]]),"-",toString(quantiles[[6]]),sep="")),na.translate=FALSE) +
  labs(fill="Top 10/Bottom 50 ratio") 
ggsave(filename="Maps/map_1050ratio_sptinc.png",p,width = 12,height=6,device = 'png')
#POST-TAX####

#Bot 50%  post-tax income
map50<-left_join(map,Ptax2[Ptax2$p=="bot50",c(1,2)],by=c("ISO"="iso"))
# map50$value<-cut(map50$value,breaks=c(0,0.12,0.17,0.22,0.27,0.50),include.lowest = T)
quantiles<-quantile(map50$value, probs = c(0,0.2,0.4,0.6,0.8,1),na.rm="TRUE")
quantiles[]<-lapply(quantiles*100,round,0)
map50$value<-cut(map50$value,breaks=c(0,quantiles[[2]]/100,quantiles[[3]]/100,quantiles[[4]]/100,quantiles[[5]]/100,1),include.lowest = T)

map50$value[map50$region=="Guinea-Bissau"]<-map50$value[map50$region=="Guinea"][1]
map50$value[map50$region=="UAE"]<-map50$value[map50$region=="Qatar"][1]
map50$value[map50$region=="Eritrea"]<-map50$value[map50$region=="Sudan"][1]
map50$value[map50$region=="Bhutan"]<-map50$value[map50$region=="Nepal"][1]
map50$value[map50$region=="Kosovo"]<-map50$value[map50$region=="Serbia"][1]
map50$value[map50$region=="Montenegro"]<-map50$value[map50$region=="Serbia"][1]
map50$value[map50$region=="Iraq"]<-map50$value[map50$region=="Iran"][1]
map50$value[map50$region=="Turkmenistan"]<-map50$value[map50$region=="Iran"][1]
map50$value[map50$region=="Libya"]<-map50$value[map50$region=="Egypt"][1]
map50$value[map50$region=="South Sudan"]<-map50$value[map50$region=="Sudan"][1]
map50$value[map50$region=="Angola"]<-map50$value[map50$region=="Namibia"][1]
map50$value[map50$region=="Somalia"]<-map50$value[map50$region=="Ethiopia"][1]
map50$value[map50$region=="Tajikistan"]<-map50$value[map50$region=="Uzbekistan"][1]
map50$value[map50$region=="Western Sahara"]<-map50$value[map50$region=="Morocco"][1]
map50$value[map50$region=="Suriname"]<-map50$value[map50$region=="Brazil"][1]
p<- ggplot() +
theme_map() +
borders(colour="black") +
geom_polygon(data = map50, aes(
x = long,
y = lat,
group = group,
fill = value),colour="white",size=0.125)+
coord_fixed(xlim = c(-130, 160), ylim = c(-50, 75), ratio = 1.3) +
# scale_fill_gradient(low="yellow",high="red")+
  scale_fill_manual(
    values = c( "#fcee21","#edbc24", "#df8b27","#d0592a","#a51a21"),na.value="white", labels = c(paste(toString(quantiles[[1]]),"-",toString(quantiles[[2]]),"%",sep=""),paste(toString(quantiles[[2]]),"-",toString(quantiles[[3]]),"%",sep=""),paste(toString(quantiles[[3]]),"-",toString(quantiles[[4]]),"%",sep=""),paste(toString(quantiles[[4]]),"-",toString(quantiles[[5]]),"%",sep=""),paste(toString(quantiles[[5]]),"-",toString(quantiles[[6]]),"%",sep="")),na.translate=FALSE) +
  labs(fill="Share") 
ggsave(filename="Maps/map_b50_di.png",p,width = 12,height=6,device = 'png')

# Mid40% income post tax
map40<-left_join(map,Ptax2[Ptax2$p=="mid40",c(1,2)],by=c("ISO"="iso"))
# map40$value<-cut(map40$value,breaks=c(0,0.30,0.35,0.40,0.45,0.60),include.lowest = T)
quantiles<-quantile(map40$value, probs = c(0,0.2,0.4,0.6,0.8,1),na.rm="TRUE")
quantiles[]<-lapply(quantiles*100,round,0)
map40$value<-cut(map40$value,breaks=c(0,quantiles[[2]]/100,quantiles[[3]]/100,quantiles[[4]]/100,quantiles[[5]]/100,1),include.lowest = T)

map40$value[map40$region=="Guinea-Bissau"]<-map40$value[map40$region=="Guinea"][1]
map40$value[map40$region=="UAE"]<-map40$value[map40$region=="Qatar"][1]
map40$value[map40$region=="Eritrea"]<-map40$value[map40$region=="Sudan"][1]
map40$value[map40$region=="Bhutan"]<-map40$value[map40$region=="Nepal"][1]
map40$value[map40$region=="Kosovo"]<-map40$value[map40$region=="Serbia"][1]
map40$value[map40$region=="Montenegro"]<-map40$value[map40$region=="Serbia"][1]
map40$value[map40$region=="Iraq"]<-map40$value[map40$region=="Iran"][1]
map40$value[map40$region=="Turkmenistan"]<-map40$value[map40$region=="Iran"][1]
map40$value[map40$region=="Libya"]<-map40$value[map40$region=="Egypt"][1]
map40$value[map40$region=="South Sudan"]<-map40$value[map40$region=="Sudan"][1]
map40$value[map40$region=="Angola"]<-map40$value[map40$region=="Namibia"][1]
map40$value[map40$region=="Somalia"]<-map40$value[map40$region=="Ethiopia"][1]
map40$value[map40$region=="Tajikistan"]<-map40$value[map40$region=="Uzbekistan"][1]
map40$value[map40$region=="Western Sahara"]<-map40$value[map40$region=="Morocco"][1]
map40$value[map40$region=="Suriname"]<-map40$value[map40$region=="Brazil"][1]
p <- ggplot() +
theme_map() +
borders(colour="black") +
geom_polygon(data = map40, aes(
x = long,
y = lat,
group = group,
fill = value),colour="white",size=0.125)+
coord_fixed(xlim = c(-130, 160), ylim = c(-50, 75), ratio = 1.3) +
# scale_fill_gradient(low="yellow",high="green4")+
  scale_fill_manual(
    values = c( "palegreen","springgreen1","springgreen3","springgreen4","darkslategrey"),na.value="white", labels = c(paste(toString(quantiles[[1]]),"-",toString(quantiles[[2]]),"%",sep=""),paste(toString(quantiles[[2]]),"-",toString(quantiles[[3]]),"%",sep=""),paste(toString(quantiles[[3]]),"-",toString(quantiles[[4]]),"%",sep=""),paste(toString(quantiles[[4]]),"-",toString(quantiles[[5]]),"%",sep=""),paste(toString(quantiles[[5]]),"-",toString(quantiles[[6]]),"%",sep="")),na.translate=FALSE) +
  labs(fill="Share") 
ggsave(filename="Maps/map_m40_di.png",p,width = 12,height=6,device = 'png')

# Top10% income
map10<- left_join(map,Ptax2[Ptax2$p=="top10",c(1,2)],by=c("ISO"="iso"))
# map10$value<-cut(map10$value,breaks=c(0.1,0.35,0.40,0.45,0.50,0.90),include.lowest = T)
quantiles<-quantile(map10$value, probs = c(0,0.2,0.4,0.6,0.8,1),na.rm="TRUE")
quantiles[]<-lapply(quantiles*100,round,0)
map10$value<-cut(map10$value,breaks=c(0,quantiles[[2]]/100,quantiles[[3]]/100,quantiles[[4]]/100,quantiles[[5]]/100,1),include.lowest = T)

map10$value[map10$region=="Guinea-Bissau"]<-map10$value[map10$region=="Guinea"][1]
map10$value[map10$region=="UAE"]<-map10$value[map10$region=="Qatar"][1]
map10$value[map10$region=="Eritrea"]<-map10$value[map10$region=="Sudan"][1]
map10$value[map10$region=="Bhutan"]<-map10$value[map10$region=="Nepal"][1]
map10$value[map10$region=="Kosovo"]<-map10$value[map10$region=="Serbia"][1]
map10$value[map10$region=="Montenegro"]<-map10$value[map10$region=="Serbia"][1]
map10$value[map10$region=="Iraq"]<-map10$value[map10$region=="Iran"][1]
map10$value[map10$region=="Turkmenistan"]<-map10$value[map10$region=="Iran"][1]
map10$value[map10$region=="Libya"]<-map10$value[map10$region=="Egypt"][1]
map10$value[map10$region=="South Sudan"]<-map10$value[map10$region=="Sudan"][1]
map10$value[map10$region=="Angola"]<-map10$value[map10$region=="Namibia"][1]
map10$value[map10$region=="Somalia"]<-map10$value[map10$region=="Ethiopia"][1]
map10$value[map10$region=="Tajikistan"]<-map10$value[map10$region=="Uzbekistan"][1]
map10$value[map10$region=="Western Sahara"]<-map10$value[map10$region=="Morocco"][1]
map10$value[map10$region=="Suriname"]<-map10$value[map10$region=="Brazil"][1]
p <- ggplot() +
theme_map() +
borders(colour="black") +
geom_polygon(data = map10, aes(
x = long,
y = lat,
group = group,

fill = value),colour="white",size=0.125)+
coord_fixed(xlim = c(-130, 160), ylim = c(-50, 75), ratio = 1.3) +
# scale_fill_gradient(low="lightblue1",high="blue4")+
  scale_fill_manual(
    values = c( "azure2", "skyblue","deepskyblue1","royalblue","navy"),na.value="white", labels = c(paste(toString(quantiles[[1]]),"-",toString(quantiles[[2]]),"%",sep=""),paste(toString(quantiles[[2]]),"-",toString(quantiles[[3]]),"%",sep=""),paste(toString(quantiles[[3]]),"-",toString(quantiles[[4]]),"%",sep=""),paste(toString(quantiles[[4]]),"-",toString(quantiles[[5]]),"%",sep=""),paste(toString(quantiles[[5]]),"-",toString(quantiles[[6]]),"%",sep="")),na.translate=FALSE) +
  labs(fill="Share") 
ggsave(filename="Maps/map_m10_di.png",p,width = 12,height=6,device = 'png')

#Wealth####

#Bot 50% wealth
map50<- left_join(map,wealth2[wealth2$p=="bot50",c(2,4)],by=c("ISO"="iso"))
# map50$value<-cut(map50$value,breaks=c(-0.05,0.015,0.035,0.065,0.1,0.18),include.lowest = T)
quantiles<-quantile(map50$value, probs = c(0,0.2,0.4,0.6,0.8,1),na.rm="TRUE")
quantiles[]<-lapply(quantiles*100,round,1)
map50$value<-cut(map50$value,breaks=c(quantiles[[1]]/100,quantiles[[2]]/100,quantiles[[3]]/100,quantiles[[4]]/100,quantiles[[5]]/100,1),include.lowest = T)

map50$value[map50$region=="Western Sahara"]<-map50$value[map50$region=="Morocco"][1]
map50$value[map50$region=="Kosovo"]<-map50$value[map50$region=="Serbia"][1]
p <- ggplot() +
  theme_map() +
  borders(colour="black") +
  geom_polygon(data = map50, aes(
    x = long,
    y = lat,
    group = group,
    
    fill = value),colour="white",size=0.125)+
  coord_fixed(xlim = c(-130, 160), ylim = c(-50, 75), ratio = 1.3) +
  # scale_fill_gradient(low="yellow",high="red")+
  scale_fill_manual(
    values = c( "#fcee21","#edbc24", "#df8b27","#d0592a","#a51a21"),na.value="white", labels = c(paste(toString(quantiles[[1]]),"-",toString(quantiles[[2]]),"%",sep=""),paste(toString(quantiles[[2]]),"-",toString(quantiles[[3]]),"%",sep=""),paste(toString(quantiles[[3]]),"-",toString(quantiles[[4]]),"%",sep=""),paste(toString(quantiles[[4]]),"-",toString(quantiles[[5]]),"%",sep=""),paste(toString(quantiles[[5]]),"-",toString(quantiles[[6]]),"%",sep="")),na.translate=FALSE) +
  labs(fill="Share") 
ggsave(filename="Maps/map_m50_w.png",p,width = 12,height=6,device = 'png')


# Mid40% wealth2
map40<- left_join(map,wealth2[wealth2$p=="mid40",c(2,4)],by=c("ISO"="iso"))
# map40$value<-cut(map40$value,breaks=c(0.1,0.20,0.25,0.30,0.35,0.60),include.lowest = T)
quantiles<-quantile(map40$value, probs = c(0,0.2,0.4,0.6,0.8,1),na.rm="TRUE")
quantiles[]<-lapply(quantiles*100,round,0)
map40$value<-cut(map40$value,breaks=c(0,quantiles[[2]]/100,quantiles[[3]]/100,quantiles[[4]]/100,quantiles[[5]]/100,1),include.lowest = T)

map40$value[map40$region=="Western Sahara"]<-map40$value[map40$region=="Morocco"][1]
map40$value[map40$region=="Kosovo"]<-map40$value[map40$region=="Serbia"][1]
p <- ggplot() +
  theme_map() +
  borders(colour="black") +
  geom_polygon(data = map40, aes(
    x = long,
    y = lat,
    group = group,
    
    fill = value),colour="white",size=0.125)+
  coord_fixed(xlim = c(-130, 160), ylim = c(-50, 75), ratio = 1.3) +
  # scale_fill_gradient(low="yellow",high="green4")+
  scale_fill_manual(
    values = c( "palegreen","springgreen1","springgreen3","springgreen4","darkslategrey"),na.value="white", labels = c(paste(toString(quantiles[[1]]),"-",toString(quantiles[[2]]),"%",sep=""),paste(toString(quantiles[[2]]),"-",toString(quantiles[[3]]),"%",sep=""),paste(toString(quantiles[[3]]),"-",toString(quantiles[[4]]),"%",sep=""),paste(toString(quantiles[[4]]),"-",toString(quantiles[[5]]),"%",sep=""),paste(toString(quantiles[[5]]),"-",toString(quantiles[[6]]),"%",sep="")),na.translate=FALSE) +
  labs(fill="Share") 
ggsave(filename="Maps/map_m40_w.png",p,width = 12,height=6,device = 'png')

# Top10% wealth2

map10<- left_join(map,wealth2[wealth2$p=="top10",c(2,4)],by=c("ISO"="iso"))
# map10$value<-cut(map10$value,breaks=c(0.3,0.55,0.60,0.65,0.75,0.90),include.lowest = T)
quantiles<-quantile(map10$value, probs = c(0,0.2,0.4,0.6,0.8,1),na.rm="TRUE")
quantiles[]<-lapply(quantiles*100,round,0)
map10$value<-cut(map10$value,breaks=c(0,quantiles[[2]]/100,quantiles[[3]]/100,quantiles[[4]]/100,quantiles[[5]]/100,1),include.lowest = T)

map10$value[map10$region=="Western Sahara"]<-map10$value[map10$region=="Morocco"][1]
map10$value[map10$region=="Kosovo"]<-map10$value[map10$region=="Serbia"][1]
p <- ggplot() +
  theme_map() +
  borders(colour="black") +
  geom_polygon(data = map10, aes(
    x = long,
    y = lat,
    group = group,
    
    fill = value),colour="white",size=0.125)+
  coord_fixed(xlim = c(-130, 160), ylim = c(-50, 75), ratio = 1.3) +
  # scale_fill_gradient(low="lightblue1",high="blue4")+
  scale_fill_manual(
    values = c( "azure2", "skyblue","deepskyblue1","royalblue","navy"),na.value="white", labels = c(paste(toString(quantiles[[1]]),"-",toString(quantiles[[2]]),"%",sep=""),paste(toString(quantiles[[2]]),"-",toString(quantiles[[3]]),"%",sep=""),paste(toString(quantiles[[3]]),"-",toString(quantiles[[4]]),"%",sep=""),paste(toString(quantiles[[4]]),"-",toString(quantiles[[5]]),"%",sep=""),paste(toString(quantiles[[5]]),"-",toString(quantiles[[6]]),"%",sep="")),na.translate=FALSE) +
  labs(fill="Share") 
ggsave(filename="Maps/map_t10_w.png",p,width = 12,height=6,device = 'png')
