
setwd('/Users/pplsuser/FAVE/FAVE-extract-thesis')
data<-read.csv('map_data_norm.csv')
listdata<-read.csv('/Users/pplsuser/FAVE/FAVE-align-thesis/all_data_exclusions.csv')
listdata$UID<-NULL
listdata$X.1<-NULL
data$File<-NULL
data$Norm_ID<-NULL
names(data)==names(listdata)
names(data)<-names(listdata)
#Before we bind we need to cycle through each speaker, identify their max ID, add that value to maptask
maxes=1:length(levels(listdata$Pseudo))
for (spk in 1:length(levels(listdata$Pseudo))){
	thisspk=listdata[listdata$Pseudo==levels(listdata$Pseudo)[spk],]
	max=max(thisspk$ID,na.rm=T)
	print(max)
	maxes[spk]<-max
}
maxes<-data.frame(cbind(levels(listdata$Pseudo),maxes))
maxes$maxes<-as.numeric(as.character(maxes$maxes))
data$ID<-as.numeric(as.character(data$ID))

newmaxes<-rep(0,length(levels(data$Pseudo)))
for (spk in 1:length(levels(data$Pseudo))){
	mymax=maxes[maxes$V1==levels(data$Pseudo)[spk],]$maxes
	if(length(mymax)>0){
	newmaxes[spk]<-mymax}
	#data[data$Pseudo==levels(data$Pseudo)[spk],]$ID<-data[data$Pseudo==unique(data$Pseudo)[spk],]$ID+mymax
}
for (spk in 1:length(levels(data$Pseudo))){
	data[data$Pseudo==levels(data$Pseudo)[spk],]$ID<-data[data$Pseudo==levels(data$Pseudo)[spk],]$ID+newmaxes[spk]
}

data<-rbind(data,listdata)
bio<- read.csv('/Users/pplsuser/FAVE/FAVE-align-thesis/Biodata_summary_social.csv')
bio[,28:30]<-bio1[,47:49]
#Need to get the social dimensions
get_dims<-function(Pseudo,dim){
	n<-length(bio[bio$Norm_ID==Pseudo,dim])
	if (n>0){
	return(as.numeric(bio[bio$Norm_ID==Pseudo,dim][1]))}
	else {return('NA')}
}
data$Pseudo<-as.character(data$Pseudo)
bio$Norm_ID<-as.character(bio$Norm_ID)
data$dim1<-sapply(data$Pseudo,'get_dims','MR1')
data$dim2<-sapply(data$Pseudo,'get_dims','MR2')
data$dim3<-sapply(data$Pseudo,'get_dims','MR3')
write.csv(data,'/Users/pplsuser/FAVE/FAVE-align-thesis/all_data_exclusions_social.csv')

spkmeans<-function(Pseudo,data,vowel,point,formant){
	if(formant==1){
return(mean(data[data$Pseudo==Pseudo&data$Vowel==vowel&data$frame==point,]$F1.norm,na.rm=T))
}
if(formant==2){
	return(mean(data[data$Pseudo==Pseudo&data$Vowel==vowel&data$frame==point,]$F2.norm,na.rm=T))
}
if(formant==0){
	return(mean(data[data$Pseudo==Pseudo&data$Vowel==vowel&data$frame==point,]$Duration,na.rm=T))
}
if(formant==12){
	return(mean(data[data$Pseudo==Pseudo&data$Vowel==vowel,]$Eucdist,na.rm=T))
}
if(formant==13){
	return(mean(data[data$Pseudo==Pseudo&data$Vowel==vowel,]$F2.dct2,na.rm=T))
}
if(formant==14){
	return(mean(data[data$Pseudo==Pseudo&data$Vowel==vowel,]$F2.dct3,na.rm=T))
}
if(formant==15){
	return(mean(data[data$Pseudo==Pseudo&data$Vowel==vowel,]$rocf1hz,na.rm=T))
}
}

#What evidence is there for apparent time change in uw F2?
uwf1<-sapply(unique(data$Pseudo),'spkmeans',data=data[data$Style=="Wordlist",],vowel="UW1",point=10,formant=1)
uwf2<-sapply(unique(data$Pseudo),'spkmeans',data=data[data$Style=="Wordlist",],vowel="UW1",point=10,formant=2)
uwdur<-sapply(unique(data$Pseudo),'spkmeans',data=data[data$Style=="Wordlist",],vowel="UW1",point=10,formant=0)
yob<-sapply(unique(data$Pseudo),'get_yob')
gen<-sapply(unique(data$Pseudo),'get_gen')
edu<-sapply(unique(data$Pseudo),'get_edu')
dim1<-sapply(unique(data$Pseudo),'get_dims',1)
dim2<-sapply(unique(data$Pseudo),'get_dims',2)
dim3<-sapply(unique(data$Pseudo),'get_dims',3)
uwlist<-data.frame(unique(data$Pseudo),yob,gen,dim1,dim2,dim3,edu,uwf1,uwf2,uwdur)
uwlist$Style="Wordlist"
uwf1<-sapply(unique(data$Pseudo),'spkmeans',data=data[data$Style=="Maptask",],vowel="UW1",point=10,formant=1)
uwf2<-sapply(unique(data$Pseudo),'spkmeans',data=data[data$Style=="Maptask",],vowel="UW1",point=10,formant=2)
uwdur<-sapply(unique(data$Pseudo),'spkmeans',data=data[data$Style=="Maptask",],vowel="UW1",point=10,formant=0)
yob<-sapply(unique(data$Pseudo),'get_yob')
gen<-sapply(unique(data$Pseudo),'get_gen')
edu<-sapply(unique(data$Pseudo),'get_edu')
dim1<-unlist(sapply(unique(data$Pseudo),'get_dims',1))
dim2<-sapply(unique(data$Pseudo),'get_dims',2)
dim3<-sapply(unique(data$Pseudo),'get_dims',3)
uwmap<-data.frame(unique(data$Pseudo),yob,gen,dim1,dim2,dim3,edu,uwf1,uwf2,uwdur)
uwmap$Style="Maptask"
uw<-rbind(uwlist,uwmap)
names(uw)[1]<-"Pseudo"
ggplot(uw[uw$Style=="Wordlist",],aes(x=dim1,y=uwf2,color=gen))+geom_text(aes(label=Pseudo))+geom_smooth(method='lm')
ggplot(uw[uw$Style=="Wordlist",],aes(x=dim2,y=uwf2,color=gen))+geom_text(aes(label=Pseudo))+geom_smooth(method='lm')
ggplot(uw[uw$Style=="Wordlist",],aes(x=dim3,y=uwf2,color=gen))+geom_text(aes(label=Pseudo))+geom_smooth(method='lm')



+facet_wrap(~Style)

#What evidence is there for apparent time change in ow F2?
owf1<-sapply(unique(data$Pseudo),'spkmeans',data=data[data$Style=="Wordlist",],vowel="OW1",point=10,formant=1)
owf2<-sapply(unique(data$Pseudo),'spkmeans',data=data[data$Style=="Wordlist",],vowel="OW1",point=10,formant=2)
owdur<-sapply(unique(data$Pseudo),'spkmeans',data=data[data$Style=="Wordlist",],vowel="OW1",point=10,formant=0)
owlist<-data.frame(unique(data$Pseudo),yob,gen,edu,owf1,owf2,owdur,dim1,dim2,dim3)
owlist$Style="Wordlist"
owf1<-sapply(unique(data$Pseudo),'spkmeans',data=data[data$Style=="Maptask",],vowel="OW1",point=10,formant=1)
owf2<-sapply(unique(data$Pseudo),'spkmeans',data=data[data$Style=="Maptask",],vowel="OW1",point=10,formant=2)
owdur<-sapply(unique(data$Pseudo),'spkmeans',data=data[data$Style=="Maptask",],vowel="OW1",point=10,formant=0)
owmap<-data.frame(unique(data$Pseudo),yob,gen,edu,owf1,owf2,owdur,dim1,dim2,dim3)
owmap$Style="Maptask"
ow<-rbind(owmap,owlist)
names(ow)[1]<-'Pseudo'
ggplot(ow,aes(x=yob,y=owf2,color=gen))+geom_point()+geom_smooth(method='lm')+facet_wrap(~Style)
ggplot(ow[ow$Style=="Wordlist",],aes(x=dim1,y=owf2,color=gen))+geom_text(aes(label=Pseudo))+geom_smooth(method='lm')
ggplot(ow[ow$Style=="Wordlist",],aes(x=dim2,y=owf2,color=gen))+geom_text(aes(label=Pseudo))+geom_smooth(method='lm')
ggplot(ow[ow$Style=="Wordlist",],aes(x=dim3,y=owf2,color=gen))+geom_text(aes(label=Pseudo))+geom_smooth(method='lm')

eucdist<-function(Pseudo,ID){
	#indexing here is a hack -- duplicate values
	x1<-data[data$Pseudo==Pseudo&data$ID==ID&data$frame==2,]$F1.norm[1]
	y1<-data[data$Pseudo==Pseudo&data$ID==ID&data$frame==2,]$F2.norm[1]
	x2<-data[data$Pseudo==Pseudo&data$ID==ID&data$frame==18,]$F1.norm[1]
	y2<-data[data$Pseudo==Pseudo&data$ID==ID&data$frame==18,]$F2.norm[1]
	return(sqrt((x1-x2)^2+(y1-y2)^2))
}

edata<-subset(data,frame==2|frame==18)
edata$Eucdist<-NA

data$Eucdist<-NA
for(sp in 1:length(unique(data$Pseudo))){
thisspk<-data[data$Pseudo==unique(data$Pseudo)[sp],]
data[data$Pseudo==unique(data$Pseudo)[sp],]$Eucdist<-unlist(mapply('eucdist',ID=thisspk$ID,Pseudo=thisspk$Pseudo))
}


for(sp in 1:length(unique(edata$Pseudo))){
thisspk<-edata[edata$Pseudo==unique(edata$Pseudo)[sp],]
edata[edata$Pseudo==unique(edata$Pseudo)[sp],]$Eucdist<-unlist(mapply('eucdist',ID=thisspk$ID,Pseudo=thisspk$Pseudo))
}
write.csv(edata,'euclidean_distances_all.csv')
edata<-read.csv('euclidean_distances_all.csv')
eyeuc<-sapply(unique(edata$Pseudo),'spkmeans',data=edata[edata$Style=="Maptask",],vowel="EY1",point=14,formant=12)
oweuc<-sapply(unique(edata$Pseudo),'spkmeans',data=edata[edata$Style=="Maptask",],vowel="OW1",point=14,formant=12)
owdur<-sapply(unique(data$Pseudo),'spkmeans',data=edata[edata$Style=="Maptask",],vowel="OW1",point=14,formant=0)
eydur<-sapply(unique(data$Pseudo),'spkmeans',data=edata[edata$Style=="Maptask",],vowel="EY1",point=14,formant=0)
dipmap<-data.frame(unique(edata$Pseudo),yob,Gender,ID,oweuc,eyeuc,owdur,eydur,as.numeric(as.character(dim1)),as.numeric(as.character(dim2)))
dipmap$Style="Maptask"

eyeuc<-sapply(unique(edata$Pseudo),'spkmeans',data=edata[edata$Style=="Wordlist",],vowel="EY1",point=14,formant=12)
oweuc<-sapply(unique(edata$Pseudo),'spkmeans',data=edata[edata$Style=="Wordlist",],vowel="OW1",point=14,formant=12)
owdur<-sapply(unique(data$Pseudo),'spkmeans',data=edata[edata$Style=="Wordlist",],vowel="OW1",point=14,formant=0)
eydur<-sapply(unique(data$Pseudo),'spkmeans',data=edata[edata$Style=="Wordlist",],vowel="EY1",point=14,formant=0)
diplist<-data.frame(unique(edata$Pseudo),yob,Gender,ID,york,oweuc,eyeuc,owdur,eydur)
diplist$Style="Wordlist"

dip<-rbind(dipmap,diplist)

age<-sapply(unique(data$Pseudo),'get_age')
york<-sapply(unique(data$Pseudo),'get_york')
ID<-sapply(unique(data$Pseudo),'get_P')
code<-sapply(unique(data$Pseudo),'get_code')
Gender<-sapply(unique(data$Pseudo),'get_gen')
yob<-sapply(unique(data$Pseudo),'get_yob')
dip<-data.frame(unique(edata$Pseudo),yob,Gender,ID,york,oweuc,eyeuc,owdur,eydur)
ec<-ggplot(dip,aes(x=oweuc,y=eyeuc))+geom_text(aes(label=age))+geom_smooth(method='lm')+theme_bw()+theme(panel.grid.major = element_line(colour = "#808080"))
ec+xlab("Normalized mean GOAT Euclidean distance")+ylab("Normalized mean FACE Euclidean distance")+ggsave("/Users/pplsuser/Desktop/Thesis talk/Presentation/ow_ey.pdf")
summary(lm(oweuc~eyeuc,data=dip))

all<-data.frame(unique(edata$Pseudo),yob,york,code,age,owf2,uwf2,oweuc,eyeuc,owdur,eydur)

all<-cbind(ow[,c(1:3,4:7)],dip[,c(6:7,10)])
all$age<-sapply(all$yob,'get_age_2')
all$p3<-ifelse(all$uwf2>all$owf2,"/u/ > /o/","/o/ \u2265 /u/")
uo<-ggplot(all,aes(x=owf2,y=uwf2))+geom_text(aes(label=age,color=p3))+geom_smooth(method='lm')+scale_color_brewer(palette='Set1')+theme_bw()+theme(panel.grid.major = element_line(colour = "#808080"))+ theme(legend.position='none')


#Do diphthongization and fronting correlate?
all[all$uwf2>1.5,]$uwf2<-1.5
all[all$owf2>1.5,]$owf2<-1.25
all[all$uwf2<0.7,]$uwf2<-0.77
all[all$owf2<0.7,]$owf2<-0.75
all<-all[!is.na(all$Style),]
ggplot(all,aes(x=owf2,y=oweuc,color=age))+geom_text(aes(label=age))+theme_bw()+theme(panel.grid.major = element_line(colour = "#808080"))
ggplot(all,aes(x=owf2,y=oweuc,color=age))+geom_text(aes(label=age))+geom_smooth(method='lm')+theme_bw()+theme(panel.grid.major = element_line(colour = "#808080"))
#Arrows showing direction of style shifting

p<-ggplot(all,aes(x=owf2,y=oweuc,color=age,group=Pseudo,frame=Style))+geom_text(aes(label=age))

all$age<-factor(all$age)
x<-lm(oweuc~owf2,data=all[all$age=="Y",])
y<-lm(oweuc~owf2,data=all[all$age=="M",])
z<-lm(oweuc~owf2,data=all[all$age=="O",])
c<-ggplot(all,aes(x=owf2,y=oweuc,group=age))+geom_text(aes(label=age))+geom_smooth(method='lm',fullrange=T,se=FALSE,aes(linetype=age))+theme_bw()+theme(panel.grid.major = element_line(colour = "#808080"))


#Cluster analysis
library(ggplot2)
library(RColorBrewer)
library('fpc')
library('stats')
library(data.table)
library(dplyr)
library(plyr)
library(TTR)
all_data<-all 
#dbscan identifies 3 groups
all_data<-all_data[!is.na(all_data$owf2),]
all_data<-all_data[!is.na(all_data$oweuc),]

x<-dbscan(cbind(all_data[all_data$Style=="Wordlist",]$owf2,all_data[all_data$Style=="Wordlist",]$oweuc),0.075)
dt <- data.table(all_data[all_data$Style=="Wordlist",],Cluster=as.factor(x$cluster), key = "Cluster")
find_hull <- function(df) df[chull(df$owf2, df$oweuc), ]
hulls <- ddply(dt, "Cluster", find_hull)

c<-ggplot(dt, aes(x=owf2, y=oweuc,fill=Cluster))+geom_text(aes(label=age),color='black')+geom_polygon(data = hulls, alpha = 0.3)+theme_bw()+theme(panel.grid.major = element_line(colour = "#808080"))+scale_color_brewer(palette='Set1')+scale_fill_brewer(palette='Set1')
c+xlab("Normalized mean GOAT F2")+ylab("Normalized mean GOOSE F2")+geom_smooth(method='lm')
d<-as.data.frame(dt)
clusters<-d[,c('Pseudo','Cluster')]
clusters[clusters$Cluster==3,]$Cluster<-2
write.csv(clusters,'clusters.csv')
#Dynamic patterns
library(gss)
data$age<-as.factor(sapply(data$YOB,'get_age_2'))
data$Gender<-as.factor(data$Gender)
uw<-subset(data,Vowel=="UW1")
uwfitf1<-ssanova(F1.norm~frame*age*Gender*Style,data=uw)
uwfitf2<-ssanova(F2.norm~frame*age*Gender*Style,data=uw)
uwgrid<-expand.grid(frame=seq(1,20,length=100),age=levels(uw$age),Gender=levels(uw$Gender),Style=levels(uw$Style))
uwgrid$F1 <- predict(uwfitf1,uwgrid,se = T)$fit
uwgrid$F1SE <- predict(uwfitf1,uwgrid,se = T)$se.fit
uwgrid$F2 <- predict(uwfitf2,uwgrid,se = T)$fit
uwgrid$F2SE <- predict(uwfitf2,uwgrid,se = T)$se.fit
formant.comparison <- ggplot(uwgrid,aes(x = frame,fill=age))
formant.comparison<-formant.comparison + geom_line(aes(y = F1),alpha = 1,colour = "grey20")
formant.comparison<-formant.comparison + geom_line(aes(y = F2),alpha = 1, colour = "grey20")
formant.comparison<-formant.comparison + geom_ribbon(aes(ymin = F1-(1.96*F1SE), ymax = F1+(1.96*F1SE)),alpha = 0.5,colour = "NA")
uwformant.comparison<-formant.comparison + geom_ribbon(aes(ymin = F2-(1.96*F2SE), ymax = F2+(1.96*F2SE)),alpha = 0.5,colour = "NA")
uwformantcomparsion<-uwformant.comparison+theme_bw()+theme(panel.grid.major = element_line(colour = "#808080"))+scale_fill_brewer(palette="Set1")+xlab("Measurement point")+ylab("F2/S(F2)")
uwformantcomparsion+facet_grid(Style~Gender)+ggtitle("GOOSE")+ggsave("/Users/pplsuser/FAVE/FAVE-align-thesis/uw_contours.pdf")

ow<-subset(data,Vowel=="OW1")
owfitf1<-ssanova(F1.norm~frame*age*Gender*Style,data=ow)
owfitf2<-ssanova(F2.norm~frame*age*Gender*Style,data=ow)
owgrid<-expand.grid(frame=seq(1,20,length=100),age=levels(ow$age),Gender=levels(ow$Gender),Style=levels(ow$Style))
owgrid$F1<- predict(owfitf1,owgrid,se = T)$fit
owgrid$F1SE <- predict(owfitf1,owgrid,se = T)$se.fit
owgrid$F2<- predict(owfitf2,owgrid,se = T)$fit
owgrid$F2SE <- predict(owfitf2,owgrid,se = T)$se.fit
formant.comparison <- ggplot(owgrid,aes(x = frame,fill=age))
formant.comparison<-formant.comparison + geom_line(aes(y = F1),alpha = 1,colour = "grey20")
formant.comparison<-formant.comparison + geom_line(aes(y = F2),alpha = 1, colour = "grey20")
formant.comparison<-formant.comparison + geom_ribbon(aes(ymin = F1-(1.96*F1SE), ymax = F1+(1.96*F1SE)),alpha = 0.5,colour = "NA")
formant.comparison<-formant.comparison + geom_ribbon(aes(ymin = F2-(1.96*F2SE), ymax = F2+(1.96*F2SE)),alpha = 0.5,colour = "NA")
formant.comparison<-formant.comparison+theme_bw()+theme(panel.grid.major = element_line(colour = "#808080"))+scale_fill_brewer(palette="Set1")+xlab("Measurement point")+ylab("F2/S(F2)")
formant.comparisoncggtitle("GOAT")+ggsave("/Users/pplsuser/FAVE/FAVE-align-thesis/ow_contours.pdf")
formant.comparison<-formant.comparison + ylab("Hz")+ylim(300,2000)

#Experimenting with polynomials
data$PreSeg[data$PrePhon=="DZ"|data$PrePhon=="Dj"|data$PrePhon=="DJ"]<-"DZ"
data$PreSeg=as.character(data$PreSeg)
data[,c("Pre_voice","Pre_place","Pre_manner")]<-t(sapply(data$PreSeg,'placemannervoice'))
data[,c("Fol_voice","Fol_place","Fol_manner")]<-t(sapply(data$FolSeg,'placemannervoice'))
data[,c("Pre_voice","Pre_place","Pre_manner")]<-t(sapply(data$PreSeg,'placemannervoice'))
data[,c("Fol_voice","Fol_place","Fol_manner")]<-t(sapply(data$FolSeg,'placemannervoice'))
class(data$Duration)
data<-data[!data$frame>20,]
t <- poly((unique(data$frame)), 3)
data[,paste("ot", 1:3, sep="")] <- t[data$frame, 1:3]
data$Age<-ifelse(data$YOB<=1960,"O",ifelse(data$YOB<=1980,"M","Y"))
fit3<-lmer(F2.norm~poly(frame,4,raw=TRUE)*PreSeg*FolSeg*Style+(1+Pseudo|frame),data=uw[uw$age=="O",])
m1.res<-step(lmer(F1 ~ (ot1+ot2+ot3)*Pre_place+(ot1+ot2+ot3)*Fol_place+(ot1+ot2+ot3)*Fol_voice+(ot1+ot2+ot3)*LogDur+(ot1+ot2+ot3 | Pseudo), control = lmerControl(optimizer="bobyqa"),data=ow, REML=F,verbose=T))
m2.null<-lmer(F2 ~ ((ot1+ot2+ot3):Pre_place)+((ot1+ot2+ot3):Pre_manner)+((ot1+ot2+ot3):Pre_voice)+((ot1+ot2+ot3):Fol_place)+((ot1+ot2+ot3):Fol_manner)+((ot1+ot2+ot3):Fol_voice)+((ot1+ot2+ot3):LogDur)+(ot1+ot2+ot3 | Pseudo:Pre_place)+(ot1+ot2+ot3 | Pseudo:Pre_manner)+(ot1+ot2+ot3 | Pseudo:Pre_voice)+(ot1+ot2+ot3 | Pseudo:Fol_place)+(ot1+ot2+ot3 | Pseudo:Fol_manner)+(ot1+ot2+ot3 | Pseudo:Fol_voice)+(ot1+ot2+ot3 | Pseudo:LogDur), control = lmerControl(optimizer="bobyqa"),data=ow, REML=F)
#Biodata functions
get_yob<-function(speaker){
	return(unique(data[data$Pseudo==speaker,]$YOB))
}
get_gen<-function(speaker){
	return(unique(data[data$Pseudo==speaker,]$Gender))
}
get_edu<-function(speaker){
	return(unique(data[data$Pseudo==speaker,]$Education))
}
get_york<-function(speaker){
	return(unique(data[data$Pseudo==speaker,]$York))
}

get_age<-function(speaker){
YOB<-unique(data[data$Pseudo==speaker,]$YOB)
return(ifelse(YOB<=1960,"O",ifelse(YOB<=1980,"M","Y")))
}

get_P<-function(speaker){
	return(unique(data[data$Pseudo==speaker,]$Pseudo))
}


get_parents_edu<-function(speaker){
	return(unique(data[data$Pseudo==speaker,]$Parents_Education))
}


get_code<-function(speaker){
	cod<-as.character(unique(data[data$Pseudo==speaker,]$Postcode))
	if(cod!="?"){
		cod<-paste(unlist(strsplit(cod,split=''))[1:4],collapse="")
	}
	return(cod)
}


get_age_2<-function(YOB){
return(ifelse(YOB<=1960,"O",ifelse(YOB<=1980,"M","Y")))
}




#loading a shapefile
setRepositories(ind=1:2)
install.packages('rgdal')
install.packages('maptools')
install.packages('RCurl')
install.packages('ggmap')
library(sp)
library(rgdal)
library(maptools)
library(RCurl)
library(ggmap)
map<-readRDS("GBR_adm2.rds")
setwd('/Users/pplsuser/FAVE/FAVE-extract-thesis/counties')
codes = readOGR(dsn=".", layer="Sectors")
codes@data$name<-as.character(codes@data$name)
york<-codes[grep('YO[1-32]',codes@data$name),]
york@data$name<-factor(york@data$name)
york<-york[grep('YO3',york@data$name),]
get_loc<-function(Pseudo){
	code<-data[data$Pseudo==Pseudo,]$Postcode[1]
	if(code!="?"){
	x<-getURL(paste('api.postcodes.io/postcodes/',as.character(code),sep=''))
	x<-strsplit(x,split="\\{")
	x<-x[[1]][3]
	x<-strsplit(x,split='"')
	long=x[[1]][21]
		lat=x[[1]][23]
			long=gsub(',','',long)
			long=as.numeric(gsub(':','',long))
			lat=gsub(',','',lat)
			lat=as.numeric(gsub(':','',lat))
				return(data.frame('Pseudo'=Pseudo,'Lat'=lat,'Long'=long))}
}
get_lat<-function(Pseudo){
	return(df[df$Pseudo==Pseudo,]$Lat[1])
}
get_long<-function(Pseudo){
	return(df[df$Pseudo==Pseudo,]$Long[1])
}
loc<-sapply(unique(data$Pseudo),'get_loc')
df<-do.call('rbind',loc)
map<-ggplot(york)
map<-get_map('York, UK',zoom=12,scale=5,source='stamen',maptype='toner')
ggmap(map,extent='panel')+geom_point(data=owframe,aes(x=Long,y=Lat,size=Eucdist,color=Eucdist))
+geom_polygon(aes(x=long,y=lat,group=group),alpha=0.25)+geom_point(data=df,aes(x=Long,y=Lat))

install.packages('rJava')
install.packages('OpenStreetMap')

owf2 <- summarySE(ow, measurevar="F2.norm", groupvars=c("Pseudo","Style"))
oweuc<- summarySE(ow, measurevar="Eucdist", groupvars=c("Pseudo","Style"))

placemannervoice<-function(seg){
	manner=NA 
	place=NA 
	voice=NA 
	if(seg=='hK'){seg="K"}
	if (length(grep('1',seg)>0)){return(c("Vowel","Vowel","Vowel"))}
		if (length(grep('2',seg)>0)){return(c("Vowel","Vowel","Vowel"))}
			if (length(grep('0',seg)>0)){return(c("Vowel","Vowel","Vowel"))}
			if (length(grep('3',seg)>0)){return(c("Vowel","Vowel","Vowel"))}
				if(seg=="A"){return(c("Vowel","Vowel","Vowel"))}
								if(seg=="q"){return(c("Vowel","Vowel","Vowel"))}
voiced<-c('G','D','B','R','N','M','JH','DH','Dj','Z')
if(seg%in%voiced){voice="Voiced"} else if(seg%in%c('?','sp','ns')){voice='glottalpause'}else {voice="Voiceless"}
orallabial<-c('B','P','V','F')
nasallabial<-c('M')
oralapical<-c('D', 'T', 'Z', 'S', 'TH', 'DH','R')
nasalapical<-c('N')
palatal<-c('ZH', 'SH', 'JH', 'CH','DZ')
velar<-c('G', 'K')
lateral<-'L'
w<-c('W')
j<-c('J','Y','Dj')
glottalpause<-c('sp','ns','?','q','HH')
if(seg%in%orallabial){place="labial"}
if(seg%in%nasallabial){place="labial"}
if(seg%in%oralapical){place="apical"}
if(seg%in%nasalapical){place="apical"}
if(seg%in%palatal){place="palatal"}
if(seg%in%velar){place="velar"}
if(seg%in%w){place="w"}
if(seg%in%j){place="j"}
if(seg%in%lateral){place="lateral"}
if(seg%in%glottalpause){place="glottalpause"}

stop<-c('B','P','K','G','T','?','D')
affricate<-c('JH','CH','DZ')
fricative<-c('S','F','V','Z','TH','DH','SH','HH')
nasal<-c('M','N')
lateral<-'L'
glide<-c('W','J','Dj','Y','R')
pause<-c('ns','sp')
if(seg%in%stop){manner="stop"}
if(seg%in%affricate){manner="affricate"}
if(seg%in%fricative){manner="fricative"}
if(seg%in%nasal){manner="nasal"}
if(seg%in%lateral){manner="lateral"}
if(seg%in%glide){manner="glide"}
if(seg%in%pause){manner="pause"}
return(c(voice,place,manner))

}

JF_codes<-function(vowel,pre,fol,preseg,folseg){

if (vowel=="UW1"){
	if ((pre=="apical"|pre=="palatal")){
		if(fol=='lateral'){return('TuwL')}
		else if(fol=='nasal'){return('TuwN')}
		else if(folseg=='sp'){return('TuwF')}
		else return('Tuw')
}
	else{
		if(fol=='lateral'){return('uwL')}
		else if(fol=='nasal'){return('uwN')}
		else if(folseg=='sp'){return('uwF')}
		else if (pre=='j'){return('j')}
		else return('uw')
	}
}
if (vowel=="OW1"){
	if (pre=='nasal'&&folseg!='sp'){return('Now')}
	else if(fol=='nasal'){return('owN')}
	else if(pre!="nasal"&&folseg=="sp"){return('owF')}
	else if(fol=='lateral'){return('owL')}
	else if(pre=='apical'|pre=='palatal'){return('Tow')}
	else if(pre=='velar'){return('Kow')}
	else return('ow')
}
}
\item{Key environments: }
\item{Juw (preceding j)}
\item{Tuw (preceding alveolar)}
\item{Kuw (preceding velar)}
\item{uw  (all other contexts)}
uw$PrePhon<-as.character(uw$PrePhon)
uw$FolPhon<-as.character(uw$FolPhon)
uw<-uw[!is.na(uw$PrePhon),]
jf_codes_new<-function(vowel,pre,fol,preseg,folseg){

if (vowel=="UW1"){
	if (preseg=="Dj"|preseg=="Tch"|preseg=="Y"|preseg=="Dz"|preseg=="DJ"|preseg=="Tch"){return('Juw')
}else{
		if(fol=='lateral'){return('uwL')}
		else if(pre=='apical'){return('Tuw')}
		else if(pre=='velar'){return('Kuw')}
		else return('uw')
	}
}
if (vowel=="OW1"){
	if (pre=='nasal'&&folseg!='sp'){return('Now')}
	else if(fol=='nasal'){return('owN')}
	else if(pre!="nasal"&&folseg=="sp"){return('owF')}
	else if(fol=='lateral'){return('owL')}
	else if(pre=='apical'|pre=='palatal'){return('Tow')}
	else if(pre=='velar'){return('Kow')}
	else return('ow')
}
}
ow$JF<-unlist(mapply('jf_codes_new',vowel=ow$Vowel,pre=ow$Pre_place,fol=ow$Fol_manner,preseg=ow$PrePhon,folseg=ow$FolSeg))

uw$JF<-unlist(mapply('jf_codes_new',vowel=uw$Vowel,pre=uw$Pre_place,fol=uw$Fol_place,preseg=uw$PrePhon,folseg=uw$FolSeg))
data$PreSeg=as.character(data$PreSeg)
data$PreSeg[data$PrePhon=="DZ"|data$PrePhon=="Dj"|data$PrePhon=="DJ"]<-"DZ"
data[,c("Pre_voice","Pre_place","Pre_manner")]<-t(sapply(data$PreSeg,'placemannervoice'))
data[,c("Fol_voice","Fol_place","Fol_manner")]<-t(sapply(data$FolSeg,'placemannervoice'))
class(data$Duration)
data[,"LogDur"]<-log(data$Duration)
data<-data[!data$frame>20,]

uw<-data[data$Vowel=="UW1"&data$frame==10,]

ow<-data[data$Vowel=="OW1"&data$frame==10,]

ow[ow$JF=="owF",]$JF<-'ow'
ow[ow$Fol_manner=='nasal',]$JF<-'ow'
cbbPalette <- c("#000000", "#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00", "#CC79A7", "#F0E442")
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ggplot(uw,aes(x=YOB,y=F2.norm,color=JF))+stat_summary(alpha=0.1)+geom_smooth(alpha=0.25,formula=y~poly(x,3),method='lm')+ylim(0.50,1.60)+xlab('Year of Birth')+ylab('Normalized F2 at midpoint')+theme_bw()+ggtitle('/u/')+labs(color='')
+ggsave("/Users/pplsuser/Desktop/Second_Year_Report_copy/Presentation/uw_phonetic_env.pdf",width=3.5,height=3.5)
ggplot(ow[ow$JF!='owL',],aes(x=YOB,y=F2.norm,color=JF))+stat_summary(alpha=0.1)+geom_smooth(alpha=0.25,formula=y~poly(x,3),method='lm')+xlab('Year of Birth')+ylab('')+theme_bw()+scale_color_manual(values=cbbPalette,breaks=c('Tow','Kow','ow'))+ggtitle('/o/')+labs(color='')+ggsave("/Users/pplsuser/Desktop/Second_Year_Report_copy/Presentation/ow_phonetic_env.pdf",width=3.5,height=3.5)
ggplot(uw,aes(x=YOB,y=F2.norm,color=JF))+stat_summary(alpha=0.1)+geom_smooth(alpha=0.25,formula=y~poly(x,3),method='lm')+ylim(0.50,1.60)+xlab('Year of Birth')+ylab('Normalized F2 at midpoint')+theme_bw()+scale_color_manual(values=cbbPalette,breaks=c('Juw','Tuw','Kuw','uw','uwL'))+ggtitle('/u/')+labs(color='')+ggsave("/Users/pplsuser/Desktop/Second_Year_Report_copy/Presentation/uw_phonetic_env.pdf",width=3.5,height=3.5)
uw$Vowel="/u/"
ow$Vowel="/o/"
ggplot(rbind(ow,uw),aes(x=YOB,y=F2.norm,color=Vowel))+stat_summary(alpha=0.1)+geom_smooth(alpha=0.25,formula=y~poly(x,3),method='lm')+xlab('Year of Birth')+ylab('Normalized F2 at midpoint')+theme_bw()+scale_color_manual(values=cbbPalette)+labs(color='')
+ggsave("/Users/pplsuser/Desktop/Second_Year_Report_copy/Presentation/ow_uw_parallel.pdf",width=5,height=4)
newgrid<-expand.grid(YOB=1935:2000)
newgrid$ow<-predict(lm(F2~poly(YOB,4),data=ow),newdata=newgrid)
newgrid$uw<-predict(lm(F2~poly(YOB,4),data=uw[uw$JF!="Juw",]),newdata=newgrid)
newgrid$owse<-predict(lm(F2~poly(YOB,4),data=ow),newdata=newgrid,se=T)$se.fit
newgrid$uwse<-predict(lm(F2~poly(YOB,4),data=uw[uw$JF!="Juw",]),newdata=newgrid,se=T)$se.fit
newgrid$uw.ROC<-ROC(newgrid$uw,1)
newgrid$ow.ROC<-ROC(newgrid$ow,1)
newgrid$uwse.ROC<-ROC(newgrid$uwse,1)
newgrid$owse.ROC<-ROC(newgrid$owse,1)
ggplot(newgrid,aes(x=YOB,y=uw.ROC))+geom_line(color="#009E73")+geom_line(aes(y=ow.ROC),color="#000000")+xlab('Year')+ylab('Est. % change')+theme_bw()+scale_color_manual(values=cbbPalette)+labs(color='')+geom_hline(yintercept=0,linetype='dotted')+ggsave("/Users/pplsuser/Desktop/Second_Year_Report_copy/Presentation/ow_uw_parallel_ROC.pdf",width=5,height=4)

ggplot(uw,aes(x=YOB,y=F2.norm,color=JF))+stat_summary(alpha=0.25)+geom_smooth(alpha=0.25,formula=y~poly(x,3),method='lm')+xlab('Year of Birth')+ylab('Normalized F2 at midpoint')+theme_bw()+scale_color_manual(values=cbbPalette)+ggtitle('/u/')+labs(color='')+ggsave("/Users/pplsuser/Desktop/Second_Year_Report_copy/Presentation/uw_phonetic_env_large.pdf",width=3.5,height=3.5)
ggplot(ow[ow$JF!='owL',],aes(x=YOB,y=F2.norm,color=JF))+stat_summary(alpha=0.25)+geom_smooth(alpha=0.25,formula=y~poly(x,3),method='lm')+xlab('Year of Birth')+ylab('')+theme_bw()+scale_color_manual(values=cbbPalette)+ggtitle('/o/')+labs(color='')+ggsave("/Users/pplsuser/Desktop/Second_Year_Report_copy/Presentation/ow_phonetic_env_large.pdf",width=3.5,height=3.5)
Tuw=loess(F2.norm~YOB,data=uw[uw$JF=='Juw',])
TuwF=loess(F2.norm~YOB,data=uw[uw$JF=='Kuw',])
TuwL=loess(F2.norm~YOB,data=uw[uw$JF=='Tuw',])
xuwx=loess(F2.norm~YOB,data=uw[uw$JF=='uw',])
uwL=loess(F2.norm~YOB,data=uw[uw$JF=='uwL',])

newgrid<-expand.grid(YOB=seq(1935,2001,1))
newgrid$Juw<-predict(Tuw,newdata=newgrid)
newgrid$Kuw<-predict(TuwF,newdata=newgrid)
newgrid$Tuw<-predict(TuwL,newdata=newgrid)
newgrid$uw<-predict(xuwx,newdata=newgrid)
newgrid$uwL<-predict(uwL,newdata=newgrid)

newgrid$Juw.ROC<-ROC(newgrid$Juw,n=2,type='continuous')
newgrid$Tuw.ROC<-ROC(newgrid$Tuw,n=2,type='continuous')
newgrid$Kuw.ROC<-ROC(newgrid$Kuw,n=2,type='continuous')
newgrid$uw.ROC<-ROC(newgrid$uw,n=2,type='continuous')
newgrid$uwL.ROC<-ROC(newgrid$uwL,n=2,type='continuous')

newgrid<-expand.grid(YOB=seq(1935,2001,1))
newgrid$uwf2<-predict(loess(F2.norm~YOB,data=uw),newdata=newgrid)
newgrid$owf2<-predict(loess(F2.norm~YOB,data=ow[ow$JF!="owL",]),newdata=newgrid)
newgrid$uwroc<-ROC(predict(loess(F2.norm~YOB,data=uw),newdata=newgrid),n=2,type='continuous')
newgrid$owroc<-ROC(predict(loess(F2.norm~YOB,data=ow[ow$JF!="owL",]),newdata=newgrid),n=2,type='continuous')
newgrid$uwroc.se<-ROC(predict(loess(F2.norm~YOB,data=uw),newdata=newgrid,se=T)$se.fit,n=2,type='continuous')
newgrid$owroc.se<-ROC(predict(loess(F2.norm~YOB,data=ow),newdata=newgrid,se=T)$se.fit,n=2,type='continuous',)
x<-melt(newgrid,id='YOB',measurevars=c('uwf2','owf2'))
ggplot(data=x[x$variable=='uwf2'|x$variable=='owf2',],aes(x=YOB,y=value,color=variable))+geom_smooth()
formant.comparison <- ggplot(newgrid,aes(x = YOB))
formant.comparison<-formant.comparison + geom_line(aes(y = uwroc),alpha = 1,colour = "grey20")
formant.comparison<-formant.comparison + geom_line(aes(y = owroc),alpha = 1, colour = "grey20")
formant.comparison<-formant.comparison + geom_ribbon(aes(ymin = uwroc-(1.96*uwroc.se), ymax = uwroc+(1.96*uwroc.se)),alpha = 0.5,colour = "NA")
formant.comparison<-formant.comparison + geom_ribbon(aes(ymin = owroc-(1.96*owroc.se), ymax = owroc+(1.96*owroc.se)),alpha = 0.5,colour = "NA")

x<-newgrid[,c('YOB','uwf2','owf2')]
x<-melt(x,id='YOB',measurevars=c('uwf2','owf2'))
ggplot(x,aes(x=YOB,y=value,color=variable))+geom_smooth()
+geom_smooth(data=newgrid,aes(x=YOB,y=uwf2))


ggplot(newgrid,aes(x=YOB,y=Tuw))+geom_smooth()+geom_smooth(aes(x=YOB,y=TuwF),color='red')+geom_smooth(aes(x=YOB,y=TuwL),color='green')+geom_smooth(aes(x=YOB,y=uw),color='purple')+geom_smooth(aes(x=YOB,y=uwF),color='yellow')+geom_smooth(aes(x=YOB,y=uwL),color='orange')

ggplot(newgrid,aes(x=YOB))+geom_smooth(aes(x=YOB,y=uwF.ROC),color='yellow')


ggplot(newgrid,aes(x=YOB))+geom_smooth(aes(x=YOB,y=Juw.ROC),color='yellow')+geom_smooth(aes(x=YOB,y=Kuw.ROC),color='blue')+geom_smooth(aes(x=YOB,y=Tuw.ROC),color='red')+geom_smooth(aes(x=YOB,y=uw.ROC),color='green')+geom_smooth(aes(x=YOB,y=uwL.ROC),color='orange')


df<-melt(grid,id=c('YOB'))
ROC<-df[grep('.ROC',df$variable),]$value
F2.norm<-df[-grep('.ROC',df$variable),]$value
df$Type<-'RAW'
df[grep('.ROC',df$variable),]$Type<-'ROC'
df$F2.norm<-F2.
f1 <- ggplot(df,aes(x=YOB,y=value))+geom_smooth()+facet_wrap(Type~variable,scales='free',drop=TRUE,ncol=6)
f1<-f1+geom_smooth(subset=.(variable=='uwF')
f1+geom_step(subset=.(variable=='Total.Members'))
+geom_smooth(aes(x=YOB,y=TuwL.ROC),color='green')
+geom_smooth(aes(x=YOB,y=uw.ROC),color='purple')
+geom_smooth(aes(x=YOB,y=uwF.ROC),color='yellow')
+geom_smooth(aes(x=YOB,y=uwL.ROC),color='orange')

#Same thing with multilevel models
library(lme4)
#Polynomial for year of birth
t <- c(unique(uw$YOB),poly(unique(uw$YOB, 4)))
uw[,paste("ot", 1:3, sep="")] <- poly(uw$YOB,3)[,1:3]

mod1<-lmer(F2.norm~(ot1+ot2+ot3)*JF+(1|Pseudo)+(1|Word),data=uw)
grid<-expand.grid(YOB=seq(1935,2001,1),JF=unique(uw$JF))
grid[,paste("ot", 1:3, sep="")] <- poly(grid$YOB,3)[,1:3]
grid$F2<-predict(mod1,newdata=grid,re.form=NA)
grid$ROC<-ROC(grid$F2,n=1,type='continuous')

#Trying all contexts
#Polynomial for year of birth
t <- c(unique(uw$YOB),poly(unique(uw$YOB, 4)))
uw[,paste("ot", 1:3, sep="")] <- poly(uw$YOB,3)[,1:3]

mod1<-lmer(F2.norm~(ot1+ot2+ot3)*JF+(1|Pseudo)+(1|Word),data=uw)
grid<-expand.grid(YOB=seq(1935,2001,1),Pre_place=unique(uw$Pre_place))
grid[,paste("ot", 1:3, sep="")] <- poly(grid$YOB,3)[,1:3]
grid$F2<-predict(mod1,newdata=grid,re.form=NA)
grid$ROC<-ROC(grid$F2,n=1,type='continuous')
 year of birth
t <- c(unique(ow$YOB),poly(unique(ow$YOB, 4)))
ow[,paste("ot", 1:3, sep="")] <- poly(ow$YOB,3)[,1:3]

mod1<-lmer(F2.norm~(ot1+ot2+ot3)*JF+(1|Pseudo)+(1|Word),data=ow)
owgrid<-expand.grid(YOB=seq(1935,2001,1),JF=unique(ow$JF))
owgrid[,paste("ot", 1:3, sep="")] <- poly(owgrid$YOB,3)[,1:3]
owgrid$F2<-predict(mod1,newdata=owgrid,re.form=NA)
owgrid$ROC<-ROC(owgrid$F2,n=1,type='continuous')


ggplot(owgrid,aes(y=ROC,x=YOB,color=))+geom_smooth()

ggplot(owgrid,aes(y=F2,x=YOB,color=JF))+geom_smooth()
ggplot(owgrid,aes(y=F2,x=YOB,color=JF))+geom_smooth()

ggplot(grid,aes(y=F2,x=YOB,color=JF))+geom_smooth()

ggplot(grid,aes(y=ROC,x=YOB,color=JF))+geom_smooth(se=F)


grid$Vowel="UW"
owgrid$Vowel="OW"

all<-rbind(grid,owgrid)
all[all$JF=="TuwL",]$JF<-'Tuw'
all[all$JF=="TuwF",]$JF<-'Tuw'
all[all$JF=="uwF",]$JF<-'uw'
ggplot(all,aes(y=F2,x=YOB,color=JF))+geom_smooth()+facet_wrap(~Vowel)
ggplot(all,aes(y=F2,x=YOB,color=Vowel))+geom_smooth()



#What linguistic constraints apply to each cluster of speakers?
head(clusters)
get_clust<-function(Pseudo){
	return(clusters[clusters$Pseudo==Pseudo,]$Cluster[1])
}
data$Cluster<-sapply(data$Pseudo,'get_clust')
ow<-data[data$Vowel=="OW1",]
uw<-data[data$Vowel=='UW1',]

ow$JF<-unlist(mapply('jf_codes',vowel=ow$Vowel,pre=ow$Pre_place,fol=ow$Fol_place,preseg=ow$PreSeg,folseg=ow$FolSeg))

uw$JF<-unlist(mapply('jf_codes',vowel=uw$Vowel,pre=uw$Pre_place,fol=uw$Fol_place,preseg=uw$PreSeg,folseg=uw$FolSeg))

#Linguistic constraints by group
library(lme4)
t <- c(unique(ow$frame),poly(unique(ow$frame, 3)))
ow[,paste("ot", 1:3, sep="")] <- poly(ow$frame,3)[,1:3]

mod1<-lmer(F2~(ot1+ot2+ot3)*JF*Cluster*Style+(1|Pseudo)+(1|Word),data=ow)
mod2<-lmer(F1~(ot1+ot2+ot3)*JF*Cluster*Style+(1|Pseudo)+(1|Word),data=ow)
owgrid<-expand.grid(frame=seq(1,20,1),JF=unique(ow$JF),Cluster=unique(ow$Cluster),Style=unique(ow$Style))
t <- c(unique(ow$frame),poly(unique(ow$frame, 3)))
owgrid[,paste("ot", 1:3, sep="")] <- poly(owgrid$frame,3)[,1:3]
owgrid$F2<-predict(mod1,newdata=owgrid,re.form=NA)
owgrid$F1<-predict(mod2,newdata=owgrid,re.form=NA)
ggplot(owgrid,aes(y=F2,x=frame,color=JF))+geom_smooth()+geom_smooth(data=owgrid,aes(y=F1,x=frame))+facet_grid(Style~Cluster)

#Random thought -- do polys fit apparent time data better than linear?
mod1<-lmer(F2.norm~YOB*JF+(JF|Pseudo)+(1|Word),data=uw)
mod2<-lmer(F2.norm~YOB+JF+(JF|Pseudo)+(1|Word),data=uw)
mod3<-lmer(F2.norm~poly(YOB,3)+JF+(JF|Pseudo)+(1|Word),data=uw)
anova(mod2,mod3)

anova(mod1,mod3)
mod1<-lmer(F2.norm~YOB*JF+(JF|Pseudo)+(1|Word),data=ow,reml=F)
mod2<-lmer(F2.norm~YOB+JF+(JF|Pseudo)+(1|Word),data=ow,reml=F)
mod3<-lmer(F2.norm~poly(YOB,2)+JF+(JF|Pseudo)+(1|Word),data=ow,reml=F)
anova(mod2,mod3)

#Nope! But consistenly lowers AIC

#Animated plots for intro

data<-data[!data$frame>20,]
t <- poly((unique(data$frame)), 3)
data[,paste("ot", 1:3, sep="")] <- t[data$frame, 1:3]
data$Age<-ifelse(data$YOB<=1960,"O",ifelse(data$YOB<=1980,"M","Y"))
ow=data[data$Vowel=="OW1",]
uw=data[data$Vowel=="UW1",]
uw<-uw[!is.na(uw$FolPhon),]
ow$JF<-unlist(mapply('jf_codes_new',vowel=ow$Vowel,pre=ow$Pre_place,fol=ow$Fol_manner,preseg=ow$PrePhon,folseg=ow$FolSeg))
uw$JF<-unlist(mapply('jf_codes_new',vowel=uw$Vowel,pre=uw$Pre_place,fol=uw$Fol_place,preseg=uw$PrePhon,folseg=uw$FolSeg))

mod1<-lmer(F2~(ot1+ot2+ot3):YOB+(ot1+ot2+ot3|Pseudo)+(ot1+ot2+ot3|Word),data=ow,control = lmerControl(optimizer="bobyqa"),data=ow, REML=F)
mod2<-lmer(F1~(ot1+ot2+ot3):YOB+(ot1+ot2+ot3|Pseudo)+(ot1+ot2+ot3|Word),data=ow,control = lmerControl(optimizer="bobyqa"),data=ow, REML=F)


mod1<-lm(F2~(ot1+ot2+ot3)*YOB,data=ow)
mod2<-lm(F1~(ot1+ot2+ot3)*YOB,data=ow)

owgrid<-expand.grid(frame=seq(1,20,1),YOB=seq(1940,2001,1))
t <- c(unique(ow$frame),poly(unique(ow$frame, 3)))
owgrid[,paste("ot", 1:3, sep="")] <- poly(owgrid$frame,3)[,1:3]
owgrid$F2<-predict(mod1,newdata=owgrid,re.form=NA)
owgrid$F1<-predict(mod2,newdata=owgrid,re.form=NA)
for (year in unique(owgrid$YOB)){
print(ggplot(owgrid[owgrid$YOB==year,],aes(x=frame,y=F2))+geom_smooth()+geom_smooth(aes(x=frame,y=F1)))
}

mod1<-lmer(F2~(ot1+ot2+ot3)*YOB*JF+(1|Pseudo)+(1|Word),data=uw)
mod2<-lmer(F1~(ot1+ot2+ot3)*YOB*JF+(1|Pseudo)+(1|Word),data=uw)

owgrid<-expand.grid(frame=seq(1,20,1),JF=unique(ow$JF),YOB=seq(1940,2001,1))
t <- c(unique(ow$frame),poly(unique(ow$frame, 3)))
owgrid[,paste("ot", 1:3, sep="")] <- poly(owgrid$frame,3)[,1:3]
owgrid$F2<-predict(mod1,newdata=owgrid,re.form=NA)
owgrid$F1<-predict(mod2,newdata=owgrid,re.form=NA)
for (year in unique(owgrid$YOB)){
print(ggplot(owgrid[owgrid$YOB==year,],aes(x=frame,y=F2,color=JF))+geom_smooth()+geom_smooth(aes(x=frame,y=F1)))
}

uwgrid<-expand.grid(frame=seq(1,20,1),JF=unique(uw$JF),YOB=seq(1940,2001,1))
t <- c(unique(uw$frame),poly(unique(uw$frame, 3)))
uwgrid[,paste("ot", 1:3, sep="")] <- poly(uwgrid$frame,3)[,1:3]
uwgrid$F2<-predict(mod1,newdata=uwgrid,re.form=NA)
uwgrid$F1<-predict(mod2,newdata=uwgrid,re.form=NA)
for (year in unique(uwgrid$YOB)){
print(ggplot(uwgrid[uwgrid$YOB==year,],aes(x=frame,y=F2,color=JF))+geom_smooth()+geom_smooth(aes(x=frame,y=F1)))
}

#Experiments with DCT transform
library(emuR)
#Let's get one vowel
ow<-data[data$Vowel=='OW1',]
vowel<-ow[ow$ID==73&ow$Interval==29,]
dct2<-dct(vowel$F2,fit=F,m=4)
dct2fit<-dct(vowel$F2,fit=T,m=4)
dct1<-dct(vowel$F1,fit=F,m=4)
dct1fit<-dct(vowel$F1,fit=T,m=4)
fit<-as.data.frame(cbind(dct1fit,dct2fit))
fit$t<-1:20
ggplot(fit,aes(x=t,y=dct2fit))+geom_line()+geom_line(aes(y=dct1fit))
#OK cool
#Now iterate through dataset(by vowel) and generate dtc coefficients
get_dct<-function(data,pseudo,ID,formant,m){
vowel<-data[data$Pseudo==pseudo&data$ID==ID,]
if (formant==1){
vec<-vowel$F1
}
if(formant==2){
vec<-vowel$F2
}
if (m>1&m<(length(vec)-1)){
coeffs<-dct(vec,fit=F,m=m)
fit<-dct(vec,fit=T,m=m)}else{
	coeffs<-rep(NA,m)
	fit<-rep(NA,length(vec))
}
return(list(coeffs,fit))
}


data<-data[!data$frame>20,]
data$F1.dct1<-NA
data$F1.dct2<-NA
data$F1.dct3<-NA
data$F1.dct4<-NA
data$F1.dctfit<-NA
data$F2.dct1<-NA
data$F2.dct2<-NA
data$F2.dct3<-NA
data$F2.dct4<-NA
data$F2.dctfit<-NA

for(pseudo in unique(data$Pseudo)){
	thisspk<-data[data$Pseudo==pseudo,]
	for(ID in unique(thisspk$ID)){
		coeffs<-try(get_dct(data=thisspk,pseudo=pseudo,ID=ID,formant=1,m=4),silent=T)[1]
		if ('try-error' %in% class(coeffs)) next
		else{
		thisspk[thisspk$Pseudo==pseudo&thisspk$ID==ID,]$F1.dct1<-coeffs[[1]][1]
				thisspk[thisspk$Pseudo==pseudo&thisspk$ID==ID,]$F1.dct2<-coeffs[[1]][2]
						thisspk[thisspk$Pseudo==pseudo&thisspk$ID==ID,]$F1.dct3<-coeffs[[1]][3]
						thisspk[thisspk$Pseudo==pseudo&thisspk$ID==ID,]$F1.dct4<-coeffs[[1]][4]
						thisspk[thisspk$Pseudo==pseudo&thisspk$ID==ID,]$F1.dctfit<-unlist(get_dct(data=thisspk,pseudo=pseudo,ID=ID,formant=1,m=4)[2])}
		coeffs<-try(get_dct(data=thisspk,pseudo=pseudo,ID=ID,formant=2,m=4),silent=T)[1]
		if ('try-error' %in% class(coeffs)) next
		else{
		thisspk[thisspk$Pseudo==pseudo&thisspk$ID==ID,]$F2.dct1<-coeffs[[1]][1]
				thisspk[thisspk$Pseudo==pseudo&thisspk$ID==ID,]$F2.dct2<-coeffs[[1]][2]
						thisspk[thisspk$Pseudo==pseudo&thisspk$ID==ID,]$F2.dct3<-coeffs[[1]][3]
						thisspk[thisspk$Pseudo==pseudo&thisspk$ID==ID,]$F2.dct4<-coeffs[[1]][4]
						thisspk[thisspk$Pseudo==pseudo&thisspk$ID==ID,]$F2.dctfit<-unlist(get_dct(data=thisspk,pseudo=pseudo,ID=ID,formant=2,m=4)[2])}
	}
data[data$Pseudo==pseudo,]<-thisspk
}
data$F1Hz.dct1<-NA
data$F1Hz.dct2<-NA
data$F1Hz.dct3<-NA
data$F1Hz.dct4<-NA
data$F1Hz.dctfit<-NA
data$F2Hz.dct1<-NA
data$F2Hz.dct2<-NA
data$F2Hz.dct3<-NA
data$F2Hz.dct4<-NA
data$F2Hz.dctfit<-NA

for(pseudo in unique(data$Pseudo)){
	thisspk<-data[data$Pseudo==pseudo,]
	for(ID in unique(thisspk$ID)){
		coeffs<-try(get_dct(data=thisspk,pseudo=pseudo,ID=ID,formant=1,m=4),silent=T)[1]
		if ('try-error' %in% class(coeffs)) next
		else{
		thisspk[thisspk$Pseudo==pseudo&thisspk$ID==ID,]$F1Hz.dct1<-coeffs[[1]][1]
				thisspk[thisspk$Pseudo==pseudo&thisspk$ID==ID,]$F1Hz.dct2<-coeffs[[1]][2]
						thisspk[thisspk$Pseudo==pseudo&thisspk$ID==ID,]$F1Hz.dct3<-coeffs[[1]][3]
						thisspk[thisspk$Pseudo==pseudo&thisspk$ID==ID,]$F1Hz.dct4<-coeffs[[1]][4]
						thisspk[thisspk$Pseudo==pseudo&thisspk$ID==ID,]$F1Hz.dctfit<-unlist(get_dct(data=thisspk,pseudo=pseudo,ID=ID,formant=1,m=4)[2])}
		coeffs<-try(get_dct(data=thisspk,pseudo=pseudo,ID=ID,formant=2,m=4),silent=T)[1]
		if ('try-error' %in% class(coeffs)) next
		else{
		thisspk[thisspk$Pseudo==pseudo&thisspk$ID==ID,]$F2Hz.dct1<-coeffs[[1]][1]
				thisspk[thisspk$Pseudo==pseudo&thisspk$ID==ID,]$F2Hz.dct2<-coeffs[[1]][2]
						thisspk[thisspk$Pseudo==pseudo&thisspk$ID==ID,]$F2Hz.dct3<-coeffs[[1]][3]
						thisspk[thisspk$Pseudo==pseudo&thisspk$ID==ID,]$F2Hz.dct4<-coeffs[[1]][4]
						thisspk[thisspk$Pseudo==pseudo&thisspk$ID==ID,]$F2Hz.dctfit<-unlist(get_dct(data=thisspk,pseudo=pseudo,ID=ID,formant=2,m=4)[2])}
	}
data[data$Pseudo==pseudo,]<-thisspk
}
write.csv(data,'data_all_dct.csv')
library(ggplot2)
data<-read.csv('data_all_dct.csv')
ggplot(data[data$Vowel=='UW1'&data$F2.dct1<2.5&data$F2.dct1>1.0,],aes(x=YOB,y=F2.dct1))+stat_summary()+geom_smooth(method='lm')
ggplot(data[data$Vowel=='UW1'&data$F2.dct1<2.5&data$F2.dct1>1.0,],aes(x=YOB,y=F2.dct2))+stat_summary()+geom_smooth(method='lm')
ggplot(data[data$Vowel=='UW1'&data$F2.dct1<2.5&data$F2.dct1>1.0,],aes(x=YOB,y=F2.dct3))+stat_summary()+geom_smooth(method='lm')

ggplot(data[data$Vowel=='UW1',],aes(x=YOB,y=F2.dct1))+stat_summary()+geom_smooth(method='lm')
ggplot(data[data$Vowel=='UW1',],aes(x=YOB,y=F2.dct2))+stat_summary()+geom_smooth(method='lm')
ggplot(data[data$Vowel=='UW1',],aes(x=YOB,y=F2.dct3))+stat_summary()+geom_smooth(method='lm')


ggplot(data[data$Vowel=='OW1',],aes(x=YOB,y=F2.dct1))+stat_summary()+geom_smooth(method='lm')
ggplot(data[data$Vowel=='OW1',],aes(x=YOB,y=F2.dct2))+stat_summary()+geom_smooth(method='lm')
ggplot(data[data$Vowel=='OW1',],aes(x=YOB,y=F2.dct3))+stat_summary()+geom_smooth(method='lm')

ggplot(data[data$Vowel=='OW1',],aes(x=F2.dct1,y=F2.dct2,group=Speaker,z=F2.dct3))+stat_summary_2d()

library('Rmisc')
ow<-summarySE(data[data$Vowel=='OW1',], measurevar="F2.dct1", groupvars=c("Pseudo","Style","YOB","Gender"))
ow2<-summarySE(data[data$Vowel=='OW1',], measurevar="F2.dct2", groupvars=c("Pseudo","Style","YOB","Gender"))
ow3<-summarySE(data[data$Vowel=='OW1',], measurevar="F2.dct3", groupvars=c("Pseudo","Style","YOB","Gender"))
ow4<-summarySE(data[data$Vowel=='OW1',], measurevar="F2.dct3", groupvars=c("Pseudo","Style","YOB","Gender"))

ow_all<-cbind(ow,ow2,ow3)
names(ow_all)<-gsub('F2','OW_F2',names(ow_all))
ggplot(ow_all[ow_all$Style=="Wordlist",],aes(x=F2.dct1,y=F2.dct2))+geom_text(aes(label=Age))
+geom_text(aes(label=Pseudo))+facet_wrap(~Style)

uw<-summarySE(data[data$Vowel=='UW1',], measurevar="F2.dct1", groupvars=c("Pseudo","Style","YOB","Gender"))
uw2<-summarySE(data[data$Vowel=='UW1',], measurevar="F2.dct2", groupvars=c("Pseudo","Style","YOB","Gender"))
uw3<-summarySE(data[data$Vowel=='UW1',], measurevar="F2.dct3", groupvars=c("Pseudo","Style","YOB","Gender"))
uw_all<-cbind(uw,uw2,uw3)
names(uw_all)<-gsub('F2','UW_F2',names(uw_all))

all<-cbind(uw_all,ow_all)

cbbPalette <- c("#000000", "#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00", "#CC79A7", "#F0E442")
ggplot(all,aes(x=YOB,y=OW_F2.dct1,color=Gender,linetype=Gender))+geom_point()+geom_smooth(method='lm',formula=y~poly(x,3))
ggplot(all[all$Style=='Maptask',],aes(x=YOB,y=OW_F2.dct2,color=Gender))+geom_text(aes(label=Pseudo),alpha=0.5)+geom_smooth(method='lm',formula=y~poly(x,2),se=F)+xlim(1930,2005)+theme_bw()+ylab('/o/ : Second DCT coefficient')+scale_color_manual(values=cbbPalette)+ggsave('/Users/pplsuser/Desktop/Second_Year_Report_copy/Presentation/OW_DCT_2.pdf',height=5,width=7)
ggplot(all,aes(x=YOB,y=OW_F2.dct3,color=Gender))+geom_point()+geom_smooth(method='lm')


ggplot(all,aes(x=YOB,y=UW_F2.dct1,color=Gender))+geom_point()+geom_smooth(method='lm',formula=y~poly(x,3))
ggplot(all,aes(x=YOB,y=UW_F2.dct2,color=Gender))+geom_point()+geom_smooth(method='lm',formula=y~poly(x,4))
ggplot(all,aes(x=YOB,y=UW_F2.dct3,color=Gender))+geom_point()+geom_smooth(method='lm')


ggplot(all[all$OW_F2.dct1<2,],aes(x=YOB,y=OW_F2.dct3))+geom_point()

ggplot(all[all$OW_F2.dct1<2,],aes(x=YOB,y=UW_F2.dct1))+geom_point()+geom_smooth()
ggplot(all[all$OW_F2.dct1<2,],aes(x=YOB,y=UW_F2.dct2))+geom_point()+geom_smooth()
ggplot(all[all$OW_F2.dct1<2&all$OW_F2.dct1>1,],aes(x=YOB,y=UW_F2.dct3))+geom_point()+geom_smooth()

ggplot(all[all$OW_F2.dct1<2,],aes(x=OW_F2.dct1,y=OW_F2.dct2))+geom_text(aes(label=Pseudo))
library(scatterplot3d)
s3d<-scatterplot3d(all$YOB,   # x axis
                 all$UW_F2.dct3,     # y axis
                 all$UW_F2.dct1,    # z axis
                 )


ggplot(all[all$OW_F2.dct1<2&all$Style=='Maptask',],aes(x=YOB,y=OW_F2.dct2,group=Gender,color=Gender))+geom_text(aes(label=Pseudo))+geom_smooth(method='lm',formula=y~(poly(x,3)))
ggplot(all[all$OW_F2.dct1<2&all$Style=='Maptask',],aes(x=YOB,y=OW_F2.dct1,group=Gender,color=Gender))+geom_text(aes(label=Pseudo))+geom_smooth(method='lm',formula=y~(poly(x,3)))
ggplot(all[all$OW_F2.dct1<2&all$Style=='Maptask',],aes(x=YOB,y=OW_F2.dct3,group=Gender,color=Gender))+geom_text(aes(label=Pseudo))+geom_smooth(method='lm',formula=y~(poly(x,3)))

ggplot(all[all$OW_F2.dct1<2&all$Style=='Maptask',],aes(x=YOB,y=UW_F2.dct2,group=Gender,color=Gender))+geom_text(aes(label=Pseudo))+geom_smooth(method='lm',formula=y~(poly(x,3)))
ggplot(all[all$OW_F2.dct1<2&all$Style=='Maptask',],aes(x=YOB,y=UW_F2.dct1,group=Gender,color=Gender))+geom_text(aes(label=Pseudo))+geom_smooth(method='lm',formula=y~(poly(x,3)))
ggplot(all[all$OW_F2.dct1<2&all$Style=='Maptask',],aes(x=YOB,y=UW_F2.dct3,group=Gender,color=Gender))+geom_text(aes(label=Pseudo))+geom_smooth(method='lm',formula=y~(poly(x,3)))

ggplot(all[all$OW_F2.dct1<2&all$Style=='Maptask',],aes(x=UW_F2.dct1,y=OW_F2.dct1))+geom_point()+geom_smooth(method='lm')+xlim(1,2)+ylim(1,2)
ggplot(all[all$OW_F2.dct1<2&all$OW_F2.dct1>1&all$Style=='Wordlist',],aes(x=OW_F2.dct1,y=OW_F2.dct2))+geom_text(aes(label=Pseudo))+geom_smooth()
+xlim(1,2)+ylim(1,2)

#MEMs

library(lme4)
ow<-data[data$Vowel=='OW1',]
uw<-data[data$Vowel=='UW1',]
ow$JF<-unlist(mapply('jf_codes_new',vowel=ow$Vowel,pre=ow$Pre_place,fol=ow$Fol_manner,preseg=ow$PrePhon,folseg=ow$FolSeg))
uw$JF<-unlist(mapply('jf_codes_new',vowel=uw$Vowel,pre=uw$Pre_place,fol=uw$Fol_manner,preseg=uw$PreSeg,folseg=uw$FolSeg))

mod0<-lmer(F2.dct1~YOB+Pre_place+Fol_place+LogDur+(1|Pseudo)+(1|Word),data=uw)
mod1<-lmer(F2.dct1~(YOB*Pre_place)+(YOB*Fol_place)+LogDur+(1|Pseudo)+(1|Word),data=uw)
anova(mod0,mod1)
mod1<-lmer(F2.dct1~(YOB*Pre_place)+(YOB*Fol_place)+LogDur+(1|Pseudo)+(1|Word),data=uw)
mod0<-lmer(F2.dct2~YOB+Pre_place+Fol_place+LogDur+(1|Pseudo)+(1|Word),data=uw)
mod2<-lmer(F2.dct2~(YOB*Pre_place)+(YOB*Fol_place)+LogDur+(1|Pseudo)+(1|Word),data=uw)
mod0<-lmer(F2.dct3~YOB+Pre_place+Fol_place+LogDur+(1|Pseudo)+(1|Word),data=uw)
mod3<-lmer(F2.dct3~(YOB*Pre_place)+(YOB*Fol_place)+LogDur+(1|Pseudo)+(1|Word),data=uw)
anova(mod0,mod2)
anova(mod0,mod3)
grid<-expand.grid(YOB=1935:2001,Gender=unique(uw$Gender),Pre_place=unique(uw$Pre_place),Fol_place=unique(uw$Fol_place),LogDur=min(uw$LogDur):max(uw$LogDur))
grid$F2.dct1<-predict(mod1,newdata=grid,re.form=NA)
grid$F2.dct2<-predict(mod2,newdata=grid,re.form=NA)
grid$F2.dct3<-predict(mod3,newdata=grid,re.form=NA)
ggplot(grid,aes(x=YOB,y=F2.dct1))+geom_smooth()+geom_smooth(aes(x=YOB,y=F2.dct2),color='red')+geom_smooth(aes(x=YOB,y=F2.dct3),color='black')
x<-melt(uw[uw$frame==10,c('Pseudo','YOB','Gender','Pre_place','Fol_place','LogDur','F2.dct1','F2.dct2','F2.dct3')],id.vars=c('Pseudo','YOB','Gender','Pre_place','Fol_place','LogDur'),measurevars=c('F2.dct1','F2.dct2','F2.dct3'))
ggplot(x,aes(x=YOB,linetype=Gender,y=value))+geom_smooth(method='lm',formula=y~(poly(x,3)))+facet_grid(variable~.,scales='free',space='free')

x<-melt(ow[ow$frame==10,c('Pseudo','YOB','Gender','Pre_place','Fol_place','LogDur','F2.dct1','F2.dct2','F2.dct3')],id.vars=c('Pseudo','YOB','Gender','Pre_place','Fol_place','LogDur'),measurevars=c('F2.dct1','F2.dct2','F2.dct3'))
ggplot(x,aes(x=YOB,linetype=Gender,y=value))+geom_smooth(method='lm')+facet_grid(variable~.,scales='free',space='free')


mod2<-lmer(F2.dct2~YOB*Pre_place+LogDur+(1|Pseudo)+(1|Word),data=uw)
mod3<-lmer(F2.dct2~(poly(YOB,2):Gender)+(poly(YOB,2):Pre_place)+LogDur+(1|Pseudo)+(1|Word),data=uw)
mod4<-lmer(F2.dct2~poly(YOB,2)*Pre_place+Gender+LogDur+(1|Pseudo)+(1|Word),data=uw)
grid<-expand.grid(YOB=1935:2001,Gender=unique(uw$Gender),Pre_place=unique(uw$Pre_place),LogDur=min(uw$LogDur):max(uw$LogDur))
grid$F2.dct2<-predict(mod3,newdata=grid,re.form=NA)
ggplot(grid,aes(x=YOB,y=F2.dct1))+geom_smooth()+geom_point(data=all[all$OW_F2.dct1<2,],aes(x=YOB,y=UW_F2.dct1))
+stat_summary(data=grid,aes(x=YOB,y=F2.dct2))


#Understanding DCT coefficients
#This is a function for reconstructing the signal from dct
library(ggplot2)
get_cosines<-function(transdat,m){
    data <- transdat
    ldat=20
    transdat <- vector(length = ldat)
    if(is.null(m)){
      m <- 1:(ldat - 1)}
    else{
      m <- 1:m
    for (n in 0:(ldat - 1)) {
      transdat[n + 1] <- (1/sqrt(2)) * data[1] * cos((pi * 0 * (2 * n + 1))/(2 * ldat)) + sum(data[m + 1] * cos((pi * m * (2 * n + 1))/(2 * ldat)))
    }
  }
  transdat
}
x<-data[data$Vowel=='OW1'&data$Pseudo=='DA_M_1980_4'&data$ID==73,]$F2.norm
a<-dct(x)
y=1:20
m=19
cozero<-1:length(a)
    for (n in 0:(length(x) - 1)) {
      cozero[n + 1] <- (1/sqrt(2)) * a[1]* cos((pi * 0 * (2 * n + 1))/(2 * length(x)-1)) }
co1<-1:length(a)
    for (n in 0:(length(x) - 1)) {
      co1[n + 1] <- (1/sqrt(2)) * a[1]* cos((pi * 0 * (2 * n + 1))/(2 * length(x)-1))+ sum(a[m + 1] * cos((pi * m * (2 * n + 1))/(2 * length(x)-1)))}

n<-1:20
cozero<-(1/sqrt(2)) * a[1]* cos((pi * 0 * (2 * n + 1))/(2 * length(x)-1))
co1<-(1/sqrt(2))*a[2] * cos((19 * (2 * n + 1))/(2 * 20))
co2<-(1/sqrt(2))*a[3] * cos((19 * (2 * n + 1))/(2 * 20))
b<-dct(x,fit=T)

edata$dim1<-as.numeric(edata$dim1)
edata$dim2<-as.numeric(as.character(edata$dim2))
ow<-edata[edata$Vowel=="OW1",]
ggplot(ow,aes(x=dim1,y=Eucdist))+stat_summary()+geom_smooth(method='lm')
ggplot(ow,aes(x=dim2,y=Eucdist))+stat_summary()+geom_smooth(method='lm')
ggplot(ow,aes(x=dim1,y=F2.norm))+stat_summary()+geom_smooth(method='lm')+ylim(0.5,1.5)
ggplot(ow,aes(x=dim2,y=F2.norm))+stat_summary()+geom_smooth(method='lm')+ylim(0.5,1.5)
library(lme4)
owmod<-lmer(F2.norm~(PreSeg+FolSeg+Duration)+YOB+dim1+dim2+Style+(1|Pseudo)+(1|Word),data=ow[ow$frame==2,])
owmod2<-lmer(F2.norm~(PreSeg+FolSeg+Duration)+YOB+dim1+dim2+(1|Pseudo)+(1|Word),data=ow[ow$frame==2,])
d=ow[ow$frame==2,]
owmod<-lmer(Eucdist~(PreSeg+FolSeg+Duration)+YOB+dim1+dim2+Style+(1|Pseudo)+(1|Word),data=ow[ow$frame==2,])
owmod2<-lmer(Eucdist~(PreSeg+FolSeg+Duration)+YOB+dim1+dim2+(1|Pseudo)+(1|Word),data=ow[ow$frame==2,])
owmod3<-lmer(Eucdist~(PreSeg+FolSeg+Duration)+YOB+dim1+Style+(1|Pseudo)+(1|Word),data=ow[ow$frame==2,])
owmod3<-lmer(Eucdist~(PreSeg+FolSeg+Duration)+YOB+dim2+Style+(1|Pseudo)+(1|Word),data=ow[ow$frame==2,]
newdata<-expand.grid(PreSeg=levels(owmod@frame$PreSeg),FolSeg=levels(owmod@frame$FolSeg),Duration=min(d$Duration):max(d$Duration),YOB=1935:2000,dim1=min(d$dim1,na.rm=T):max(d$dim1,na.rm=T),dim2=min(d$dim2,na.rm=T):max(d$dim2,na.rm=T),Style=levels(d$Style))
newdata$Eucdist<-predict(owmod,newdata=newdata,re.form=NA)
ggplot(newdata,aes(x=dim1,y=Eucdist))+geom_smooth(method='lm')
ggplot(newdata,aes(x=dim2,y=Eucdist))+geom_smooth(method='lm')

uw<-na.omit(data[data$Vowel=="UW1",])
ggplot(uw,aes(x=dim1,y=Eucdist))+stat_summary()+geom_smooth(method='lm')
ggplot(uw,aes(x=dim2,y=Eucdist))+stat_summary()+geom_smooth(method='lm')
ggplot(uw,aes(x=dim1,y=F2.norm))+stat_summary()+geom_smooth(method='lm')+ylim(0.5,1.5)
ggplot(uw,aes(x=dim2,y=F2.norm))+stat_summary()+geom_smooth(method='lm')+ylim(0.5,1.5)
uwmod<-lmer(F2.norm~(PreSeg+FolSeg+Duration)+YOB+dim1+dim2+(1|Speaker)+(1|Word),data=uw[uw$frame==10,])
uwmod2<-lmer(F2.norm~(PreSeg+FolSeg+Duration)+YOB+dim1+(1|Speaker)+(1|Word),data=uw[uw$frame==10,])
uwmod3<-lmer(F2.norm~(PreSeg+FolSeg+Duration)+YOB+dim2+(1|Speaker)+(1|Word),data=uw[uw$frame==10,])
uwmod4<-lmer(F2.norm~(PreSeg+FolSeg+Duration)+YOB+(1|Speaker)+(1|Word),data=uw[uw$frame==10,])
uwmod5<-lmer(F2.norm~(PreSeg+FolSeg+Duration)+YOB+Style+(1|Speaker)+(1|Word),data=uw[uw$frame==10,])

uw$dim2<-as.numeric(uw$dim2)
uw<-uw[!is.na(uw$dim2),]
uwmod3<-lmer(F2.norm~(PreSeg+FolSeg+Duration)+YOB*dim2+(1|Speaker)+(1|Word),data=uw[uw$frame==10,])
uwmod1<-lmer(F2.norm~(PreSeg+FolSeg+Duration)+YOB+(1|Speaker)+(1|Word),data=uw[uw$frame==10,])
uwmod2<-lmer(F2.norm~(PreSeg+FolSeg+Duration)+YOB+dim2+(1|Pseudo)+(1|Word),data=uw[uw$frame==10,])
uwmod1<-lmer(F2.norm~(PreSeg+FolSeg+Duration)+YOB+(1|Speaker)+(1|Word),data=uw[uw$frame==10,])
uwmodnull<-lmer(F2.norm~(PreSeg+FolSeg+Duration)+(1|Speaker)+(1|Word),data=uw[uw$frame==10,])


ey<-edata[edata$Vowel=="EY1",]
ggplot(ey,aes(x=dim1,y=Eucdist))+stat_summary()+geom_smooth(method='lm')
ggplot(ey,aes(x=dim2,y=Eucdist))+stat_summary()+geom_smooth(method='lm')

################Fitting full models#######################
data<-read.csv('/Users/pplsuser/FAVE/FAVE-align-thesis/all_data_exclusions_social_dct.csv')
data[,c("Pre_voice","Pre_place","Pre_manner")]<-t(sapply(data$PreSeg,'placemannervoice'))
data[,c("Fol_voice","Fol_place","Fol_manner")]<-t(sapply(data$FolSeg,'placemannervoice'))
class(data$Duration)
data[,"LogDur"]<-log(data$Duration)
data<-data[!data$frame>20,]
t <- poly((unique(data$YOB)), 3)
data[,paste("ot", 1:3, sep="")] <- t[data$frame, 1:3]
ow<-data[data$Vowel=='OW1',]
ey<-data[data$Vowel=='EY1',]
uw<-data[data$Vowel=='UW1',]
################lmer at point of largest av. difference#######################
ggplot(ow,aes(x=frame,y=F2.norm,color=Age))+stat_summary()
ggplot(uw,aes(x=frame,y=F2.norm,color=Age))+stat_summary()
ggplot(ey,aes(x=frame,y=F2.norm,color=Age))+stat_summary

library('lme4')
library('lmerTest')
library(ggplot2)
uw<-uw[uw$Fol_manner!='glide',]
uwmod<-lmer(F2.norm~Pre_manner+Fol_manner+Pre_place+Fol_place+Pre_voice+Fol_voice+LogDur+Gender+Style+dim1+dim2+dim3+YOB+(Style|Pseudo)+(1|Pseudo)+(1|Word),data=uw[uw$frame==15,])
final<-step(uwmod)
#Style+YOB
uwmod<-final$model
#bootstrapping cis
bootfit <- bootMer(uwmod, FUN=function(x)predict(x),nsim=1000)
uw15<-uwmod@frame
uw15$lci<-apply(bootfit$t, 2, quantile,0.025)
uw15$uci<-apply(bootfit$t, 2, quantile,0.975)
uw15$med<-apply(bootfit$t, 2, quantile,0.5)
uw15$Pred<-fitted(uwmod)
ggplot(uw15[uw15$Pseudo!='DA_M_1991_5'&uw15$Pseudo!='JC_M_1952_3',],aes(x=YOB,y=F2.norm))+stat_summary(aes(y=F2.norm,ymax=uci,ymin=lci,group=Pseudo),geom='pointrange',fun.data='mean_se',size=0.25)+geom_smooth(method='lm',se=F,color='#377eb8')+geom_smooth(method='lm',aes(y=uci),se=F,linetype='dotted',color='#377eb8')+geom_smooth(method='lm',aes(y=lci),se=F,linetype='dotted',color='#377eb8')+theme_bw()+theme(panel.border=element_rect(size=1.5,color='black'))+ylab('Normalized F2')+xlab('')+ggtitle('/u/ Fronting (YOB)')+ggsave('/Users/pplsuser/Desktop/Labphon Posters/u_fronting_yob.pdf',width=3,height=3)
ggplot(uw15[uw15$Pseudo!='DA_M_1991_5',],aes(x=YOB,y=Pred))+stat_summary(aes(y=Pred,ymax=uci,ymin=lci,group=Pseudo),geom='pointrange',fun.data='mean_se',size=0.25)+geom_smooth(aes(y=med),method='lm',se=F,color='#377eb8')+geom_smooth(method='lm',aes(y=uci),se=F,linetype='dotted',color='#377eb8')+geom_smooth(method='lm',aes(y=lci),se=F,linetype='dotted',color='#377eb8')+theme_bw()+theme(panel.border=element_rect(size=1.5,color='black'))+ylab('Normalized F2')+xlab('')+ggtitle('/u/ Fronting (YOB)')+ggsave('/Users/pplsuser/Desktop/Labphon Posters/u_fronting_yob.pdf',width=3,height=3)

save(uw15,file='uw15.RData')

names(uwmod@frame)[9]<-'YOB'
ggplot(uwmod@frame,aes(x=YOB,y=predict(uwmod)))+stat_summary(fun.data='mean_cl_boot')+stat_smooth(method='lm')+theme_bw()+theme(panel.border=element_rect(size=1.5))+ylab('Normalized F2')+xlab('Year of Birth')+ggtitle('/u/ Fronting (Year of Birth)')+ggsave('/Users/pplsuser/Desktop/Murcia/u_fronting_yob.pdf',width=3.5,height=3.5)
ggplot(uwmod@frame,aes(x=Style,y=predict(uwmod)))+geom_violin()+stat_summary(fun.data='mean_cl_boot', geom="pointrange", color="blue",size=1.5)+geom_jitter(aes(group=Pseudo),alpha=0.025)+theme_bw()+theme(panel.border=element_rect(size=1.5))+ylab('Normalized F2')+xlab('Style')+ggtitle('/u/ Fronting (Style)')+ggsave('/Users/pplsuser/Desktop/Murcia/u_fronting_style.pdf',width=3.5,height=3.5)
ggplot(uwmod@frame,aes(x=Style,y=predict(uwmod)))+geom_violin()+stat_summary(fun.data='mean_cl_boot', geom="pointrange", color="blue",size=1.5)+geom_jitter(aes(group=Pseudo),alpha=0.025)+theme_bw()+theme(panel.border=element_rect(size=1.5))+ylab('Normalized F2')+xlab('Style')+ggtitle('/u/ Fronting (Style)')+facet_wrap(~Pseudo)
+ggsave('/Users/pplsuser/Desktop/Murcia/u_fronting_style.pdf',width=3.5,height=3.5)


owmod<-lmer(F2.norm~(Pre_manner+Fol_manner+Pre_place+Fol_place+LogDur)+(1|Pseudo)+(1|Word),data=ow[ow$frame==15,])
owfinal<-step(owmod)
owmod<-lmer(F2.norm~(Pre_manner+Fol_manner+Pre_place+Fol_place+LogDur)+YOB+Gender+dim1+dim2+dim3+Style+(Style|Pseudo)+(1|Pseudo)+(1|Word),data=ow[ow$frame==15,])
owfinal<-step(owmod)
owmod<-owfinal$model
bootfit <- bootMer(owmod, FUN=function(x)predict(x),nsim=1000)
ow15<-ow[ow$frame==15,]
ow15$lci<-apply(bootfit$t, 2, quantile,0.025)
ow15$uci<-apply(bootfit$t, 2, quantile,0.975)
ow15$med<-apply(bootfit$t, 2, quantile,0.5)
ggplot(ow15,aes(x=YOB,y=fitted(owmod)))+stat_summary(aes(y=fitted(owmod),ymin=lci,ymax=uci,group=Pseudo))+geom_smooth(method='lm',se=F,color='#377eb8')+geom_smooth(method='lm',aes(y=uci),se=F,linetype='dotted',color='#377eb8')+geom_smooth(method='lm',aes(y=lci),se=F,linetype='dotted',color='#377eb8')+theme_bw()+theme(panel.border=element_rect(size=1.5,color='black'))+ylab('Normalized F2')+xlab('')+ggtitle('/o/ Fronting (YOB)')+ggsave('/Users/pplsuser/Desktop/Labphon Posters/o_fronting_yob.pdf',width=3,height=3)
save(ow15,file='ow15.RData')
ow15$Pred<-fitted(owmod)
ggplot(ow15[ow15$Pseudo!='DA_M_1991_5',],aes(x=YOB,y=Pred))+stat_summary(aes(y=Pred,ymax=uci,ymin=lci,group=Pseudo),size=0.25)+geom_smooth(method='lm',aes(y=med),se=F,color='#377eb8')+geom_smooth(method='lm',aes(y=uci),se=F,linetype='dotted',color='#377eb8')+geom_smooth(method='lm',aes(y=lci),se=F,linetype='dotted',color='#377eb8')+theme_bw()+theme(panel.border=element_rect(size=1.5,color='black'))+ylab('Normalized F2')+xlab('')+ggtitle('/o/ Fronting (YOB)')+ggsave('/Users/pplsuser/Desktop/Labphon Posters/o_fronting_yob.pdf',width=3,height=3)
#Style+YOB
owmod<-owfinal$model
names(uwmod@frame)[9]<-'YOB'
ggplot(owmod@frame,aes(x=YOB,y=predict(owmod)))+stat_summary(fun.data='mean_cl_boot')+stat_smooth(method='lm')+theme_bw()+theme(panel.border=element_rect(size=1.5))+ylab('Normalized F2')+xlab('Year of Birth')+ggtitle('/o/ Fronting (Year of Birth)')+ggsave('/Users/pplsuser/Desktop/Murcia/o_fronting_yob.pdf',width=3.5,height=3.5)
ggplot(owmod@frame,aes(x=Style,y=predict(owmod)))+geom_violin()+stat_summary(fun.data=mean_sdl, geom="pointrange", color="blue")+geom_jitter(aes(group=Pseudo),alpha=0.025)+theme_bw()+theme(panel.border=element_rect(size=1.5))+ylab('Normalized F2')+xlab('Style')+ggtitle('/o/ Fronting (Style)')+ggsave('/Users/pplsuser/Desktop/Murcia/o_fronting_style.pdf',width=3.5,height=3.5)

uwmoddip<-lmer(Eucdist~Fol_manner+Pre_place+Fol_place+LogDur+YOB+Gender+Style+dim1+dim2+dim3+(Style|Pseudo)+(1|Pseudo)+(1|Word),data=uw[uw$frame==15,])
uwdipfinal<-step(uwmoddip)
uwmoddip<-uwdipfinal$model
bootfit <- bootMer(uwmoddip, FUN=function(x)predict(x),nsim=1000)
uwdip15<-uwmoddip@frame
uwdip15$Eucdist<-uw[uw$frame==15,]$Eucdist
uwdip15$lci<-apply(bootfit$t, 2, quantile,0.025)
uwdip15$uci<-apply(bootfit$t, 2, quantile,0.975)
uwdip15$med<-apply(bootfit$t, 2, quantile,0.5)
ggplot(uwdip15,aes(x=YOB,y=fitted(uwmoddip)))+stat_summary(aes(y=fitted(uwmoddip),ymin=lci,ymax=uci,group=Pseudo),size=0.25)+geom_smooth(method='lm',aes(y=med),se=F,color='#377eb8')+geom_smooth(method='lm',aes(y=uci),se=F,linetype='dotted',color='#377eb8')+geom_smooth(method='lm',aes(y=lci),se=F,linetype='dotted',color='#377eb8')+theme_bw()+theme(panel.border=element_rect(size=1.5,color='black'))+ylab('Normalized Eucdist')+xlab('')+ggtitle('/u/ Diphthongization (YOB)')+ggsave('/Users/pplsuser/Desktop/Labphon Posters/u_dip_yob.pdf',width=3,height=3)
save(uwdip15,file='uwdip15.RData')


#Style + YOB
ggplot(uwmoddip@frame,aes(x=YOB,y=predict(uwmoddip)))+stat_summary(fun.data='mean_cl_boot')+stat_smooth(method='lm')+theme_bw()+theme(panel.border=element_rect(size=1.5))+ylab('Normalized Euclidean distance')+xlab('Year of Birth')+ggtitle('/u/ Diphthongization (Year of Birth)')+ggsave('/Users/pplsuser/Desktop/Murcia/u_dip_yob.pdf',width=3.5,height=3.5)
ggplot(uwmoddip@frame,aes(x=Style,y=predict(uwmoddip)))+geom_violin()+stat_summary(fun.data=mean_sdl, geom="pointrange", color="blue")+geom_jitter(aes(group=Pseudo),alpha=0.05)+theme_bw()+theme(panel.border=element_rect(size=1.5))+ylab('Normalized F2')+xlab('Style')+ggtitle('/u/ Diphthongization (Style)')+ggsave('/Users/pplsuser/Desktop/Murcia/u_dip_style.pdf',width=3.5,height=3.5)
ggplot(uwmoddip@frame,aes(x=dim3,y=predict(uwmoddip)))+stat_summary(fun.data='mean_cl_boot')+stat_smooth(method='lm')+theme_bw()+theme(panel.border=element_rect(size=1.5))+ylab('Normalized Euclidean distance')+xlab('Mobility index (n.s.)')+ggtitle('/u/ Diphthongization (Social class)')+ggsave('/Users/pplsuser/Desktop/Murcia/u_dip_class.pdf',width=3.5,height=3.5)

owmoddip<-lmer(Eucdist~Pre_manner+Fol_manner+Pre_place+Fol_place+LogDur+YOB+Gender+dim1+dim2+dim3+Style+(Style|Pseudo)+(1|Pseudo)+(1|Word),data=ow[ow$frame==15,])
owdipfinal<-step(owmoddip)
owmoddip<-owdipfinal$model
#Style + dim1 + dim3
ggplot(owmoddip@frame,aes(x=YOB,y=predict(owmoddip)))+stat_summary(fun.data='mean_cl_boot')+stat_smooth(method='lm')+theme_bw()+theme(panel.border=element_rect(size=1.5))+ylab('Normalized Euclidean distance')+xlab('Year of Birth (n.s.)')+ggtitle('/o/ Diphthongization (Year of Birth)')+ggsave('/Users/pplsuser/Desktop/Murcia/o_dip_yob.pdf',width=3.5,height=3.5)

ggplot(owmoddip@frame,aes(x=dim3,y=predict(owmoddip)))+stat_summary(fun.data='mean_cl_boot')+stat_smooth(method='lm')+theme_bw()+theme_bw()+theme(panel.border=element_rect(size=1.5))+ylab('Normalized Euclidean distance')+xlab('Mobility index')+ggtitle('/o/ Diphthongization (Social class)')+ggtitle('/o/ Diphthongization (Social class)')+ggsave('/Users/pplsuser/Desktop/Murcia/o_dip_class.pdf',width=3.5,height=3.5)
ggplot(owmoddip@frame,aes(x=Style,y=predict(owmoddip)))+geom_violin()+stat_summary(fun.data=mean_sdl, geom="pointrange", color="blue")+geom_jitter(aes(group=Pseudo),alpha=0.05)+theme_bw()+theme(panel.border=element_rect(size=1.5))+ylab('Normalized Euclidean distance')+xlab('Style')+ggtitle('/o/ Diphthongization (Style)')+ggsave('/Users/pplsuser/Desktop/Murcia/o_dip_style.pdf',width=3.5,height=3.5)

bootfit <- bootMer(owmoddip, FUN=function(x)predict(x),nsim=1000)
owdip15<-owmoddip@frame
owdip15$lci<-apply(bootfit$t, 2, quantile,0.025)
owdip15$uci<-apply(bootfit$t, 2, quantile,0.975)
owdip15$med<-apply(bootfit$t, 2, quantile,0.5)
ggplot(owdip15,aes(x=dim3,y=predict(owmoddip)))+stat_summary(aes(y=fitted(owmoddip),ymin=lci,ymax=uci))+geom_smooth(method='lm',se=F,color='#377eb8')+geom_smooth(method='lm',aes(y=uci),se=F,linetype='dotted',color='#377eb8')+geom_smooth(method='lm',aes(y=lci),se=F,linetype='dotted',color='#377eb8')+theme_bw()+theme(panel.border=element_rect(size=1.5,color='black'))+ylab('Normalized Eucdist')+xlab('')+ggtitle('/u/ Diphthongization (SES)')+ggsave('/Users/pplsuser/Desktop/Labphon Posters/o_dip_soc.pdf',width=3,height=3)
ggplot(owdip15,aes(x=dim3,y=fitted(owmoddip)))+stat_summary(aes(y=fitted(owmoddip),ymin=lci,ymax=uci,group=Pseudo),size=0.25)+geom_smooth(method='lm',aes(y=med),se=F,color='#377eb8')+geom_smooth(method='lm',aes(y=uci),se=F,linetype='dotted',color='#377eb8')+geom_smooth(method='lm',aes(y=lci),se=F,linetype='dotted',color='#377eb8')+theme_bw()+theme(panel.border=element_rect(size=1.5,color='black'))+ylab('Normalized Eucdist')+xlab('')+ggtitle('/o/ Diphthongization (SES)')+ggsave('/Users/pplsuser/Desktop/Labphon Posters/o_dip_soc.pdf',width=3,height=3)
save(owdip15,file='owdip15.RData')



eymoddip<-lmer(Eucdist~Pre_manner+Fol_manner+Pre_place+Fol_place+LogDur+YOB+Gender+dim1+dim2+dim3+Style+(Style|Pseudo)+(1|Pseudo)+(1|Word),data=ey[ey$frame==15,])
eydipfinal<-step(eymoddip)
#dim1 + dim3

################dcts################
dctdata<-read.csv('/Users/pplsuser/FAVE/FAVE-align-thesis/all_data_exclusions_social_dct.csv')
dctdata[,c("Pre_voice","Pre_place","Pre_manner")]<-t(sapply(dctdata$PreSeg,'placemannervoice'))
dctdata[,c("Fol_voice","Fol_place","Fol_manner")]<-t(sapply(dctdata$FolSeg,'placemannervoice'))
dctdata[,"LogDur"]<-log(dctdata$Duration)
ow<-dctdata[dctdata$Vowel=='OW1',]
ey<-dctdata[dctdata$Vowel=='EY1',]
uw<-dctdata[dctdata$Vowel=='UW1',]

library(ggplot2)
library(lme4)
library(lmerTest)

uw<-uw[uw$Fol_manner!='glide',]
uwmoddct<-lmer(F2.dct1~Pre_manner+Fol_manner+Pre_place+Fol_place+Pre_voice+Fol_voice+LogDur+YOB+Gender+Style+dim1+dim2+dim3+(Style|Pseudo)+(1|Pseudo)+(1|Word),data=uw[uw$frame==15,])
final<-step(uwmoddct)
#Just YOB

owmod<-lmer(F2.dct1~Pre_manner+Fol_manner+Pre_place+Fol_place+LogDur+YOB+Gender+dim1+dim2+dim3+Style+(Style|Pseudo)+(1|Pseudo)+(1|Word),data=ow[ow$frame==15,])
owfinal<-step(owmod)
#YOB and dim3, no style

uwmoddip<-lmer(F2.dct2~Fol_manner+Pre_place+Fol_place+LogDur+YOB+Gender+Style+dim1+dim2+dim3+(Style|Pseudo)+(1|Pseudo)+(1|Word),data=uw[uw$frame==15,])
uwdipfinal<-step(uwmoddip)
#Style + dim1

owmoddip<-lmer(F2.dct2~Pre_manner+Fol_manner+Pre_place+Fol_place+LogDur+YOB+Gender+dim1+dim2+dim3+Style+(Style|Pseudo)+(1|Pseudo)+(1|Word),data=ow[ow$frame==15,])
owdipfinal<-step(owmoddip)
#dim3 no style 

eymoddip<-lmer(F2.dct2~Pre_manner+Fol_manner+Pre_place+Fol_place+LogDur+YOB+Gender+dim1+dim2+dim3+Style+(Style|Pseudo)+(1|Pseudo)+(1|Word),data=ey[ey$frame==15,])
eydipfinal<-step(eymoddip)
#dim1 dim2 dim3 style

uwmodcurve<-lmer(F2.dct3~Fol_manner+Pre_place+Fol_place+LogDur+YOB+Gender+Style+dim1+dim2+dim3+(Style|Pseudo)+(1|Pseudo)+(1|Word),data=uw[uw$frame==15,])
uwcurvefinal<-step(uwmoddip)
#Style + dim1

owmodcurve<-lmer(F2.dct3~Pre_manner+Fol_manner+Pre_place+Fol_place+LogDur+YOB+Gender+dim1+dim2+dim3+Style+(Style|Pseudo)+(1|Pseudo)+(1|Word),data=ow[ow$frame==15,])
owcurvefinal<-step(owmoddip)
#dim3 no style 

eymoddip<-lmer(F2.dct3~Pre_manner+Fol_manner+Pre_place+Fol_place+LogDur+YOB+Gender+dim1+dim2+dim3+Style+(Style|Pseudo)+(1|Pseudo)+(1|Word),data=ey[ey$frame==15,])
eydipfinal<-step(eymoddip)
#dim1

ggplot(data[data$Vowel%in%c('OW1','UW1')&data$Pseudo!="DA_M_1991_5",],aes(x=F2.dct1,y=F2.dct2,color=Gender,group=Pseudo))+stat_summary()

################dct plots for labphon
owmoddip<-lmer(Eucdist~Pre_manner+Fol_manner+Pre_place+Fol_place+LogDur+YOB+Gender+dim1+dim2+dim3+Style+(Style|Pseudo)+(1|Pseudo)+(1|Word),data=ow[ow$frame==15,])
owdipfinal<-step(owmoddip)
owmoddip<-owdipfinal$model
#Style + dim1 + dim3
ggplot(owmoddip@frame,aes(x=YOB,y=predict(owmoddip)))+stat_summary(fun.data='mean_cl_boot')+stat_smooth(method='lm')+theme_bw()+theme(panel.border=element_rect(size=1.5))+ylab('Normalized Euclidean distance')+xlab('Year of Birth (n.s.)')+ggtitle('/o/ Diphthongization (Year of Birth)')+ggsave('/Users/pplsuser/Desktop/Murcia/o_dip_yob.pdf',width=3.5,height=3.5)



