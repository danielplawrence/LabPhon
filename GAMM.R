library(mgcv)
library(itsadug)
data<-read.csv('/Users/pplsuser/FAVE/FAVE-align-thesis/all_data_exclusions_social.csv')
head(data)
data[,c("Pre_voice","Pre_place","Pre_manner")]<-t(sapply(data$PreSeg,'placemannervoice'))
data[,c("Fol_voice","Fol_place","Fol_manner")]<-t(sapply(data$FolSeg,'placemannervoice'))
class(data$Duration)
data[,"LogDur"]<-log(data$Duration)
data<-data[!data$frame>20,]
t <- poly((unique(data$YOB)), 3)
data[,paste("ot", 1:3, sep="")] <- t[data$frame, 1:3]
data$Edu_bin<-ifelse(data$Part_edu==1|data$Part_edu==2,'LW',ifelse(data$Part_edu==3|data$Part_edu==4,"W","M"))data<-data[!is.na(data$variable),]
ow<-data[data$Vowel=='OW1',]
uw<-data[data$Vowel=='UW1',]
ow$JF<-unlist(mapply('jf_codes_new',vowel=ow$Vowel,pre=ow$Pre_place,fol=ow$Fol_manner,preseg=ow$PrePhon,folseg=ow$FolSeg))
uw$JF<-unlist(mapply('jf_codes_new',vowel=uw$Vowel,pre=uw$Pre_place,fol=uw$Fol_manner,preseg=uw$PreSeg,folseg=uw$FolSeg))
uw$JF<-as.factor(uw$JF)
ow$JF<-as.factor(ow$JF)
#First job -- model effect of phonetic environment
uw_mod1 <- bam(F2.norm ~ JF + s(frame, by=JF)+ s(frame, Pseudo, bs="fs"k=5), data=uw)
check_resid(uw_mod1, split_by=c("Pseudo", "JF"))
#Add time interaction
uw_mod2 <- bam(F2.norm ~ JF+ YOB + s(frame, by=JF,k=5)+s(YOB, by=JF,k=5)+ s(frame, Pseudo, bs="fs", k=5), data=uw)
check_resid(uw_mod2,split_by=c('Pseudo','JF'))
#Add gender
uw_mod3 <- bam(F2.norm ~ JF+ YOB + Gender+s(frame, by=JF,k=5)+s(YOB, by=JF,k=5)+ s(frame, Pseudo, bs="fs", k=5)+s(frame, by=Gender,k=5), data=uw)
compareML(uw_mod2,uw_mod3)
#Add mods and backward select
uw_mod3 <- bam(F2.norm ~ JF+ YOB + Style+s(frame, by=JF,k=5)+s(YOB, by=JF,k=5)+ s(frame, Pseudo, bs="fs", m=1)+s(frame, by=Style, k=5)+s(Style,Pseudo, bs="re", m=1), data=uw)
#Test significance of random slope
uw_mod3noslope <- bam(F2.norm ~ JF+ YOB + Style+s(frame, by=JF,k=5)+s(YOB, by=JF,k=5)+ s(frame, Pseudo, bs="fs", m=1)+s(frame, by=Style, k=5), data=uw)
compareML(uw_mod3,uw_mod3noslope)
#Prefer the slope
uw$fitted<-predict(uw_mod3)
uw$YOBcut<-cut(uw$YOB,breaks=8)
ggplot(data=uw,aes(x=frame,y=fitted,color=JF))+geom_smooth()+facet_wrap(~YOBcut)
ggplot(data=uw,aes(x=frame,y=fitted,color=JF))+geom_smooth()+facet_wrap(~Style)


#Add time interaction
ow_mod2 <- bam(F2.norm ~ JF+ YOB + s(frame, by=JF,k=5)+s(YOB, by=JF,k=5)+ s(frame, Pseudo, bs="fs", k=5), data=ow)
ow_mod3 <- bam(F2.norm ~ JF+ YOB + Gender+s(frame, by=JF,k=5)+s(YOB, by=JF,k=5)+ s(frame, Pseudo, bs="fs", k=5)+s(frame, by=Gender,k=5), data=ow)
ow_mod3 <- bam(F2.norm ~ JF+ YOB + dim1+dim2+dim3+s(frame, by=JF,k=5)+s(YOB, by=JF,k=5)+ s(frame, Pseudo, bs="fs", k=5)+s(frame, by=Gender,k=5), data=ow)
ow_mod3 <- bam(F2.norm ~ JF+ YOB + dim1+dim2+dim3+s(frame, by=JF,k=5)+s(YOB, by=JF,k=5)+ s(frame, Pseudo, bs="fs", k=5), data=ow)
ow_mod4 <- bam(F2.norm ~ JF+ YOB + dim1+dim2+s(frame, by=JF,k=5)+s(YOB, by=JF,k=5)+ s(frame, Pseudo, bs="fs", k=5), data=ow)
ow_mod4 <- bam(F2.norm ~ JF+ YOB + dim1+dim3+s(frame, by=JF,k=5)+s(YOB, by=JF,k=5)+ s(frame, Pseudo, bs="fs", k=5), data=ow)
ow_mod3 <- bam(F2.norm ~ JF+ YOB + dim2+dim3+s(frame, by=JF,k=5)+s(YOB, by=JF,k=5)+ s(frame, Pseudo, bs="fs", k=5), data=ow)

compareML(ow_mod3,ow_mod4)
#Just prefer ow_4
ow_mod4 <- bam(F2~ JF+ YOB + dim2+dim3+Style+s(frame, by=JF,k=5)+s(YOB, by=JF,k=5)+ s(frame, Pseudo, bs="fs", k=5)+s(frame, Pseudo, bs="fs", m=1)+s(frame, by=Style, k=5), data=ow)
owf1_mod4 <- bam(F1 ~ JF+ YOB + dim2+dim3+Style+s(frame, by=JF,k=5)+s(YOB, by=JF,k=5)+ s(frame, Pseudo, bs="fs", k=5)+s(frame, Pseudo, bs="fs", m=1)+s(frame, by=Style, k=5), data=ow)


#Add time interaction
ow_mod2 <- bam(F2.norm ~ JF+ YOB + s(frame, by=JF,k=5)+s(YOB, by=JF,k=5)+ s(frame, Pseudo, bs="fs", k=5), data=ow)
ow_mod3 <- bam(F2.norm ~ JF+ YOB + Gender+s(frame, by=JF,k=5)+s(YOB, by=JF,k=5)+ s(frame, Pseudo, bs="fs", k=5)+s(frame, by=Gender,k=5), data=ow)
ow_mod3 <- bam(F2.norm ~ JF+ YOB + dim1+dim2+dim3+s(frame, by=JF,k=5)+s(YOB, by=JF,k=5)+ s(frame, Pseudo, bs="fs", k=5)+s(frame, by=Gender,k=5), data=ow)
ow_mod3 <- bam(F2.norm ~ JF+ YOB + dim1+dim2+dim3+s(frame, by=JF,k=5)+s(YOB, by=JF,k=5)+ s(frame, Pseudo, bs="fs", k=5), data=ow)
ow_mod4 <- bam(F2.norm ~ JF+ YOB + dim1+dim2+s(frame, by=JF,k=5)+s(YOB, by=JF,k=5)+ s(frame, Pseudo, bs="fs", k=5), data=ow)
ow_mod4 <- bam(F2.norm ~ JF+ YOB + dim1+dim3+s(frame, by=JF,k=5)+s(YOB, by=JF,k=5)+ s(frame, Pseudo, bs="fs", k=5), data=ow)
ow_mod3 <- bam(F2.norm ~ JF+ YOB + dim2+dim3+s(frame, by=JF,k=5)+s(YOB, by=JF,k=5)+ s(frame, Pseudo, bs="fs", k=5), data=ow)

compareML(ow_mod3,ow_mod4)
#Just prefer ow_4
ow_mod4 <- bam(F2~ JF+ YOB + dim2+dim3+Style+s(frame, by=JF,k=5)+s(YOB, by=JF,k=5)+ s(frame, Pseudo, bs="fs", k=5)+s(frame, Pseudo, bs="fs", m=1)+s(frame, by=Style, k=5), data=ow)
owf1_mod4 <- bam(F1 ~ JF+ YOB + dim2+dim3+Style+s(frame, by=JF,k=5)+s(YOB, by=JF,k=5)+ s(frame, Pseudo, bs="fs", k=5)+s(frame, Pseudo, bs="fs", m=1)+s(frame, by=Style, k=5), data=ow)


newdata<-ow_mod4$model
newdata$Fit.F2<-fitted(ow_mod4)
newdata$Fit.F1<-fitted(owf1_mod4)
newdata$dim3cut<-cut(newdata$dim3,breaks=4)
newdata$dim2cut<-cut(newdata$dim2,breaks=4)

ggplot(data=newdata,aes(x=frame,y=Fit.F1,color=dim3cut))+geom_smooth()+geom_smooth(aes(x=frame,y=Fit.F2,color=dim3cut))

ggplot(data=newdata,aes(x=frame,y=Fit.F1,color=Style))+geom_smooth()+geom_smooth(aes(x=frame,y=Fit.F2,color=Style))+facet_wrap(~dim3cut)
#So it would be cool to know 1.where the groups differ the most
#2. Within dim groups, which time points are the most important

pvisgam(ow_mod4, view=c("frame", "YOB"))

fvisgam(ow_mod4, view=c("frame", "YOB"))

fvisgam(uw_mod3, view=c("frame", "YOB"))

fvisgam(ow_mod4, view=c("frame", "dim3"))

fvisgam(ow_mod4, view=c("frame", "dim2"))
plot_smooth(ow_mod4, view="frame", plot_all="Style", rm.ranef=TRUE)

+geom_smooth()+facet_wrap(~YOBcut)
ggplot(data=ow,aes(x=frame,y=fitted,color=Style))+geom_smooth()+facet_wrap(~Style)
ggplot(data=ow,aes(x=frame,y=fitted,color=Style))+geom_smooth()+facet_wrap(~Style)


ow$Pre_place<-factor(ow$Pre_place)
#Playing around with phonetic effects again
ow_mod <- bam(F2.norm ~ JF+ s(frame, by=JF,k=5)+ s(Pseudo, bs="fs", k=5,by=JF), data=ow)
newdata<-ow_mod$model
newdata$F2.fit<-fitted(ow_mod)
ggplot(data=newdatLabphona,aes(x=frame,y=F2.fit,color=JF))+geom_smooth()
+geom_smooth(aes(x=frame,y=Fit.F2,color=dim3cut))
ow_mod_style <- bam(F2.norm ~ JF+(dim3*Style)+s(frame, by=JF,k=5)+ s(frame, by=Style,k=5)+s(frame, by=dim3,k=5)+s(Pseudo, bs="fs", k=5,by=JF), data=ow)
compareML(ow_mod,ow_mod_style)
newdata$F2.fit<-fitted(ow_mod_style)
newdata<-ow_mod_style$model
ggplot(data=newdata,aes(x=frame,y=F2.fit,color=Style))+geom_smooth()+facet_wrap(~Style)

plot_diff2(ow_mod_style, view=c('frame','dim3'), comp=list(Style=c("Maptask", "Wordlist")),rm.ranef='Pseudo')
ggplot(data=newdata[newdata$dim3==min(newdata$dim3,na.rm=T)&newdata$JF=='Kow',],aes(x=frame,y=F2.fit,color=Style))+geom_smooth()
ggplot(data=newdata[newdata$dim3==max(newdata$dim3,na.rm=T)&newdata$JF=='Kow',],aes(x=frame,y=F2.fit,color=Style))+geom_smooth()

#Hypothesis test
ow_mod_style2 <- bam(F2.norm ~ JF+dim3+Style+s(frame, by=JF,k=5)+ s(frame, by=Style,k=5)+s(frame, by=dim3,k=5)+s(Pseudo, bs="fs", k=5,by=JF), data=ow)

compareML(ow_mod_style,ow_mod_style2)
plot_diff2(ow_mod_style2, view=c('frame','dim3'), comp=list(Style=c("Maptask", "Wordlist")),rm.ranef='Pseudo')
ow_mod_styledim2 <- bam(F2.norm ~ JF+dim2+Style+s(frame, by=JF,k=5)+ s(frame, by=Style,k=5)+s(frame, by=dim3,k=5)+s(Pseudo, bs="fs", k=5,by=JF), data=ow)
ow_mod_styledim2int <- bam(F2.norm ~ JF+dim2*Style+s(frame, by=JF,k=5)+ s(frame, by=Style,k=5)+s(frame, by=dim3,k=5)+s(Pseudo, bs="fs", k=5,by=JF), data=ow)
compareML(ow_mod_styledim2,ow_mod_styledim2int)
plot_diff2(ow_mod_styledim2int, view=c('frame','dim2'), comp=list(Style=c("Maptask", "Wordlist")),rm.ranef='Pseudo')

newdata<-expand.grid(JF=levels(ow_mod_styledim2int$model$JF),dim2=2:-1,Style=levels(ow_mod_styledim2int$model$Style),Pseudo=levels(ow_mod_styledim2int$model$Pseudo),frame=1:20,dim3=-2:1)
newdata$F2.fit<-predict(ow_mod_styledim2int,newdata=newdata)
ggplot(data=newdata[newdata$dim2==min(newdata$dim2,na.rm=T)&newdata$JF=='Kow',],aes(x=frame,y=F2.fit,color=Style))+geom_smooth()
ggplot(data=newdata[newdata$dim2==max(newdata$dim2,na.rm=T)&newdata$JF=='Kow',],aes(x=frame,y=F2.fit,color=Style))+geom_smooth()
head(ow_mod_styledim2int$model)

owyobclass<-bam(F2.norm ~ JF+dim2*YOB+s(frame, by=JF,k=5)+ s(frame, by=YOB,k=5)+s(frame, by=dim2,k=5)+s(Pseudo, bs="fs", k=5,by=JF), data=ow)
owyobclassnoint<-bam(F2.norm ~ JF+dim2+YOB+s(frame, by=JF,k=5)+ s(frame, by=YOB,k=5)+s(frame, by=dim2,k=5)+s(Pseudo, bs="fs", k=5,by=JF), data=ow)
newdata<-expand.grid(JF=levels(ow_mod_styledim2int$model$JF),dim2=2:-1,Style=levels(ow_mod_styledim2int$model$Style),Pseudo=levels(ow_mod_styledim2int$model$Pseudo),frame=1:20,dim3=-2:1,YOB=1935:2000)
newdata$F2.fit<-predict(owyobclassnoint,newdata=newdata)
ggplot(data=newdata[newdata$dim2==min(newdata$dim2,na.rm=T)&newdata$JF=='Kow',],aes(x=frame,y=F2.fit,color=Style))+geom_smooth()
ggplot(data=newdata[newdata$dim2==max(newdata$dim2,na.rm=T)&newdata$JF=='Kow',],aes(x=frame,y=F2.fit,color=Style))+geom_smooth()

ggplot(newdata,aes(x=frame,y=F2.fit,fill=YOB))+geom_point()

plot_diff2(owyobclassnoint, view=c('frame','YOB'), comp=list(Style=c("Maptask", "Wordlist")),rm.ranef='Pseudo')
#Plots for Labphon main poster
library(ggplot2)
#OW over time 
#Just prefer ow_4
ow_mod4 <- bam(F2~ JF+ YOB + dim2+dim3+Style+s(frame, by=JF,k=5)+ s(frame, Pseudo, bs="fs", k=5)+s(frame, Pseudo, bs="fs", m=1)+s(frame, by=Style, k=5), data=ow)
owf1_mod4 <- bam(F1 ~ JF+ YOB + dim2+dim3+Style+s(frame, by=JF,k=5)+ s(frame, Pseudo, bs="fs", k=5)+s(frame, Pseudo, bs="fs", m=1)+s(frame, by=Style, k=5), data=ow)

ow_mod4 <- bam(F2~ JF+ YOB + dim2+dim3+Style+s(frame, by=JF,k=5)+ s(frame, Pseudo, bs="fs", k=5)+s(frame, Pseudo, bs="fs", m=1)+s(frame, by=Style, k=5), data=ow)
owf1_mod4 <- bam(F1 ~ JF+ YOB + dim2+dim3+Style+s(frame, by=JF,k=5)+ s(frame, Pseudo, bs="fs", k=5)+s(frame, Pseudo, bs="fs", m=1)+s(frame, by=Style, k=5), data=ow)

ow_mod4 <- bam(F2~ YOB*Gender +s(frame,k=7)+s(frame,YOB,k=5),data=ow)

compareML(ow_mod4,ow_mod5)
owf1_mod4 <- bam(F1~ YOB*Gender +s(frame,k=7)+s(frame,YOB,k=5),data=ow)
ow$JF<-factor(ow$JF)
newdata<-expand.grid(YOB=1935:2000,JF=levels(ow$JF),frame=1:20,Gender=levels(ow$Gender))
newdata$Fit.F2<-predict(ow_mod4,newdata=newdata,exclude='Pseudo',se.fit=T)$fit
newdata$Fit.F2.se<-predict(ow_mod4,newdata=newdata,exclude='Pseudo',se.fit=T)$se.fit
newdata$F2upper<-(newdata$Fit.F2+(newdata$Fit.F2.se*1.96))
newdata$F2lower<-(newdata$Fit.F2-(newdata$Fit.F2.se*1.96))
newdata$Fit.F1<-predict(owf1_mod4,newdata=newdata,exclude='Pseudo')
newdata$Fit.F1.se<-predict(owf1_mod4,newdata=newdata,exclude='Pseudo',se.fit=T)$se.fit
newdata$F1upper<-(newdata$Fit.F1+(newdata$Fit.F1.se*1.96))
newdata$F1lower<-(newdata$Fit.F1-(newdata$Fit.F1.se*1.96))
newdata$dec<-cut(newdata$YOB,breaks=6)
newdata$dec<-factor(newdata$dec)

ggplot(data=newdata,aes(x=frame,y=Fit.F2))+geom_smooth(aes(x=frame,group=dec),color='black')+geom_line(aes(y=F2upper,color=dec),stat='smooth',method='loess',formula=y~x,size=1,alpha=0.5)+geom_line(aes(y=F2lower,color=dec),stat='smooth',method='loess',formula=y~x,size=1,alpha=0.5)+geom_smooth(aes(y=Fit.F1,x=frame,group=dec),color='black')+geom_line(aes(y=F1upper,color=dec),stat='smooth',method='loess',formula=y~x,size=1,alpha=0.5)+geom_line(aes(y=F1lower,color=dec),stat='smooth',method='loess',formula=y~x,size=1,alpha=0.5)+scale_color_brewer(palette='Set1')+facet_wrap(~Gender)
+geom_line(aes(y=F2upper,color=dec),stat='smooth',method='lm',formula=y~x,alpha=0.05)
+geom_line(aes(y=F2lower,color=dec),stat='smooth',method='lm',formula=y~x,alpha=0.05)+geom_smooth(aes(x=frame,y=Fit.F1,group=dec),color='black')+geom_smooth(aes(y=F1upper,color=dec),alpha=0.05)+geom_smooth(aes(y=F1lower,color=dec),alpha=0.05)

class(ow$dec)
ggplot(data=newdata,aes(x=frame,y=Fit.F1,color=dec))+geom_smooth()+geom_smooth(aes(x=frame,y=Fit.F2,color=dec))

ggplot(data=newdata,aes(x=frame,y=Fit.F1,color=Style))+geom_smooth()+geom_smooth(aes(x=frame,y=Fit.F2,color=Style))+facet_wrap(~dim3cut)
#So it would be cool to know 1.where the groups differ the most


ggplot(data=ow,aes(x=frame,y=F2,color=dec))+stat_summary()







#dim2

#Functions for adding context
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


##GAM modelling for Labphon
#Questions: What evidence is there for change in /o/?
			#How do fronting and diphthongization interact?
library(mgcv)
library(itsadug)
data<-read.csv('/Users/pplsuser/FAVE/FAVE-align-thesis/all_data_exclusions_social.csv')
head(data)
data[,c("Pre_voice","Pre_place","Pre_manner")]<-t(sapply(data$PreSeg,'placemannervoice'))
data[,c("Fol_voice","Fol_place","Fol_manner")]<-t(sapply(data$FolSeg,'placemannervoice'))
class(data$Duration)
data[,"LogDur"]<-log(data$Duration)
data<-data[!data$frame>20&data$Style=='Wordlist',]
t <- poly((unique(data$YOB)), 3)
data[,paste("ot", 1:3, sep="")] <- t[data$frame, 1:3]
ow<-data[data$Vowel=='OW1',]
uw<-data[data$Vowel=='UW1',]
ow$JF<-unlist(mapply('jf_codes_new',vowel=ow$Vowel,pre=ow$Pre_place,fol=ow$Fol_manner,preseg=ow$PrePhon,folseg=ow$FolSeg))
uw$JF<-unlist(mapply('jf_codes_new',vowel=uw$Vowel,pre=uw$Pre_place,fol=uw$Fol_manner,preseg=uw$PreSeg,folseg=uw$FolSeg))
uw$JF<-as.factor(uw$JF)
ow$JF<-as.factor(ow$JF)

#First job, model the phonetic effects
#This is a model predicting f2 as an arbitrarily wiggly function of time, plus a random nonlinear smooth for each individual
ow_mod1 <- bam(F2.norm ~ JF + s(frame, by=JF), data=ow)
#random slope
ow_mod2 <- bam(F2.norm ~ JF + s(frame, by=JF)+ s(frame, Pseudo, bs="re"), data=ow)
compareML(ow_mod1,ow_mod2)
#random smooth
ow_mod3 <- bam(F2.norm ~ JF + s(frame, by=JF)+ s(frame, Pseudo, bs="fs"), data=ow)
compareML(ow_mod2,ow_mod3)
#prefer smooth
#Word effects
ow_mod4 <- bam(F2.norm ~ JF + s(frame, by=JF)+ s(frame, Pseudo, bs="fs")+s(frame, Word, bs="fs"), data=ow)
compareML(ow_mod3,ow_mod4)
ow_mod5 <- bam(F2.norm ~ JF + s(frame, by=JF)+ s(frame, Pseudo, bs="fs")+s(frame, Word, bs="re"), data=ow)
compareML(ow_mod4,ow_mod5)
#now add the effect of YOB using 'te' as different scales
ow_mod5 <- bam(F2.norm ~ JF+YOB+s(frame, by=JF)+s(frame, Pseudo, bs="fs")+s(frame, Word, bs="fs"), data=ow)
compareML(ow_mod4,ow_mod5)
ow_mod6 <- bam(F2.norm ~ JF:YOB+s(frame, by=JF)+s(frame, Pseudo, bs="fs")+s(frame, Word, bs="fs"), data=ow)
compareML(ow_mod5,ow_mod6)
ow_mod7 <- bam(F2.norm ~ JF*YOB+s(frame, by=JF)+s(frame, Pseudo, bs="fs")+s(frame, Word, bs="fs"), data=ow)
compareML(ow_mod6,ow_mod7)
#best model has interaction of YOB with linguistic constraints
#So evidence that the linear function changes over time by YOB
#Taking out word for plotting
ow_mod7 <- bam(F2.norm ~ JF*YOB+s(frame, by=JF)+s(frame, Pseudo, bs="fs"),data=ow)
ow$Pseudo<-factor(ow$Pseudo)
ow$Word<-factor(ow$Word)
newdata<-expand.grid(JF=levels(ow$JF),YOB=1935:2005,Pseudo=levels(ow$Pseudo),frame=1:20)
newdata$F2.fit<-predict(ow_mod7,newdata=newdata,exclude=c('Pseudo','Word'))
newdata$dec<-cut(newdata$YOB,7)
ggplot(newdata,aes(x=frame,y=F2.fit,color=JF))+geom_smooth()+facet_wrap(~dec)
#OK so our current setup allows the intercept to vary by year
#what we really want is to allow the effect of frame to vary by YOB
ow_mod8 <- bam(F2.norm ~ JF*YOB+s(frame, by=JF)+s(frame, Pseudo, bs="fs")+te(frame,YOB)+s(frame, Word, bs="fs"), data=ow)
compareML(ow_mod7,ow_mod8)
#Prefer mod8
ow_mod8 <- bam(F2.norm ~ JF*YOB+s(frame, by=JF)+s(frame, Pseudo, bs="fs")+te(frame,YOB), data=ow)
newdata<-expand.grid(JF=levels(ow$JF),YOB=1935:2005,Pseudo=levels(ow$Pseudo),frame=seq(1,20,length=100))
newdata$F2.fit<-predict(ow_mod8,newdata=newdata,exclude=c('Pseudo','Word'))
newdata$dec<-cut(newdata$YOB,8)
ggplot(newdata,aes(x=frame,y=F2.fit,color=dec))+geom_smooth()
save(ow_mod8,file='ow_mod8.Rdata')
save(newdata,file='owmod8_predictions.Rdata')
#working nicely :) adding the by-year smooth captures changes in curvature
#so now we want to test gender
ow_mod9 <- bam(F2.norm ~ JF*YOB*Gender+s(frame, by=JF)+s(frame, Pseudo, bs="fs")+te(frame,YOB)+s(frame, Word, bs="fs"), data=ow)
compareML(ow_mod8,ow_mod9)
#oh boy looks like a gender interaction!
#maybe two two way interactions?
ow_mod10 <- bam(F2.norm ~ JF+YOB+(JF:YOB)+(Gender:YOB)+s(frame, by=JF)+s(frame, Pseudo, bs="fs")+te(frame,YOB)+s(frame, Word, bs="fs"), data=ow)
compareML(ow_mod9,ow_mod10)
#nope
ow_mod9 <- bam(F2.norm ~ JF*YOB*Gender+s(frame, by=JF)+s(frame, Pseudo, bs="fs")+te(frame,YOB), data=ow)
newdata<-expand.grid(JF=levels(ow$JF),YOB=1935:2005,Pseudo=levels(ow$Pseudo),frame=seq(1,20,length=20),Gender=levels(ow$Gender))
newdata$F2.fit<-predict(ow_mod9,newdata=newdata,exclude=c('Pseudo','Word'))
newdata$dec<-cut(newdata$YOB,8)
ggplot(newdata,aes(x=frame,y=F2.fit,color=JF))+geom_smooth()+facet_wrap(~Gender)
#Try a smooth for gender
ow_mod10 <- bam(F2.norm ~ JF*YOB*Gender+s(frame,by=Gender)+s(frame,by=JF)+s(frame, Pseudo, bs="fs")+te(frame,YOB)+s(frame, Word, bs="fs"), data=ow)
compareML(ow_mod9,ow_mod10)
#Marginally prefer no smooth
save(ow_mod9,'ow_mod9.Rdata')
save(newdata,file='owmod9_predictions.Rdata')

load('ow_mod9.Rdata')
ow$Pseudo<-factor(ow$Pseudo)
#Now I want to test the effect of social class, regional identity index, mobility index
ow_mod9 <- bam(F2.norm ~ JF*YOB*Gender+s(frame, by=JF)+s(frame, Pseudo, bs="fs")+te(frame,YOB)+s(frame, Word, bs="fs"), data=ow)

ow_mod10 <- bam(F2.norm ~ JF*YOB*Gender*dim1+s(frame, by=JF)+s(frame, Pseudo, bs="fs")+te(frame,YOB)+s(frame, Word, bs="fs"), data=ow)
ow_mod10 <- bam(F2.norm ~ JF*YOB*Gender*dim1+s(frame, by=JF)+s(frame, Pseudo, bs="fs")+te(frame,YOB), data=ow)
compareML(ow_mod9,ow_mod10)
newdata<-expand.grid(JF=levels(ow$JF),YOB=1935:2005,Pseudo=levels(ow$Pseudo),frame=seq(1,20,length=20),Gender=levels(ow$Gender),dim1=-2:2)
newdata$F2.fit<-predict(ow_mod10,newdata=newdata,exclude=c('Pseudo','Word'))
newdata$dec<-cut(newdata$YOB,8)
newdata$dimcut<-cut(newdata$dim1,3)
ggplot(newdata,aes(x=frame,y=F2.fit,color=JF))+geom_smooth()+facet_grid(dimcut~Gender)

#This is very slow so I have resolved to do these again, this time just with the smooth terms?
#effect of context + smoothed context + YOB varying by frame
ow_mod2 <- bam(F2.norm ~ JF + s(frame, by=JF)+s(frame, Pseudo, bs="fs"), data=ow)
ow_mod3 <- bam(F2.norm ~ JF + s(frame, by=JF)+ te(YOB,frame)+s(frame, Pseudo, bs="fs"), data=ow)
compareML(ow_mod2,ow_mod3)
#prefer this model considerably. but does it just capture dynamic changes?
ow$Pseudo=factor(ow$Pseudo)
newdata<-expand.grid(JF=levels(ow$JF),YOB=1935:2005,Pseudo=levels(ow$Pseudo),frame=1:20)
newdata$F2.fit<-predict(ow_mod3,newdata=newdata,exclude=c('Pseudo','Word'))
newdata$dec<-cut(newdata$YOB,8)
ggplot(newdata,aes(x=frame,y=F2.fit,color=dec))+geom_smooth()
#AHA! so if we don't put in the intercept term, everybody starts in the same place. Adding intercept term
ow_mod4 <- bam(F2.norm ~ JF + YOB+s(frame, by=JF)+ te(YOB,frame)+s(frame, Pseudo, bs="fs"), data=ow)
compareML(ow_mod3,ow_mod4)
#Marginally prefer this model. 
newdata<-expand.grid(JF=levels(ow$JF),YOB=1935:2005,Pseudo=levels(ow$Pseudo),frame=1:20)
newdata$F2.fit<-predict(ow_mod4,newdata=newdata,exclude=c('Pseudo','Word'))
newdata$dec<-cut(newdata$YOB,8)
ggplot(newdata,aes(x=frame,y=F2.fit,color=dec))+geom_smooth()
#Okay -- what if we interact YOB and JF
ow_mod5 <- bam(F2.norm ~ JF +YOB+s(frame, by=JF)+ te(YOB,frame,by=JF)+s(frame, Pseudo, bs="fs"), data=ow)
compareML(ow_mod4,ow_mod5)
#Really prefer mod5, let's have a look
newdata<-expand.grid(JF=levels(ow$JF),YOB=1935:2005,Pseudo=levels(ow$Pseudo),frame=1:20)
newdata$F2.fit<-predict(ow_mod5,newdata=newdata,exclude=c('Pseudo','Word'))
newdata$dec<-cut(newdata$YOB,8)
ggplot(newdata,aes(x=frame,y=F2.fit,color=dec))+geom_smooth()
#Gonna add duration as a predictor
ow_mod6 <- bam(F2.norm ~ JF +YOB+s(frame, by=JF)+ te(YOB,frame,by=JF)+te(frame,LogDur)+s(frame, Pseudo, bs="fs"), data=ow)
compareML(ow_mod5,ow_mod6)
#Really prefer mod5, let's have a look
newdata<-expand.grid(JF=levels(ow$JF),YOB=1935:2005,Pseudo=levels(ow$Pseudo),frame=1:20)
newdata$F2.fit<-predict(ow_mod5,newdata=newdata,exclude=c('Pseudo','Word'))
newdata$dec<-cut(newdata$YOB,8)
ggplot(newdata,aes(x=frame,y=F2.fit,color=dec))+geom_smooth()
#Changing to normal preceding following
ow$Pre_place<-factor(ow$Pre_place)
ow_mod5 <- bam(F2.norm ~ Pre_place +YOB+s(frame, by=Pre_place)+ te(YOB,frame,by=Pre_place)+s(frame, Pseudo, bs="fs"), data=ow)
compareML(ow_mod4,ow_mod5)
newdata<-expand.grid(Pre_place=levels(ow$Pre_place),YOB=1935:2005,Pseudo=levels(ow$Pseudo),frame=1:20,LogDur=min(ow$LogDur):max(ow$LogDur))
newdata$F2.fit<-predict(ow_mod5,newdata=newdata,exclude=c('Pseudo','Word','Pre_place'))
newdata$dec<-cut(newdata$YOB,8)
newdata$dur<-cut(newdata$LogDur,4)
ggplot(newdata,aes(x=frame,y=F2.fit,color=dec))+geom_smooth()
#
ow_mod6 <- gam(F2.norm ~ Pre_place +YOB+s(frame, by=Pre_place)+ te(YOB,frame,by=Pre_place)+te(frame,LogDur)+s(frame, Pseudo, bs="fs"), data=ow)
compareML(ow_mod5,ow_mod6)
newdata<-expand.grid(Pre_place=levels(ow$Pre_place),YOB=1935:2005,Pseudo=levels(ow$Pseudo),frame=1:20,LogDur=min(ow$LogDur):max(ow$LogDur))
newdata$F2.fit<-predict(ow_mod5,newdata=newdata,exclude=c('Pseudo','Word','Pre_place'))
newdata$dec<-cut(newdata$YOB,8)
newdata$dur<-cut(newdata$LogDur,4)
ggplot(newdata,aes(x=frame,y=F2.fit,color=dec))+geom_smooth()


ow_mod6 <- gam(F2.norm ~ Pre_place +YOB+s(frame, by=Pre_place)+ te(YOB,frame,by=Pre_place)+s(frame, Pseudo, bs="fs"), data=ow)
#trying gender
ow_mod7 <- gam(F2.norm ~ Pre_place +YOB+s(frame, by=Pre_place)+ te(YOB,frame,by=Pre_place)+te(YOB,frame,by=Gender)+s(frame, Pseudo, bs="fs"), data=ow)
compareML(ow_mod7,ow_mod6)

ow_mod7 <- gam(F2.norm ~ YOB+s(frame, by=Pre_place)+ te(YOB,frame), data=ow)
ow_mod8 <- gam(F2.norm ~ YOB+dim3+s(frame)+ s(dim3)+te(YOB,frame,by=dim3), data=ow)


newdata<-expand.grid(Pre_place=levels(ow$Pre_place),YOB=1935:2005,frame=seq(1,20,by=.1),dim3=-2:2)
newdata$F2<-predict(ow_mod8,newdata)
newdata$Dec<-cut(newdata$YOB,8)
newdata$Mob<-cut(newdata$dim3,3)
ggplot(newdata,aes(x=frame,y=F2,color=Dec))+stat_summary()+facet_wrap(~Mob)

#I hypothesize that there is an interaction between MOB and decade
ow$Dec<-cut(ow$YOB,3)
ow$Mob<-cut(ow$dim3,2)
ow_null<- gam(F2.norm ~ s(frame),data=ow)
ow_yob<- gam(F2.norm ~ Dec+s(frame),data=ow)
compareML(ow_null,ow_yob)
newdata<-expand.grid(frame=seq(1,20,by=.1),Dec=levels(ow$Dec))
newdata$F2<-predict(ow_yob,newdata)
ggplot(newdata,aes(x=frame,y=F2,fill=Dec))+geom_smooth()
#add a smooth for yob
ow_yob_sm<- gam(F2.norm ~ Dec+te(frame,by=Dec),data=ow,method='ML')
compareML(ow_yob_sm,ow_yob)
newdata<-expand.grid(frame=seq(1,20,by=.1),Dec=levels(ow$Dec))
newdata$F2<-predict(ow_yob_sm,newdata)
ggplot(newdata,aes(x=frame,y=F2,fill=Dec))+geom_smooth()
ggplot(ow,aes(x=frame,y=F2,fill=Mob))+geom_smooth()+facet_wrap(~Dec)+geom_smooth(aes(x=frame,y=F1,fill=Mob))

ow_mob_yob<- gam(F2.norm ~ Dec+Mob+s(frame,by=Dec),data=ow,method='ML')
compareML(ow_mob_yob,ow_yob_sm)
ow_mob_yob_sm<- gam(F2.norm ~ Dec+Mob+s(frame,by=Mob)+s(frame,by=Dec),data=ow[ow$Style=='Wordlist',],method='ML')
compareML(ow_mob_yob,ow_mob_yob_sm)
newdata<-expand.grid(frame=seq(1,20,by=.1),Dec=levels(ow$Dec),Mob=levels(ow$Mob))
newdata$F2<-predict(ow_mob_yob_sm,newdata)
ggplot(newdata,aes(x=frame,y=F2,fill=Mob))+geom_smooth()+facet_wrap(~Dec)


ggplot(ow,aes(x=frame,y=F2.norm,fill=Mob))+geom_smooth()+facet_wrap(~Dec)

ow_null<- gam(F2.norm ~ s(frame, Pseudo, bs="fs", m=1),data=ow,method='ML')
ow_yob<- gam(F2.norm ~ Dec+ s(frame, Pseudo, bs="fs", m=1),data=ow,method='ML')


compareML(ow_yob,ow_null)

ow<-ow[ow$Style=='Wordlist',]
ow_mob_yob_smf2<- gam(F2.norm ~ Dec+Mob+s(frame,by=Mob)+s(frame,by=Dec)+s(frame, Pseudo, bs="fs", m=1),data=ow,method='ML')




ow_mob_yob_smf1<- gam(F1.norm ~ Dec+Mob+s(frame,by=Mob)+s(frame,by=Dec)+s(frame, Pseudo, bs="fs", m=1),data=ow,method='ML')



newdata<-expand.grid(frame=seq(1,20,by=.1),Dec=levels(ow$Dec),Mob=levels(ow$Mob),Pseudo=levels(ow$Pseudo))
newdata$F2<-predict(ow_mob_yob_smf2,newdata)
newdata$F2.se<-predict(ow_mob_yob_smf2,newdata,se.fit=T)$se.fit
newdata$F1<-predict(ow_mob_yob_smf1,newdata)
ggplot(newdata,aes(x=frame,y=F2,fill=Mob))+geom_ribbon(aes(ymax=F2+(.5*F2.se),ymin=F2+(.5*F2.se)),linetype='dotted')
+geom_ribbon(aes(y=F2-(.5*F2.se)),,linetype='dotted')+facet_wrap(~Dec)
+geom_ribbon(aes(ymax=F2+(F2.se),ymin=F2-(F2.se),group=Mob))
+geom_ribbon(aes(x=frame,ymax=F2+F2.se,ymin=F2-F2.se,fill=Mob))
+facet_wrap(~Dec)
+geom_smooth(aes(x=frame,y=F1,color=Mob))+facet_wrap(~Dec)
+geom_line(aes(x=frame,y=F1,fill=Mob))+facet_wrap(~Dec)



#Models for CLS


ow1<- gam(F2.norm ~ JF+s(frame,by=JF),data=ow,method='ML')
ow1.5<-gam(F2.norm ~ JF+YOB+s(frame,by=JF),data=ow,method='ML')
ow2<- gam(F2.norm ~ JF+YOB+te(frame,YOB,by=JF,k=4),data=ow,method='ML')
ow3<- gam(F2.norm ~ JF+YOB+Gender+te(frame,YOB,by=JF)+te(frame,YOB,by=Gender),data=ow,method='ML')
ow4<- gam(F2.norm ~ JF+YOB+Gender+Edu_bin+te(frame,YOB,by=JF)+te(frame,YOB,by=Edu_bin)+te(frame,YOB,by=Gender),data=ow,method='ML')

newdata<-expand.grid(frame=seq(1,20,by=.1),YOB=1935:2000,JF=levels(ow$JF))
newdata$F2<-predict(ow2,newdata)
newdata$F2.se<-predict(ow2,newdata,se.fit=T)$se.fit
newdata<-newdata[newdata$JF!='owF'&newdata$JF!='owL',]
newdata$JF<-factor(newdata$JF)
newdata$JF<-factor(newdata$JF,levels=levels(newdata$JF)[c(2,1,3,4)])
names(newdata)[3]<-'Environment'
ggplot(newdata[newdata$frame==15,],aes(x=YOB,y=F2,linetype=Environment))+geom_ribbon(aes(ymax=F2+1.96*F2.se,ymin=F2-1.96*F2.se),color='black',alpha=.001)+geom_line(aes(linetype=Environment))+theme_bw()+ylab('F2/S(F2)')+xlab('Year')+theme(legend.title=element_blank(),panel.border=element_rect(size=1.5))+ggtitle('/o/')+ggsave('/Users/pplsuser/Desktop/CLS52AuthorKit/owphoneticconditioning.pdf',width=3,height=2)

uw1<- gam(F2.norm ~ JF+s(frame,by=JF,k=4),data=uw,method='ML')
uw1.5<-gam(F2.norm ~ JF+YOB+s(frame,by=JF,k=4),data=uw,method='ML')
uw2<- gam(F2.norm ~ JF+YOB+te(frame,YOB,by=JF,k=4),data=uw,method='ML')
compareML(uw2,uw1.5)

uwnewdata<-expand.grid(frame=seq(1,20,by=.1),YOB=1935:2000,JF=levels(uw$JF))
uwnewdata$F2<-predict(uw2,uwnewdata)
uwnewdata$F2.se<-predict(uw2,uwnewdata,se.fit=T)$se.fit
uwnewdata$JF<-factor(uwnewdata$JF)
names(uwnewdata)[3]<-'Environment'
ggplot(uwnewdata[uwnewdata$frame==15,],aes(x=YOB,y=F2,linetype=Environment))+geom_ribbon(aes(ymax=F2+1.96*F2.se,ymin=F2-1.96*F2.se),color='black',alpha=.001)+geom_line(aes(linetype=Environment))+theme_bw()+ylab('F2/S(F2)')+xlab('Year')+theme(legend.title=element_blank(),panel.border=element_rect(size=1.5))+ylim(c(0.5,1.6))+ggtitle('/u/')+ggsave('/Users/pplsuser/Desktop/CLS52AuthorKit/uwphoneticconditioning.pdf',width=3,height=2)


#the role of vowel dynamics -- for harrington
ow$Mod<-cut(ow$dim3,2)
ow$Dec<-cut(ow$YOB,3)
uw$Mod<-cut(uw$dim3,2)
uw$Dec<-cut(uw$YOB,3)
ow4<- bam(F2.norm ~ JF + Dec+Mod+s(frame, by=JF)+s(frame,by=Dec)+s(frame,by=Mod), data=ow)
newdata<-expand.grid(frame=seq(1,20,by=.1),JF=levels(ow$JF),Mod=levels(ow$Mod),Dec=levels(ow$Dec))
newdata$F2<-predict(ow4,newdata)
newdata$F2.se<-predict(ow4,newdata,se.fit=T)$se.fit
newdata$Dec<-cut(newdata$YOB,3)
levels(newdata$Mod)<-c('Lower mobility','Higher mobility')
levels(newdata$Dec)<-c('1935-1960','1961-1980','1981-2000')
ggplot()+geom_line(data=newdata[newdata$JF=='ow',],aes(x=frame,y=F2,color=Mod,group=Mod,stat='smooth',method='loess',linetype=Mod))+geom_ribbon(data=newdata[newdata$JF=='ow',],aes(x=frame,ymax=F2+1.96*F2.se,ymin=F2-1.96*F2.se,color=Mod),alpha=.001)+facet_wrap(~Dec)+theme_bw()+theme(legend.title=element_blank(),panel.border=element_rect(color='black',size=1.5),strip.background=element_rect(color='black',fill='white',size=1.5))+xlab('Measurement point')+ylab('Normalized F2')+ggtitle('/o/ F2 Dynamics (SES)')+scale_color_manual(values=c('#377eb8','#000000'))+guides(color = guide_legend(reverse=TRUE),linetype = guide_legend(reverse=TRUE))+ggsave('/Users/pplsuser/Desktop/Labphon_posters/owdynamicsclassh.pdf',height=3.25,width=7.5)

uw2<- bam(F2.norm ~ JF + Dec+s(frame, by=JF)+s(frame,by=Dec), data=uw)
uw4<- bam(F2.norm ~ JF + Dec+Mod+s(frame, by=JF)+s(frame,by=Dec)+s(frame,by=Mod), data=uw)
uw$Pseudo<-factor(uw$Pseudo)
uwnewdata<-expand.grid(frame=seq(1,20,by=.1),JF=levels(uw$JF),Mod=levels(uw$Mod),Dec=levels(uw$Dec))
uwnewdata$F2<-predict(uw4,uwnewdata,exclude='Pseudo')
uwnewdata$F2.se<-predict(uw4,uwnewdata,se.fit=T,exclude='Pseudo')$se.fit
uwnewdata$Dec<-cut(uwnewdata$YOB,3)
levels(uwnewdata$Mod)<-c('Lower mobility','Higher mobility')
levels(uwnewdata$Dec)<-c('1935-1960','1961-1980','1981-2000')
ggplot()+geom_line(data=uwnewdata[uwnewdata$JF=='Kuw',],aes(x=frame,y=F2,color=Mod,group=Mod,stat='smooth',method='loess',linetype=Mod))+geom_ribbon(data=uwnewdata[uwnewdata$JF=='Kuw',],aes(x=frame,ymax=F2+1.96*F2.se,ymin=F2-1.96*F2.se,color=Mod),alpha=.001)+facet_wrap(~Dec)+theme_bw()+scale_color_grey(start=0.5,end=0.1)+theme(legend.title=element_blank(),panel.border=element_rect(color='black',size=1.5),strip.background=element_rect(color='black',fill='white',size=1.5))+xlab('Measurement point')+ylab('F2/S(F2)')+ggtitle('/u/')+guides(color = guide_legend(reverse=TRUE),linetype = guide_legend(reverse=TRUE))+ggsave('/Users/pplsuser/Desktop/CLS52AuthorKit/uwdynamicsclass.pdf',height=3,width=7)
