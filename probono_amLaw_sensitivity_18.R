## Project: Status and Collaboration: The Case of Pro Bono Network Inequalities in Corporate Law
## Purpose: Sensitivity analysis using American Law Review data
## Code written by Diego F. Leal (www.diegoleal.info)
## Last revision: 7/27/2018


## clear all
rm(list=ls())
## setting working directory
setwd("C:/Users/diego/Dropbox/DLeal/lenovo_recovered/Documents/universidades/phd/probono_2017/probono_output_data_files")

#import data
data <- read.csv(file="finalDataAmLawRev.csv",header=TRUE,as.is=TRUE)

#drop AM Lw firms with missing data
dataLSM <- subset(data,firm_1!="Davis Wright Tremaine")
dataLSM <- subset(dataLSM,firm_2!="Davis Wright Tremaine")
dataLSM <- subset(dataLSM,firm_1!="Cahill Gordon")
dataLSM <- subset(dataLSM,firm_2!="Cahill Gordon")
dataLSM <- subset(dataLSM,firm_1!="McKee Nelson")
dataLSM <- subset(dataLSM,firm_2!="McKee Nelson")
dataLSM <- subset(dataLSM,firm_1!="Drinker Biddle")
dataLSM <- subset(dataLSM,firm_2!="Drinker Biddle")
dataLSM <- subset(dataLSM,firm_1!="Choate Hall and Stewart")
dataLSM <- subset(dataLSM,firm_2!="Choate Hall and Stewart")
dataLSM <- subset(dataLSM,firm_1!="Thelen Reid")
dataLSM <- subset(dataLSM,firm_2!="Thelen Reid")

#make dyads a data.frame
data<-as.data.frame(dataLSM$dyad)
colnames(data)<-"dyad"

setwd("C:/Users/diego/Dropbox/DLeal/USC/papers/Probono/probono_2018/sensitivity_analysis/amLaw")
write.csv(x=dataLSM,file="dataLSMAmLAw.csv",row.names=TRUE)

##### clean and computre controls (this is basically a copy of the things done in the contex of the
##### model_selection.do do-file)

#computer Jaccard Index
data$jaccard100<-dataLSM$jaccardIndex * 100


#classify dyads based on their headquarters
data$sameCity2<-ifelse(dataLSM$location_1==dataLSM$location_2 &
                       dataLSM$location_1=="New York",
                       yes=1,
                       no=0
                      )

data$sameCity3<-ifelse(dataLSM$location_1==dataLSM$location_2 &
                       dataLSM$location_1!="New York",
                       yes=1,
                       no=0
                      )

data$location_1<-dataLSM$location_1
data$location_2<-dataLSM$location_2

#compute difference in revenue
dataLSM$gross_revenue_1<-as.numeric(dataLSM$gross_revenue_1)
dataLSM$gross_revenue_2<-as.numeric(dataLSM$gross_revenue_2)
dataLSM$rev_diff<-abs(dataLSM$gross_revenue_1 - dataLSM$gross_revenue_2)
data$logrev_diff<-log(dataLSM$rev_diff + 1)

#compute difference in percent female
dataLSM$percentfemale_1<-as.numeric(dataLSM$percentfemale_1)
dataLSM$percentfemale_2<-as.numeric(dataLSM$percentfemale_2)
data$gender_diff=abs(dataLSM$percentfemale_1-dataLSM$percentfemale_2)

#compute difference in percent minority
dataLSM$percentminority_1<-as.numeric(dataLSM$percentminority_1)
dataLSM$percentminority_2<-as.numeric(dataLSM$percentminority_2)
data$race_diff=abs(dataLSM$percentminority_1-dataLSM$percentminority_2)

#compute difference in size
dataLSM$num_attorneys_1<-as.numeric(dataLSM$num_attorneys_1)
dataLSM$num_attorneys_2<-as.numeric(dataLSM$num_attorneys_2)
data$size_diff=abs(dataLSM$num_attorneys_1-dataLSM$num_attorneys_2)

#compute difference in pb hrs
dataLSM$total_hours_1<-as.numeric(dataLSM$total_hours_1)
dataLSM$total_hours_2<-as.numeric(dataLSM$total_hours_2)
data$pbavg_diff=abs(dataLSM$total_hours_1-dataLSM$total_hours_2)

#compute difference in firms' age
dataLSM$age_1<-as.numeric(dataLSM$age_1)
dataLSM$age_2<-as.numeric(dataLSM$age_2)
data$age_diff=abs(dataLSM$age_1-dataLSM$age_2)

#compute difference in number of practice areas
dataLSM$practiceAreas_1<-as.numeric(dataLSM$practiceAreas_1)
dataLSM$practiceAreas_2<-as.numeric(dataLSM$practiceAreas_2)
data$practiceArea_diff=abs(dataLSM$practiceAreas_1-dataLSM$practiceAreas_2)

#store the amLaw 200 rank
dataLSM$amlaw200_rank_1<-as.numeric(dataLSM$amlaw200_rank_1)
dataLSM$amlaw200_rank_2<-as.numeric(dataLSM$amlaw200_rank_2)

## classify am law dyads based on the following threshold 1st-25th / 26th-75th / 76th-100th
data$amLaw_2<-ifelse(
              (dataLSM$amlaw200_rank_1>=50 & dataLSM$amlaw200_rank_1<=150) &
              (dataLSM$amlaw200_rank_2>=50 & dataLSM$amlaw200_rank_2<=150),
              yes=1,
              no=0)

##calculate the status difference measure
data$amLaw_3<-ifelse( dataLSM$amlaw200_rank_1>150 &
                      dataLSM$amlaw200_rank_2>150 ,
                      yes=1,
                      no=0)


dataLSM$amLaw_CAT_1<-ifelse(dataLSM$amlaw200_rank_1 <=50,yes=1,no=
                            ifelse(dataLSM$amlaw200_rank_1 > 50 &
                            dataLSM$amlaw200_rank_1 < 150,yes=2,no=3))

dataLSM$amLaw_CAT_2<-ifelse(dataLSM$amlaw200_rank_2 <=50,yes=1,no=
                            ifelse(dataLSM$amlaw200_rank_2 > 50 &
                            dataLSM$amlaw200_rank_2 < 150,yes=2,no=3))

dataLSM$dif_CAT<-abs(dataLSM$amLaw_CAT_1 - dataLSM$amLaw_CAT_2)

data$amLaw_4<-ifelse(dataLSM$dif_CAT==1,yes=1,no=0)

data$amLaw_5<-ifelse(dataLSM$dif_CAT==2,yes=1,no=0)    

data$firm_1<-dataLSM$firm_1
data$firm_2<-dataLSM$firm_2

dataLSM<-data

YYY<-as.vector(unique(dataLSM$firm_1)) ## unique names in firm_1.x
AAA<-as.vector(unique(dataLSM$firm_2)) ## unique names in firm_2.x
TTT<-c(YYY,AAA)                        ## unique names, these are all the names in dataLSM
TTT<-unique(TTT)

BBB<-as.data.frame(cbind(TTT,TTT))
BBB$loops<-paste(BBB[,1],BBB[,2],sep="-")

colnames(BBB)<-c("firm_1","firm_2","dyad")

data<-merge(dataLSM,BBB,by=("dyad"),all=T)

#Z is the resulting data set
ZZZ<-data

ZZZ$firm_1.x<-as.character(ZZZ$firm_1.x)
ZZZ$firm_2.x<-as.character(ZZZ$firm_2.x)
ZZZ$firm_1.y<-as.character(ZZZ$firm_1.y)
ZZZ$firm_2.y<-as.character(ZZZ$firm_2.y)

ZZZ$firm_1<-ifelse(is.na(ZZZ$firm_1.y),yes=ZZZ$firm_1.x,no=ZZZ$firm_1.y)
ZZZ$firm_2<-ifelse(is.na(ZZZ$firm_2.y),yes=ZZZ$firm_2.x,no=ZZZ$firm_2.y)
View(ZZZ)

# order ZZZ base din dyads' names
PPP<-ZZZ[with(ZZZ,order(ZZZ$firm_2,ZZZ$firm_1)),]
View(PPP)

# make loops 0
PPP[is.na(PPP)] <- 0

dataLSM<-PPP

dataLSM$firm_1<-as.character(dataLSM$firm_1)
dataLSM$firm_2<-as.character(dataLSM$firm_2)

library(sna)

g<-list() 

variables<-as.vector(c(
  "jaccard100",  "dyad" ,"gender_diff"	,"race_diff",	"size_diff"	,"age_diff",
  "practiceArea_diff",	"logrev_diff",	"pbavg_diff",	"amLaw_2",
  "amLaw_3",	"amLaw_4",	"amLaw_5",	"sameCity2",	"sameCity3"))

for (i in 1:length(variables))                          ## for each variable:
{
  setwd("C:/Users/diego/Dropbox/DLeal/USC/papers/Probono/probono_2018/sensitivity_analysis/amLaw")
  library(xlsx)
  require(reshape2)
  mat<-acast(dataLSM,firm_2~firm_1,value.var=variables[i])  ## create the corresponding matrix using reshape2
  mat<-symmetrize(mat,rule="lower")                         ## symmetrize the matrix because the acast function will put all the info in the upper triangle
  g[[i]]<-mat                                               ## store the new variable/matrix in the list g
  x<-paste(variables[i],"xlsx", sep='.')
  write.xlsx(x=g[[i]],x,row.names=F,col.names=F)
}


# dependent variables
jaccard<-g[[1]]  ## jaccard coefficient
invLogW<-g[[2]]  ## inverse log-weighted similarity

# Create the 'graph stack' of covariates
covariates <- list(middleStatus=g[[10]],lowStatus=g[[11]],oneDiffStatus=g[[12]],
                   twoDiffStatus=g[[13]],
                   pbHrsDiff=g[[9]],logRevDif=g[[8]],
                   sizeDiff=g[[5]],ageDiff=g[[6]],genderDiff=g[[3]],raceDiff=g[[4]],
                   pracAreDiff=g[[7]],sameCities=g[[14]],NYC=g[[15]])



# run the regressions
set.seed(64783)

#########################################################################
#                               OLS MODELS
#########################################################################

## All variables/unrestricted model

system.time(olsjaccardALL <- netlm(jaccard,covariates,nullhyp="classical"))
print.summary.netlm(olsjaccardALL)

#########################################################################
#           MRQAP using Dekker's "semi-partialling plus" procedure
#########################################################################

## All variables/unrestricted model

system.time(qapjaccardALL <- netlm(jaccard,covariates,nullhyp="qapspp",reps=2000))
print.summary.netlm(qapjaccardALL)


save.image("amLaw_Objects.Rdata")
