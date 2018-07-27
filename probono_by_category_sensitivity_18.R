## Project: Status and Collaboration: The Case of Pro Bono Network Inequalities in Corporate Law
## Purpose: Sensitivity analysis by type of pro bono category (cause-oriented and legal services)
## Code written by Diego F. Leal (www.diegoleal.info)
## All rights reserved
## Last revision: 7/27/2018

## clear all
rm(list=ls(all=TRUE))

## session info
sessionInfo()

# R version 3.5.0 (2018-04-23)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows >= 8 x64 (build 9200)
# 
# Matrix products: default
# 
# locale:
#   [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252   
# [3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
# [5] LC_TIME=English_United States.1252    
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] igraph_1.2.1         reshape2_1.4.3       xlsx_0.6.1           sna_2.4             
# [5] statnet.common_4.1.4 network_1.13.0.1     reshape_0.8.7       
# 
# loaded via a namespace (and not attached):
#   [1] Rcpp_0.12.17    lattice_0.20-35 xlsxjars_0.6.1  grid_3.5.0      plyr_1.8.4     
# [6] magrittr_1.5    coda_0.19-1     stringi_1.1.7   tools_3.5.0     stringr_1.3.1  
# [11] compiler_3.5.0  pkgconfig_2.0.1 rJava_0.9-10  

##########################
## Importing data files ##
##########################


detach(package:igraph)
detach(package:sna)
detach(package:network)

r<-2  ## if R = 1, then the results re: legal services are calculated
      ## if R = 2, then the results re: cause-oriented services are calculated

## setting working directory
#reading in the raw network data sent by Steve
setwd("C:/Users/diego/Dropbox/DLeal/USC/papers/Probono/probono_2018/sensitivity_analysis/matrices_by_org_category")

name_data<-c("Pro_Bono_Final_Clean-aipversionCSV_ACLU_G2.csv", "Pro_Bono_Final_Clean-aipversionCSV_ACLU_G3.csv")

rawNetData <- read.csv(name_data[r],header=TRUE,as.is=TRUE)

setwd("C:/Users/diego/Dropbox/DLeal/USC/papers/Probono/probono_2018/probono_input_data_files")
#reading in the list of firms with complete info (see probono_work_floe_master_file.doc)
firms<-read.csv(file="final_list_of_firms_18.csv",header=TRUE,as.is=TRUE)

#reading in Vault rank data 
vaultRank<-read.csv(file="vault_lexisNexis_CSV_18.csv",header=TRUE,as.is=TRUE)

#reading in the firm-level data different from the Vault data (e.g. revenue)
firmData <- read.csv(file="firmdata2005CSV.csv",header=TRUE,as.is=TRUE)

############################
## Creating a master file ##
############################

#merging rawNetData and vaultFirms
FULLDATA<-merge(rawNetData,firms, by= c("firm"), all=F )

#merging FULLDATA and vault rank firms
FULLDATA<-merge(FULLDATA,vaultRank, by= c("firm"), all=F )

FULLDATA<-subset(FULLDATA, category==r+1)

#export full data set
setwd("C:/Users/diego/Dropbox/DLeal/USC/papers/Probono/probono_2018/sensitivity_analysis/matrices_by_org_category")
write.csv(x=FULLDATA,file="fullData.csv",row.names=TRUE)

#loading relevant packages
library (igraph)
library (reshape)

############################################################################
# generating the two-mode net and the relevant one-mode network projection #
############################################################################

#creating and object to store the two-mode network (orgs X firms) in edgelist form
orgFirmEdgeList <-data.frame(FULLDATA$finalorg,FULLDATA$firm)

#generating the two-mode network (orgs X firms) in matrix form
orgFirmMatrix<-as.matrix(table(orgFirmEdgeList))

#binarizing the two-mode network bc the original data (i.e. Pro_Bono_Final_Clean-aipversionCSV_ACLU.csv) has some rows repeated
orgFirmMatrix<-ifelse(test=orgFirmMatrix>0,yes=1,no=0)

#generating a one-mode firm x firm network projection:
firmFirmMatrix<-tcrossprod(t(orgFirmMatrix))

#generating a one-mode org x org network projection:
orgOrgMatrix<-tcrossprod(orgFirmMatrix)

##########################################################################
#### jaccard index of similarity and the inverse log-weighted similarity #
##########################################################################

#creating a two-mode network in igraph 
iGraphObject<-graph_from_adjacency_matrix(firmFirmMatrix, mode="undirected")

#computing the Jaccard index of similarity and inv log-w 
jaccardSimilarity<-as.matrix(similarity(iGraphObject,mode="all",method="jaccard",loops=FALSE))
invLogWSimilarity<-as.matrix(similarity(iGraphObject,mode="all",method="invlogweighted",loops=FALSE))

#labeling the resulting matrices
rownames(invLogWSimilarity)<-colnames(invLogWSimilarity)<-rownames(jaccardSimilarity)<-colnames(jaccardSimilarity)<-(colnames(firmFirmMatrix))

#generating missing values for all cells in the upper triangle (i.e. ties are undirected) and the main diagonal
jaccardSimilarity[upper.tri(jaccardSimilarity)] <- NA
invLogWSimilarity[upper.tri(invLogWSimilarity)] <- NA
diag(jaccardSimilarity)<- NA
diag(invLogWSimilarity)<- NA

#generating the edgelist (i.e. dyadic) representation of similarity matrices
jaccardSimilarity<-melt(jaccardSimilarity)
invLogWSimilarity<-melt(invLogWSimilarity)
names(jaccardSimilarity)[names(jaccardSimilarity)=="value"]<-"jaccardIndex"
names(invLogWSimilarity)[names(invLogWSimilarity)=="value"]<-"inveLogIndex"

#delete NAs and loops
jaccardSimilarity <- jaccardSimilarity[complete.cases(jaccardSimilarity$jaccardIndex),]
invLogWSimilarity <- invLogWSimilarity[complete.cases(invLogWSimilarity$inveLogIndex),]

#create a new dyadic label
jaccardSimilarity$dyad <- paste(jaccardSimilarity$X1,jaccardSimilarity$X2, sep='-')
invLogWSimilarity$dyad <- paste(invLogWSimilarity$X1,invLogWSimilarity$X2, sep='-')

#######################################################################
# similarity matrix: merging jaccard data and inv. log weighted data ##
#######################################################################

#merging jaccard similarity and inv log similarity data
dyadicData<-merge(jaccardSimilarity,invLogWSimilarity, by= c("dyad"), all=T )

#cleaning and rearrangind the similarity data set
dyadicData <- dyadicData[, ! names(dyadicData) %in% "X1.x", drop = F] ## delete variable
dyadicData <- dyadicData[, ! names(dyadicData) %in% "X2.x", drop = F] ## delete variable
names(dyadicData)[names(dyadicData)=="X1.y"]<-"firm_1"
names(dyadicData)[names(dyadicData)=="X2.y"]<-"firm_2"
dyadicData<-dyadicData[with(dyadicData, order(firm_2, firm_1)), ]
dyadicData<-dyadicData[,c("dyad", "firm_1","firm_2","jaccardIndex","inveLogIndex")]

###################################
# preparing firm data for merge ###
###################################

#renaming merging variable
names(firmData)[names(firmData)=="organization_name"]<-"firm"

#adding degree centrality to the firm-level data set
detach(package:igraph)
library(network)
library(sna)

#### DEGREE

net<-as.network.matrix(firmFirmMatrix, matrix.type="adjacency",directed=FALSE)
newVariable<-degree(net, cmode="indegree")
newVariable<-as.data.frame(cbind(names,newVariable))
names(newVariable)[names(newVariable)=="names"]<-"firm"
names(newVariable)[names(newVariable)=="newVariable"]<-"degree"

### AGE

#adding age to the firm-level data set
setwd("C:/Users/diego/Dropbox/DLeal/USC/papers/Probono/probono_2018/probono_input_data_files")
newVariable <- read.csv(file="ageFirm.csv",header=TRUE,as.is=TRUE)
colnames(newVariable)<-c("firm", "age")
firmData<-merge(firmData,newVariable, by= c("firm"), all=F )

## PRACTICE AREAS

#adding practice areas to the firm-level data set
newVariable <- read.csv(file="practiceAreaFirm.csv",header=TRUE,as.is=TRUE)
colnames(newVariable)<-c("firm", "practiceAreas")
firmData<-merge(firmData,newVariable, by= c("firm"), all=F )

## VAULT RANK

#adding adding LexisNexis and Vault data (there are data for 86 firms only)
newVariable <- read.csv(file="vault_lexisNexis_CSV_18.csv",header=TRUE,as.is=TRUE)
names(newVariable)[names(newVariable)=="firmName"]<-"firm"
firmData<-merge(firmData,newVariable, by= c("firm"), all=F )

#make a copy of the data set, one for each firm in a given dyad
firmData_1<-firmData
firmData_2<-firmData
colnames(firmData_1)<- paste(colnames(firmData),"1",sep = "_")
colnames(firmData_2)<- paste(colnames(firmData),"2",sep = "_")

#merging the data for each firm in a given dyad to the similarity data
dyadicData<-merge(firmData_1, dyadicData, by= c("firm_1"), all=F )
dyadicData<-merge(firmData_2, dyadicData, by= c("firm_2"), all=F )

#There should be (67*(67-1))/2 = 2211 for category # 2 (legal services)
dim(dyadicData)

#import dyadic data created inprobono_data_processing_18.do and merge loops 
setwd("C:/Users/diego/Dropbox/DLeal/USC/papers/Probono/probono_2018/sensitivity_analysis/matrices_by_org_category")
dataLSM<-read.csv(file="dataLSMbyCat.csv",header=TRUE,as.is=TRUE)

#merge dyadicData (the data with the correct/restricted # dyads) and the full dyadic data set(dataLSM)
#the merge have to excplicitly say "all.x" the get all the loops
dataLSM<-merge(dataLSM,dyadicData, by= c("dyad"), all.x=T )
dim(dataLSM)

#flag the loops
dataLSM$loop<-ifelse(dataLSM$firm_1.x==dataLSM$firm_2.x,yes=1,no=0)

YYY<-as.vector(unique(dataLSM$firm_1.x)) ## unique names in firm_1.x
AAA<-as.vector(unique(dataLSM$firm_2.x)) ## unique names in firm_2.x
TTT<-unique(YYY,AAA)                     ## unique names, these are all the names in dataLSM
ZZZ<-(colnames(firmFirmMatrix))          ## retrieve firms names based in the firm by firm matrix
OOO<-c(TTT,ZZZ)                          ## combine all names
CCC<-names(which(table(OOO) == 1))       ## make a list with the firms that are only listed once, these firms have to be excluded
 
dataLSM<-subset(dataLSM,loop==1 | vault2005_1 >= 0)  ## keep dyads that represent either loops/seld-ties or thta have an actual JaccardInded

for(a in 1:length(CCC))   ## subset the dyadic data set so that the firms in CCC are excluded
{
  element<-CCC[a]
  dataLSM<-subset(dataLSM,firm_1.x!=element | firm_2.x!=element)
}

## dimsentions of the resulting data set:
## there should be should be 
## (67*(67-1))/2 = 2211 + 67 loops = 2, for category # 2 (legal services)
##
dim(dataLSM)

g<-list()  # this list will contain all the (14) variables under analysis.

## multiple original jaccardIndex * 100
dataLSM$jaccard100<-(dataLSM$jaccardIndex)*100

#create a new variable to store firm_1 labels
dataLSM$firm_1P<-dataLSM$firm_1.x
#create a new variable to store firm_2 labels
dataLSM$firm_2P<-ifelse(is.na(dataLSM$firm_2.x),yes = dataLSM$firm_2.y,no=dataLSM$firm_2.x)
#create a new variable to store dyads labels
dataLSM$dyad_P<-ifelse(dataLSM$firm_1P == dataLSM$firm_2P,yes = paste(dataLSM$firm_1P, dataLSM$firm_2P,sep="-"),no=dataLSM$dyad)


#add 0 to loops
dataLSM[is.na(dataLSM)] <- 0


## label variables

variables<-as.vector(c(
  "jaccard100",  "inveLogIndex.x" ,"gender_diff"	,"race_diff",	"size_diff"	,"age_diff",
  "practiceArea_diff",	"logrev_diff",	"pbavg_diff",	"vaultVar2",
  "vaultVar3",	"vaultVar4",	"vaultVar5",	"sameCityDum2",	"sameCityDum3"))

##export excel files in order to usew them in UCINEt

for (i in 1:length(variables))                          ## for each variable:
{
  setwd("C:/Users/diego/Dropbox/DLeal/USC/papers/Probono/probono_2018/sensitivity_analysis/matrices_by_org_category")
  library(xlsx)
  require(reshape2)
  mat<-acast(dataLSM,firm_2P~firm_1P,value.var=variables[i])  ## create the corresponding matrix using reshape2
  mat<-symmetrize(mat,rule="upper")                         ## symmetrize the matrix because the acast function will put all the info in the upper triangle
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

image_name<-c("legal_services_Objects.Rdata","cause-oriented_Objects.Rdata")

setwd("C:/Users/diego/Dropbox/DLeal/USC/papers/Probono/probono_2018/sensitivity_analysis/matrices_by_org_category")
save.image(image_name[r])

detach(package:sna)
detach(package:network)



