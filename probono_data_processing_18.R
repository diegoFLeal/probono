## Project: Status and Collaboration: The Case of Pro Bono Network Inequalities in Corporate Law
## Purpose: Main data processing and analysis script
## Code written by Diego F. Leal (www.diegoleal.info)
## All rights reserved
## Last revision: 7/27/2018

## clear all
rm(list=ls())
## setting working directory
setwd("C:/Users/diego/Dropbox/DLeal/USC/papers/Probono/probono_2018/probono_input_data_files")
## session info
sessionInfo()

# R version 3.5.0 (2018-04-23)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows >= 8 x64 (build 9200)
# 
# Matrix products: default
# 
# locale:
#   [1] LC_COLLATE=English_United States.1252 
# [2] LC_CTYPE=English_United States.1252   
# [3] LC_MONETARY=English_United States.1252
# [4] LC_NUMERIC=C                          
# [5] LC_TIME=English_United States.1252    
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods  
# [7] base     
# 
# loaded via a namespace (and not attached):
#  [1] compiler_3.5.0 tools_3.5.0   


##########################
## Importing data files ##
##########################

#reading in the raw network data sent by Steve
rawNetData <- read.csv(file="Pro_Bono_Final_Clean-aipversionCSV_ACLU.csv",header=TRUE,as.is=TRUE)

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

#export full data set
setwd("C:/Users/diego/Dropbox/DLeal/USC/papers/Probono/probono_2018/probono_output_data_files")
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

#adding firms labels
newVariable<-as.data.frame(newVariable) 
newVariable<-subset(newVariable,select=-firm)
newVariable$firm<-firms$firm
firmData<-merge(firmData, newVariable, by= c("firm"), all=F )

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

#since there are 84 firms, the number of possible dyads 
#should be (84*(84-1))/2 = 3486
dim(dyadicData)

#exporting the dyadic data
setwd("C:/Users/diego/Dropbox/DLeal/USC/papers/Probono/probono_2018/probono_output_data_files")
dyadicData<-dyadicData[with(dyadicData, order(firm_2, firm_1)), ]
dyadicData[is.na(dyadicData)] <- "." 
dyadicData<-as.matrix(dyadicData)
write.csv(x=dyadicData,file="finalData.csv",row.names=T)
library(readstata13)
dyadicDatadf<-read.csv(file="finalData.csv",header=TRUE,as.is=TRUE)
save.dta13(dyadicDatadf, "finalData.dta")

###################################
#binarizing the one-mode matrices #
###################################

# binarizing the networks
firmFirmMatrixBinary<-ifelse(test=firmFirmMatrix>0,yes=1,no=0)
orgOrgMatrixBinary<-ifelse(test=orgOrgMatrix>0,yes=1,no=0)

#exporting firmFirmVault as a CSV file:
diag(firmFirmMatrixBinary)<- 0
diag(orgOrgMatrixBinary)<- 0
diag(firmFirmMatrix)<- 0
diag(orgOrgMatrix)<- 0

#################################
# Exporting all matrices
################################

write.csv(x=firmFirmMatrixBinary,file="firmFirmMatrixBinary.csv",row.names=TRUE)
write.csv(x=orgOrgMatrixBinary,file="orgOrgMatrixBinary.csv",row.names=TRUE)
write.csv(x=firmFirmMatrix,file="firmFirmMatrix.csv",row.names=TRUE)
write.csv(x=orgOrgMatrix,file="orgOrgMatrix.csv",row.names=TRUE)
write.csv(x=orgFirmMatrix,file="firmOrgMatrix.csv",row.names=TRUE)

######
#
# ONE-MODE DESCRIPTIVES
#
######

library(igraph)

#create an igraph object
binaryIGraph<-graph.adjacency(firmFirmMatrixBinary, mode = "undirected", add.colnames = NULL, diag = F)

#net size
dim(firmFirmMatrixBinary) # 84

#number of ties
sum(firmFirmMatrixBinary) / 2 # 2155
binaryIGraph

#density 0.62
gden(firmFirmMatrixBinary, g=NULL, diag=F, mode="graph", ignore.eval=FALSE)

#degree by node
degOneMode<-degree(binaryIGraph,mode="in",loops=F)

#mean degree
mean(degOneMode)
sd(degOneMode)

## number of isolates
min(degOneMode) # no isolate

## degree centralization
centr_degree(binaryIGraph)$centralization

## mean geodesic distance
mean(distances(binaryIGraph))
sd(distances(binaryIGraph))

#max geodistance
max(distances(binaryIGraph))

## extract degree and category of Orgs
detach(package:igraph)
library (sna)
library (network)

## setting working directory
setwd("C:/Users/diego/Dropbox/DLeal/USC/papers/Probono/probono_2018/probono_input_data_files")

#reading in the orgs' names
orgsAttributes <- read.csv(file="final_list_of_orgs_18.csv",header=TRUE,as.is=TRUE)
netOrgs<-as.network.matrix(orgOrgMatrixBinary, matrix.type="adjacency",directed=FALSE)

#compute degree
orgsAttributes$degree<-(degree(netOrgs,cmode="indegree"))

## setting working directory
setwd("C:/Users/diego/Dropbox/DLeal/USC/papers/Probono/probono_2018/probono_output_data_files")
write.csv(x=orgsAttributes,file="degreeNonProfits.csv",row.names=T)

#merge attributes and original data
orgsData<-merge(orgsAttributes,rawNetData,by=c("finalorg"), all=F)

#delete duplicates (legal serivices = category # 2, cause-oriented = category # 3)
orgsAttributes<- as.data.frame(unique(orgsData[ ,c("finalorg","degree","category", "issue")]))
orgsAttributes<-orgsAttributes[!duplicated(orgsAttributes$finalorg), ]

#export orgs attributes
write.csv(x=orgsAttributes,file="orgsAttributes.csv",row.names=T)

## create Figure 4 Number of Connections of Pro Bono Client Organizations to Law Firms

#left panel

hist(orgsAttributes$degree,
     col="gray",
     breaks = 25,
     main="All Pro Bono Organizations",
     xlab = "Degree",
     xlim = c(1, 140), ylim = NULL)

#mid panel

legalServ<-subset(orgsAttributes, category==2)

hist(legalServ$degree,
     col="gray",
     breaks = 25,
     main="Legal Services",
     xlab = "Degree",
     xlim = c(1, 140), ylim = NULL)

# right panel

causeOrie<-subset(orgsAttributes, category==2)

hist(causeOrie$degree,
     col="gray",
     breaks = 25,
     main="Cause-oriented",
     xlab = "Degree",
     xlim = c(1, 140), ylim = NULL)


##################################
# firm-level descriptve stats (Table 1, right panel)
##################################

descriptive<-as.data.frame(firmData)

#getting rid of firms not included in the final analysis
descriptive<-subset.data.frame(descriptive,voultCode<=2)
descriptive<-subset.data.frame(descriptive,firm!="Cahill Gordon")
descriptive<-subset.data.frame(descriptive,firm!="Choate Hall and Stewart")
descriptive<-subset.data.frame(descriptive,firm!="Thelen Reid")

dim(descriptive) # there must be 84 rows


mean(descriptive$avg_hrs_per_lawyer)
sd(descriptive$avg_hrs_per_lawyer)

mean(log(descriptive$gross_revenue))
sd(log(descriptive$gross_revenue))
mean(descriptive$num_attorneys)
sd(descriptive$num_attorneys)
2005 - (mean(descriptive$age))
sd(descriptive$age)
mean(descriptive$percentfemale,na.rm=T)
sd(descriptive$percentfemale, na.rm=T)
mean(descriptive$minority_percentage)
sd(descriptive$minority_percentage)
mean(descriptive$practiceAreas)
sd(descriptive$practiceAreas)

descriptive$status1<-ifelse(descriptive$vault2005<26,1,0)
descriptive$status2<-ifelse(descriptive$vault2005>=26&descriptive$vault2005<76,1,0)
descriptive$status3<-ifelse(descriptive$vault2005>=76,1,0)
descriptive$status<-NA
descriptive$status[descriptive$status1==1] <- 1
descriptive$status[descriptive$status2==1] <- 2
descriptive$status[descriptive$status3==1] <- 3

#avg hours per lawyer by status
sapply(split(descriptive$avg_hrs_per_lawyer, descriptive$status), mean) 
sapply(split(descriptive$total_hours, descriptive$status), mean) 



sum(descriptive$status1, na.rm = T)
sum(descriptive$status2, na.rm = T)
sum(descriptive$status3, na.rm = T)

mean(descriptive$nyc)

cor.test(descriptive$vault2005,descriptive$total_hours)

plot(descriptive$vault2005, descriptive$total_hours, main="Scatterplot Example", 
     xlab="Vault Rank", ylab="Total Hours", pch=19)
abline(lm(descriptive$total_hours~descriptive$vault2005 ), col="red") # regression line (y~x) 


### 2-D and 3-D plots of Pro Bono Hours by Rank and Status


descriptive$Status <- factor(descriptive$status)

library(car)
library(rgl)

scatterplot(total_hours ~ vault2005 | Status,  data=descriptive,
            xlab="Vault Rank", ylab="Total Hours", 
            main="Pro Bono Hours by Rank")


scatter3d(total_hours~ vault2005 | status, data=descriptive, 
            xlab="Vault Rank", ylab="Total Hours", 
            main="Pro Bono Hours by Rank")

test<-descriptive[,c("firm","total_hours","status1","status2","status3")]
degree<-as.numeric(descriptive[,c("degree")])
degree<-as.data.frame(degree)
test<-cbind(test,degree)
#write.csv(x=test,file="test.csv",row.names=T)

#################
# Network descriptive stats, Table 2
################

gden(net)
centralization(net,degree,mode="graph",normalize=T)
gplot(net,gmode="graph")
x<-geodist(net)
y<-x$gdist
isolates(net)
mean(y) ## mean geodist
max(y) ## max geo dist
x<-degree(net,gmode="graph")
mean(x)
sd(x)
x<-clique.census(net, mode = "graph", tabulate.by.vertex = F,
              clique.comembership = "bysize", enumerate = TRUE,
              na.omit = TRUE)

###########################################################################
# import dyadic data set, include loops/selfties
###########################################################################

#extract loops form the firm firm sociomatrix
selfties<-melt(firmFirmMatrix)
selfties$loops<-ifelse(test=selfties$FULLDATA.firm==selfties$FULLDATA.firm.1,yes=1,no=0)
selfties$dyad<-paste(selfties$FULLDATA.firm,selfties$FULLDATA.firm.1, sep='-')
selfties<-subset(selfties,loops==1)
selfties<-as.data.frame(selfties[,c("dyad","FULLDATA.firm","FULLDATA.firm.1")])

setwd("C:/Users/diego/Dropbox/DLeal/USC/papers/Probono/probono_2018/probono_output_data_files")


#import dyadic data created in stata (see model_selection_18.do) and merge loops 
dataLSM<-read.csv(file="dataAmLawRev_18.csv",header=TRUE,as.is=TRUE)
write.csv(x=dataLSM,file="dataLSM_18.csv",row.names=T)


#Descriptive stats for the dyadic data set (Table 1, left panel)
descriptiveDyadic<-dataLSM
mean(descriptiveDyadic$jaccard100)
sd(descriptiveDyadic$jaccard100)

mean(descriptiveDyadic$vaultVar2)
mean(descriptiveDyadic$vaultVar3)
mean(descriptiveDyadic$vaultVar4)
mean(descriptiveDyadic$vaultVar5)

mean(descriptiveDyadic$pbavg_diff)
sd(descriptiveDyadic$pbavg_diff)

mean(descriptiveDyadic$logrev_diff)
sd(descriptiveDyadic$logrev_diff)

mean(descriptiveDyadic$size_diff)
sd(descriptiveDyadic$size_diff)

mean(descriptiveDyadic$age_diff)
sd(descriptiveDyadic$age_diff)

mean(descriptiveDyadic$gender_diff)
sd(descriptiveDyadic$gender_diff)

mean(descriptiveDyadic$race_diff)
sd(descriptiveDyadic$race_diff)

mean(descriptiveDyadic$practiceArea_diff)
sd(descriptiveDyadic$practiceArea_diff)

mean(descriptiveDyadic$sameCityDum2) * 100
mean(descriptiveDyadic$sameCityDum3) * 100

## end of descroptive stats for the dyadic data


#add lopps/selfties
dataLSM<-merge(selfties,dataLSM, by= c("dyad"), all=T )
dataLSM<-dataLSM[order(dataLSM$dyad),]
Y<-dataLSM
Y$firm_2<-gsub(",", "", Y$firm_2)
Y$firm_1<-gsub(",", "", Y$firm_1)
Y$FULLDATA.firm[is.na(Y$FULLDATA.firm)] <- Y$firm_1[is.na(Y$FULLDATA.firm)]
Y$FULLDATA.firm.1[is.na(Y$FULLDATA.firm.1)] <- Y$firm_2[is.na(Y$FULLDATA.firm.1)]
dataLSM<-Y
dataLSM<-dataLSM[,-c(5,6)]
names(dataLSM)[names(dataLSM) == "FULLDATA.firm"]<-"firm_1"
names(dataLSM)[names(dataLSM) == "FULLDATA.firm.1"]<-"firm_2"

#add 0 to loops
dataLSM[is.na(dataLSM)] <- 0

#this data set (dataLSMbyCat) is a key inout for probono_by_category_sensitivity.R
dataLSMbyCat<-dataLSM
setwd("C:/Users/diego/Dropbox/DLeal/USC/papers/Probono/probono_2018/sensitivity_analysis/matrices_by_org_category")
write.csv(x=dataLSMbyCat,file="dataLSMbyCat.csv",row.names=T)

###########################################################################
# Export matrices containing DVs and IVs to run MRQAP in UCINET
###########################################################################
# Note: The original format of the variables is a traditional obs X variables
# data set. I, therefore, transformed the variables to matrix format 
# using the package "reshape2"

g<-list()  # this list will contain all the (14) variables under analysis.                     

variables<-as.vector(c(
  "jaccard100",  "inveLogIndex" ,"gender_diff"	,"race_diff",	"size_diff"	,"age_diff",
  "practiceArea_diff",	"logrev_diff",	"pbavg_diff",	"vaultVar2",
  "vaultVar3",	"vaultVar4",	"vaultVar5",	"sameCityDum2",	"sameCityDum3"))

for (i in 1:length(variables))                          ## for each variable:
{
  setwd("C:/Users/diego/Dropbox/DLeal/USC/papers/Probono/probono_2018/probono_output_data_files/UCINET")
  library(xlsx)
  require(reshape2)
  mat<-acast(dataLSM,firm_2~firm_1,value.var=variables[i])  ## create the corresponding matrix using reshape2
  mat<-symmetrize(mat,rule="upper")                         ## symmetrize the matrix because the acast function will put all the info in the upper triangle
  g[[i]]<-mat                                               ## store the new variable/matrix in the list g
  x<-paste(variables[i],"xlsx", sep='.')
  write.xlsx(x=g[[i]],x,row.names=F,col.names=F)
}

###########################################################################
# Create obkects for the IVs (Jaccard and Inverse Log-Weighted Similarity)
# and for relevant set of Covariates: all, status only, controls only
###########################################################################

# dependent variables
jaccard<-g[[1]]  ## jaccard coefficient
invLogW<-g[[2]]  ## inverse log-weighted similarity

# Create the 'graph stack' of covariates
covariates <- list(middleStatus=g[[10]],lowStatus=g[[11]],oneDiffStatus=g[[12]],
                   twoDiffStatus=g[[13]],
                   pbHrsDiff=g[[9]],logRevDif=g[[8]],
                   sizeDiff=g[[5]],ageDiff=g[[6]],genderDiff=g[[3]],raceDiff=g[[4]],
                   pracAreDiff=g[[7]],sameCities=g[[14]],NYC=g[[15]])

statusOnly <- list(middleStatus=g[[10]],lowStatus=g[[11]],oneDiffStatus=g[[12]],
                   twoDiffStatus=g[[13]])

controlsOnly <- list(pbHrsDiff=g[[9]],logRevDif=g[[8]],
                   sizeDiff=g[[5]],ageDiff=g[[6]],genderDiff=g[[3]],raceDiff=g[[4]],
                   pracAreDiff=g[[7]],sameCities=g[[14]],NYC=g[[15]])

# run the regressions
set.seed(64783)


#########################################################################
#########################################################################
#########################################################################
#                               OLS MODELS
#########################################################################
#########################################################################
#########################################################################

## All variables

system.time(olsjaccardALL <- netlm(jaccard,covariates,nullhyp="classical"))
print.summary.netlm(olsjaccardALL)

## Controls only

system.time(olsjaccardCONTROLS <- netlm(jaccard,controlsOnly,nullhyp="classical"))
print.summary.netlm(olsjaccardCONTROLS)

## IVs (i.e. status) only

system.time(olsjaccardSTATUS <- netlm(jaccard,statusOnly,nullhyp="classical"))
print.summary.netlm(olsjaccardSTATUS)

## Inverse log-weighted

system.time(olsinv <- netlm(invLogW,covariates,nullhyp="classical"))
print.summary.netlm(olsinv)


#########################################################################
#########################################################################
#########################################################################
#           MRQAP using Dekker's "semi-partialling plus" procedure
#########################################################################
#########################################################################
#########################################################################

set.seed(64783)

## All variables

system.time(qapjaccardALL <- netlm(jaccard,covariates,nullhyp="qapspp",reps=2000))
print.summary.netlm(qapjaccardALL)

## Controls only

system.time(qapjaccardCONTROLS <- netlm(jaccard,controlsOnly,nullhyp="qapspp",reps=50))
print.summary.netlm(qapjaccardCONTROLS)

## IVs (i.e. status) only

system.time(qapjaccardSTATUS <- netlm(jaccard,statusOnly,nullhyp="qapspp",reps=50))
print.summary.netlm(qapjaccardSTATUS)

## Inverse log-weighted

system.time(qapinv <- netlm(invLogW,covariates,nullhyp="qapspp",reps=2000))
print.summary.netlm(qapinv)

#########################################################################
#########################################################################
#########################################################################
#########################################################################
#                         LATENT SPACE MODELS
#########################################################################
#########################################################################
#########################################################################


#impot data from AmLawRev

setwd("C:/Users/diego/Dropbox/DLeal/USC/papers/Probono/probono_2018/probono_output_data_files")
el<-read.csv(file="dataAmLawRev_18.csv",header=TRUE,as.is=TRUE)
el<-el[,c(2,3,4)]
head(el, n=10)

#delete commas from firms' names

el$firm_2<-gsub(",", "",el$firm_2)
el$firm_1<-gsub(",", "",el$firm_1)

#creare node-attributes status
attributes<-merge(firms,vaultRank,by= c("firm"), all=F)
attributes<-attributes[,c("firm","vault2005")]  
attributes$status<-ifelse(attributes$vault2005<26,1,
                          ifelse(attributes$vault2005>=26&attributes$vault2005<76,2,
                                 ifelse(attributes$vault2005>=76,3,9)))

#delete commas from firms' names
attributes$firm<-gsub(",", "", attributes$firm)

net <- network.initialize(nrow(attributes),dir=F)

network.vertex.names(net) <- attributes[,1]
head(attributes[,1], n=10)
y<-get.vertex.attribute(net,"vertex.names")

#edgelist
pairs<-as.matrix(el[,c(2,3)])
head(pairs,n=10)

#Add the edges
net[pairs] <- 1

# Set vert attribute: position in Vault Rank (i.e. status)
set.network.attribute(net,"status",attributes$status) 
y<-get.network.attribute(net,"status")

# Define edge attribute (jaccard100) using weight. NOTE: the tie and its weight are separated pieces of data
y<-set.edge.attribute(net,"eweight",as.numeric(el[,1]))
Y<-get.edge.attribute(net,"eweight")

#put the variables together

covariates <- list(middleStatus=g[[10]],lowStatus=g[[11]],oneDiffStatus=g[[12]],
                   twoDiffStatus=g[[13]],
                   pbHrsDiff=g[[9]],logRevDif=g[[8]],
                   sizeDiff=g[[5]],ageDiff=g[[6]],genderDiff=g[[3]],raceDiff=g[[4]],
                   pracAreDiff=g[[7]],sameCities=g[[14]],NYC=g[[15]])


# Generate a vector with the covariates' names
covariatesNames<-as.vector(c("middleStatus","lowStatus","oneDiffStatus",
                             "twoDiffStatus", "pbHrsDiff", "logRevDif",
                             "sizeDiff","ageDiff","genderDiff","raceDiff",
                             "pracAreDiff","sameCities","NYC"))


# attached the corresponding label to each one of the covariates
for (i in 1:length(covariatesNames))                          
{
  set.network.attribute(net,covariatesNames[i],covariates[[i]])                                            ## store the new variable/matrix in the list g
}

# Check that the loop worked properly
y<-get.network.attribute(net,"middleStatus")
y<-get.network.attribute(net,"logRevDif")


#set memoty size
memory.size(max=16305)

# Estimate the model
library(latentnet)

##################################
# FIRST LSM: ALL IVs AND CONTROLS
##################################
  
set.seed(64783)
g.time <-Sys.time()

  system.time(lsmPbNetALLX <- ergmm(net~euclidean(d=2)+edgecov(covariates[[1]])+
                                  edgecov(covariates[[2]])+edgecov(covariates[[3]])+
                                  edgecov(covariates[[4]])+edgecov(covariates[[5]])+
                                  edgecov(covariates[[6]])+edgecov(covariates[[7]])+
                                  edgecov(covariates[[8]])+edgecov(covariates[[9]])+
                                  edgecov(covariates[[10]])+edgecov(covariates[[11]])+
                                  edgecov(covariates[[12]])+edgecov(covariates[[13]]),
                                response="eweight",family="normal.identity",
                                fam.par=list(prior.var=0.001,prior.var.df=0.001),
                                control=ergmm.control(sample.size=50000,
                                                      interval=100,
                                                      burnin=400000,
                                                      threads=20
                                                       )))
  


##################################
# SECOND LSM: ONLY IVs 
##################################

system.time(lsmPbNetSTATUS <- ergmm(net~euclidean(d=2)+edgecov(covariates[[1]])+
                                edgecov(covariates[[2]])+edgecov(covariates[[3]])+
                                edgecov(covariates[[4]]),
                              response="eweight",family="normal.identity",
                              fam.par=list(prior.var=0.001,prior.var.df=0.001),
                              control=ergmm.control(sample.size=50000,
                                                    interval=100,
                                                    burnin=400000,
                                                    threads=20)))



##################################
# SECOND LSM: ONLY CONTROLS
##################################

system.time(lsmPbNetCONTROLS <- ergmm(net~euclidean(d=2)+edgecov(covariates[[5]])+
                                   edgecov(covariates[[6]])+edgecov(covariates[[7]])+
                                   edgecov(covariates[[8]])+edgecov(covariates[[9]])+
                                   edgecov(covariates[[10]])+edgecov(covariates[[11]])+
                                   edgecov(covariates[[12]])+edgecov(covariates[[13]]),
                                 response="eweight",family="normal.identity",
                                 fam.par=list(prior.var=0.001,prior.var.df=0.001),
                                 control=ergmm.control(sample.size=50000,
                                                       interval=100,
                                                       burnin=400000,
                                                       threads=20)))


#check how much time did the LSMs take to run
print(Sys.time() - g.time)

#save image
save.image("probono2018_Objects.Rdata")

#print LSM results
summary(lsmPbNetALLX)
summary(lsmPbNetSTATUS)
summary(lsmPbNetCONTROLS)
  
##### generate sociogram of the two-mode network

#loading colors
firmColor<-"lightslateblue"
orgColor<-"indianred3"
labelColor<-"blue"

#generating a sociogram without labels for the two-mode vault network
gplot(orgFirmMatrix,gmode="twomode",usearrows=FALSE,
      edge.col=rgb(red=0,green=0,blue=0,alpha=0.25),
      label.cex=0.1,label.pos=5, vertex.sides=25,
      vertex.col=c(rep(orgColor,nrow(orgFirmMatrix)),
                   rep(firmColor,ncol(orgFirmMatrix))),
      vertex.cex=c(rep(1.5,nrow(orgFirmMatrix)),
                   rep(1.5,ncol(orgFirmMatrix))),label.col=labelColor,
      vertex.enclose=F)







