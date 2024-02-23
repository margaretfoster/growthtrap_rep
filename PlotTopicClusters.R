
## This plot to graph the topic clusters for the continuous
## "elapsed days" model
############################
## Initialization
############################

rm(list=ls())

loadPkg=function(toLoad){
  for(lib in toLoad){
  if(! lib %in% installed.packages()[,1])
    { install.packages(lib, repos='http://cran.rstudio.com/') }
  suppressMessages( library(lib, character.only=TRUE) ) }}

#### probably longer, but more easy to read:


visualization <- c("ggplot2", 'igraph')
datawrangling=c('foreign', "scales",'data.table', "lubridate", "tidyr")
textprocessing <- c('tm', 'XML', 'stm', 'RCurl', 'stringr',
    'SnowballC', 'topicmodels', 'mclust', 'slam', "Rfast" )

packs=c(visualization, datawrangling, textprocessing)

loadPkg(packs)

#######################################
## access the data:
######################################
dataPath <- "./data/"
codePath <- "~/Dropbox/OverPressure/AQAP/code/"
plotsPath <- "~/Dropbox/OverPressure/AQAP/analysis/"
binariesPath <- "~/Dropbox/LargeAQAPBinaries/"

#####################################
## Load some custom scripts
####################################
setwd("~/Dropbox/OverPressure/AQAP/code/")

source("STMfunctionsalt.R")
source("plotcontinuousalt.R")
source("produce_cmatrixalt.R")
source("simbetasalt.R")

####################################
## load prepped AQAP Corpus:
## Looking for, but can't find:
load(paste0(dataPath, "selectModAQAP70UT.Rdata"))

##load("selectedModUT7018.Rdata")
##load("estimatedEffect_UT70_18Model.Rdata")
##ls()

##load("../analysis/AQAPCorpusBinaryVars70T.Rdata")

##########################################
## Load data & estimate effect
#########################################

### Take 10 for the k=18
selected18 <-selectMod70UT18$runout[[10]]

allTopics18 <- estimateEffect(formula= c(1:18) ~ s(elapsedDaysAll),
                      stmobj=selected18,
                      metadata= output$meta,
                              uncertainty="Global")

#################################################
## Plots
###################################################

monthseq <- seq(from=as.Date("2004-06-18"),
                to=as.Date("2016-09-16"), by="month")
monthnames <- months(monthseq)

quarterseq <- seq(from=as.Date("2004-06-18"),
                to=as.Date("2016-09-16"), by="quarter")
quarternames <- months(quarterseq)


yearseq <- seq(from=as.Date("2004-06-18"),
                  to=as.Date("2016-09-16"), by="year")
yearnames <- year(yearseq)

##################################################
## Peliminary Diagnostic Plots:

plot(selected18)
labelTopics(selected18)


### commentary:
## (did verify that the corpus built from
## a version w/o summaries)

## In order of frequency:
## topic 17: Local ops: days
### topic 7: Local ops: targets
## topic 8: structure of dispatches
## topic 12: generic jihadi platitudes 
## topic 4: rewards to the martyr/religion
## topic 14:Important people (imprisoned/memoirs)
## topic 18: Attacks on the US
## topic 1: narration
## topic 13: fire and brimstone
## topic 5: generic jihad, religious terms
## topic 9: jihadi revolution 
## topic 3: southern Yemen
## topic 6: shout outs to leaders
## topic 11: US involvement in Yemen
## topic 10: opsec
## topic 15: factionalism
## topic 2: Down with al-Saud
## topic 16: Narration

topicNames <- c("Editing: Transcripts", "Demise of al-Saud",
                "Southern Yemen","Rewards to the Martyr",
                "Clash of Civilizations","Greetings to Leaders",
                "Local Targets", "Editing: Structure",
                "Jihadi Revolution", "Operational Security",
                "US in Yemen", "Ummah and Brotherhood",
                "Fire and Brimstone", "Prisoners and Memoirs",
                "Jihadi Factionalism", "Editing: Narration",
                "Houthis", "Attacks on West" )

## groupings:
## Local conflict: 3, 7, 11, 17
## Attacks and Operations: 18, 10
## Transnational jihadi: 6, 9, 14, 
## General jihadi: 2, 4, 12, 15
## Religion: 5, 13
## Editing: 8, 1, 16

## make a list of clusters:

clusters <- list(local=c(3,7,11,17),
                 attacks= c(18, 10),
                 trans = c(6, 9, 14),
                 gen.jihad = c(2, 4,12,15),
                 religion = c(5, 13),
                 edits= c(8,1,16))


###############################
## build out the custom clustering
##############################

ci.level=.95
offset <- (1-ci.level)/2

covariate <- "elapsedDaysAll"
method <-  "continuous"
n <- 10^3
topics=1:18


cdat <- produce_cmatrixalt(prep=allTopics18,
                           covariate=covariate,
                           method=method,
                           npoints=n,
                           moderator=NULL,
                           moderator.value=NULL)

cdata <- cdat$cdata
cmat <- cdat$cmatrix

## x-values for plotting:
uvals1 <- cdata[,covariate]

## simulate betas:

simbetas <- simBetas(allTopics18$parameters,
                     nsims=n)


## make sure it does plot:
x1 <- plotContinuousAlt(prep=allTopics18,
                        covariate=covariate,
                        topics=topics,
                        cdata=cdata,
                        cmat=cmat,
                        simbetas=simbetas,
                        printlegend=FALSE,
                        offset=offset)

#########################3
## Establish Y-values
## (means of topic estimates)

meanscust <- list()
ciscust <- list()

for(i in 1:length(topics)){
    ##Simulate values                                                   
    sims <- cmat%*%t(simbetas[[which(
        allTopics18$topics==topics[i])]])
    ##Find means and cis                                                                
    meanscust[[i]] <- rowMeans(sims)
    ciscust[[i]] = apply(sims,1, function(x)
               quantile(x, c(offset,1-offset)))
}


## generate clusters:
## taking "clusters" object from above:

clusteredmeans <- list()

clusteredmeans[[1]] <- meanscust[[3]] +  meanscust[[7]] +
    meanscust[[11]]+ meanscust[[17]]

clusteredmeans[[2]] <- meanscust[[18]] +  meanscust[[10]]

clusteredmeans[[3]] <- meanscust[[2]] +  meanscust[[4]] +
    meanscust[[12]]+ meanscust[[15]]

clusteredmeans[[4]] <- meanscust[[5]] +  meanscust[[13]]

clusteredmeans[[5]] <- meanscust[[8]] +  meanscust[[1]] +
    meanscust[[16]]



### plot the clusters

dev.off()


xlimcust <- c(min(uvals1), max(uvals1))
ylimcust <- c(min(unlist(ciscust), na.rm=T),
              max(unlist(ciscust), na.rm=T))
ylabcust <- "Expected Topic Proportion"
plot(0, 0,col="white",
     xlim=xlimcust,
     ylim=ylimcust,
     main=NULL)

## y=0 line

##ugly hack:
yline <- rep(0,length(uvals1))

lines(uvals1,
      yline,
      col="red",
      lty=2)

                                                                                        
nclusters <- length(clusteredmeans)                                                                         
cols = grDevices::rainbow(nclusters)

## all:
for(i in 1:nclusters){
    lines(uvals1,
          clusteredmeans[[i]],
          col=cols[i],
          lty=5)
}

################################
## just clusters 1 and 3
## local conflict and transnational jihad:
##############################


## First some information about the total proportion of
## expected topic proportions for clusters 1 & 3
## (To make point that the two are not necessarily zero-sum

totals <- clusteredmeans[[1]] + clusteredmeans[[3]]

summary(totals)

hist(totals)

plot(totals~uvals1)

dev.off()

pdf(file="./output/localtranscluster.pdf")

xlimcust <- c(min(uvals1), max(uvals1))
ylimcust <- c(min(unlist(ciscust), na.rm=T),
              max(unlist(ciscust), na.rm=T))
ylabcust <- "Expected Topic Proportion"
xlabcust <- "Document Date"


plot(0, 0,col="white",
     xlim=xlimcust,
     ylim=ylimcust,
     ylab=ylabcust,
     xlab=xlabcust,
     xaxt="n",## make no text on x-axis
     main="Point Estimates of Local and Transnational Topic Clusters")

## y=0 line
yline <- rep(0,length(uvals1))
lines(uvals1,
      yline,
      col="red",
      lty=2)

## X-Axis as date:
axis(1, at=as.numeric(yearseq)- min(as.numeric(yearseq)),
     labels=yearnames, las=2, cex=.25)


## Want black  and gray in cols:
## clusters 1 (local ops) and 3 (transnational)
lines(uvals1, ## Local 
      clusteredmeans[[1]],
      col="gray8",
      lty=5 #longdash
      )
lines(uvals1,## transnational
      clusteredmeans[[3]],
      col="gray48",
      lty=3) #dotted
    
legend(x=max(uvals1)*.3, y=0.5,
       c("Local Conflict", "Transnational Jihadi"),
       fill=c("gray8", "gray48"),
       title="Topic Clusters")

dev.off()


