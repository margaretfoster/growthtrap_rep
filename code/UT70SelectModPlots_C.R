
## This for diagnostics on the AQAP selectModel runs
## for the AQAP ksearch results

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


visualization <- c("ggplot2", "stm")

packs=c(visualization)

loadPkg(packs)

#######################################
## access the data:
######################################

## load prepped AQAP Corpus:
dataPath = "./data/"
load(paste0(dataPath, "selectModAQAP70UT.Rdata"))

##########################################
## Model comparison
## Here presenting for K=18
#########################################

plotModels(selectMod70UT18, main="Select Mod Output K=18")

### Take 10 for the k=18
selected18 <-selectMod70UT18$runout[[10]]

## take 7 for K=10
#selected10 <- selectMod70UT10$runout[[7]]

#save(selected10,
#     file="selectedModUT7010.Rdata")

############################################################
## estimate effect
###########################################################

allTopics18 <- estimateEffect(formula= c(1:18) ~ s(elapsedDaysAll),
                      stmobj=selected18,
                      metadata= output$meta,
                              uncertainty="Global")

## send to separate rdata files

eighteenList <- c(selected18, allTopics18)

save(eighteenList,
     file="estimatedEffect_UT70_18Model.Rdata")

######################################################
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
## Preliminary Diagnostic Plots: 

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

# Paper Figure 1:
pdf("./figures/topicProportions_UT70EDModel.pdf")
plot.STM(selected18,
         type="summary",
         custom.labels=topicNames,
         main="Top Topics Model 1 \n AQAP Corpus With Day Covariates")

dev.off()


############################################
## Plot groupings
##########################################

pdf("./figures/dayModel_UT70_cluster1.pdf")
                
plot.STM(selected18,
         type="labels",
         labeltype="frex",        
         # n=10,
         width =66,

         main="Local Conflict Topics",
         topics=c(3, 7,11, 17))

dev.off()


pdf("./figures/dayModel_UT70_cluster2.pdf")

plot.STM(selected18,
         type="labels",
         labeltype="frex",   
         # n=10,
         width =66,
         main="Attacks and Operations",
         topics=c(10, 18))

dev.off()



pdf("./figures/dayModel_UT70_cluster3.pdf")
plot.STM(selected18,
         type="labels",
         labeltype="frex",                  
         # n=10,
         width =66,
         main="Trasnational Jihadi",
         topics=c(2,5, 9, 15))
                
dev.off()

pdf("./figures/dayModel_UT70_cluster4.pdf")

plot.STM(selected18,
         type="labels",
         labeltype="frex", 
         # n=10,
         width =66,
         main=c("Jihadi Platitudes and Religion"),
         topics=c(4, 6, 14, 12, 13))
dev.off()


############################
## topics: 7, 17:
## Local Targets, Houthis
###########################
##Topic 5 Top Words:
## Highest Prob: mujahideen, ansar, alshariah, soldier, caption, citi, abyan 
##FREX: caption, zinjibar, tank, abyan, ansar, waqar, alshariah

dev.off()

## Fig A4_left:
pdf("./figures/HouthiandLT_UT7_K18_top.pdf",width=8, height=4)
plot(selected18,
     type="labels",
     labeltype="frex",
     topic.names = c('Local Targets', 'Houthis'),
     topics=c(7, 17))
dev.off()

pdf("./figures/HouthiandLT_UT7_K18_bottom.pdf",width=8, height=4)
plot.estimateEffect(allTopics18,
                    "elapsedDaysAll",
                    xlab="Time (2004-2016)",
                    xaxt="n",
                    topics=c(7, 17),
                    method="continuous",
                    linecol = c("gray", "black"),
                    main="Local Targets' (7)  and  'Houthis' (17) Topics")
axis(1, at=as.numeric(yearseq)- min(as.numeric(yearseq)),
     labels=yearnames, las=2, cex=.25)
abline(h=0, col="grey", lty="dashed")

dev.off()


par(mar=c(1,1, 1,1))
#par(mfrow=c(2, 1))

## Fig A4_right
pdf("./figures/AASAndMiscYemen_UT7_K18_top.pdf",width=8, height=4)
plot(selected18,
     type="labels",
     labeltype="frex",
     text.cex = 1,
     topic.names = c('US in Yemen', 'Southern Yemen'),
     topics=c(3, 11))
dev.off()


## Appendix bottom righ
pdf("./figures/AASAndMiscYemen_UT7_K18_bottom.pdf",width=8, height=4)
plot.estimateEffect(allTopics18,
                    "elapsedDaysAll",
                    xlab="Time (2004-2016)",
                    xaxt="n",
                    topics=c(3, 11),
                    method="continuous",
                    linecol = c("gray", "black"),
                    main="'US in Yemen' (3)  and  'Southern Yemen' (11) Topics \n")
axis(1, at=as.numeric(yearseq)- min(as.numeric(yearseq)),
     labels=yearnames, las=2, cex=.25)
abline(h=0, col="grey", lty="dashed")

dev.off()
 
##
## Fig A5 left
pdf("./figures/ClashFactions_UT7_K18_top.pdf",width=8, height=4)
plot(selected18,
     type="labels",
     labeltype="frex",
     topic.names = c('Clash of Civilizations', 'Jihadi Factions'),
     topics=c(5, 15))
dev.off()

pdf("./figures/ClashFactions_UT7_K18_bottom.pdf",width=8, height=4)
plot.estimateEffect(allTopics18,
                    "elapsedDaysAll",
                    xlab="Time (2004-2016)",
                    xaxt="n",
                    topics=c(5, 15),
                    method="continuous",
                    linecol = c("gray", "black"),
                    main="'Clash of Civilizations' (5) and 'Jihadi Factions' (15) Topics")
axis(1, at=as.numeric(yearseq)- min(as.numeric(yearseq)),
     labels=yearnames, las=2, cex=.25)
abline(h=0, col="grey", lty="dashed")

dev.off()


## Fig A5 right

pdf("./figures/TransJihadiAlt_UT7_K18_top.pdf",width=8, height=4)
plot(selected18,
     type="labels",
     labeltype="frex",
     topic.names = c('Down with Al-Saud', 'Jihadi Revolution'),
     topics=c(5, 9))
dev.off()

pdf("./figures/TransJihadiAlt_UT7_K18_bottom.pdf",width=8, height=4)
plot.estimateEffect(allTopics18,
                    "elapsedDaysAll",
                    xlab="Time (2004-2016)",
                    xaxt="n",
                    topics=c(5, 9),
                    method="continuous",
                    linecol = c("gray", "black"),
                    main="' Down with al-Saud' (2) and 'Jihadi Revolution' (9) Topics")
axis(1, at=as.numeric(yearseq)- min(as.numeric(yearseq)),
     labels=yearnames, las=2, cex=.25)
abline(h=0, col="grey", lty="dashed")

dev.off() 
