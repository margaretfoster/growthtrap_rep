### Replication for the random Forest and PCA visualization

rm(list=ls())

## Set user

loadPkg=function(toLoad){
  for(lib in toLoad){
    if(!(lib %in% installed.packages()[,1])){ 
      install.packages(lib, repos='http://cran.rstudio.com/') }
    suppressMessages( library(lib, character.only=TRUE) )
  }
}

toLoad <- c("tm", "kernlab",
            "caret",
            "dplyr", 'tsne',
            'lubridate',
            "splitstackshape", "e1071", 
            "randomForest", "edarf",
            "rattle","rpart", 
            "randomForestExplainer", "lubridate")

loadPkg(toLoad)

## Edarf
##devtools::install_github("zmjones/edarf", subdir = "pkg")

## Data
setwd("/Users/Promachos/Dropbox (Personal)/Research/YemenEventData/Replication")

dataPath <- "./data/"

dev <- read.csv(paste0(dataPath,
                       "develop_df.csv"),
                stringsAsFactors=FALSE)

val <- read.csv(paste0(dataPath,
                       "validate_df.csv"),
                stringsAsFactors=FALSE)

table(dev$label)

## Take the dev set and make 10 subsets:
## with 1/10 of the AQAP and Houthi stories
## and the Ansar al-Shariah stories

set.seed(110122)
k= 10

k.aqap <- rep(1:k, length.out = 260) 
assignments.aqap <- sample(k.aqap)
assignments.aqap


k.houthi <- rep(1:k, length.out = 174) 
assignments.h <- sample(k.houthi)
assignments.h

dev$kgroup <- 0
dev[which(dev$label=="AQAP/Al-Qaeda"), "kgroup"] <- assignments.aqap
dev[which(dev$label=="Houthi/Ansarallah"), "kgroup"] <- assignments.h

table(dev$kgroup) ##0= 27, so AAS entries



## Homebrew Kfold, so that it's stratified

## For each subset:

koutlist <- list()
confusiondf <- data.frame()

for(f in 1:k){
 
    print(paste0("fold is", f))
    ## subset: 
    dev.subset <- dev[which(dev$kgroup %in% c(0, f)),]
    table(dev.subset$label)
    
    dev.df <- subset(dev.subset, select= -c(event.date, storyid, kgroup))
    val.df <- subset(val, select= -c(event.date, storyid, test))

    ## remove the "label" field and two indicative words that snuck through 
    justDat.dev <- subset(dev.df, select= -c(X, label, arabic, alshariah))
    justDat.val <- subset(val.df, select= -c(X, label, arabic, alshariah))
    
    dim(justDat.dev)
    dim(dev.df)
    
    ## Analysis: random forest
    library(randomForest)
    set.seed(6889)
    rf.all <- randomForest(x=as.matrix(justDat.dev),
                         y=as.factor(dev.df$label),
                       xtest=justDat.val,
                       ytest=as.factor(val.df$label),
                       ntree=500,
                       keep.forest=TRUE,
                       importance=TRUE, ## assess importance of predictors
                       proximity=TRUE) #calculates promiximty of nodes

  koutlist[[f]] <- rf.all 
  tmp <- as.data.frame(koutlist[[f]]$confusion)
  tmp$round <- f
  
  confusiondf <- rbind(confusiondf, tmp)
}


confusiondf$group <- gsub('[[:digit:]]+', '', rownames(confusiondf))
                          
gg.classerror <-ggplot(confusiondf,
                       aes(x=round,
                           y=class.error,
                           group= group,
                           linetype=group))+
  geom_line()+
  theme_bw()+
  scale_x_continuous("Fold", 
                   breaks=c(1:10)) +
  scale_y_continuous("Class Error", 
                  limits=c(0, 1)) +
  labs(linetype="Group")+
  theme(legend.position="bottom")


gg.classerror
ggsave(gg.classerror, 
       file="KfoldClassError.pdf")

## confusion matrix:
## low AAH class error case: 
## Fold 3

library(xtable)
xtable(koutlist[[3]]$confusion)

## variable importance plot, Fold 3:
varImpPlot(koutlist[[3]])

## high class error case: Round 8