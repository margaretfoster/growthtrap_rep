#OA# SVM and Visualization on the large data

## Libraries

rm(list=ls())

loadPkg=function(toLoad){
    for(lib in toLoad){
            if(!(lib %in% installed.packages()[,1])){ 
              install.packages(lib, repos='http://cran.rstudio.com/') }
	  suppressMessages( library(lib, character.only=TRUE) )
      }
    }

toLoad <- c("tm", "kernlab", "caret", "dplyr",
            "splitstackshape", "e1071",
            "rattle","rpart", "randomForestExplainer",
            "lubridate")

loadPkg(toLoad)

#library("rpart.plot")

dataPath <- './data/'
   
dev <- read.csv(paste0(dataPath,
                       "develop_df.csv"),
                stringsAsFactors=FALSE)



val <- read.csv(paste0(dataPath,
                       "validate_df.csv"),
                stringsAsFactors=FALSE)


tail(colnames(dev)) ## metadata is label, event.date, storyid

## Stories without the metadata and story id tags:

meta <- c("event.date", "storyid")

dev.df <- subset(dev, select= -c(event.date, storyid))
val.df <- subset(val, select= -c(event.date, storyid, test))


#####################
## SVM
## (Kernel SVM allows for non-linear separation)
#####################

## prediction
## (scale error, but that's ok, already scaled in tf-idf)



df.model<-ksvm(label~., data= dev.df,
               kernel="rbfdot")

setdiff(colnames(dev.df), colnames(val.df))
setdiff(colnames(val.df), colnames(dev.df))

df.pred<-predict(df.model, val.df)
df.pred ##list of factor values


## Assigned:
predlabels <- as.data.frame(cbind(as.character(df.pred),
                    as.character(val.df$label),
                                  val$event.date))



cors <- which(as.character(predlabels$V1)==as.character(predlabels$V2))


predlabels[cors,"correct"] <- 'yes'
predlabels[-cors,"correct"] <- "no"

colnames(predlabels) <- c("prediction",
                          "label",
                          "date",
                          "correct")


gg <- ggplot(predlabels,
             aes(x = date, y =prediction,
                 color=label, shape=correct))
gg <- gg +geom_point()
gg <- gg + theme_bw()
labs(x = "Month", y = "Prediction")
plot(gg)


head(predlabels)

predlabels

##print(con.matrix)

### with predicted probabilities

df.model.probs <- ksvm(label~., data=dev.df,
                       kernel="rbfdot",prob.model=TRUE)


df.pred.probs <- predict(df.model.probs, val.df,
                         type="probabilities")


fitted(df.model.probs)


predProbs <- cbind(round(as.data.frame(df.pred.probs),3),
                   as.character(val.df$label),
                   as.Date(val$event.date))
    
predProbs


plot(df.pred.probs)
## visualization:


predlabels$date <-as.Date(predlabels$date)

gg <- ggplot(predlabels,
aes(x = date, y =correct, color=label))
gg <- gg +geom_point()
gg <- gg + theme_bw()
labs(x = "Month", y = "Prediction")
plot(gg)

dim(df.pred.probs)## 144x3

head(df.pred.probs)

attributes(df.pred.probs)

caret::confusionMatrix(df.pred.probs$pred, df.pred.probs$true)

colnames(predProbs) <- c("AAS", "AQAP",
"Houthi", "Group", "Event_Date")
head(predProbs)

## graph of which stories are predicted for
## AQAP| AAS or AAS | AQAP

gga <- ggplot(aaswrong, aes(x=Event_Date,
y=AQAP,
colour="AQAP| AAS"))
gga <- gga+geom_point() + ylim(0, 1)
gga <- gga+ theme_bw()
gga <- gga+ geom_point(data=aqapall,
aes(x=Event_Date,
y=AAS,
colour="AAS|A QAP"))
gga <- gga + labs(title = "SVM Mispredictions: AQAP and Ansar al-Shariah",
x = "Article Date", y = "Prediction",
color = "Prediction| Label")


plot(gga)

ggsave(gga,
file="PredictedProbabilities_MisPreds.pdf")

## plot:  given label is Ansar al-Shariah,
## visualization of predicted probabilities according to time:

aaswrong <- predProbs[which(predProbs$Group=="Ansar al-Shariah"),]

dim(aaswrong)

aqapall <- predProbs[which(predProbs$Group=="AQAP/Al-Qaeda"),]

head(aqapall)

## dif in prediction for AQAP as AQAP and AQAP as AAS:

ggaq <- ggplot(aqapall, aes(x=Event_Date,
y=AQAP,
colour="AQAP| AQAP"))
ggaq <- ggaq+geom_point() + ylim(0, 1)
ggaq <- ggaq+ theme_bw()
ggaq <- ggaq+ geom_point(data=aqapall,
aes(x=Event_Date,
y=AAS,
colour="AAS| AQAP"))
ggaq <- ggaq+ scale_shape_discrete(name ="Predicted Prob.")
ggaq <- ggaq + labs(title = "SVM Predictions, AQAP Stories",
x = "Article Date", y = "Prediction",
color = "Prediction| Label")

plot(ggaq)

ggsave(ggaq,
file="PredictedProbabilities_AQAPPreds.pdf")



#################
## random forest
## when you write up:
## https://cran.r-project.org/web/packages/randomForestExplainer/vignettes/randomForestExplainer.html
################

#install.packages("randomForest")

library(randomForest)
#library(quanteda)


## subset of the data b/c need to remove the "label" field

justDat.dev <- subset(dev.df, select= -c(X, label))
justDat.val <- subset(val.df, select= -c(X, label))


set.seed(6889)

rf <- randomForest(x=as.matrix(justDat.dev),
                   y=as.factor(dev.df$label),
                   importance=TRUE,## evaluates importance predictors
                   proximity=TRUE) ## assess proximity of nodes

print(rf)

### importance of predictors (words)
iM <- as.data.frame(round(rf$importance, 3))
im <- iM[order(-iM$MeanDecreaseGini),]

im[1:50, ] ## note that "alqaeda" is in the doc, but not
## very important for classification, likely b/c the stories
## about both kinds of uprisings mention other security challenges

set.seed(6889)

rf2 <- randomForest(x=as.matrix(justDat.dev),
                    y=as.factor(dev.df$label),
                    xtest=justDat.val,
                    ytest=as.factor(val.df$label),
                    ntree=500,
                    importance=TRUE, ## assess importance of predictors
                    proximity=TRUE) #calculates promiximty of nodes

##rf2

## importance of predictors
iM2 <- as.data.frame(round(rf2$importance, 3))

im2 <- iM2[order(-iM2$MeanDecreaseGini),]

im2[1:20,]

##########
## Dimensionality reduction via t-SNE
##  t-distributed stochastic neighbor embedding (t-SNE) 
#########

### data prep: 
## remove 'label" and "X"
## val.df also has "test"

labels <- c("label", "X")
            
train.matrix <- as.matrix(dev.df[,-which(colnames(dev.df) %in% labels)])
val.matrix <- as.matrix(val.df[,-which(colnames(val.df) %in% labels)])


library(tsne)
library(lubridate)

set.seed(6889)
dev.tsne <- tsne(train.matrix, ##this is on the training set
             initial_config = NULL, k = 2,
             perplexity = 30, max_iter = 1000,
             min_cost = 0, epoch_callback = NULL,
             whiten = TRUE,epoch=100)

class(dev.tsne)


dev.tsne.df <- cbind(as.data.frame(dev.tsne),
                   as.character(dev.df[,"label"]),
                   as.Date(dev$event.date))

colnames(dev.tsne.df) <- c("X", "Y", "label", "date")

dev.tsne.df$year <- lubridate::year(as.Date(dev.tsne.df$date))

head(dev.tsne.df)

table(dev.tsne.df$year)##2009-2014 have most data


library(ggplot2)
library(lattice)


gg <- ggplot(dev.tsne.df,
             aes(x = X,  y = Y, colour = label))
gg <- gg +geom_point()
gg <- gg + theme_bw()

plot(gg)
            
ggsave(gg, file="testData_tSNEDimRed.pdf")


## Now by year:

gg <- ggplot(dev.tsne.df[which(dev.tsne.df$year==2009),],
             aes(x = X,  y = Y, colour = label))
gg <- gg +geom_point()
gg <- gg + theme_bw()

plot(gg)

ggsave(gg, file="2009testData_tSNEDimRed.pdf")

## function

yearPlot <- function(df,listOfYears){

    for(i in listOfYears){
    gg <- ggplot(df[which(df$year==i),],
                 aes(x = X,  y = Y, colour = label))
    gg <- gg +geom_point()
    gg <- gg + theme_bw()
    
    plot(gg)
    
    ggsave(gg, file=paste0(dataPath, i, "testData_tSNEDimRed.pdf"))
    print(paste0("done with year: ", i))
}
    print("check plots!")
}

yearPlot(dev.tsne.df, listOfYears=c(2009, 2010, 2011, 2012, 2013, 2014))
 
 
