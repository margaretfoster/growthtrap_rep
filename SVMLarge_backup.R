## Clustering Visualizations

rm(list=ls())

## Customize the working directory:
setwd('~/Dropbox/GrowthTrapCode/Replication/')
dataPath <- './data/'


loadPkg=function(toLoad){
  for(lib in toLoad){
    if(!(lib %in% installed.packages()[,1])){ 
      install.packages(lib, repos='http://cran.rstudio.com/') }
    suppressMessages( library(lib, character.only=TRUE) )
  }
}

toLoad <- c("tm", "kernlab", "caret", "dplyr",
            "splitstackshape", "e1071", 'edarf',
            "rattle","rpart", "randomForestExplainer",
            "lubridate")

loadPkg(toLoad)

#library("rpart.plot")

dev <- read.csv(paste0(dataPath,
                       "develop_df.csv"),
                stringsAsFactors=FALSE)

val <- read.csv(paste0(dataPath,
                       "validate_df.csv"),
                stringsAsFactors=FALSE)


dev$X <- NULL
val$X <- NULL

#####################
## SVM
## (Kernel SVM allows for non-linear separation)
#####################

## Prediction

#meta <- c("event.date", "storyid")

#dev_df <- subset(dev, select= -c(event.date, storyid)) ## 461
#val_df <- subset(val, select= -c(event.date, storyid, test))

## ksvm() call fails unless outcome is factor:
#df_model<-ksvm(as.factor(label)~., data= dev_df,
kernel="rbfdot")

#df_pred<-predict(df_model, val_df)
#df_pred ##list of factor values

## Assigned:
#predlabels <- as.data.frame(cbind(as.character(df_pred),
#                    as.character(val_df$label),
#                                  val$event.date))
#cors <- which(as.character(predlabels$V1)==as.character(predlabels$V2))


#predlabels[cors,"correct"] <- 'yes'
#predlabels[-cors,"correct"] <- "no"

#colnames(predlabels) <- c("prediction", "label","date", "correct")

### With predicted probabilities

df_model_probs <- ksvm(as.factor(label) ~., data=dev_df,
                       kernel="rbfdot",prob.model=TRUE)


df_pred_probs <- predict(df_model_probs, val_df,
                         type="probabilities")


predProbs <- cbind(round(as.data.frame(df_pred_probs),3),
                   as.character(val_df$label),
                   as.Date(val$event.date))

plot(df_pred_probs)

#################
## Random forest
################

library(randomForest)

## subset of the data b/c need to remove the "label" field

justDat_dev <- subset(dev_df, select= -c(label))
justDat_val <- subset(val_df, select= -c(label))


set.seed(6889)

rf <- randomForest(x=as.matrix(justDat_dev),
                   y=as.factor(dev_df$label),
                   importance=TRUE,## evaluates importance predictors
                   proximity=TRUE) ## assess proximity of nodes

### importance of predictors (words)
iM <- as.data.frame(round(rf$importance, 3))
im <- iM[order(-iM$MeanDecreaseGini),]

im[1:50, ] ## note that "alqaeda" is in the doc, but not
## very important for classification, likely b/c the stories
## about both kinds of uprisings mention other security challenges

set.seed(6889)

rf2 <- randomForest(x=as.matrix(justDat_dev),
                    y=as.factor(dev_df$label),
                    xtest=justDat_val,
                    ytest=as.factor(val_df$label),
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
## remove 'label"
## val_df also has "test"

labels <- c("label")

train.matrix <- as.matrix(dev_df[,-which(colnames(dev_df) %in% labels)])
val.matrix <- as.matrix(val_df[,-which(colnames(val_df) %in% labels)])


library(tsne)
library(lubridate)

set.seed(6889)
dev.tsne <- tsne(train.matrix, ##this is on the training set
                 initial_config = NULL, k = 2,
                 perplexity = 30, max_iter = 1000,
                 min_cost = 0, epoch_callback = NULL,
                 whiten = TRUE,epoch=100)

class(dev.tsne)


dev.tsne_df <- cbind(as.data.frame(dev.tsne),
                     as.character(dev_df[,"label"]),
                     as.Date(dev$event.date))

colnames(dev.tsne_df) <- c("X", "Y", "label", "date")

dev.tsne_df$year <- lubridate::year(as.Date(dev.tsne_df$date))

head(dev.tsne_df)

table(dev.tsne_df$year)##2009-2014 have most data


library(ggplot2)
library(lattice)


gg <- ggplot(dev.tsne_df,
             aes(x = X,  y = Y, colour = label))
gg <- gg +geom_point()
gg <- gg + theme_bw()

plot(gg)

ggsave(gg, file="testData_tSNEDimRed.pdf")


## Now by year:

gg <- ggplot(dev.tsne_df[which(dev.tsne_df$year==2009),],
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

yearPlot(dev.tsne_df, listOfYears=c(2009, 2010, 2011, 2012, 2013, 2014))

## compare to PCA-- can not, needs more units than vars


#####################
## SVM visualization
## (Kernel SVM allows for non-linear separation)
#####################

## What the objects are:
##df_model<-ksvm(label~., data= dev_df,
##               kernel="rbfdot")

##df_pred<-predict(df_model, val_df)

## Assigned:
## predlabels= dataframe of SVM predictions

##predlabels <- as.data.frame(cbind(as.character(df_pred),
##                    as.character(val_df$label),
##                                  val$event.date))

##cors <- which(as.character(predlabels$V1)==as.character(predlabels$V2))


predlabels$date <-as.Date(predlabels$date)

gg <- ggplot(predlabels,
             aes(x = date, y =correct, color=label))
gg <- gg +geom_point()
gg <- gg + theme_bw()
labs(x = "Month", y = "Prediction")
plot(gg)


### with predicted probabilities

## df_model_probs <- ksvm(label~., data=dev_df,
##                       kernel="rbfdot",prob.model=TRUE)

##df_pred_probs <- predict(df_model_probs, val_df,
#                         type="probabilities")

### predProbs = dataframe of predictied probabilities from SVM 


dim(df_pred_probs)## 144x3

head(df_pred_probs)

attributes(df_pred_probs)

caret::confusionMatrix(df_pred_probs$pred, df_pred_probs$true)

colnames(predProbs) <- c("AAS", "AQAP",
                         "Houthi", "Group", "Event_Date")
head(predProbs)

## plot:  given label is Ansar al-Shariah, 
## visualization of predicted probabilities according to time:

aaswrong <- predProbs[which(predProbs$Group=="Ansar al-Shariah"),]

dim(aaswrong)

aqapall <- predProbs[which(predProbs$Group=="AQAP/Al-Qaeda"),]

head(aqapall)

## see if predicted probs dimnishes

aqapright

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



### print model coefficients and other information


#################
## random forest
## when you write up:
################


## subset of the data b/c need to remove the "label" field

##justDat_dev <- subset(dev_df, select= -c(X, label))
##justDat_val <- subset(val_df, select= -c(X, label))


## Only on development set:
##rf <- randomForest(x=as.matrix(justDat_dev),
##                 y=as.factor(dev_df$label),
##                   importance=TRUE,## evaluates importance predictors
##                  proximity=TRUE) ## assess proximity of nodes

### importance of predictors (words)
##iM <- as.data.frame(round(rf$importance, 3))
##im <- iM[order(-iM$MeanDecreaseGini),]

im[1:50, ] 

## Using the validation set as a test set

## rf2 <- randomForest(x=as.matrix(justDat_dev),
##                     y=as.factor(dev_df$label),
##                     xtest=justDat_val,
##                     ytest=as.factor(val_df$label),
##                     ntree=500,
##                     importance=TRUE, ## assess importance of predictors
##                     proximity=TRUE) #calculates promiximty of nodes


set.seed(6889)
library(randomForest)

rf3 <- randomForest(x=as.matrix(justDat_dev),
                    y=as.factor(dev_df$label),
                    xtest=justDat_val,
                    ytest=as.factor(val_df$label),
                    ntree=500,
                    keep.forest=TRUE,
                    importance=TRUE, ## assess importance of predictors
                    proximity=TRUE) #calculates promiximty of nodes


attributes(rf3)

rf3$classes

plot(rf3, log="y")

rf3.mds <- cmdscale(1 - rf3$proximity, eig=TRUE)



## Predict on test:

ls()

impr3 <- importance(rf3, type=1)

varImpPlot(rf3)


partialPlot(rf3,
            pred.data=as.matrix(justDat_dev),
            x.var=rebels,
            which.class="Houthi/Ansarallah")

##edarf

pd_rf3 <- partial_dependence(rf3, vars=c("rebels"),
                             data=justDat_dev)
plot_pd(pd_rf3)

class(rf3$predicted)
class(rf3$confusion)

dim(rf3$confusion)

round(rf3$confusion, 2)

###
library(xtable)
xtable(rf3$confusion, 2)

##

ls()

varimp <- variable_importance(rf3, nperm=3, data=justDat_dev)

## Also using the eDarf package:
library(edarf)
#
prox <- extract_proximity(rf2)
pca <- prcomp(prox, scale=TRUE)

pdf(file= "pca_proximityRF_label.pdf")
plot_prox(pca,
          color=dev_df$label,
          color_label = "News Articles",
          size = 2)
dev.off()



## importance of predictors
##iM2 <- as.data.frame(round(rf2$importance, 3))

im2[1:20,] ##

pdf(file=paste0(dataPath,"randomForestRes.pdf"))
varImpPlot(rf3, sort=TRUE, n.var=15,
           main="15 Most Important Words For Story Classification")
dev.off()


varImpPlot(rf)
##########
## Dimensionality reduction via t-SNE
##  t-distributed stochastic neighbor embedding (t-SNE) 
#########

### data prep: 
## remove 'label" and "X"
## val_df also has "test"

labels <- c("label", "X")

train.matrix <- as.matrix(dev_df[,-which(colnames(dev_df) %in% labels)])
val.matrix <- as.matrix(val_df[,-which(colnames(val_df) %in% labels)])


library(tsne)
library(lubridate)

set.seed(6889)
dev.tsne <- tsne(train.matrix, ##this is on the training set
                 initial_config = NULL, k = 2,
                 perplexity = 30, max_iter = 1000,
                 min_cost = 0, epoch_callback = NULL,
                 whiten = TRUE,epoch=100)

class(dev.tsne)


set.seed(6889)

dim(rbind(train.matrix, val.matrix)) ## 576 x2222


##both dev and validation set:
all.tsne <- tsne(rbind(train.matrix,
                       val.matrix), ##this is on both sets
                 initial_config = NULL, k = 2,
                 perplexity = 30, max_iter = 1000,
                 min_cost = 0, epoch_callback = NULL,
                 whiten = TRUE,epoch=100)

class(all.tsne)


## Convert to dataframes

dev.tsne_df <- cbind(as.data.frame(dev.tsne),
                     as.character(dev_df[,"label"]),
                     as.Date(dev$event.date))
colnames(dev.tsne_df) <- c("X", "Y", "label", "date")

dev.tsne_df$year <- lubridate::year(as.Date(dev.tsne_df$date))

###

all.tsne_df <- cbind(as.data.frame(all.tsne),
                     as.character(c(dev_df[,"label"],
                                    val_df[,"label"])),
                     as.Date(c(dev$event.date,
                               val$event.date)))

all.tsne_df$year <- lubridate::year(as.Date(all.tsne_df$date))

colnames(all.tsne_df) <- c("X", "Y", "label", "date", "year")


head(dev.tsne_df)

head(all.tsne_df)

table(dev.tsne_df$year)##2009-2014 have most data


library(ggplot2)
library(lattice)


gg <- ggplot(dev.tsne_df,
             aes(x = X,  y = Y, colour = label))
gg <- gg +geom_point()
gg <- gg + theme_bw()

plot(gg)

ggsave(gg, file="testData_tSNEDimRed.pdf")


## Now by year:

gg <- ggplot(dev.tsne_df[which(dev.tsne_df$year==2009),],
             aes(x = X,  y = Y, colour = label))
gg <- gg +geom_point()
gg <- gg + theme_bw()
gg <- gg + theme(axis.text.x=NULL)

plot(gg)

ggsave(gg, file="2009testData_tSNEDimRed.pdf")

## function

yearPlot <- function(df,listOfYears,typeOfData){
  
  for(i in listOfYears){
    gg <- ggplot(df[which(df$year==i),],
                 aes(x = X,  y = Y, colour = label))
    gg <- gg +geom_point()
    gg <- gg+  theme(axis.title.x=element_blank(),## remove x-axis labels
                     axis.text.x=element_blank(),
                     axis.ticks.x=element_blank())
    
    gg <- gg+ theme(axis.title.y=element_blank(),## remove y labs
                    axis.text.y=element_blank(),
                    axis.ticks.y=element_blank())
    
    gg <- gg + theme_bw()
    
    plot(gg)
    
    ggsave(gg, file=paste0(dataPath, i, typeOfData, "Data_tSNEDimRed.pdf"))
    print(paste0("done with year: ", i))
  }
  print("check plots!")
}

yearPlot(dev.tsne_df, listOfYears=c(2009, 2010, 2011, 2012, 2013, 2014),
         typeOfData="DevData")


yearPlot(all.tsne_df, listOfYears=c(2009, 2010, 2011, 2012, 2013, 2014),
         typeOfData="DevAndVal")


head(all.tsne_df)

class(all.tsne_df$label)

## date histogram:
gg2 <- ggplot(all.tsne_df, aes(x=date,
                               fill=factor(label)))+
  geom_histogram(position="identity",alpha=0.5,
                 binwidth=50)
gg2 <- gg2+ theme_bw()

gg2

#save.image(file=paste0(dataPath,"ksvm_large.Rdata"))

