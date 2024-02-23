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

toLoad <- c("tm", "kernlab", "caret",
            "dplyr", 'tsne', 'lubridate',
            "splitstackshape", "e1071", 
            "randomForest", "edarf",
             "rattle","rpart", 
            "randomForestExplainer", "lubridate")

loadPkg(toLoad)

## Edarf
devtools::install_github("zmjones/edarf", subdir = "pkg")

## Data
dataPath <- "./data/"

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

rm(meta) ## just a character list

## Data:


#################
## random forest
## when you write up:
################

## subset of the data b/c need to remove the "label" field

justDat.dev <- subset(dev.df, select= -c(X, label))
justDat.val <- subset(val.df, select= -c(X, label))


set.seed(6889)
library(randomForest)

rf3 <- randomForest(x=as.matrix(justDat.dev),
                    y=as.factor(dev.df$label),
                    xtest=justDat.val,
                    ytest=as.factor(val.df$label),
                    ntree=500,
                    keep.forest=TRUE,
                    importance=TRUE, ## assess importance of predictors
                    proximity=TRUE) #calculates promiximty of nodes


attributes(rf3)

rf3$classes

plot(rf3, log="y")

rf3.mds <- cmdscale(1 - rf3$proximity, eig=TRUE)

## Visualize important predictors:

impr3 <- randomForest::importance(rf3, type=1)

varImpPlot(rf3)


partialPlot(x=rf3,
            pred.data=as.matrix(justDat.dev),
            x.var=rebels,
            which.class="Houthi/Ansarallah")

##Use edarf package for visualizations:

pd_rf3 <- partial_dependence(rf3, vars=c("rebels"),
                             data=justDat.dev)
plot_pd(pd_rf3)

## Confusion matrix, training data:

round(rf3$confusion, 2)

## confusion matrix, test data:
round(rf3$test$confusion, 2)

###
library(xtable)

xtable(rf3$confusion, 2) ## Training data
xtable(rf3$test$confusion, 2) ##Test data


## visualize PCA decomposision
## Also using the eDarf package:

library(edarf)
                                        #
prox <- extract_proximity(rf3)
pca <- prcomp(prox, scale=TRUE)

pca_test <- prcomp(rf3$test$proximity)

##Training

pdf(file= paste0(dataPath, "pca_proximityRF.pdf"))
plot_prox(pca,
          color=dev.df$label)
dev.off()


## Test
pdf(file= paste0(dataPath, "pca_proximityRF_test.pdf"))
plot_prox(pca_test,
          color=val.df$label)
dev.off()


## Plot important words:
pdf(file=paste0(dataPath,"randomForestRes.pdf"))
varImpPlot(rf3, sort=TRUE, n.var=15,
           main="15 Most Important Words For Story Classification")
dev.off()


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

dev.tsne.df <- cbind(as.data.frame(dev.tsne),
                   as.character(dev.df[,"label"]),
                   as.Date(dev$event.date))
colnames(dev.tsne.df) <- c("X", "Y", "label", "date")

dev.tsne.df$year <- lubridate::year(as.Date(dev.tsne.df$date))

###

all.tsne.df <- cbind(as.data.frame(all.tsne),
                     as.character(c(dev.df[,"label"],
                                        val.df[,"label"])),
                     as.Date(c(dev$event.date,
                               val$event.date)))

all.tsne.df$year <- lubridate::year(as.Date(all.tsne.df$date))

colnames(all.tsne.df) <- c("X", "Y", "label", "date", "year")


head(dev.tsne.df)

head(all.tsne.df)

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

yearPlot(dev.tsne.df, listOfYears=c(2009, 2010, 2011, 2012, 2013, 2014),
         typeOfData="DevData")


yearPlot(all.tsne.df, listOfYears=c(2009, 2010, 2011, 2012, 2013, 2014),
         typeOfData="DevAndVal")


head(all.tsne.df)

class(all.tsne.df$label)

## date histogram:
gg2 <- ggplot(all.tsne.df, aes(x=date,
               fill=factor(label)))+
    geom_histogram(position="identity",alpha=0.5,
                   binwidth=50)
gg2 <- gg2+ theme_bw()

gg2


### save to pick up tomorrow:
save.image(file=paste0(dataPath,"ksvm_large.Rdata"))

