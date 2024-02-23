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

setwd("~/Dropbox/GrowthTrapCode/Replication/")

dataPath <- "./data/"

dev <- read.csv(paste0(dataPath,
                       "develop_df.csv"),
                stringsAsFactors=FALSE)

val <- read.csv(paste0(dataPath,
                       "validate_df.csv"),
                stringsAsFactors=FALSE)

table(dev$label)
table(val$label)

dev <- dev[which(dev$label %in% c("Ansar al-Shariah","AQAP/Al-Qaeda")),]

val <- val[which(val$label %in% c("Ansar al-Shariah", "AQAP/Al-Qaeda")),]

table(dev$label)
table(val$label)

## Take the dev set and make 10 subsets:
## with 1/10 of the AQAP and Houthi stories
## and the Ansar al-Shariah stories

set.seed(110122)
k= 10

k.aqap <- rep(1:k, length.out = 260) 
assignments.aqap <- sample(k.aqap)
assignments.aqap

dev$kgroup <- 0
dev[which(dev$label=="AQAP/Al-Qaeda"), "kgroup"] <- assignments.aqap

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
  scale_x_continuous("Data Subset (Fold)", 
                     breaks=c(1:10)) +
  scale_y_continuous("Probability of Misclassification", 
                     limits=c(0, 1)) +
  labs(linetype="Group")+
  theme(legend.position="bottom")


gg.classerror

ggsave(gg.classerror, 
       file="KfoldClassErrorSunniOnly.pdf")

## confusion matrix:
## low AAH class error case: Fold 3
fold3 <- koutlist[[3]]

library(xtable)
xtable(fold3$confusion)

## variable importance plot, Fold 3:
varImpPlot(fold3)

impt.fold3 <- fold3$importance %>% 
  data.frame() %>% 
  mutate(feature = row.names(.))  %>%
  arrange(desc(MeanDecreaseGini)) ## descending order

impt.fold3$fold <- 3
impt.fold3$condition <- "Low Error"


## high class error case: Round 7

library(xtable)

fold7 <- koutlist[[7]]

xtable(fold7$confusion)

## variable importance plot, Fold 3: 

varImpPlot(fold7)

## Plot important words:

impt.fold7 <- fold7$importance %>% 
  data.frame() %>% 
  mutate(feature = row.names(.))  %>%
  arrange(desc(MeanDecreaseGini)) ## descending order

impt.fold7$fold <- 7
impt.fold7$condition <- "High Error"

ggdat <- rbind(impt.fold7[1:10,c("MeanDecreaseGini", "feature",
                             "fold", "condition")],
               impt.fold3[1:10,c("MeanDecreaseGini", "feature",
                             "fold", "condition")])

gg <- ggplot(ggdat, 
             aes(x = feature, 
                        y = MeanDecreaseGini)) +
  geom_bar(stat='identity') +
  coord_flip() +
  theme_classic() +
  facet_wrap(~fold, scales = "free")+
  labs(
    x     = "Feature",
    y     = "Importance",
    title = "Feature Importance: \nLow vs High Error Models"
  )

gg

ggsave(gg, file=paste0(dataPath,"randomForestComparison.pdf"))

varImpPlot(fold7, sort=TRUE, n.var=15,
           main="15 Most Important Words For Story Classification \n High Error Model")

dev.off()

### Over time:

p.mp <- as.data.frame(cbind(koutlist[[3]] $predicted,
                            dev[which(dev$kgroup %in% c(0, 3)),]$label, 
                            dev[which(dev$kgroup %in% c(0, 3)),]$event.date,
                            dev[which(dev$kgroup %in% c(0, 3)),]$storyid))

colnames(p.mp) <- c("predicted", "real", "date", "id")

p.mp$pred.class <- NA
p.mp[which(p.mp$predicted== '1'), 
     "pred.class"] <- "Ansar al-Shariah"
p.mp[which(p.mp$predicted== '2'),
     "pred.class"] <- "AQAP/Al-Qaeda"

p.mp$success <- ifelse(p.mp$real== p.mp$pred.class,
                       1, 0)

view(p.mp)

## MJF: Pick up here
p.mp$year <-  as.Date(p.mp$date, format="%Y%m%d")

view(p.mp)

p.mp %>%
  group_by(year = as.Date(as.year(mdy(date)), 1)) 

pp1 <- ggplot(p.mp,
              aes(x=as.Date(date),
                  y=success))+
  geom_point(aes(shape=real))+
##  facet_wrap(~real,
   ##          nrow=3) + 
  theme_bw() +
  theme(axis.text.x = 
          element_text(angle = 45, 
                       vjust = 0.5, 
                       hjust=1)) +
  scale_x_date(breaks="1 year", 
               date_labels = "%b %d")

pp1