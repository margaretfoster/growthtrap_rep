## This file to start analyzing the Yemen story
## data from ICEWS
## Follows YemenTestTrainSplit.R

rm(list=ls())

setwd("~/Dropbox/Research/YemenEventData/Replication/")

#####################
## Load training data:
####################

dataPath <- "./data/"

trainData <- read.csv(paste0(dataPath,"yemenTrainData.csv"),
                      stringsAsFactors=FALSE)

##Load the "all events key" to see what stories are tagged in the
## data as being violent:

## load Yemen data from ICEWS

allEvents <- read.csv(paste0(dataPath,"yemenAllEvents.csv"),
                      stringsAsFactors=FALSE)


#############################
### SUBSET TO VIOLENT EVENTS
############################

## pull out event types that are violence-related
violentEvents <- c("Threaten with military force",
                   "Use unconventional violence",
                   "Violate ceasefire",
                   "Use as human shield",
                   "Threaten",
                   "Occupy Territory",
                   "Physically assault",
                   "Mobilize or increase armed forces",
                   "Engage in violent protest for leadership change",
                   "Engage in mass killing",
                   "Conduct suicide, car, or other non-military bombing",
                   "Carry our suicide bombing",
                   "Attempt to assassinate",
                   "Assassinate",
                   "Abduct, hijack, or take hostage",
                   "fight with small arms and light weapons",
                   "fight with artillery and tanks")


violentEventsSubset <- allEvents[which(allEvents$event_name
                                       %in% violentEvents),]

## Note: this is a formatting issue:
## convert the text to ASCII and see if that solves the problem
## of having some characters with >2 bytes.
## converting with NA works, but drops a bunch of the text
## all of the other conversion options run into the same
## byte size issue on mTurk. Thus, will say that the
## algorithms were done on a "subset of stories about AQ/Houthi/AAS" operations

trainData$cleaned <- iconv(trainData$cleaned,
                           from="UTF-8", to="ASCII")

trainData$rawtext <- iconv(trainData$rawtext,
                           from="UTF-8", to="ASCII")

dim(na.omit(trainData))


violentEventsSubset$acii.text<- iconv(violentEventsSubset$rawtext,
                                     from="UTF-8", to="ASCII")

## this loses about 1/3 of the entries
## but as long as they're random, should be ok for the
## conclusions of the classifiers

trainData <- na.omit(trainData)

colnames(trainData)

## See how many training stories are in the pre-defined
## list of violent events

dim(trainData) ##4231

## Pull out the story IDs that are in my "training" set:

lst <- intersect(trainData$storyid,
                 violentEventsSubset$story_id) ##1772

length(lst) ## 1772 stories

## Here I subset the full violent event data into a random
## collection of 1772
violentEventsTrain <- trainData[which(trainData$storyid %in% lst),]


colnames(violentEventsTrain)


set.seed(6889)
rnd <- sample(dim(trainData)[1], size=600, replace=FALSE)

## here is where I dropped the dates in the original
sbset <- trainData[rnd,c("rawtext", "partition") ]

sbsetWithDates <- trainData[rnd,c("rawtext", "partition",
                                  "storyid", "event_date")]

write.csv(sbsetWithDates, file=(paste0(dataPath, "sbsetWithDates.csv")))


write.csv(sbset,file=paste0(dataPath, "sbset.csv"))

write.csv(violentEventsTrain[,c("rawtext",
                                "storyid",
                                "partition",
                                "event_date")],
          file=paste0(dataPath,"violentEventsTrain.csv"))

#################### Term document matrix
###############

library(tm)

stopwords <- stopwords('en')

stories <-Corpus(VectorSource(trainData$cleaned))

stories <- tm_map(stories, removeWords, stopwords("english"))


storiesDTM<- DocumentTermMatrix(stories)

wordMatrix <- as.data.frame(as.matrix(storiesDTM))

dim(wordMatrix)## 6460 x 59678

##extract high frequency words

wordFreqs <- colSums(wordMatrix)

head(wordFreqs)

## sort into ascending order, keep the names
wordFreqs <- sort(wordFreqs)

length(wordFreqs)

## top 10% of words= 596 words
## note that a lot of these top 10% words are location or possible
## targets, so should keep

## pulling out words from top 10% that are not location or people

extendedStop <- c("will", "said", "came", "around", "media", "held",
                  "home", "among", "trying", "carried","many", "dead",
                  "may", "one", "since", "news", "last", "xinhua",
                  "told", "also", "two","others", "afp", "reuters",
                  "security", "report", "online", "official", 
                  "yemen", "yemeni", stopwords)


length(extendedStop) ##204

### reprocess words to remove the stopwords again:

stories <- tm_map(stories, removeWords, extendedStop)

storiesDTM<- DocumentTermMatrix(stories)

inspect(storiesDTM)

###term frequency inverse document frequency
storiesTFIDF <- weightTfIdf(storiesDTM)

mattfidf <- as.matrix(storiesTFIDF)

## Exploratory k-means clustering
## with three centers
## Maybe AQAP, AAH, Houthi
## Spoiler: is not
#### Kmeans

kMeans <- kmeans(mattfidf, centers=3)

## Save output
save(kMeans, file=paste0(dataPath, "YemenKMeansClustering.rda"))

## Print Statement announcing completion:

print("Kmeans finished and saved!")

## head(kMeans$cluster)



