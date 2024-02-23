
rm(list=ls())

setwd("~/GrowthTrapCode/Replication/")

## load packages to clean and work with data

library(ggplot2)
library(tidyverse)
library(RColorBrewer)

## AQAP GED Subset
## Event data from:
## Sundberg, Ralph, and Erik Melander. "Introducing the UCDP georeferenced event dataset." Journal of peace research 50, no. 4 (2013): 523-532.

dataPath = "./data/"
ucdpAQAP <- read.csv(paste0(dataPath,"gedevents-2021-AQAP2.csv"))

ucdpAQAP$X <- NULL ## drop the added "X" row names

dim(ucdpAQAP) ## 1272 x 48

##%%%%%%%%%%%%%%%
## Descriptive Statistics
##%%%%%%%%%%%%%%

## How much activity in 2009-2016

ucdpAQAP$startdate2 <- as.Date(ucdpAQAP$date_start,
                               format="%m/%d/%Y")

table(ucdpAQAP$year)

## 2009 5
## 2010 63
## 2011 175
## 2012 249
## 2013 126
## 2014 243
## 2015 67
## 2016 97
## 2017 125
## 2018 78
## 2019 44


unique(ucdpAQAP$type_of_violence) ## 1 and 2

table(ucdpAQAP$type_of_violence)
## value 1 [state-based conflict] 910 (AQAP-Yemen state)
## value 2 [non-state conflict] 362 (prob Houthi conflict)


table(ucdpAQAP$side_a)

## AQAP 326, Ansarallah 36
## Government of Yemen 910
## (by definition this is always the government in a
## government-NSA dyad)

table(ucdpAQAP$side_b)
## AQAP 946
## forces of Hadi 301
## IS 25
## (looks like the violence with AQAP as side A will be
## forces of Hadi and IS)

length(unique(ucdpAQAP$dyad_name)) ## 4 unique dyads
table(ucdpAQAP$dyad_name)

##Ansarallah - AQAP: 36
## AQAP - Forces of Hadi 301
## AQAP - IS 25
## Government of Yemen (North Yemen) - AQAP ## 910

##%%%%%%%%%%%%%%%
## Plot events by year
##%%%%%%%%%%%%%%%%

## Want to plot events by year; colored (or separate lines)
## by the dyad. To see if AQAP has also transitioned primarily

ggdat <- ucdpAQAP[,c("year", "dyad_name", "side_a", "side_b")]

pp <- ggplot(ggdat,
             aes(x=year,
                 fill= dyad_name))+
       geom_bar(stat="bin", position="fill") +
    theme_minimal()+
  ylab("Dyad Proportion")+
##  geom_col(color = "black")+
  scale_x_continuous("Year",
                  breaks=seq(2009, 2019, by=1))+
     theme(legend.position="bottom")+
  scale_fill_grey() + 
  labs(fill = "Dyad")

pp

ggsave(pp, 
       file="AQAPActivities.pdf")

#########
