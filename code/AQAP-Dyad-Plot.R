## Event data from:
## Sundberg, Ralph, and Erik Melander. 
##"Introducing the UCDP georeferenced event dataset." 
## Journal of peace research 50, no. 4 (2013): 523-532.



ucdpAQAP <- readr::read_csv(file.path(dataPath,
                                      "gedevents-2021-AQAP2.csv"))
ucdpAQAP$X <- NULL ## drop the added "X" row names

dim(ucdpAQAP) ## check data: 1272 x 48

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

## Format for the alluvial plot
dyad_alluvial <- ggdat %>%
  group_by(year, dyad_name) %>% 
  summarise(count = n(), .groups = "drop")

## Variation with slightly differnt
# Define color palette from RColorBrewer
colors <- brewer.pal(n = length(unique(dyad_alluvial$dyad_name)), name = "Set3")

dyadplot = ggplot(dyad_alluvial,
       aes(x = year,
           stratum = dyad_name,
           alluvium = dyad_name,
           y = count,
           fill = dyad_name,
           label = dyad_name)) +
  geom_flow(stat = "alluvium", lode.guidance = "frontback", alpha = 0.8) +  # Increase alpha for clarity
  geom_stratum() +
  scale_fill_manual(values = colors) +  # Assign custom colors
  theme_bw(base_size = 14) +  # Increase font size for publication quality
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = -90, hjust = 0.5, size = 10),  # Rotate axis text to avoid overlap
    axis.text.y = element_text(size = 10),  # Set y-axis text size
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Title style
    axis.title = element_text(size = 12)  # Axis label size
  ) +
  scale_x_continuous(breaks = seq(min(ggdat$year), max(ggdat$year), by = 1)) +
  ylab("Number of Events") +
  xlab("Year") +
  labs(title = "AQAP Conflict Dyads",  
  caption = "(UCDP Conflict Data)")

dyadplot

ggsave(dyadplot, 
       file= file.path(figPath,"AQAPActivities.pdf"))
