## The visualization data comes from the REVMOD dataset
## Benjamin Acosta,  "Reconceptualizing Resistance Organizations and Outcomes:
## Introducing the Revolutionary and Militant Organizations dataset (REVMOD)," 
## Journal of Peace Research 56, 5 (2019): 724-734

## Loading a subset of the data that has the following orgs:
## Al-Qaeda in Yemen: group 152
## Al-Qaeda Arabia: 224
## AQAP: 80

ggdat_aqap <- read_csv(file.path(dataPath,"rdynv2_AQAP_subset.csv"))

ggdat_aqap$graphname <- NA

ggdat_aqap[which(ggdat_aqap$revmodid==80),c("graphname")] <- "AQAP"
ggdat_aqap[which(ggdat_aqap$revmodid==152),c("graphname")] <- "al-Qaeda in Yemen +"
ggdat_aqap[which(ggdat_aqap$revmodid==224),c("graphname")] <- "al-Qaeda in Arabia"

p1 = ggplot(ggdat_aqap,
       aes(y=size,
           x=year,
           group=as.factor(graphname),
           color=as.factor(graphname)))+
  geom_line()  + 
  ylab("Estimated Size")+
  xlab("Year") +
  theme_bw()+
  scale_x_continuous(breaks=c(1990:2014))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  labs(colour = "")+
  labs(caption = "(REVMOD Growth Data)")+
  theme(legend.position="bottom")

p1

#ggsave("aqapgrowthchart.pdf")

colors <- brewer.pal(n = length(unique(ggdat_aqap$graphname)),
                     name = "Set3")

## keep only data after 2009:
## Connects with the dyad evolution plot:
ggdat_aqap = ggdat_aqap[which(ggdat_aqap$year >= 2009),]
## just AQAP b/c the others are not meaningful after 2009:
ggdat_aqap = ggdat_aqap[which(ggdat_aqap$graphname >= "AQAP"),] 

growthplot =  ggplot(ggdat_aqap,
             aes(y = size,
                 x = year,
                 group = as.factor(graphname),
                 color = as.factor(graphname))) +
  geom_line(size = 1.2) +  # Increase line thickness
  ylab("Estimated Size") +
  xlab("Year") +
  scale_x_continuous(breaks = c(2003:2014)) +
  scale_color_manual(values = colors) +  # Same color palette
  theme_bw(base_size = 14) +  # Use same base font size as the first plot
  theme(
    axis.text.x = element_text(angle = -90, hjust = 0),  # Rotate x-axis labels
    axis.text.y = element_text(size = 10),  # Set y-axis text size
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),  # Axis label size
    legend.position = "none",  # Keep legend position consistent
    legend.title = element_blank(),  # Remove title from legend
    legend.text = element_blank()  # Adjust legend text size
  ) +
  labs(title = "AQAP Growth Trajectory", 
       caption = "(REVMOD Growth Data)")

growthplot

ggsave(growthplot, 
       file=file.path(figPath,"AQAP_growth.pdf"))
