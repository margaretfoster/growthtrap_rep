
## This plot to graph the topic clusters
#######################################
## Load pre-computed clustered topics
######################################
dataPath <- "./data/"
 
## load pre-computed grouped topics:
load(file.path(dataPath,"precomputed_topic_groups.RData"))

################################
## Plot clusters 1 and 3
## local conflict and transnational jihad:
##############################

## First some information about the total
## expected topic proportions for clusters 1 & 3
## (To make point that the two are not necessarily zero-sum

## Validate that these are not the total topic space
## Eg: both don't sum to 1:
totals <- clusteredmeans[[1]] + clusteredmeans[[3]]
summary(totals)
hist(totals)
plot(totals~uvals1) ## uvals is date

dev.off()

pdf(file=file.path(figPath,"localtranscluster.pdf"),
    width = 8,    # Width in inches
    height = 6)   # Height in inches
 
xlimcust <- c(min(uvals1), max(uvals1))
ylimcust <- c(min(unlist(ciscust), na.rm=T),
              max(unlist(ciscust), na.rm=T))
ylabcust <- "Exp. Topic Prop."
xlabcust <- "Document Date"

par(mar = c(4, 4, 2, 1)) 
plot(0, 0,col="white",
     xlim=xlimcust,
     ylim=ylimcust,
     ylab=ylabcust,
     xlab=xlabcust,
     xaxt="n",## make no text on x-axis
     main="Estimated Topic Themes")

## y=0 line
yline <- rep(0,length(uvals1))
lines(uvals1,
      yline,
      col="red",
      lty=2)

## X-Axis as date:
axis(1, at=as.numeric(yearseq)- min(as.numeric(yearseq)),
     labels=yearnames, las=2, cex=.25)

## Black  and gray in cols:
## clusters 1 (local ops) and 3 (transnational)
set3_colors <- brewer.pal(12, "Set3") #set consistency with other plots

## teal and purple
local_color <- set3_colors[1]   # First color (e.g., Local Conflict)
transnational_color <- set3_colors[10]  # Second color (e.g., Transnational Jihadi)

lines(uvals1, ## Local 
      clusteredmeans[[1]],
      col = local_color,
      lty=5 #longdash
      )
lines(uvals1,## transnational
      clusteredmeans[[3]],
     col = transnational_color,
      lty=3) #dotted
    
legend(x = max(uvals1) * 0.3,
       y = -0.1,  # Posits the legend below the data
      legend = c("Local Conflict", "Transnational Jihadi"),
      fill = c(local_color, transnational_color),
      #fill = c("gray8", "gray48"),
      title = "Topic Clusters",
      bty = "n",    # No box around the legend
      cex = 1,    
      ncol = 2,     
      xpd = TRUE)   # allow drawing outside of plot

dev.off()

cat("Finished local vs transnational plot")
