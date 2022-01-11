# Attach libraries

library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)
library(data.table)
library(flipMultivariates)
library(psych)
library(rstudioapi)
library(rstatix)
library(tictoc)
library(cowplot)
library(readr)

# Set seed for reproducability
set.seed(1)

# Set working directory using RStudio API
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

# Read data
data <- read.csv('Data/SPD_withClusters_and_reducedGenre.csv')
data$X <- NULL
data$Unnamed..0 <- NULL

# Merge cluster 6 with cluster 4
data$clusterID[data$clusterID == 6] <- 4

# prune dataset to include only what we're interested in

pCompData <- subset(data, select=c(playlistID,
                                   TrackID,
                                   label,
                                   userCat,
                                   demoCat,
                                   length,
                                   nFoll,
                                   nTracks,
                                   clusterID))


pCompData$playlistID <- as.factor(pCompData$playlistID)
pCompData$TrackID <- as.factor(pCompData$TrackID)
pCompData$userCat <- as.factor(pCompData$userCat)
pCompData$demoCat <- as.factor(pCompData$demoCat)



# calculate the distribution of clusters in the playlists

propData <- subset(pCompData, select=c(playlistID, clusterID))
propData$clusterID <- as.factor(propData$clusterID)
propData.table <- prop.table(table(propData), margin=1)
propData.df <- as.data.frame(prop.table(table(propData), margin=1))

# filter down to only playlists
dupIndex <- duplicated(pCompData[,c('playlistID')])
pCompData.unique <- pCompData[!dupIndex,]
pCompData.unique <- subset(pCompData.unique, select=-c(label, length, clusterID, TrackID))

# Check how the general distribution of clusters within playlists look like. 
# Run through them, and sort per row

propData.sorted = t(apply(propData.table, 1, sort, decreasing = TRUE))

propData.sorted.df = as.data.frame(propData.sorted)
propData.sorted.df <- cbind(playlistID = rownames(propData.sorted.df), propData.sorted.df)
rownames(propData.sorted.df) <- 1:nrow(propData.sorted.df)

colnames(propData.sorted.df) <- c('playlistID', '1st', '2nd', '3rd', '4th', '5th', '6th')

# now melt it
pCompSorted.melted <- melt(propData.sorted.df, id.vars=c('playlistID'))


# ggplot:
w=6
h=4
dpi=300

withinPlaylist <- ggplot(pCompSorted.melted,aes(x=variable, y=value, fill=variable))+
  geom_boxplot(position = position_nudge(x = 0, y = 0), notch=FALSE) +
  #geom_flat_violin(position = position_nudge(x = 0, y = 0)) +
  ylab('')+xlab('')+
  theme_cowplot()+guides(fill = FALSE)+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle('Within-playlist clusterization') +
  theme(text = element_text(size=12))
withinPlaylist

ggsave('Plots/withinPlaylistClusterDistribution.png', width = w, height = h, dpi=dpi)







# Calculate the specific distribution of clusters in spotify/user playlists

# calculate the distribution of clusters in the playlists


propDataAll <- subset(pCompData, select=c(playlistID, clusterID))
propDataAll$clusterID <- as.factor(propDataAll$clusterID)
propDataAll <- prop.table(table(propDataAll), margin=1)

propDataAllDF <- as.data.frame.array(propDataAll)
propDataAllDF <- cbind(playlistID = rownames(propDataAllDF), propDataAllDF)
rownames(propDataAllDF) <- 1:nrow(propDataAllDF)
colnames(propDataAllDF) <- c('playlistID', 'C1', 'C2', 'C3', 'C4', 'C5', 'C6')


# add in userCat


propDataAllComp <- merge(propDataAllDF, pCompData.unique, by='playlistID')

library(ggpubr)

propDataAllComp <- subset(propDataAllComp, userCat!='')

propDataAll.UvsS <- subset(propDataAllComp, select = c(C1, C2, C3, C4, C5, C6, userCat))

describeBy(subset(propDataAllComp, select=c('C1','C2', 'C3', 'C4', 'C5', 'C6', 'userCat')) , group='userCat')


# make plot

propDataAll.UvsS.melted = melt(propDataAll.UvsS, by=usercat)




UvsS <- ggplot(propDataAll.UvsS.melted,aes(x=variable, y=value, fill=userCat))+
  geom_boxplot(notch=FALSE) +
  #geom_flat_violin(position = position_nudge(x = 0, y = 0)) +
  ylab('')+xlab('')+
  theme_cowplot()+guides(fill = FALSE)+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle('Comparison of clusters in user vs Spotify playlists') +
  theme(text = element_text(size=12))
UvsS


ggsave('PlotsForPublication/Comparison of clusters in user vs Spotify playlists.pdf', width = w, height = h, dpi=dpi)
ggsave('PlotsForPublication/Comparison of clusters in user vs Spotify playlists.png', width = w, height = h, dpi=dpi)
