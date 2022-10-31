###### FinPrint to IUCN #######

library(beepr)
library(tidyr)
library(dplyr)
library(MASS)
library(tidyverse)
library(lme4)
library(ggplot2)
library(ggrepel)
library(betareg)
library(ggbreak)

finprint <- read.csv("FinPrint_Set_Data.csv")
head(finprint)

fp_useful <- subset(finprint, select= c(3,14,16,17,18,52,82))
head(fp_useful)

countries <- unique(fp_useful$location_name)
countries

### Remove NZ
fp_data <- fp_useful[!(fp_useful$location_name == "New Zealand"), ]
head(fp_data)

count_nonz <- unique(fp_data$location_name)
count_nonz

#### Check functional groups
fnlgrp <- unique(fp_data$functional_group)
fnlgrp

### Summarize Data
summaxn <- fp_useful %>%
  group_by(functional_group)


### Summary Stats
nBRUV <- fp_useful %>% count(fp_useful$functional_group, location_name)
nBRUV

maxnCount <- fp_useful %>% count(fp_useful$functional_group, fp_useful$location_name, wt=maxn)
maxnCount

videotime <- fp_useful %>% count(location_name, functional_group, wt=video_length_watched)
videotime

head(fp_useful)

fpsums <- read.csv("SharkRayFPInfo_BRT.csv")
head(fpsums)

thrprop <- read.csv("FPSums4R.csv")
head(thrprop)

thrspue <- left_join(thrprop, fpsums, by = "location_name", all.x = TRUE, all.y=TRUE)
head(thrspue)


thrspue2 <- thrspue[-c(12), ]
head(thrspue2)

thrspue2$ThrPerc <- (thrspue2$ThrProp/100)
head(thrspue2)
print(thrspue2)

### Needs values that are not zero or one so add miniscule amount to all values of SPUE
# subtract miniscule amount to all values of threat percentage
thrspue2$spuetotal <- (thrspue2$spuetotal+0.000000000001)
head(thrspue2)

thrspue2$ThrPerc <- (thrspue2$ThrPerc-0.000000000001)
head(thrspue2)

### Relation of Proportion Threatened and Total CPUE
propcpue <- betareg(ThrPerc ~ spuetotal, data=thrspue2)
summary(propcpue)

meanthr <- mean(thrspue2$ThrProp)
meanthr

meanspue <- mean(thrspue2$spuetotal)
meanspue

medianthr <- median(thrspue2$ThrProp)
medianthr

medianspue <- median(thrspue2$spuetotal)
medianspue

#############################################################################################
plotdata <- read.csv("FPSums4R.csv")
head(plotdata)
print(plotdata)

newplotdata <- plotdata[-c(7), ]
print(newplotdata)

#### Create Scatterplot ###
FPfig <- ggplot(thrspue2, aes(x=spuetotal, y=ThrProp, colour=Region)) + 
  geom_point() +
  geom_text_repel(label=thrspue2$location_name) +
  #geom_text(label=thrspue2$location_name, nudge_y=0.6) +
  #geom_smooth(method=lm, se=FALSE) +
  ylab('Species Threatened (%)') +
  xlab('SPUE (MaxN / hr)') +
  geom_hline(yintercept=medianthr, linetype="dashed") +
  geom_vline(xintercept=medianspue, linetype = "dashed") +
  theme(legend.key=element_blank()) +
  xlim(-0.2, 3.1) +
  ylim(30, 100) +
  labs(colour = "Continent") +
  theme(legend.position = c(0.85, 0.3)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

FPfig



