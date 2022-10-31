### Coral Reef RLI Calculation

rm(list=ls()) # removes all objects from the environment
cat("\014") # clears the console

library(stringr)
library(plyr)
library(readxl)
library(xlsx)
library(officer)
library(rvg)
library(png)
library(grid)
library(tidyverse)
`%notin%` <- Negate(`%in%`)

## LOAD Status all spp
IUCN_categories_global <- as.data.frame(read_excel("GSTP_GLOBAL_SpeciesList_210602.xlsx", sheet = 'Merge'))
IUCN_categories_global$friendly_name_Category[IUCN_categories_global$friendly_name_Category == "Hemitrygon bennettii"] <- "Hemitrygon bennetti"

## LOAD coral reef status 2005 real
IUCN_categories_2005real <- as.data.frame(read_excel("CoralReefSpecies_210602.xlsx", sheet = 'SpeciesList'))
IUCN_categories_2005real$Species[IUCN_categories_2005real$Species == "Urobatis jamacensis"] <- "Urobatis jamaicensis"

## LOAD other taxa RLI
IUCN_categories_othertaxa <- as.data.frame(read_excel("CoralReefSpecies_210602.xlsx", sheet = 'OtherReefGroups'))

## Selec with Habitat (remove freshwater species)
IUCN_categories_global_coralreef <- IUCN_categories_global[IUCN_categories_global$friendly_name_Category %in% IUCN_categories_2005real$Species, ]


## Build status df ; no 1970 for now
status_global <- data.frame('species'= IUCN_categories_global_coralreef$friendly_name_Category
                            , 'subclass' = IUCN_categories_global_coralreef$Subclass
                            # , 'y1980'= IUCN_categories_global_coralreef$`1980RLI`
                            , 'y2005real'= IUCN_categories_2005real$ActualRedListStatus2005
                            , 'y2005hindcast'= IUCN_categories_global_coralreef$`2005RLI`
                            , 'y2020'= IUCN_categories_global_coralreef$GSTP_2020_RLCategory
)
status_global <- status_global[status_global$y2020 %notin% c('DD', 'NE'),]

# write.xlsx(status_global, file = "status_global.xlsx", row.names = F)

## Remove species with DD (and temporary with '0' value)
# status_global <- status_global[!(apply(status_global[,3:ncol(status_global)],1,function(x){any(x == 'DD')})),]


# all(status_global_temp[7,3:5] %notin% c('DD', 'NE'))
# status_global_temp <- status_global
# status_global_temp$test <- apply(status_global_temp[,3:ncol(status_global_temp)],1,function(x){all(x %notin% c('DD', 'NE'))})
##
create_RLI_df <- function(subclass = 'all', print='yes', status_df = status_df, type_RLI = type_RLI){
  if(subclass != 'all'){
    status_global_temp <- status_df[status_df$subclass == subclass, ]
  } else{status_global_temp <- status_df}
  if(type_RLI == 'hindcast'){
    status_global_temp$y2005 <- status_global_temp$y2005hindcast
    
  } else{
    status_global_temp <- status_global_temp[apply(status_global_temp[,3:ncol(status_global_temp)],1,function(x){all(x %notin% c('DD', 'NE'))}), ]
    if(type_RLI == 'real'){
      status_global_temp$y2005 <- status_global_temp$y2005real
    }
    if(type_RLI == 'hindcast_realdataset'){
      status_global_temp$y2005 <- status_global_temp$y2005hindcast
    }
  }
  status_global_temp$y2005real <- status_global_temp$y2005hindcast <- NULL
  
  status_sp <- as.data.frame(t(status_global_temp[,3:ncol(status_global_temp)]))
  status_sp <- cbind(as.integer(sub('.', '', row.names(status_sp))),status_sp)
  colnames(status_sp) <- c('year', status_global_temp[,1])
  rownames(status_sp) <- NULL
  status_sp[,2:ncol(status_sp)] <- apply(status_sp[,2:ncol(status_sp)], 2, function(x){as.integer(mapvalues(x, from=c('CR(PE)', 'CR', 'EN', 'VU', 'NT', 'LC'), to=c(4,4,3,2,1,0), warn_missing = F))})
  
  RLI_df <- data.frame('year'=status_sp$year, 'RLI'= NA_real_) 
  RLI_df$RLI <- apply(RLI_df,1,function(df){
    xxx <- apply(as.data.frame(table(as.numeric(status_sp[status_sp$year == df['year'],2:ncol(status_sp)])), stringsAsFactors =F),2,as.numeric)
    1 - (sum(apply(xxx,1,prod)) / (5 * sum(xxx[,2])))
  })
  if(print == 'yes'){print(RLI_df)}
  return(RLI_df)
}


plot_RLI <- function(status_df=status_global, type_RLI = c('hindcast', 'real', 'hindcast_realdataset')[1], subclass = 'all', additional_sperated_RLI_Sharks_Rays = 'no', print = 'no'){
  
  main_plot <- paste0('Red List Index : ', paste0(type_RLI, collapse = ', '))
  par(mar=c(1.8,2.6,.5,.5),mgp = c(3, .4, 0))
  year_start_lim = c(1980-2,2020+2)
  plot(NA, main = main_plot, ylim=c(0,1), xlim = year_start_lim, xaxt = 'n', yaxt = 'n', pch=19, yaxs="i", xaxs="i", bty="n", ylab = "", xlab = "", col.axis = 'grey40', col.lab = 'grey60')
  axis(1, at=c(1980,2005,2020), tck=-.01, labels=c(1980,2005,2020), las=1, col= 'grey60', col.axis = 'grey40', cex.axis=1, mgp = c(3, .2, 0))
  axis(2, at=seq(0,1,by=.2), tck=-.01, labels=seq(0,1,by=.2), las=1, col= 'grey60', col.axis = 'grey40', cex.axis=1)
  
  lines(c(0.984,0.809)~c(1996,2008), lwd=2, col= 'grey60') #corals
  points(c(0.984,0.809)~c(1996,2008), pch=19, lwd=4, col= 'grey60') #corals
  lines(c(0.906,0.899)~c(1988,2016), lwd=2, col= 'grey60') #birds
  points(c(0.906,0.899)~c(1988,2016), pch=19, lwd=4, col= 'grey60') #birds
  lines(c(0.854,0.847)~c(1996,2008), lwd=2, col= 'grey60') #mammals
  points(c(0.854,0.847)~c(1996,2008), pch=19, lwd=4, col= 'grey60') #mammals
  lines(c(0.76,0.738)~c(1980,2004), lwd=2, col= 'grey60') #amphibians
  points(c(0.76,0.738)~c(1980,2004), pch=19, lwd=4, col= 'grey60') #amphibians
  lines(c(0.587,0.563)~c(2003,2014), lwd=2, col= 'grey60') #cycads
  points(c(0.587,0.563)~c(2003,2014), pch=19, lwd=4, col= 'grey60') #cycads
  
  
  # RLI_df <- create_RLI_df(subclass = subclass, print = print, status_df = status_df, type_RLI = type_RLI[1]) # global RLI_df
  # lines(RLI~year, data = RLI_df, lwd=3)
  # points(RLI~year, data = RLI_df, pch=19, lwd=6)
  # text(RLI_df$year, y=RLI_df$RLI, as.character(round(RLI_df$RLI,2)), cex=.9, col='black', pos = 1)
  # RLI_type_temp=type_RLI[2]
  for(RLI_type_temp in type_RLI){
    RLI_df <- create_RLI_df(subclass = subclass, print = print, status_df = status_df, type_RLI = RLI_type_temp) # global RLI_df
    if(length(type_RLI) == 1){
      lines(RLI~year, data = RLI_df, lwd=3)
      points(RLI~year, data = RLI_df, pch=19, lwd=6)
      text(RLI_df$year, y=RLI_df$RLI, as.character(round(RLI_df$RLI,2)), cex=.9, col='black', pos = 1)
      text(2020, y=RLI_df$RLI[RLI_df$year == 2020], RLI_type_temp, col='black', pos = 4)
    } else{
      lines(RLI~year, data = RLI_df, lwd=3, col=c('darkblue', 'lightblue')[which(type_RLI == RLI_type_temp)])
      points(RLI~year, data = RLI_df, pch=19, lwd=6, col=c('darkblue', 'lightblue')[which(type_RLI == RLI_type_temp)])
      # text(RLI_df$year, y=RLI_df$RLI, as.character(round(RLI_df$RLI,2)), cex=.9, col='black', pos = 1)
      text(1982, y=c(0.2,0.15)[which(type_RLI == RLI_type_temp)], paste0(RLI_type_temp, '  (RLI2005:', round(RLI_df$RLI[RLI_df$year == 2005],2), ' ; RLI2020:', round(RLI_df$RLI[RLI_df$year == 2020],2), ')'), cex = .9, col=c('darkblue', 'lightblue')[which(type_RLI == RLI_type_temp)], pos = 4)
    }
  }
  
  if(additional_sperated_RLI_Sharks_Rays == 'yes'){
    RLI_df <- create_RLI_df(subclass = 'ray', print = print, status_df = status_df, type_RLI = type_RLI)
    lines(RLI~year, data = RLI_df, lwd=2, col='grey40')
    points(RLI~year, data = RLI_df, pch=19, lwd=2.5, col='grey40')
    text(2020, y=RLI_df$RLI[RLI_df$year == 2020], 'ray', cex=.8, col='grey40', pos = 1)
    
    RLI_df <- create_RLI_df(subclass = 'shark', print = print, status_df = status_df, type_RLI = type_RLI)
    lines(RLI~year, data = RLI_df, lwd=2, col='grey40')
    points(RLI~year, data = RLI_df, pch=19, lwd=2.5, col='grey40')
    text(2020, y=RLI_df$RLI[RLI_df$year == 2020], 'shark', cex=.8, col='grey40', pos = 3)
  }
  
  points(2006,RLI_global_shark_elife2014 <- 0.788, pch=19, lwd=6, col='grey30')
  text(x=2003, y=.8, 'Global\nsharks', cex=.9, col='grey30')
  
  box('plot', lwd = 1, col= 'grey60')
  mtext(side = 1, line = 1.2, "Year", col='grey40', cex=1.4)
  mtext(side = 2, line = 1.8, "Red List Index", col='grey40', cex=1.4)
  
  mypng = readPNG('./taxa pics/birds.png')
  grid.raster(mypng, x=grconvertX(1988.5,from='user',to='npc'), y=grconvertY(0.92,from='user',to='npc'), width = .08)
  mypng = readPNG('./taxa pics/mammals.png')
  grid.raster(mypng, x=grconvertX(1995.5,from='user',to='npc'), y=grconvertY(.85,from='user',to='npc'), width = .09)
  mypng = readPNG('./taxa pics/amphibians.png')
  grid.raster(mypng, x=grconvertX(1985,from='user',to='npc'), y=grconvertY(.71,from='user',to='npc'), width = .07)
  mypng = readPNG('./taxa pics/corals.png')
  grid.raster(mypng, x=grconvertX(1996,from='user',to='npc'), y=grconvertY(.94,from='user',to='npc'), width = .06)
  mypng = readPNG('./taxa pics/cycads.png')
  grid.raster(mypng, x=grconvertX(2002.5,from='user',to='npc'), y=grconvertY(.63,from='user',to='npc'), width = .08) 
}


plot_RLI(status_df=status_global, type_RLI = c('real', 'hindcast_realdataset'), subclass = 'all', additional_sperated_RLI_Sharks_Rays = 'no', print = 'no')

## CREATE POWERPOINT
pptx <- read_pptx()
# SLIDE RLI 2020 Hindcast
pptx <- add_slide(pptx,layout = "Title and Content", master = "Office Theme")
ph_with(pptx, dml(plot_RLI(status_df=status_global, type_RLI = 'hindcast', subclass = 'all', additional_sperated_RLI_Sharks_Rays = 'yes', print = 'no'), bg = "transparent"), location =
          ph_location(width = 6, height = 5)
        # ph_location_type(type = "body")
)
# SLIDE RLI 2020 real
pptx <- add_slide(pptx,layout = "Title and Content", master = "Office Theme")
ph_with(pptx, dml(plot_RLI(status_df=status_global, type_RLI = 'real', subclass = 'all', additional_sperated_RLI_Sharks_Rays = 'yes', print = 'no'), bg = "transparent"), location =
          ph_location(width = 6, height = 5)
        # ph_location_type(type = "body")
)
# SLIDE RLI 2020 real and 2020 hindcast_realdataset
pptx <- add_slide(pptx,layout = "Title and Content", master = "Office Theme")
ph_with(pptx, dml(plot_RLI(status_df=status_global, type_RLI = c('real', 'hindcast_realdataset'), subclass = 'all', additional_sperated_RLI_Sharks_Rays = 'no', print = 'no'), bg = "transparent"), location =
          ph_location(width = 6, height = 5)
)
print(pptx, target = "RLI_coral_reef_species.pptx")
