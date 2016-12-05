# Replication file for 
# Nisser, A. & Weidmann, N.B. (2017)
# "Measuring Ethnic Preferences with Mobile Advertising
# in Bosnia and Herzegovina"
# PLoS One

# Script for Fig 4 and Fig 5.

# 2015-11-13, updated 2016-09-26
# (c) Annerose Nisser


# -------------- # 
# Empty the working directory:
rm(list = ls())

# This displays Bosnian characters in the console correctly:
Sys.setlocale("LC_CTYPE", "UTF-8")

# Import the data:
municipalities <- read.csv("data/municipalities.csv")
sarajevo <- read.csv("data/sarajevo.csv") 
regions <- read.csv("data/regions.csv") 

names(municipalities)
class(municipalities$majorityshare)
#------------------------------------------#
# How many municipalities have a majority ethnic click share below 33%? 
sum(municipalities[(municipalities$clicks)>=30,]$majorityclickshare<0.33)

# What's the percentage of those municipalities? 
sum(municipalities[(municipalities$clicks)>=30,]$majorityclickshare<0.33)/length(municipalities[(municipalities$clicks)>=30,]$majorityclickshare)

#------------------------------------------#
# Add majorityclicksharedeviation variable, 
# order by majorityclicksharedeviation: 

mcsd_order <- function(df) {
  # add variable: 
  df[, "majorityclicksharedeviation"] <- 
    round((df[, "majorityclickshare"]-0.3333333)*100, 3)
  # order by majorityclicksharedeviation: 
  df <- df[with(df, order(-majorityclicksharedeviation)), ]
  
  return(df)
}

regions <- mcsd_order(regions)
municipalities <- mcsd_order(municipalities)
sarajevo <- mcsd_order(sarajevo)

# Restrict the municipalities that are included in the analysis: 
# Take only municipalities with at least 30 clicks
municipalities <- municipalities[(municipalities$clicks)>=30,]

# Restrict regions to RS and Federation (don't include Brcko District in regional analysis): 
regions <- regions[regions$ID_1!=1, ]
#------------------------------------------#
# Fig 4 ----

# Function for creating the entire plot: 
plot.f <- function(){
  # create an empty plot which has the correct coordinates: 
  reg <- regions$majorityclicksharedeviation
  mun <- municipalities$majorityclicksharedeviation
  sar <- sarajevo$majorityclicksharedeviation
  
  maxy <- max(max(sar, na.rm=T), max(reg, na.rm=T), max(mun, na.rm=T))
  miny <- min(min(sar, na.rm=T), min(reg, na.rm=T), min(mun, na.rm=T))
  
  yrange <- round(max(maxy, abs(miny)) + 4, 0)
  
  xrange <- nrow(regions) + 3 + nrow(municipalities) + 3 + nrow(sarajevo) + 1
  
  plot(1, type="n", axes=F, xlab="", ylab="", 
       ylim = range(-yrange, yrange), xlim=range(0, xrange))
  
  #------------#
  # create the lines at the 33-line: 
  lines(c(0, length(reg) + 1), c(0, 0)) # horizontal line at zero - regions
  lines(c(length(reg) + 3, length(reg) + 3 + length(mun) + 1), c(0, 0)) # horizontal line at zero - municipalities
  lines(c(length(reg) + 3 + length(mun) + 3, xrange), c(0, 0)) # horizontal line at zero - sarajevo
  
  #------------#
  # add bars for the three different levels of analysis, 
  # color dependent on the majority of a given entity: 
  
  # for regional level: 
  for (i in 1:length(reg)){
    if(i==1) {col <- "#1f78b4"} # "Bosniak" color
    else {col <- "#b2df8a"} # "Serb" color
    lines(c(i, i), c(reg[i], -0.1), lwd = 4, col = col) 
    }
  
  # define a variable on whether click share is below
  # or above 33%: 
  above <- ifelse(mun > 0, TRUE, FALSE)
  y2 <- ifelse(above, 0.1, -0.2)
  
  # for municipality level: 
  for (i in 1:length(mun)) {
    if(municipalities$majority[i]=="bs") {col <- "#1f78b4"} # "Bosniak" color
    if(municipalities$majority[i]=="sr") {col <- "#b2df8a"} # "Serb" color
    if(municipalities$majority[i]=="hr") {col <- "#a6cee3"} # "Croat" color
    lines(c(i + (length(reg) + 3), i + (length(reg) + 3)), 
            c(mun[i], y2[i]), lwd = 5,
            col = col)
    }
  
  # define a variable on whether click share is below or above 33%: 
  above <- ifelse(sar >= 0, TRUE, FALSE)
  y2 <- ifelse(above, 0.1, -0.2)
  
  # for city district level: 
  for (i in 1:length(sar)){
    if(sarajevo$ID_3[i]==120) {col <- "#b2df8a"} # "Serb" color
    else {col <- "#1f78b4"} # Bosniak color
      lines(c(i+(length(reg) + 3 + length(mun) + 3),
              i+(length(reg) + 3 + length(mun) + 3)), 
            c(sar[i], y2[i]), lwd=5, 
            col = col)
      }
  
  #------------#
  # add one axis at the very left: 
  axis(2, at = c(-15, -10, -5, 0, 5, 10, 15), 
       label = c(18, 23, 28, 33, 38, 43, 48),
       cex.axis = 0.9)
  
  #------------#
  # add x-axis labels for the different regions: 
  text(2, -16, "Federal\nLevel", cex = 0.9)
  text(nrow(municipalities)/2 + nrow(regions) + 3, -16, "Municipality Level", cex = 0.9)
  text(1 + nrow(regions) + 3 + nrow(municipalities) + 3 + nrow(sarajevo)/2, -16, 
       "Neighborhood\nLevel\n(Sarajevo)", cex = 0.9)
  
  #------------#
  # adding label texts for each entity: 
  # at the regional level: 
  names_region <- c("Federation", "RS")
  for (i in 1:length(reg)){
    text(i, reg[i] - 1, names_region[i],
         srt = 45, cex = .75, adj = 1) 
  }
  
  # at the municipality level: 
  names_municipalities <- as.character(municipalities$name)
  names_municipalities[2] <- "Gornji Vakuf"
  names_municipalities[45] <- "Brcko Distrikt FBiH"
  names_municipalities[46] <- "Brcko Distrikt RS"
  
  
  above0 <- 1 
  while (mun[above0] > 0){
    above0 <- above0 + 1
  }
  above0 <- above0 - 1
  
  for (i in 1:above0){
    text(i + (length(reg) + 3), 
         mun[i] + 1, names_municipalities[i],
         srt = 45, cex = .7, adj = 0)
  }
  
  for (i in (above0+1):length(mun)){
    text(i + (length(reg) + 3), 
         mun[i]-1, names_municipalities[i],
         srt=45, cex=.7, adj=1)
  }
  
  # at the city level: 
  above0 <- 1 
  while (sar[above0] > 0){
    above0 = above0 + 1
  }
  above0 <- above0 - 1
  
  names_sarajevo <- as.character(sarajevo$name)
  names_sarajevo[5] <- 'East New Sarajevo'
  
  for (i in 1:above0){
    text(i + nrow(regions) + 3 + nrow(municipalities) + 3, sar[i] + 1,
         names_sarajevo[i],
         srt = 45, cex = .7, adj = 0)
  }
  
  
  for (i in above0+1:length(sar)){
    text(i + nrow(regions) + 3 + nrow(municipalities) + 3, 
         sar[i] - 1, names_sarajevo[i],
         srt = 45, cex = .7, adj = 1)
  }
  
  #------------#
  # add y-axis text to explain what is plotted
  mtext("Majority Ethnic Click Share (%)",  WEST <-2, at = 0,
        line = 2.5, cex = .9, srt = 90)

  #------------#
  # add legend for the ethnic majority (colors):
  legend(50, 15, title = "Ethnic majority", 
         c("Bosniak", "Serb", "Croat"),
         lwd = 5, col = c("#1f78b4","#b2df8a", "#a6cee3"),
         bty = "n", cex = .75)
  
}

# set the margins: 
par(oma=c(2,2,1,2), mar=c(2,3,1,0)) # bottom, left, top, right

plot.f()

# Export via the device (choose "Export" -> "Save as Pdf" -> "15*6 inches" -> "Use cairo_pdf device")
# Without choosing cairo_pdf device, the Bosnian characters won't be displayed correctly.

# ------------------------------------ # 
# ------------------------------------ # 
# Fig 5 ---- 
# Getting the spatial data: 
library(sp)
# GADM adm3 level:
gadm <- readRDS("data/shapefiles/BIH_adm3.rds")
# GADM adm1 level: 
gadm_1 <- readRDS("data/shapefiles/BIH_adm1.rds")

lonlat_towns <- read.csv("data/lonlat_towns.csv", sep=",",
                         header=TRUE, stringsAsFactors = FALSE)

par(mar = c(2, 1, 2, 2)) # bottom, left, top, right

# merge the municipalities (census) data with the gadm data:
gadm@data <- merge(gadm@data, municipalities, by = "ID_3", 
                   all.x = TRUE)

names(gadm@data)

par(mar = c(2, 2, 2, 2)) 

# Make a function to plot the majority clickshare deviation
plotdev <- function(mcsd) {
  # Make categorical out of frequency variable:
  frequencies <- data.frame(orig = mcsd, # original values
                            trans = factor(
    ifelse(mcsd<(-10), "<-10",
           ifelse(mcsd<(-5), "-10-(-5)",
                  ifelse(mcsd<0, "-5-0", 
                         ifelse(mcsd<5, "0-5",
                                ifelse(mcsd<10, "5-10",
                                              ifelse(mcsd>10, ">10", NA)))))),
     levels = c("<-10", "-10-(-5)", 
                "-5-0", "0-5", "5-10", ">10")))
  
  grays <- rev(c("#2166ac", "#3e8cda", "#94bfeb", # blue scale
       "#f08f9a", "#e74b5e", "#b2182b")) # red scale

  # Create a colors vector for plotting the municipalities:
  colors <- vector("character", length = length(frequencies$trans))
  for (i in seq_along(frequencies$trans)){
    col <- grays[which(levels(frequencies$trans)==frequencies$trans[i])]
    colors[i] <- if(length(col)>0) col else NA
  }
    
  par(mar = c(1, 0.2, 1, 1)) # bottom, left, top, right
  plot(gadm, col = colors, border = "grey")
  plot(gadm_1, add = T, col = adjustcolor("white", alpha=0), border = "red")

  levels <- c("below -10%", "-10% to -5%", 
              "-5% to 0%", "0% to 5%", "5% to 10%", "above 10%")

  legend("bottomleft", inset=c(0.02, 0.05), levels, # pch = 15, 
         fill = grays, title = "Majority click share deviation", # add here for example the title
         cex = 0.8, 
         bty = "n" # ,  title.adj = -5
  )
  
  indices <- which(lonlat_towns$town=="Sarajevo" | lonlat_towns$town=="Banja Luka" | 
                     lonlat_towns$town=="Tuzla" | lonlat_towns$town=="Zenica" | 
                     lonlat_towns$town=="Mostar" | lonlat_towns$town=="Bihac")
  
 for (i in seq_along(indices)) {
    points(lonlat_towns[indices[i], c("lon", "lat")], 
           pch = 20, col = "#001a33", cex = 1.4)
    text(x = lonlat_towns[indices[i], "lon"],
         y = lonlat_towns[indices[i], "lat"], 
         labels = lonlat_towns[indices[i], "town"], 
         pos = 4, # position text right from the dots
         font = 2, # for bold font
         col = "#001a33"
    )
  }
}

par(mar = c(1, 3, 1, 1)) # bottom, left, top, right
plotdev(gadm@data$majorityclicksharedeviation)
# Save with (width = 7, height = 6)