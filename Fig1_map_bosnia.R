# Replication file for 
# Nisser, A. & Weidmann, N.B. (2016)
# "Measuring Ethnic Preferences with Mobile Advertising
# in Bosnia and Herzegovina"
# PLoS One

# Script for Fig 1.

# 2016-09-20
# (c) Annerose Nisser

#------------#
# Empty the working directory:
rm(list = ls())

# Load required packages: 
require(rgdal) # (version: 1.2-4)

# Display Bosnian characters in the console correctly:
Sys.setlocale("LC_CTYPE", "UTF-8")


# Import the data:
municipalities <- read.csv("data/municipalities.csv") 
lonlat_towns <- read.csv("data/lonlat_towns.csv", sep=",",
                         header=TRUE, stringsAsFactors = FALSE)

#------------#
adm3 <- readOGR(dsn = "data/shapefiles", layer = "BIH_adm3") #
adm1 <- readOGR(dsn = "data/shapefiles", layer = "BIH_adm1") #

# merge the municipalities (census) data with the adm3 data:
adm3@data <- merge(adm3@data, municipalities, by = "ID_3", 
                   all.x = TRUE)

# -------------- # 
# For plotting functions of maps, see especially
# https://www.students.ncl.ac.uk/keith.newman/r/maps-in-r-using-gadm

# Make a map that displays the Federation of Bosnia and Herzegovina, 
# the Republika Srpska and the Brcko District. 
plotmap <- function() {
  plot(adm1, density=c(0,10,10), angle=c(0,45,90), border = "red")
  levels <- c("Brcko District", "Federation of Bosnia and \nHerzegovina",
              "Republika Srpska")
  legend("bottomleft", inset=c(0.02, 0.05), levels,  # pch = 15, 
         density=c(0,20,25), angle=c(0,45,90), # add here for example the title
         pt.cex = 1.5, cex = 0.8, # ,  title.adj = -5
         bty = "n", y.intersp=1.5)
  
  # Add some point for the five biggest cities of Bosnia
  # (Sarajevo, Tuzla, Banja Luka, Zenica, Mostar, Bihac)
  
  indices <- which(lonlat_towns$town=="Sarajevo" | lonlat_towns$town=="Banja Luka" | 
                     lonlat_towns$town=="Tuzla" | lonlat_towns$town=="Zenica" | 
                     lonlat_towns$town=="Mostar" | lonlat_towns$town=="Bihac")
  
  # How to get the right color: 
  # http://stackoverflow.com/questions/14099833/transparency-with-polygon-command
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

#------------#

# Make a function that plots the distributions of the different ethnic groups: 
plotdist <- function(ethnic.prop, bluelayout = T,
                     ...) {
  # Create categorical variable from continuous variable,
  # make five categories: 
  # (1) > 80% ; in black
  # (2) 60 - 80 ; in grey
  # (3) 40 - 60 ; in grey
  # (4) 20 - 40 ; in grey
  # (5) < 20 ; in white
  frequencies <- data.frame(orig = ethnic.prop, # original values
                            trans = factor(ifelse(ethnic.prop<0.2, "<20",
                                                  ifelse(ethnic.prop<=0.40, "20-40",
                                                         ifelse(ethnic.prop<=0.60, "40-60",
                                                                ifelse(ethnic.prop<=0.80, "60-80", ">80")))),
                                           levels = c("<20", "20-40", "40-60", "60-80", ">80")))
  # transform frequencies into factor levels. 
  
  if (bluelayout==T) {
    grays <- rev(c("#ffffff", "#C6DBEF", "#6BAED6", "#2171B5","#001a33")) # blue scale
  }
  if (bluelayout==F) {
    grays <- gray.colors(length(levels(frequencies$trans)), start = 0, end = 1) # grey scale
  }
  
  # Create a color vector for plotting the municipalities:
  colors <- ifelse(frequencies$trans==levels(frequencies$trans)[1], grays[5], 
                   ifelse(frequencies$trans==levels(frequencies$trans)[2], grays[4],
                          ifelse(frequencies$trans==levels(frequencies$trans)[3], grays[3],
                                 ifelse(frequencies$trans==levels(frequencies$trans)[4], grays[2], grays[1]))))

  
  levels <- c("> 80%", "60-80%", "40-60%", "20-40%", "< 20%")
  if (bluelayout==T) {col <- c("#ffffff", "#C6DBEF", "#6BAED6", "#2171B5","#001a33")} # blue scale
  if (bluelayout==F) {col <- c("#FFFFFF", "#E0E0E0", "#BABABA", "#888888","#000000")} # grey scale
  
  plot(adm3, col = colors, border = "grey")
  plot(adm1, add = T, col = adjustcolor("white", alpha=0), border = "red")
  
  legend("bottomleft", inset=c(0.03, 0.05), levels, # pch = 15, 
         fill = rev(col), ... , # add here for example the title
         cex = 0.8, 
         bty = "n" # ,  title.adj = -5
  )
}

l <- function(legendtitle) {
  legend("topleft", legendtitle, cex = 1.5, text.font = 2, bty = "n", # no legend borders
         inset=c(-0.06, 0)) }


par(mar = c(0.5, 0, 0.5, 0), 
    oma = c(0, 0, 0, 0)) # bottom, right, top, left

layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))

plotmap()
l("A")


plotdist(ethnic.prop = adm3@data$serbs.prop, 
         title = "Proportion of Serbs")
l("B")

plotdist(ethnic.prop = adm3@data$bosniaks.prop, 
         title = "Proportion of Bosniaks")
l("C")

plotdist(ethnic.prop = adm3@data$croats.prop, 
         title = "Proportion of Croats")
l("D")

# Save with (width = 8, height = 8)