# Replication file for 
# Nisser, A. & Weidmann, N.B. (2017)
# "Measuring Ethnic Preferences with Mobile Advertising
# in Bosnia and Herzegovina"
# PLoS One

# Script for Fig 3.

# 2015-12-14, updated 2016-09-20
# (c) Annerose Nisser


# -------------- # 
# Empty the working directory:
rm(list = ls())

# Load required packages: 
require(stringr)

# Display Bosnian characters in the console correctly:
Sys.setlocale("LC_CTYPE", "UTF-8")

# Import the data:
temporal <- read.csv("data/temporal.csv", stringsAsFactors = F)
spatial <- read.csv("data/spatial.csv", stringsAsFactors = F)

names(temporal)
names(spatial)
# -------------- # 
# Temporal distribution: 
temporal <- temporal[temporal$round=="second", ] # take only data for second round
temporal$day <- str_sub(as.character(temporal$date), 1, 10) # adjust date format
temp.cleaned  <- subset(temporal, bot==0)
temp.bot <- subset(temporal, bot==1)


dayclickratecleaned <- as.vector(by(temp.cleaned, temp.cleaned$day,
                                    function(x) sum(x$clicked==1)/nrow(x)))*100

dayclickratebot <- as.vector(by(temp.bot, temp.bot$day,
                                function(x) sum(x$clicked==1)/nrow(x)))*100

# -------------- # 
# Spatial distribution: 
# Calculate turnout, bot clickrate and cleaned clickrate per municipality:
spatial$turnout <- spatial$validtotal/spatial$total
spatial$bot.clickrate <- spatial$bot.clicks/spatial$bot.impressions
spatial$cleaned.clickrate <- spatial$cleaned.clicks/spatial$cleaned.impressions

# Regressions for clickrate on banner displays (for bot and cleaned displays):
spat.cleaned <- subset(spatial, spatial$cleaned.impressions>=500)
summary(lm(spat.cleaned$cleaned.clickrate~spat.cleaned $turnout))

spat.bot <- subset(spatial, spatial$bot.impressions>=500)
summary(lm(spat.bot$bot.clickrate~spat.bot$turnout))


# -------------- # 
# Function for plotting the two panels of Fig 3:
plot.f <- function(){
  
  # first panel of the plot with the temporal distribution
  plot(dayclickratecleaned, type="l",
       ylab="Click Rate (%)", xlab="Date", xaxt="n", lty=1, lwd=3, 
       ylim=range(c(dayclickratebot,dayclickratecleaned)), col="blue")
  
  par(new=T)
  
  plot(dayclickratebot, type="l", 
       ylab="", xlab="", xaxt="n", yaxt="n", lty=2, lwd=3, 
       ylim=range(c(dayclickratebot,dayclickratecleaned)), col="red")
  
  axis(1, at=1:7, labels=c("10/7", "10/8", "10/9", 
                           "10/10", "10/11", "10/12", "10/13")) 
  lines(c(6, 6), range(c(dayclickratebot-0.2,dayclickratecleaned+0.2)), lwd=3)
  text(5.85, 0.55, labels="election day", srt=90 #, font=2
  )
  legend(2.5, 0.7, c("cleaned data", "bot data"),  
         lwd=3, bty = "n", cex=.9, y.intersp=1.5,
         col=c("blue", "red"), lty=c(1,2))
  
  title(main="Temporal Distribution", cex.main=1.1)
  
  #------------#
  # second panel of the plot with the spatial distribution
  par(mar=c(5,4,3,2))
  # spatial <- subset(spatial, spatial$turnout<0.8)
  
  xmin <- min(min(spat.bot$turnout, na.rm = T), min(spat.cleaned$turnout, na.rm = T))
  xmax <- max(max(spat.bot$turnout, na.rm = T), max(spat.cleaned$turnout, na.rm = T))
  
  ymin <- min(min(spat.bot$bot.clickrate*100, na.rm = T), min(spat.cleaned$cleaned.clickrate*100, na.rm = T))
  ymax <- max(max(spat.bot$bot.clickrate*100, na.rm = T), max(spat.cleaned$cleaned.clickrate*100, na.rm = T))
  
  plot(spat.cleaned$turnout, spat.cleaned$cleaned.clickrate*100,  pch=16, xlab="Turnout per Municipality", 
       ylab="Click Rate (%)", xlim=c(xmin,xmax), ylim=c(ymin,ymax),
       col="blue")
  
  abline(lm(spat.cleaned$cleaned.clickrate*100~spat.cleaned$turnout), lty=1, lwd=3, col="blue")
  title(main="Spatial Distribution", cex.main=1.1)
  
  par(new=T)
  plot(spat.bot$turnout, spat.bot$bot.clickrate*100,  pch=4, ylab="", xlab="", 
       xaxt="n", yaxt="n", xlim=c(xmin,xmax), ylim=c(ymin,ymax), 
       lwd=2, col="red")
  abline(lm(spat.bot$bot.clickrate*100~spat.bot$turnout), lty=2, lwd=3, col="red")
  
  legend(0.2, 2.4, c("cleaned data", "bot data"), lty=c(1,2), 
         lwd=c(1,2), bty = "n", cex=.9, pch=c(16,4), y.intersp=1.5, col=c("blue", "red"))
  
}



par(mfrow=c(1,2), mar=c(5,5,3,2))
plot.f()
# Save with (width = 14.56, height = 6)