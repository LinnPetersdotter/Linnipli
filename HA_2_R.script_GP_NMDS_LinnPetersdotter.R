##########################################################
######## ASSIGNMENT II - Multidimensional Scaling ########
##########################################################


# setting working directory, loading packages and opengraph function 

setwd("~/Documents/Lund Uni/StatisticsPSYP13") 

library(lsr)
library(psych) 
library(MASS)
library(smacof)
library(ggplot2)

openGraph = function( width=7 , height=7 , mag=1.0 , ... ) {
  # if( class( try(RStudioGD(),silent=TRUE) ) == "try-error" ) {
  # Not in RStudio, use graphic windows
  if ( .Platform$OS.type != "windows" ) { # Mac OS, Linux
    if ( .Platform$GUI != "AQUA" ) { # Linux
      tryInfo = try( X11( width=width*mag , height=height*mag , 
                          type="cairo" , ... ) )
      if ( class(tryInfo)=="try-error" ) {
        lineInput = readline("WARNING: Previous graphics windows will be closed because of too many open windows.\nTO CONTINUE, PRESS <ENTER> IN R CONSOLE.\n")
        graphics.off() 
        X11( width=width*mag , height=height*mag , type="cairo" , ... )
      }
    } else {
      # Mac OS - use quartz device
      tryInfo = try( quartz(width=width*mag , height=height*mag ,
                            ... ) )
      if ( class(tryInfo)=="try-error") {
        if ( class(tryInfo)=="try-error" ) {
          lineInput = readline("WARNING: Previous graphics windows will be closed because of too many open windows.\nTO CONTINUE, PRESS <ENTER> IN R CONSOLE.\n")
          graphics.off() 
          quartz( width=width*mag , height=height*mag , ... )
        } 
      }
    }
  } else { # Windows OS
    tryInfo = try( windows( width=width*mag , height=height*mag , ... ) )
    if ( class(tryInfo)=="try-error" ) {
      lineInput = readline("WARNING: Previous graphics windows will be closed because of too many open windows.\nTO CONTINUE, PRESS <ENTER> IN R CONSOLE.\n")
      graphics.off() 
      windows( width=width*mag , height=height*mag , ... )    
    }
  }
  # }
}

openGraph()

# import data Nations.txt 
Nations <- read.delim("Nations.txt")

##########################################################
##################### applying nMDS ######################
##########################################################


# looking for unusual or missing data
describe(Nations)
summary(Nations)

# non metric scale --> non metric multi dimensional scaling

#creating distances and converting similarities to dissimilarities, so that larger distances actually represent dissimilarity between the countries 
Nations.d <-sim2diss(Nations, method = 7, to.dist=T)

# performing nMDS on the distance matrix
Nations.mds <- isoMDS(Nations.d)

# creating a stress decomposition chart
res <- smacofSym(Nations.d)
x11()
plot(res, plot.type = "stressplot", ylim = c(2,15))

# creating a bubbleplot
x11()
plot(res, plot.type = "bubbleplot")

# defining the two dimensions for visualizing the distance matrix
x <- Nations.mds$points[,1]

y <- Nations.mds$points[,2]

print(x)
print(y)

# Two-dimensional solution from non-metric multidimensional scaling of distance matrix of nations
openGraph()
plot(x, y, xlab = "Coordinate1", ylab = "Coordinate2", font.axis=2, font.lab=2, cex.lab = 1.2, xlim = range(Nations.mds$points[,1])*1.2, type = "p", pch = 19, las=1)
text(x,y, labels = colnames(Nations), col = "deepskyblue4", cex=1.5) 
abline(h=0, v=0, col = "gray60", lty=2)
abline(a=0, b=1, col = "gray80", lty=3)
abline(a=0, b=-1, col = "gray80", lty=3)

# creating the coordinates for the nations on the two dimensions
Nations_sh <- Shepard(Nations[lower.tri(Nations)], Nations.mds$points)
Nations.mds$points

# visualizing the NMDS with a Shepard Plot, displaying the dissimilarities between the two dimensional solution and the original dissimilarities
Nation_shep <- Shepard(Nations.d, Nations.mds$points)

openGraph()
plot(Nation_shep, pch = "°", xlab = "Dissimilarity",
     ylab = "Distance", col= "darkgreen", main = "Shephard's Diagram")
lines(Nation_shep$x, Nation_shep$yf, type = "S")

########################################################

# In case we would want a better scaling solution, a three dimensiponal solution could improve our stress value
    
Nations.mds <- isoMDS(Nations.d, k = 3)

######################## but as I am supposed to follow Everitt and Hothorn, I stick with 2 components for the interpretation 
