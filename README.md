#SENTIR
##(S)imple lin(E)ar regressio(N) and (T)emporal-dynam(I)c fi(R)e model
Authors:           
David Franca - dvdgmf@gmail.com
Sacha Sianni - sacha@ortiz.com.br
Liana Anderson - liana.anderson@gmail.com

latest updated in: 21 Sep 2017                                       

The current script is divided in SIX major sections, just navigate the code using CTRL+F to search for each section.

##:SEC01-GRID:
Section ONE address the loading of a spatial grid that will hold the minimum unit of analysis.                 
                                                   
##:SEC02-DATA:
Section TWO loads the fire product from NASA. [Detailed info about this product can be found here.](https://earthdata.nasa.gov/earth-observation-data/near-real-time/firms)
                                            
##:SEC03-A-F(X):
Section TREE-A is the definition of the model function.                          

##:SEC03-B-RUN:
Section TREE-B is a sample call of the model function.                         

##:SEC04-VLD:
Section FOUR concerns the validation of the model output.                      

##:SEC05-PLOT:
Section FIVE holds the scripts for ploting the model data.                      

##[R] LIBRARIES: Required packages and libs 

library(rgdal)    # readOGR
library(dplyr)    # summarise
library(raster)   # raster
library(ggplot2)  # plot
library(maptools) # over
