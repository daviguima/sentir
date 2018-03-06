## SENTIR
### (S)imple lin(E)ar regressio(N) and (T)emporal-dynam(I)c fi(R)e model

Authors:<br/>
> David Franca - dvdgmf@gmail.com<br/>
> Sacha Sianni - sacha@ortiz.com.br<br/>
> Liana Anderson - liana.anderson@gmail.com<br/>
> latest updated in: 21 Sep 2017<br/>                       

The current script is divided in SIX major sections, just navigate the code using CTRL+F to search for each section.<br/>
<br/>
### :SEC01-GRID:
Section ONE address the loading of a spatial grid that will hold the minimum unit of analysis.<br/>

### :SEC02-DATA:
Section TWO loads the fire product from NASA. [Detailed info about this product can be found here.](https://earthdata.nasa.gov/earth-observation-data/near-real-time/firms)<br/>

### :SEC03-A-F(X):
Section TREE-A is the definition of the model function.<br/>
<br/>
### :SEC03-B-RUN:
Section TREE-B is a sample call of the model function.<br/>
<br/>
### :SEC04-VLD:
Section FOUR concerns the validation of the model output.<br/>
<br/>
### :SEC05-PLOT:
Section FIVE holds the scripts for ploting the model data.<br/>
<br/>
### LIBRARIES: Required packages and libs

> library(rgdal)    &#35; readOGR<br/>
> library(dplyr)    &#35; summarise<br/>
> library(raster)   &#35; raster<br/>
> library(ggplot2)  &#35; plot<br/>
> library(maptools) &#35; over<br/>
