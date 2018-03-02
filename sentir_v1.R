#__________________________________________________________________________________
# SENTIR: (S)imple lin(E)ar regressio(N) and (T)emporal-dynam(I)c fi(R)e model ----
#__________________________________________________________________________________
# Authors:                                                                         
# ___ David Franca - dvdgmf@gmail.com                                              
# ___ Sacha Sianni - sacha@ortiz.com.br                                            
# ___ Liana Anderson - liana.anderson@gmail.com                                    
# latest updated in: 21 Sep 2017                                                  
#                                                                                  
# The current script is divided in SIX major sections:                             
# DESCRIPTION ----                                                                                 
#   :SEC01-GRID:
#   Section ONE address the loading                                                
#   of a spatial grid that will hold the minimum unit of analysis.                 
#                                                                                  
#   :SEC02-DATA:
#   Section TWO loads the fire product from NASA.
#   Detailed info about this product can be found at:
#   https://earthdata.nasa.gov/earth-observation-data/near-real-time/firms
#                                                                                  
#   :SEC03-A-F(X):
#   Section TREE-A is the definition of the model function.                          
#                                                                                  
#   :SEC03-B-RUN:
#   Section TREE-B is a sample call of the model function.                         
#                                                                                    
#   :SEC04-VLD:
#   Section FOUR concerns the validation of the model output.                      
#                                                                                  
#   :SEC05-PLOT:
#   Section FIVE holds the scripts for ploting the model data.                      
#                                                                                  
#____________________________
# LIBRARIES ----
# Required packages and libs 
#____________________________
library(rgdal)    # readOGR
library(dplyr)    # summarise
library(raster)   # raster
library(ggplot2)  # plot
library(maptools) # over

#__________________________
# :SEC01-GRID: ----
# Loading the celular grid
#__________________________

# Selecting the working directory in wich the grid is
#setwd("/media/francadgm/WDTREES/Projeto_Acre/") # LINUX
setwd("G:/gridv2/") # WIN
# Loading the shp file into a var
acrebox = readOGR("output_modelo_reproj_2003-2016.shp","output_modelo_reproj_2003-2016")
# CLEAR THE SHAPEFILE DATA SAVING ONLY THE ID COLUMN
keeps = ("ID")
acrebox@data = acrebox@data[,(names(acrebox@data) %in% keeps),drop=F]

#_________________________________________
# :SEC02A-DATA:FIRMS ----
# Loading input fire data from FIRMS
#_________________________________________

# Selecting the working directory in wich the fire data is
setwd("G:/Projeto_Acre/FIRMS_ANNUAL_SHAPES_AQUA_C61_clip_AMZ/") #LINUX
#setwd("G:/NASA_FIRMS_2000-2017/crop_acre_by_year/") #WIN

# grab all filenames that has ".shp" in the name label and put them in a list
listafocos = dir(getwd(),"*.shp")

# remove everything from the list that has "xml" in it
listafocos = listafocos[-grep("xml",listafocos)]

# regular expression (regex) to remove the ".shp" extension from the filenames
listafocos = sub("\\.shp$","",listafocos)

# The next step will turn the string list into an actual shapefiles list

x=1 # create a simple iterator since the next loop is based on the filenames instead of numbers
listafocoshp = list() # declares an empty list
#___PROGRESSBAR___
total = length(listafocos) # progress bar max size
pb = txtProgressBar(min = 0, max = total, style = 3) #create PB
#_________________
system.time({ # return execution time at the end of the process
  for(i in listafocos){ # LOOP THROUGH EVERY FILENAME IN THE LIST
    #___PROGRESSBAR___
    setTxtProgressBar(pb, x) # update PB
    #_________________
    listafocoshp[[x]] = readOGR(".",i) # READ ALL SHAPEFILES IN THE LIST
    listafocoshp[[x]] = spTransform(listafocoshp[[x]], CRS(proj4string(acrebox))) # REPROJECT THE POINT DATA BY ANOTHER PROJECTED SHAPEFILE
    listafocoshp[[x]] = listafocoshp[[x]][acrebox,] # CUT THE SHAPEFILE TO FIT THE ACRE STATE
    listafocoshp[[x]]@data$x = 1 # CREATE A COLUMN OF 1's TO BE USED IN THE SUM
    x=x+1 # add 1 to the iterator counter
  }
})

# (OPTIONAL)
# CUT 2016 + NOWDAYS FIRE DATA FROM FIRMS
setwd("G:/Projeto_Acre/FIRMS_FOCOS_2016/DL_FIRE_M6_9153/")
#setwd("/media/francadgm/WDTREES/Projeto_Acre/FIRMS_FOCOS_2016/DL_FIRE_M6_9153/")
focos2016 = readOGR("fire_archive_M6_9153.shp","fire_archive_M6_9153")
focos2016 = spTransform(focos2016, CRS(proj4string(acrebox))) # REPROJECT THE POINT DATA BY ANOTHER PROJECTED SHAPEFILE
focos2016 = focos2016[acrebox,] # CUT THE SHAPEFILE TO FIT THE ACRE STATE
focos2016@data$x = 1 # CREATE A COLUMN OF 1's TO USE THEM IN A SUM

# SUM 2016 FOCUS POINTS OVER ACRE CELL GRID
#acrebox@data$fc2016 = over(acrebox, focos2016[,"x"], fn = sum)
# TURN NA TO ZERO
#acrebox@data$fc2016[is.na(acrebox@data$fc2016)] = 0
# TURN DATA.FRAME -> DATA.MATRIX
#acrebox@data$fc2016 = data.matrix(acrebox@data$fc2016)
# TURN DATA.MATRIX -> INTEGER
#acrebox@data$fc2016 = as.integer(acrebox@data$fc2016)
# SAVE 2016 FIRE GRID SHAPE #
#setwd("/home/trees/Projeto_Acre/OUTPUT/outputv2/")
#writeOGR(acrebox, "focos2016.shp", layer = "focos2016", driver="ESRI Shapefile", overwrite_layer=TRUE)

# ADD 2016 + NOWDAYS FIRE DATA TO listafocoshp[[x]]
listafocoshp[[14]] = focos2016
#_________________________________________________
# :SEC02B-DATA:CPTEC ----
# Loading input fire data from CPTEC BD_QUEIMADAS
#_________________________________________________

# WIN
setwd("G:/Projeto_Acre/CPTEC_FIRE_DATA/") # All Sensors
setwd("G:/Projeto_Acre/CPTEC_FIRE_AQUA-TM/") # MODIS-AQUA


listafocos = dir(getwd(),"*.shp")
# regular expression (regex) to remove the ".shp" extension from the filenames
listafocos = sub("\\.shp$","",listafocos)

x = 1; listafocoshp = list()
#___PROGRESSBAR___
total = length(listafocos) # progress bar max size
pb = txtProgressBar(min = 0, max = total, style = 3) #create PB
#_________________
system.time({ # return execution time at the end of the process
  for(i in listafocos){ # LOOP THROUGH EVERY FILENAME IN THE LIST
    #___PROGRESSBAR___
    setTxtProgressBar(pb, x) # update PB
    #_________________
    listafocoshp[[x]] = readOGR(".",i) # READ ALL SHAPEFILES IN THE LIST
    listafocoshp[[x]] = spTransform(listafocoshp[[x]], CRS(proj4string(acrebox))) # REPROJECT THE POINT DATA BY ANOTHER PROJECTED SHAPEFILE
    listafocoshp[[x]] = listafocoshp[[x]][acrebox,] # CUT THE SHAPEFILE TO FIT THE ACRE STATE
    listafocoshp[[x]]@data$x = 1 # CREATE A COLUMN OF 1's TO BE USED IN THE SUM
    x=x+1 # add 1 to the iterator counter
  }
}) # IN CASE OF ERROR VARIABLE "acrebox" NOT BEING FOUND, SEE SECTION :SEC01-GRID: ON HOW TO READ IT.

#________________________________
# :SEC03-A-F(X): ----
# Defining the SENTIR() function 
#________________________________

sentir = function(fires, grid, interval, mtly = F, mnth.pos, validate = F, vldata, cptec = F){
  
  # fires = list of several SpatialPointsDataFrame's containing fires data grouped by year in each position of the list (sort by smaller year to bigger. i.e. 2003,2004,2005...)
  
  # grid = shapefile containing the spatial grid in wich the fires data should be aggregated
  
  # interval = data-vector interval i.e. 2010:2015
  
  # mtly = boolean values, TRUE for monthly accumulated data, FALSE for yearly accumulated.
  
  # (IF mtly = TRUE) mnth.pos = 1 for JAN, 2 for FEB, 3:MAR and so forth...
  
  # vldata = data to be used during validation process (optional).
  
  ### BEGIN
  
  # CLEAR THE gridFILE DATA SAVING ONLY THE ID COLUMN IN ORDER TO FUTURE STORAGE OF THE NEW FIRE DATA
  keeps = "ID"
  grid@data = grid@data[,(names(grid@data) %in% keeps),drop=F]
  
  # CREATE COLUMNS WITH THE SUM OF ALL FIRE OCCURENCIES IN A GIVEN CELL OF A GIVEN YEAR
  ano = c()
  ### PROGRESSBAR ###
  total = length(fires) # progress bar max size
  pb = txtProgressBar(min = 0, max = total, style = 3) #create PB
  acumula.mes = list()
  
  for(i in seq_along(fires)){
    
    # Quick-fix for fire data acquired from INPE's BD-Queimadas.
    # INPE:CPTEC----
    if(cptec){
      names(fires[[i]]@data)[1] = "ACQ_DATE"
    }
    
    ponto.focos = fires[[i]]
    ponto.focos$x = 1
    
    # CONVERT CLASS FROM FACTOR TO DATE
    
    # :SEC-ERROR:----
    # WARNING! This section can cause unexpected erros deppending on your Operational System and language settings
    # In order to decide wich one is the right format for you
    # check the format of your fire data in the column that concerns de DATE
    # in which the respective fire data was acquired!
    
    # NASA:FIRMS----
    if(length(grep("/",fires[[1]]@data$ACQ_DATE,value = T))!=0){
      ponto.focos@data$ACQ_DATE = as.Date(ponto.focos@data$ACQ_DATE, format = "%Y/%m/%d") 
    } else {
      ponto.focos@data$ACQ_DATE = as.Date(ponto.focos@data$ACQ_DATE, format = "%Y-%m-%d")
    }
    # USE THIS ONE FOR ACQ_DATE similar to: 2003/01/02
    #ponto.focos@data$ACQ_DATE = as.Date(ponto.focos@data$ACQ_DATE, format = "%Y/%m/%d") 
    
    # USE THIS ONE FOR ACQ_DATE similar to: 2003-01-02
    #ponto.focos@data$ACQ_DATE = as.Date(ponto.focos@data$ACQ_DATE, format = "%Y-%m-%d")
    
    # SAVE SELECTION BY MONTH AND BY YEAR IN A SHP LIST
    y = format(ponto.focos$ACQ_DATE[1], "%Y")
    
    if (mtly) {
      acumula.mes[[i]] = ponto.focos[format(ponto.focos$ACQ_DATE, "%m") == toString(mnth.pos),]
      
      if(nrow(acumula.mes[[i]]@data) > 0) {
        grid@data[paste0("fc", y)] = over(grid, acumula.mes[[i]][, "x"], fn = sum)
        grid@data[paste0("fc", y)][is.na(grid@data[paste0("fc", y)])] = 0
      } else {
        grid@data[paste0("fc", y)] = 0
      }
    } else {
      grid@data[paste0("fc", y)] = over(grid, fires[[i]][,"x"], fn = sum)
      grid@data[paste0("fc", y)][is.na(grid@data[paste0("fc", y)])] = 0
    }
    setTxtProgressBar(pb, i) # update PB
  }
  
  # CREATE TEMPORARY VARIABLE TO HOLD REGRESSION DATA TABLE FOR EACH CELL
  focos = rep(0, times = length(interval))
  tmpRegressao = data.frame(interval, focos)
  
  # BEGIN OF THE MODEL FITTING
  total = length(grid) #progress bar max size
  pb = txtProgressBar(min = 0, max = total, style = 3) #create PB
  for(i in 1:length(grid@data$ID)){
    
    setTxtProgressBar(pb, i) # update PB
    
    for(a in 1:length(interval)){
      tmpRegressao[a,"focos"] = grid@data[i,(grep(interval[a], names(grid@data), value = TRUE))]
    }
    # LM ----
    # SIMPLE LINEAR REGRESSION
    fit = lm(formula = tmpRegressao$focos ~ tmpRegressao$interval)
    
    # CALCULA P-VALOR
    pvalor = summary(fit)
    pvalor = pf(pvalor$fstatistic[1],
                pvalor$fstatistic[2],
                pvalor$fstatistic[3],
                lower.tail = F)
    
    grid@data$pvalor[i] = pvalor
    
    # SD ----
    grid@data$sigma[i] = sigma(fit)
    
    # summary ----
    #grid@data$smmry[i] = summary(tmpRegressao$focos)
    # grid@data$reg.1stq[i] = pvalor[2]
    # grid@data$reg.mdan[i] = pvalor[3]
    # grid@data$reg.mean[i] = pvalor[4]
    # grid@data$reg.3rdq[i] = pvalor[5]
    # grid@data$reg.max[i] = pvalor[6]
    
    # TESTA SIGNIFICANCIA P<0.05 ----
    if (!is.nan(grid@data$pvalor[i]) & grid@data$pvalor[i]<=0.05){
      grid@data$sig05[i] = 1
    } else {
      grid@data$sig05[i] = 0
    }
    
    # BETA 1 ----
    grid@data$coef[i] = fit$coefficients[2]
    # BETA 0 ----
    grid@data$intercept[i] = fit$coefficients[1]
    
    # ANGULO DE TENDENCIA DA RETA ----
    grid@data$angulo[i] = ((atan(fit$coefficients[2]) * 180) / pi)
    
    # CLASSIFICA TENDENCIA COMO POSITIVA(P), NEGATIVA(N) OU ZERO(Z) ----
    if (grid@data$angulo[i] < 0) {
      grid@data$cltend[i] = "N"
    } else if (grid@data$angulo[i]==0) {
      grid@data$cltend[i] = "Z"
    } else {
      grid@data$cltend[i] = "P"
    }
    
    # CATEGORIZA AS RETAS DE TENDENCIA ----
    if (grid@data$angulo[i] <= 90 & grid@data$angulo[i] > 60) {
      grid@data$cat[i] = 3
    } else if(grid@data$angulo[i] <= 60 & grid@data$angulo[i] > 30){
      grid@data$cat[i] = 2
    } else if(grid@data$angulo[i] <= 30 & grid@data$angulo[i] > 0){
      grid@data$cat[i] = 1
    } else if(grid@data$angulo[i] == 0){
      grid@data$cat[i] = 0
    } else if(grid@data$angulo[i] < 0 & grid@data$angulo[i] > -30){
      grid@data$cat[i] = -1
    } else if(grid@data$angulo[i] <= -30 & grid@data$angulo[i] > -60){
      grid@data$cat[i] = -2
    } else {
      grid@data$cat[i] = -3
    }
    
    # DEPRECATED / TBD
    # NEXT YEAR PROJECTION ----
    # grid@data$nxtyp[i] = grid@data$intercept[i] + grid@data$coef[i] * interval[length(interval)]+1
    # ifelse(grid@data$nxtyp[i] < 0,0,grid@data$nxtyp[i])
    
    # CELL CHECK / TBD (Consider MEDIAN instead of "interval[length(interval)]")
    if(validate){
      # VLD ----
      # Validate model trend with observed data
       #grid@data$trend[i] = vldata[i] - grid@data[paste0("fc", interval[length(interval)])][i,]
       if (vldata[i] > 0 & grid@data$cltend[i] == "P") {
         grid@data$trnd.vld[i] = TRUE
       } else {
         grid@data$trnd.vld[i] = FALSE
       }
       # else if (grid@data$trend[i] == 0 & grid@data$cltend[i] == "Z") {
       #   grid@data$trnd.vld[i] = TRUE
       # } else if (grid@data$trend[i] < 0 & grid@data$cltend[i]=="N") {
       #   grid@data$trnd.vld[i] = TRUE
       # } else {
       #   grid@data$trnd.vld[i] = FALSE
       # }
      # Validate model prediction with observed data
      # if ((grid@data$nxtyp[i] + grid@data$sigma[i] >= vldata[i]) | (grid@data$nxtyp[i] - grid@data$sigma[i] <= vldata[i])){
      #   grid@data$pred.vld[i] = TRUE
      # } else {
      #   grid@data$pred.vld[i] = FALSE
      # }

      # GRAFICO COM TODAS AS CELULAS
      
    }
    
  }
  return(grid)
}

#_______________________________
# :SEC04-B-RUN: ----
# Call of the SENTIR() function 
#_______________________________

grid = acrebox #See :SEC01: for details about the file location

setwd("G:/Projeto_Acre/");load('listafocoshp.RData');load('listafocoshp2.RData') #See :SEC05: for details

#Running the model with *outdated* fires data
cptec.a.03.15 = sentir(fires = listafocoshp,
                       grid = grid,
                       interval = 2003:2015,
                       mtly = F,
                       mnth.pos = "08",
                       validate = F,
                       vldata = z@data$fc2016, # Only works if you already have 'z' in your environment
                       cptec = T) 

# In case you get erros similar to:
# "Error in x[[jj]][iseq] <- vjj : replacement has length zero"
# See :SEC-ERROR: for details. This can be caused by system date and language settings.

#_________________________
# :SEC05-VLD: ----
# Model output validation 
#_________________________

# Count TRUE values in a logical vector
sum(z, na.rm=TRUE) 

#:VLD-ANO----
#__________________________2003:2015
m.ano.2003.2015 = sentir(
  fires = listafocoshp2,
  grid = grid,
  interval = 2003:2015,
  mtly = F,
  mnth.pos = 7,
  validate = F,
  vldata = z2@data$fc2016
)

m.ano.2005.2010 = sentir(
  fires = listafocoshp2,
  grid = grid,
  interval = 2005:2010,
  mtly = F,
  mnth.pos = 7,
  validate = F,
  vldata = z2@data$fc2016
)

m.ano.2010.2015 = sentir(
  fires = listafocoshp2,
  grid = grid,
  interval = 2010:2015,
  mtly = F,
  mnth.pos = 7,
  validate = F,
  vldata = z2@data$fc2016
)
#:VLD-JUL----
#__________________________JULHO
m.jul.2003.2015 = sentir(
  fires = listafocoshp2,
  grid = grid,
  interval = 2003:2015,
  mtly = T,
  mnth.pos = 7,
  validate = F,
  vldata = z2@data$fc2016
)

m.jul.2005.2010 = sentir(
  fires = listafocoshp2,
  grid = grid,
  interval = 2005:2010,
  mtly = T,
  mnth.pos = 7,
  validate = F,
  vldata = z2@data$fc2016
)

m.jul.2010.2015 = sentir(
  fires = listafocoshp2,
  grid = grid,
  interval = 2010:2015,
  mtly = T,
  mnth.pos = 7,
  validate = F,
  vldata = z2@data$fc2016
)
#:VLD-AGO----
#__________________________AGOSTO
m.ago.2003.2015 = sentir(
  fires = listafocoshp2,
  grid = grid,
  interval = 2003:2015,
  mtly = T,
  mnth.pos = 8,
  validate = F,
  vldata = z2@data$fc2016
)
m.ago.2005.2010 = sentir(
  fires = listafocoshp2,
  grid = grid,
  interval = 2005:2010,
  mtly = T,
  mnth.pos = 8,
  validate = F,
  vldata = z2@data$fc2016
)
m.ago.2010.2015 = sentir(
  fires = listafocoshp2,
  grid = grid,
  interval = 2010:2015,
  mtly = T,
  mnth.pos = 8,
  validate = F,
  vldata = z2@data$fc2016
)
#:VLD-SET----
#__________________________SETEMBRO
m.set.2003.2015 = sentir(
  fires = listafocoshp2,
  grid = grid,
  interval = 2003:2015,
  mtly = T,
  mnth.pos = 9,
  validate = F,
  vldata = z2@data$fc2016
)
m.set.2005.2010 = sentir(
  fires = listafocoshp2,
  grid = grid,
  interval = 2005:2010,
  mtly = T,
  mnth.pos = 9,
  validate = F,
  vldata = z2@data$fc2016
)
m.set.2010.2015 = sentir(
  fires = listafocoshp2,
  grid = grid,
  interval = 2010:2015,
  mtly = T,
  mnth.pos = 9,
  validate = F,
  vldata = z2@data$fc2016
)
#:VLD-OUT----
#__________________________OUTUBRO
m.out.2003.2015 = sentir(
  fires = listafocoshp2,
  grid = grid,
  interval = 2003:2015,
  mtly = T,
  mnth.pos = 10,
  validate = F,
  vldata = z2@data$fc2016
)
m.out.2005.2010 = sentir(
  fires = listafocoshp2,
  grid = grid,
  interval = 2005:2010,
  mtly = T,
  mnth.pos = 10,
  validate = F,
  vldata = z2@data$fc2016
)
m.out.2010.2015 = sentir(
  fires = listafocoshp2,
  grid = grid,
  interval = 2010:2015,
  mtly = T,
  mnth.pos = 10,
  validate = F,
  vldata = z2@data$fc2016
)
#:VLD-NOV----
#__________________________NOVEMBRO
m.nov.2003.2015 = sentir(
  fires = listafocoshp2,
  grid = grid,
  interval = 2003:2015,
  mtly = T,
  mnth.pos = 11,
  validate = F,
  vldata = z2@data$fc2016
)
m.nov.2005.2010 = sentir(
  fires = listafocoshp2,
  grid = grid,
  interval = 2005:2010,
  mtly = T,
  mnth.pos = 11,
  validate = F,
  vldata = z2@data$fc2016
)
m.nov.2010.2015 = sentir(
  fires = listafocoshp2,
  grid = grid,
  interval = 2010:2015,
  mtly = T,
  mnth.pos = 11,
  validate = F,
  vldata = z2@data$fc2016
)
#_____________________________
# :SEC06-PLOT: ----
# Visual aid and data ploting 
#_____________________________

require(ggplot2)
require(dplyr)
require(tidyr)

#outputFolder = "H:/Projeto_Acre/IMAGENS/histograma_focos"
outputFolder = "G:/Projeto_Acre/IMAGENS/histograma_focos"

test = z@data

test = test[test$sig05 == 1,]
test = test[test$trnd.vld == T,]
testp01 = test[test$pvalor<0.1,]
testp005 = test[test$pvalor<0.05,]
testp001 = test[test$pvalor<0.01,]

# HISTOGRAMS

#____ ALL DATA
pall = ggplot(test, aes(fc2016)) +
  geom_histogram(binwidth = 1) +
  scale_y_continuous(limits = c(0, 20))+
  scale_x_continuous(limits = c(0, 50))+
  labs(y = 'Frequência',
       x = 'Focos de calor') +
  annotate("text", x = 40, y = 15, size = 7, label = "p = n/a")
#____ p < 0.1 
p01 = ggplot(testp01, aes(fc2016)) +
  geom_histogram(binwidth = 1) +
  scale_y_continuous(limits = c(0, 20))+
  scale_x_continuous(limits = c(0, 50))+
  labs(y = 'Frequência',
       x = 'Focos de calor') +
  annotate("text", x = 40, y = 15, size = 7, label = "p < 0.1")
#____ p < 0.05 
p005 = ggplot(testp005, aes(fc2016)) +
  geom_histogram(binwidth = 1) +
  scale_y_continuous(limits = c(0, 20))+
  scale_x_continuous(limits = c(0, 50))+
  labs(y = 'Frequência',
       x = 'Focos de calor') +
  annotate("text", x = 40, y = 15, size = 7, label = "p < 0.05")
#____ p < 0.01 
p001 = ggplot(testp001, aes(fc2016)) +
  geom_histogram(binwidth = 1) +
  scale_y_continuous(limits = c(0, 20))+
  scale_x_continuous(limits = c(0, 50))+
  labs(y = 'Frequência',
       x = 'Focos de calor') +
  annotate("text", x = 40, y = 15, size = 7, label = "p < 0.01")


#___________________________
a = 
  test %>%
  select(id = ID, starts_with("fc")) %>%
  gather('ano', 'focos', -id) %>%
  mutate(ano = substr(ano, 3, 7)) %>%
  
  ggplot(aes(focos)) +
  labs(y = 'Frequência',
       x = 'Focos de calor') +
  geom_histogram() +
  facet_wrap(~ano, scales = 'free_x'); a

b = 
  test %>%
  select(id = ID, starts_with("fc")) %>%
  gather('ano', 'focos', -id) %>%
  mutate(ano = substr(ano, 3, 7)) %>%
  
  ggplot(aes(ano, focos)) +
  labs(x = 'Ano',
       y = 'Focos de calor') +
  geom_boxplot(); b

ggsave(plot = a,
       path = outputFolder, device = "png",
       filename = "hist_focos.png",
       width = 20, height = 17,  units = "cm")

ggsave(plot = a + scale_x_log10(),
       path = outputFolder, device = "png",
       filename = "hist_focos_log.png",
       width = 20, height = 17,  units = "cm")

ggsave(plot = b,
       path = outputFolder, device = "png",
       filename = "bp_focos.png",
       width = 20, height = 7,  units = "cm")

ggsave(plot = b + scale_y_log10(),
       path = outputFolder, device = "png",
       filename = "bp_focos_log_v2.png",
       width = 20, height = 7,  units = "cm")

# LIANA x=angle/y=n.fires
validados = m.ano.2003.2015@data
validados$sig05 = as.factor(validados$sig05)
validados$cltend = as.factor(validados$cltend)
validados$cat = as.factor(validados$cat)

sig.pos.validados = validados[validados$cltend=="P"&validados$sig05==1,]
sig.neg.validados = validados[validados$cltend=="N"&validados$sig05==1,]

# plotagem = ggplot(validados, aes(x=angulo, y=fc2016, color=cltend,shape=sig05)) +
#   geom_point()
# 
# print(plotagem + scale_shape_manual(values = c(4, 16)))



plotagem = ggplot(sig.pos.validados, aes(x=angulo, y=fc2016, color=fc2016)) +
  geom_point(size=2) + scale_colour_gradient(high = "red") +
  ggtitle("") +

print(plotagem + scale_shape_manual(values = c(4, 16)))

