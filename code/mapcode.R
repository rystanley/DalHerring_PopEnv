
#load libraries ---------------
library(marmap)
library(maps)
library(mapdata)
library(dplyr)
library(rmatio)
library(sp)
library(rgdal)
library(rgeos)

#load data -----------
Locations=read.csv("data/Locations.csv")

## Set map limits ------------
Lat.lim=c(40,53)
Long.lim=c(-71,-52)

#Download ClineDepthshymetry layer -----------------
curdir <- getwd()
setwd("data/")
getNOAA.bathy(lon1 = Long.lim[1], lon2 = Long.lim[2], lat1 = Lat.lim[1], lat2 = Lat.lim[2],
              resolution = 4,keep=TRUE) -> DepthData
setwd(curdir)

blues <- c("lightsteelblue4", "lightsteelblue3", "lightsteelblue2","lightsteelblue1")
greys <- c("grey40","grey55","grey70","grey80")
greys <- c(grey(0.6), grey(0.93), grey(0.99))

##Map with all species zoomed in to the main range (NL-Carolina) --------

png(filename = "SampleMap.png", 
    width = 2150, height = 1775, res = 300, bg="transparent")

plot(DepthData, deep=-200,shallow=-200, image = TRUE, land = TRUE, lwd = 0.1, 
     bpal = list(c(0, max(DepthData), greys), c(min(DepthData), 0, blues)))
map("worldHires", xlim=Long.lim, ylim=Lat.lim, col="black", fill=FALSE, resolution=0,add=TRUE)
map.axes();map.scale(ratio=FALSE)
points(Locations$Long,Locations$Lat,pch=19,cex=1.5) #Add points

dev.off()

## Inspect the depths to make sure all points are in the water
get.depth(DepthData,Locations[,c("Long","Lat")],locator = FALSE)%>%arrange(desc(lat))

#Map the problem site
map("worldHires", xlim=Long.lim, ylim=Lat.lim, col="black", fill=FALSE, resolution=0)
map.axes();map.scale(ratio=FALSE)
points(Locations$Long,Locations$Lat,pch=19,cex=1.5) #Add points
points(Locations[Locations$PopID %in% c("Ste","BDO"),"Long"],Locations[Locations$PopID %in% c("Ste","BDO"),"Lat"],col="red",pch=19,cex=1.5)

#bump the coordinates to the water
Locations[Locations$PopID=="Ste","Long"]=Locations[Locations$PopID=="Ste","Long"]-0.5
Locations[Locations$PopID=="BDO","Lat"]=Locations[Locations$PopID=="BDO","Lat"]+0.5
Locations[Locations$PopID=="BDO","Long"]=Locations[Locations$PopID=="BDO","Long"]+0.7
points(Locations[Locations$PopID %in% c("Ste","BDO"),"Long"],Locations[Locations$PopID %in% c("Ste","BDO"),"Lat"],col="blue",pch=19,cex=1.5)

#Replot points
map("worldHires", xlim=Long.lim, ylim=Lat.lim, col="black", fill=FALSE, resolution=0)
map.axes();map.scale(ratio=FALSE)
points(Locations$Long,Locations$Lat,pch=19,cex=1.5) #Add points

#get coordinates
coords <- read.mat("data/MatLabData/Coord_Shrink.mat")
xydata <- data.frame(x=as.vector(coords$glamt),y=as.vector(coords$gphit))

#Extract the environmental data -------------
  #indexing variables
    months <- c("Jan","Feb","March","April","May","June","July","Aug","Sept","Oct","Nov","Dec")
    years <- 2008:2017 # this is the data available
    months <- paste0("m",1:12,".mat")
    Temps <- c("BT","SST")

    curdir <- getwd() #save the root directory 

  #iterative extraction    
    masterdata <- NULL
    
    for(i in years){
      
      setwd(paste(curdir,"data/MatLabData/",i,sep="/"))
      
      for(j in Temps){
        
        tempyr <- xydata
        tempyr$ID <- 1:nrow(tempyr) #unique ID for each point in space
        tempyr$year <- i
        tempyr$var <- j
        
        for(m in months){
          
          temp <- as.vector(read.mat(dir()[grep(m,dir())][grep(j,dir()[grep(m,dir())])])[[1]])
          tempyr <- cbind(tempyr,temp)
          colnames(tempyr)[length(tempyr)] <- gsub(".mat","",m)
          
        } #end of months loop
        
        masterdata <- rbind(masterdata,tempyr)
        
      }#end of BT vs SST
    }# end of years loop
    
    setwd(curdir) # go back to the current directory

## Seasonal and annual means --------------

masterDat <- masterdata%>% 
  mutate(Winter = rowMeans(.[c("m1","m2","m3")]), # Seasonal Averages
         Spring = rowMeans(.[c("m4","m5","m6")]),
         Summer = rowMeans(.[c("m7","m8","m9")]),
         Fall = rowMeans(.[c("m10","m11","m12")])) 

OverallAverage <- masterDat%>%group_by(ID,var)%>%
                  summarise(Winter=mean(Winter,na.rm=T),
                            Spring=mean(Spring,na.rm=T),
                            Summer=mean(Summer,na.rm=T),
                            Fall=mean(Fall,na.rm=T))%>%
                  ungroup()%>%
                  merge(.,masterDat%>%distinct(x,y,ID),by="ID")%>%
                  data.frame()

## Data extraction and plotting --------------

    #OverallAverage plots and extractions
    seasons <- c("Spring","Summer","Fall","Winter")
        
        extractedData <- NULL
        
        for(i in Temps){
          
          tempdata <- OverallAverage%>%filter(.,var==i)
          
          for (j in seasons){
            
            #Progress lines
            writeLines(paste("Working on",j,i))
            
            xyz <- tempdata%>%dplyr::select(x,y,j)
            output <- rasterFun(xyz,locations=Locations)
            
            #collate the data extractions
            extraction <- output$extract
            extraction$var <- i
            extraction$season <- j
            extraction$year <- "Overall"
            
            extractedData <- rbind(extractedData,extraction) # this is slow but it works
            
            #save the plot
            P1 <- output$plot
            ggsave(paste0("output/",j,"_",i,"_Overall.png"),P1,height=5,width=6)
          
          }#end of season loop
        }#end of Temperature variable loop
  
    #Yearly plots and extractions
      extractedData_yr <- NULL
      
      for(i in Temps){
        for (yr in years){
          
          tempdata <- masterDat%>%dplyr::filter(.,var==i,year==yr)
        
          for (j in seasons){ # note if you want the montly data add (change seasons to seasons <- c(paste0("m",1:12),"Spring","Summer","Fall","Winter")
            
            #Progress lines
            writeLines(paste("Working on",j,i,yr))
            
            xyz <- tempdata%>%dplyr::select(x,y,j)
            output <- rasterFun(xyz,locations=Locations)
            
            #collate the data extractions
            extraction <- output$extract
            extraction$var <- i
            extraction$season <- j
            extraction$year <- yr
            
            extractedData_yr <- rbind(extractedData_yr,extraction) # this is slow but it works
            
            #save the plot
            P1 <- output$plot
            ggsave(paste0("output/",j,"_",i,"_",yr,".png"),P1,height=5,width=6)
            
          }#end of season loop
        }#end of year loop
      }#end of Temperature variable loop
      
    
## Bind all of the data together
OutputData <- rbind(extractedData,extractedData_yr)

