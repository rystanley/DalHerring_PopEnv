rasterFun <- function(xyz,resolution=7.5,fun=mean,locations=NULL){
  
  #xyz - this is the longitude, latitude and variable of interest
  #resolution  (km) - to be gridded (note 7.5 is the default and the optimal resoution for the data)
  #fun - this is the function used to aggregate multiple values per grid cell at the resolution. Decfault is mean but you could use things like sd, min, max etc.
  #locations - this is the PopID, Longitude, and Latitude of the stations. If null the raster/plot will be produced but no extractions will be made
  
  require(raster)
  require(sp)
  require(rgdal)
  require(ggplot2)
  require(maps)
  require(mapdata)
  
  #set up the data
  colnames(xyz) <- c("x","y","z")
  coordinates(xyz) <- ~x + y
  proj4string(xyz) <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
  xyz <- spTransform(xyz,CRS("+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0")) #km projection
  
  #Create raster grid
  grid <- raster(extent(xyz))
  res(grid) <- resolution
  proj4string(grid)<-proj4string(xyz)
  
  #fill in the raster
  ras <- raster::rasterize(coordinates(xyz), grid, field = xyz$z,fun=fun) # this can take a while
  ras <- projectRaster(ras,crs="+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") #project back to lat long
  
  #extrat the points
  if(!is.null(locations)){
    ID <- locations$PopID
    locations <- locations[,c("Long","Lat")]
    colnames(locations) <- c("x","y")
    coordinates(locations) <- ~x + y  
    proj4string(locations) <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
    output <- data.frame(ID=ID,temp=extract(ras,locations),stringsAsFactors = F)
  }
  
  #Get data for plotting
  
  #crop raster to the extent of data (used in plotting) with half a degree buffer
  rextent <- extent(locations)
  
  rextent[1] <- rextent[1]-0.5
  rextent[2] <- rextent[2]+0.5
  rextent[3] <- rextent[3]-0.5
  rextent[4] <- rextent[4]+0.5
 
  Long.lim  <-  c(rextent[1], rextent[2])
  Lat.lim <-  c(rextent[3], rextent[4])
  
  #Crop raster
  ras <- crop(ras,rextent)
  
  #raster to point data for ggplot
  md <- as.data.frame(rasterToPoints(ras))
  colnames(md) <- c("Longitude", "Latitude", "MAP")
  
  states <- map_data("state")
  usa <- subset(states,region %in% c("maine","new hampshire",
                                                  "massachusetts","connecticut",
                                                  "rhode island","vermont"))
  canada <- map_data("worldHires", "Canada")
  
  
    if(!dir.exists("data")){dir.create("data/")} #create a data directory if it doesn't exist
    curdir <- getwd()
    setwd("data/")
    bathy <- getNOAA.bathy(Long.lim[1],Long.lim[2],Lat.lim[1],Lat.lim[2],res=1,keep=T)
    setwd(curdir)
  
  #format objects for ggplot
  bathy <- fortify(bathy)

  p1 <- ggplot() +
    geom_tile(data=md,aes(x=Longitude,y=Latitude,fill=MAP))+
    geom_polygon(data = usa, 
                 aes(x=long, y = lat, group = group), 
                 fill = "white", 
                 color="black") +
    geom_polygon(data = canada, aes(x=long, y = lat, group = group), 
                 fill = "white", color="black") + 
    geom_point(data=data.frame(coordinates(locations)),aes(x=x,y=y),pch=19,size=3,col="black")+
    geom_contour(data=bathy,aes(x=x,y=y,z=z),breaks=c(-200),lwd=0.05,colour="grey20")+
    coord_fixed(xlim = Long.lim,  ylim = Lat.lim, ratio = 1.2,expand=0)+
    theme_bw()+
    theme(legend.position = "bottom",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white", colour = "black"),
          plot.background = element_rect(colour = "white"),
          strip.background = element_rect(colour = "black", fill = "white"))+
    labs(x=expression(paste("Longitude ",degree,"W",sep="")),
         y=expression(paste("Latitude ",degree,"N",sep="")),
         fill="")+
    scale_fill_gradientn(colours=colorRampPalette(rev(c("red","yellow","springgreen","royalblue")))(50))
    
  #return the information
  if(!is.null(locations)){outlist <- list()
                          outlist$plot <- p1
                          outlist$extract <- output
                          outlist$raster <- ras
                          outlist$bathy <- bathy
                          return(outlist)}
  
  if(is.null(locations)){outlist <- list()
                         outlist$plot <- p1
                         outlist$raster=ras
                         outlist$bathy=bathy}
    
}