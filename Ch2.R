library(ggplot2)
library(dplyr)
library(tidyverse)
require(maptools)
library(mapdata)
library(maps)
library(mapproj)
library(gridExtra)
library(grid)
library(caret)
library(TMB)
library(INLA)
library(raster)
library(sf)
library(sdmTMB)
library(rgdal)
library(ggpubr)
library(ggmap)
library(corrplot )

# Skip Fig 2.1,2.3,2.4



scallops=read.csv("bof.mwsh.JuneJuly.2012to2019.clean.csv")
scallops1=scallops%>% filter(month ==7)
scallops1 = scallops1[!scallops1$CRUISE == "RF2012",]
mydata= scallops1%>% transmute(weight = WET_MEAT_WGT, 
                              height = HEIGHT,
                              year = as.factor(year),
                              TOW_NO =TOW_NO ,
                              lon= mid.lon, lat= mid.lat 
)



draster=raster("BathyCHS_GEBCO_SEAM_mixedData_BOF_ExtentClip_100m_LatLong.asc")
r2012=raster("BtmTemp_Jul_2012.asc")
r2013=raster("BtmTemp_Jul_2013.asc")
r2014=raster("BtmTemp_Jul_2014.asc")
r2015=raster("BtmTemp_Jul_2015.asc")
r2016=raster("BtmTemp_Jul_2016.asc")
r2017=raster("BtmTemp_Jul_2017.asc")
r2018=raster("BtmTemp_Jul_2018.asc")
r2019=raster("BtmTemp_Jul_2019.asc")

s2012=raster("BtmStress_Jul_2012.asc")
s2013=raster("BtmStress_Jul_2013.asc")
s2014=raster("BtmStress_Jul_2014.asc")
s2015=raster("BtmStress_Jul_2015.asc")
s2016=raster("BtmStress_Jul_2016.asc")
s2017=raster("BtmStress_Jul_2017.asc")
s2018=raster("BtmStress_Jul_2018.asc")
s2019=raster("BtmStress_Jul_2019.asc")

a2012=raster("BtmSalinity_Jul_2012.asc")
a2013=raster("BtmSalinity_Jul_2013.asc")
a2014=raster("BtmSalinity_Jul_2014.asc")
a2015=raster("BtmSalinity_Jul_2015.asc")
a2016=raster("BtmSalinity_Jul_2016.asc")
a2017=raster("BtmSalinity_Jul_2017.asc")
a2018=raster("BtmSalinity_Jul_2018.asc")
a2019=raster("BtmSalinity_Jul_2019.asc")




mydata$depth=raster::extract(draster, y = cbind(mydata$lon , mydata$lat))

obs1=mydata%>% filter(year ==2012)
obs2=mydata%>% filter(year ==2013)
obs3=mydata%>% filter(year ==2014)
obs4=mydata%>% filter(year ==2015)
obs5=mydata%>% filter(year ==2016)
obs6=mydata%>% filter(year ==2017)
obs7=mydata%>% filter(year ==2018)
obs8=mydata%>% filter(year ==2019)

obs1$salinity=raster::extract(a2012, y = cbind(obs1$lon, obs1$lat))
obs2$salinity=raster::extract(a2013, y = cbind(obs2$lon, obs2$lat))
obs3$salinity=raster::extract(a2014, y = cbind(obs3$lon, obs3$lat))
obs4$salinity=raster::extract(a2015, y = cbind(obs4$lon, obs4$lat))
obs5$salinity=raster::extract(a2016, y = cbind(obs5$lon, obs5$lat))
obs6$salinity=raster::extract(a2017, y = cbind(obs6$lon, obs6$lat))
obs7$salinity=raster::extract(a2018, y = cbind(obs7$lon, obs7$lat))
obs8$salinity=raster::extract(a2019, y = cbind(obs8$lon, obs8$lat))
mydata$salinity=c(obs1$salinity,obs2$salinity,obs3$salinity,obs4$salinity,obs5$salinity,obs6$salinity,obs7$salinity,obs8$salinity)

obs1$temperature=raster::extract(r2012, y = cbind(obs1$lon, obs1$lat))
obs2$temperature=raster::extract(r2013, y = cbind(obs2$lon, obs2$lat))
obs3$temperature=raster::extract(r2014, y = cbind(obs3$lon, obs3$lat))
obs4$temperature=raster::extract(r2015, y = cbind(obs4$lon, obs4$lat))
obs5$temperature=raster::extract(r2016, y = cbind(obs5$lon, obs5$lat))
obs6$temperature=raster::extract(r2017, y = cbind(obs6$lon, obs6$lat))
obs7$temperature=raster::extract(r2018, y = cbind(obs7$lon, obs7$lat))
obs8$temperature=raster::extract(r2019, y = cbind(obs8$lon, obs8$lat))
mydata$temperature=c(obs1$temperature,obs2$temperature,obs3$temperature,
                     obs4$temperature,obs5$temperature,obs6$temperature,
                     obs7$temperature,
                     obs8$temperature)

obs1$stress=raster::extract(s2012, y = cbind(obs1$lon, obs1$lat))
obs2$stress=raster::extract(s2013, y = cbind(obs2$lon, obs2$lat))
obs3$stress=raster::extract(s2014, y = cbind(obs3$lon, obs3$lat))
obs4$stress=raster::extract(s2015, y = cbind(obs4$lon, obs4$lat))
obs5$stress=raster::extract(s2016, y = cbind(obs5$lon, obs5$lat))
obs6$stress=raster::extract(s2017, y = cbind(obs6$lon, obs6$lat))
obs7$stress=raster::extract(s2018, y = cbind(obs7$lon, obs7$lat))
obs8$stress=raster::extract(s2019, y = cbind(obs8$lon, obs8$lat))
mydata$stress=c(obs1$stress,obs2$stress,obs3$stress,obs4$stress,
                obs5$stress,obs6$stress,obs7$stress,obs8$stress)

mydata= mydata%>%filter(
  !is.na(depth),
  !is.na(temperature),
  !is.na(stress),
  !is.na(salinity)
) %>% transmute(weight = weight, 
                height = height,
                depth=-depth,
                year = as.factor(year),
                lon= lon, lat= lat ,         
                ID_TOW = as.factor(paste(year,TOW_NO,sep = '_')),
                depth = depth ,
                temperature = temperature,
                stress=stress,
                salinity=salinity) 



heightdata=read.csv("bof.shf.unlined.gear.2012to2019.csv")
heightdata=na.omit(heightdata) # remove RF
heightdata$year=NA
heightdata$ID_TOW=NA

for(i in 1:length(heightdata[,1]))
{ 
  if(grepl((heightdata$CRUISE[i]), "BF2012")==T   )  
  {
    heightdata$year[i]=2012    
  }
  if(grepl((heightdata$CRUISE[i]), "BF2013")==T  ) 
  {
    heightdata$year[i]=2013 
  }
  if(grepl((heightdata$CRUISE[i]), "BF2014")==T   )   
  {
    heightdata$year[i]=2014 
  }
  if(grepl((heightdata$CRUISE[i]), "BF2015")==T   ) 
  {
    heightdata$year[i]=2015
    
  }
  if(grepl((heightdata$CRUISE[i]), "BF2016")==T   )  
  {
    heightdata$year[i]=2016   
  }
  if(grepl((heightdata$CRUISE[i]), "BF2017")==T   ) 
  {
    heightdata$year[i]=2017  
  }
  if(grepl((heightdata$CRUISE[i]), "BF2018")==T   )
  {
    heightdata$year[i]=2018   
  } 
  if(grepl((heightdata$CRUISE[i]), "BF2019")==T  )  
  {
    heightdata$year[i]=2019  
  }
}

heightdata$year = as.factor(heightdata$year)
heightdata$ID_TOW =as.factor(paste(heightdata$year,heightdata$TOW_NO,sep = '_'))


# create the SH  dataset
set.seed(1)
test=c()
binocoun=matrix(0,length(heightdata[,1]),40)

for(i in 1: length(heightdata[,1]))
{
  for(k in 1:40) 
  {
    binocoun[i,k]=floor(heightdata[i,k+5]/6)+ rbinom(1,1,((heightdata[i,k+5]/6)%% 1))
    new_value =runif( binocoun[i,k] ,min=(k-1)*5,   max=((k-1)*5)+4.99)
    test=c(test,new_value)   
  }  
}


for(i in 1: length(heightdata[,1]))
  
{
  heightdata$bcoun[i]=sum(binocoun[i,seq(from=1,to=40, by=1)])  
}


fullset <- as.data.frame(lapply(heightdata, rep, heightdata$bcoun )) 
fullset$gheight=test
Julyfullset=fullset[fullset$month==7,]



# assign environmental values  for every observation 
Julyfullset$temperature=NA
Julyfullset$salinity=NA
Julyfullset$stress=NA

# spatially extract the depth values
Julyfullset$depth=raster::extract(draster, y = cbind(Julyfullset$mid.lon,
                                                     Julyfullset$mid.lat))

# spatiotemporally extract the other environmental variable values
h1=Julyfullset[Julyfullset$year==2012,]
h2=Julyfullset[Julyfullset$year==2013,]
h3=Julyfullset[Julyfullset$year==2014,]
h4=Julyfullset[Julyfullset$year==2015,]
h5=Julyfullset[Julyfullset$year==2016,]
h6=Julyfullset[Julyfullset$year==2017,]
h7=Julyfullset[Julyfullset$year==2018,]
h8=Julyfullset[Julyfullset$year==2019,]

h1$temperature=raster::extract(r2012, y = cbind(h1$mid.lon , h1$mid.lat))
h2$temperature=raster::extract(r2013, y = cbind(h2$mid.lon , h2$mid.lat))
h3$temperature=raster::extract(r2014, y = cbind(h3$mid.lon , h3$mid.lat))
h4$temperature=raster::extract(r2015, y = cbind(h4$mid.lon , h4$mid.lat))
h5$temperature=raster::extract(r2016, y = cbind(h5$mid.lon , h5$mid.lat))
h6$temperature=raster::extract(r2017, y = cbind(h6$mid.lon , h6$mid.lat))
h7$temperature=raster::extract(r2018, y = cbind(h7$mid.lon , h7$mid.lat))
h8$temperature=raster::extract(r2019, y = cbind(h8$mid.lon , h8$mid.lat))

h1$stress=raster::extract(s2012, y = cbind(h1$mid.lon,h1$mid.lat))
h2$stress=raster::extract(s2013, y = cbind(h2$mid.lon, h2$mid.lat))
h3$stress=raster::extract(s2014, y = cbind(h3$mid.lon, h3$mid.lat))
h4$stress=raster::extract(s2015, y = cbind(h4$mid.lon, h4$mid.lat))
h5$stress=raster::extract(s2016, y = cbind(h5$mid.lon, h5$mid.lat))
h6$stress=raster::extract(s2017, y = cbind(h6$mid.lon, h6$mid.lat))
h7$stress=raster::extract(s2018, y = cbind(h7$mid.lon, h7$mid.lat))
h8$stress=raster::extract(s2019, y = cbind(h8$mid.lon, h8$mid.lat))

h1$salinity=raster::extract(a2012, y = cbind(h1$mid.lon, h1$mid.lat))
h2$salinity=raster::extract(a2013, y = cbind(h2$mid.lon, h2$mid.lat))
h3$salinity=raster::extract(a2014, y = cbind(h3$mid.lon, h3$mid.lat))
h4$salinity=raster::extract(a2015, y = cbind(h4$mid.lon, h4$mid.lat))
h5$salinity=raster::extract(a2016, y = cbind(h5$mid.lon, h5$mid.lat))
h6$salinity=raster::extract(a2017, y = cbind(h6$mid.lon, h6$mid.lat))
h7$salinity=raster::extract(a2018, y = cbind(h7$mid.lon, h7$mid.lat))
h8$salinity=raster::extract(a2019, y = cbind(h8$mid.lon, h8$mid.lat))

Julyfullset$temperature=c(h1$temperature,h2$temperature,h3$temperature,
                          h4$temperature,h5$temperature,h6$temperature,
                          h7$temperature,h8$temperature)
Julyfullset$stress=c(h1$stress,h2$stress,h3$stress,h4$stress,
                     h5$stress,h6$stress,h7$stress,h8$stress)
Julyfullset$salinity=c(h1$salinity,h2$salinity,h3$salinity,h4$salinity,
                       h5$salinity,h6$salinity,h7$salinity,h8$salinity)


# SH dataset
myheight= Julyfullset%>%filter(
  !is.na(temperature),
  !is.na(depth),
  !is.na(stress),
  !is.na(salinity)
) %>% transmute(
  height = gheight,
  year = as.factor(year),
  lon= mid.lon, lat= mid.lat , 
  TOW_NO=TOW_NO,
  ID_TOW = as.factor(paste(year,TOW_NO,sep = '_')), 
  depth = -depth ,
  temperature = temperature,
  salinity=salinity,
  stress=stress)  # make sure no NA, all values should be positive



# make a UTM dataset for the MWSH dataset
all_set <- mydata %>% 
  mutate(height = as.numeric((log(height))),
         depth = as.numeric((log(depth))),
         temperature = as.numeric((log(temperature))),
         stress = as.numeric((log(stress))),
         salinity = as.numeric((log(salinity)))) %>%
  mutate(X=lon, Y=lat) %>%
  `attr<-`("projection", "LL") %>%
  `attr<-`("zone", "20") %>%
  PBSmapping::convUL() 



# create a UTM dataset for the SH dataset
hall_set <- myheight %>% 
  mutate(
    depth = depth,
    temperature = temperature,
    salinity=salinity,
    stress=stress) %>%
  mutate(X=lon, Y=lat) %>%
  `attr<-`("projection", "LL") %>%
  `attr<-`("zone", "20") %>%
  PBSmapping::convUL() 
# always check if the year is a factor




# create shape files for the study area and the Bay of Fundy
studyarea =readOGR(dsn = path.expand("BoF_Strata_extent4Joy.shp"), 
                   layer = "BoF_Strata_extent4Joy")
shape1 <- readOGR(dsn = path.expand("SPA1A_polygon_NAD83.shp"), 
                  layer = "SPA1A_polygon_NAD83")
shape2 <- readOGR(dsn = path.expand("SPA1B_polygon_NAD83.shp"), 
                  layer = "SPA1B_polygon_NAD83")
shape3 <- readOGR(dsn = path.expand("SPA4_polygon_NAD83.shp"), 
                  layer = "SPA4_polygon_NAD83")
shape4 <- readOGR(dsn = path.expand("SPA5_polygon_NAD83.shp"), 
                  layer = "SPA5_polygon_NAD83")
studyarea <- union(studyarea,shape4)
writeSpatialShape(studyarea, "studyarea.ship")
studyarea =st_read("studyarea.shp")
subs_union1 <- union(shape1,shape2)
subs_union2 <- union(shape3,shape4)
BayofFundy=union(subs_union1,subs_union2)
writeSpatialShape(BayofFundy, "Bay of Fundy.ship")



sf_use_s2(FALSE)
set.seed(100)
times = 8
N=100000
nc_point <- st_sample(x = studyarea, size = N)
nc_point=as.matrix(nc_point)
nc_point <- do.call(rbind, st_geometry(nc_point)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))

points=as.data.frame(nc_point)%>% 
  mutate(X=lon, Y=lat) %>%
  `attr<-`("projection", "LL") %>%
  `attr<-`("zone", "20") %>%
  PBSmapping::convUL() 
points=as.matrix(points)


# assign environmental values to all generated locations
mg=as.data.frame(nc_point)
mg$temperature=NA
mg$salinity=NA
mg$stress=NA

mg1=mg # for all environmental data in 2012
mg2=mg # for all environmental data in 2013
mg3=mg # for all environmental data in 2014
mg4=mg # for all environmental data in 2015
mg5=mg # for all environmental data in 2016
mg6=mg # for all environmental data in 2017
mg7=mg # for all environmental data in 2018
mg8=mg # for all environmental data in 2019

mg1$temperature=raster::extract(r2012, y = cbind(mg1$lon, mg1$lat))
mg2$temperature=raster::extract(r2013, y = cbind(mg2$lon, mg2$lat))
mg3$temperature=raster::extract(r2014, y = cbind(mg3$lon, mg3$lat))
mg4$temperature=raster::extract(r2015, y = cbind(mg4$lon, mg4$lat))
mg5$temperature=raster::extract(r2016, y = cbind(mg5$lon, mg5$lat))
mg6$temperature=raster::extract(r2017, y = cbind(mg6$lon, mg6$lat))
mg7$temperature=raster::extract(r2018, y = cbind(mg7$lon, mg7$lat))
mg8$temperature=raster::extract(r2019, y = cbind(mg8$lon, mg8$lat))

mg1$stress=raster::extract(s2012, y = cbind(mg1$lon, mg1$lat))
mg2$stress=raster::extract(s2013, y = cbind(mg2$lon, mg2$lat))
mg3$stress=raster::extract(s2014, y = cbind(mg3$lon, mg3$lat))
mg4$stress=raster::extract(s2015, y = cbind(mg4$lon, mg4$lat))
mg5$stress=raster::extract(s2016, y = cbind(mg5$lon, mg5$lat))
mg6$stress=raster::extract(s2017, y = cbind(mg6$lon, mg6$lat))
mg7$stress=raster::extract(s2018, y = cbind(mg7$lon, mg7$lat))
mg8$stress=raster::extract(s2019, y = cbind(mg8$lon, mg8$lat))

mg1$salinity=raster::extract(a2012, y = cbind(mg1$lon, mg1$lat))
mg2$salinity=raster::extract(a2013, y = cbind(mg2$lon, mg2$lat))
mg3$salinity=raster::extract(a2014, y = cbind(mg3$lon, mg3$lat))
mg4$salinity=raster::extract(a2015, y = cbind(mg4$lon, mg4$lat))
mg5$salinity=raster::extract(a2016, y = cbind(mg5$lon, mg5$lat))
mg6$salinity=raster::extract(a2017, y = cbind(mg6$lon, mg6$lat))
mg7$salinity=raster::extract(a2018, y = cbind(mg7$lon, mg7$lat))
mg8$salinity=raster::extract(a2019, y = cbind(mg8$lon, mg8$lat))


# expand mg to store environmental data across all years
rows= c(1:nrow(mg))
mg=mg[rep(rows, times),] 
mg$year=c(rep(2012,N),rep(2013,N),rep(2014,N),rep(2015,N),
          rep(2016,N),rep(2017,N),rep(2018,N),rep(2019,N))
mg$temperature=c(mg1$temperature,mg2$temperature,mg3$temperature,
                 mg4$temperature,mg5$temperature,mg6$temperature,
                 mg7$temperature,mg8$temperature)
mg$salinity=c(mg1$salinity,mg2$salinity,mg3$salinity,mg4$salinity,
              mg5$salinity,mg6$salinity,mg7$salinity,mg8$salinity)
mg$stress=c(mg1$stress,mg2$stress,mg3$stress,mg4$stress,
            mg5$stress,mg6$stress,mg7$stress,mg8$stress)
mg$depth=raster::extract(draster, y = cbind(mg$lon, mg$lat))

mg= mg%>%filter(
  !is.na(depth),
  !is.na(temperature),
  !is.na(stress),
  !is.na(salinity)
) %>% transmute(
  year = as.factor(year),
  lon= lon, lat= lat , 
  depth = -depth ,
  temperature = temperature,
  stress=stress,
  salinity=salinity)
mg=mg[mg$depth>0,] 


# make a study set with year and environmental information for all locations
hstudy_set <- mg %>% 
  mutate(
    depth = as.numeric(((depth))),
    temperature = as.numeric(((temperature))),
    stress = as.numeric(((stress))),
    salinity = as.numeric(((salinity)))) %>%
  mutate(X=lon, Y=lat) %>%
  `attr<-`("projection", "LL") %>%
  `attr<-`("zone", "20") %>%
  PBSmapping::convUL()  

#  Figure 2.1: Fishing management areas in the Bay of Fundy (ggmap package error)

#BayofFundy =st_read("Bay of Fundy.shp")
#bbox1 <- c(left = min(myheight$lon)-1.5, bottom = min(myheight$lat)-1.5, 
           #right = max(myheight$lon)+1.5, top = max((myheight$lat)+1.5))
#BD2=BayofFundy%>% dplyr::select(SP_ID, geometry)
#ggmap(get_stamenmap(bbox1, maptype = "terrain-background")) + 
  #coord_sf(crs = hst_crs(3857)) + 
  #geom_point(data = hstudy_set, aes(x = lon, y = lat), 
             #size = 0.5, color = "pink")+
  #geom_sf(data =  BD2, fill = NA,inherit.aes = FALSE)+
  #theme_bw() + 
  #labs(x = "Lon", y = "Lat")





# Figure 2.2: 2012-2019 MWSH survey data by month

f1=ggplot(scallops[scallops$month==6,], aes(x= day   )) + 
  geom_histogram(color="black", fill="white",xlab="June")+    ggtitle("June")

f2<-ggplot(scallops[scallops$month==7,], aes(x =day  )) +
  geom_histogram(color="black", fill="white",xlab="July")+    ggtitle("July")

grid.arrange(f1,f2, nrow = 1, ncol=2)



# Figure 2.3: MWSH survey tow locations in the Bay of Fundy across 2012-2019 (ggmap package error)

#bbox <- c(left = min(mydata$lon)-0.1, bottom = min(mydata$lat)-0.1, right = max(mydata$lon)+0.1, top = max((mydata$lat)+0.1))
#latitude =    mydata$lat    
#longitude = mydata$lon   
#year=as.factor(mydata$year)
#site_df = as.data.frame(cbind(latitude,longitude, year))

#site_map = ggmap(get_stamenmap(bbox, maptype = "terrain-background"))+
  #geom_point(data = site_df, aes(x = longitude, y = latitude), 
             #size = 0.1, color = "red")+
  #geom_point(data = site_df, aes(x = longitude, y = latitude), 
             #pch= 21, size = 0.1, color = "red")+ facet_wrap(year ~ ., ncol = 4)+
  #theme_bw() + 
  #labs(x = "Lon", y = "Lat")

#sbbox <- make_bbox(lon = mydata$lon, lat = mydata$lat, f = .1)
#sq_map <- get_map(location = sbbox, maptype = "satellite", source = "google")
#ggmap(sq_map) + geom_point(data = mydata, mapping = aes(x = lon, y = lat), color = "red")+ 
  #facet_wrap(year ~ ., ncol = 4)






# Figure 2.4: SH observations of the sampled sea scallop tow locations in the Bay of Fundy across 2012-2019. (ggmap error)
#sbbox <- make_bbox(lon = myheight$lon, lat = myheight$lat, f = .1)
#sq_map <- get_map(location = sbbox, maptype = "satellite", source = "google")
#ggmap(sq_map) + geom_point(data = myheight, mapping = aes(x = lon, y = lat), color = "red")+
  #facet_wrap(year ~ ., ncol = 4)

#bbox <- c(left = min(myheight$lon)-1, bottom = min(myheight$lat)-1, right = max(myheight$lon)+1, top = max((myheight$lat)+1))

#latitude =    myheight$lat    
#longitude = myheight$lon   
#year=as.factor(myheight$year)
#site_df = as.data.frame(cbind(latitude,longitude, year))
#site_map = ggmap(get_stamenmap(bbox, maptype = "terrain-background"))+
  #geom_point(data = site_df, aes(x = longitude, y = latitude), 
             #size = 0.1, color = "red")+
  #geom_point(data = site_df, aes(x = longitude, y = latitude), 
             #pch= 21, size = 0.1, color = "red")+ facet_wrap(year ~ ., ncol = 4)+
  #theme_bw() + 
  #labs(x = "Lon", y = "Lat")


# Figure 2.5: BNAM temperature, stress, and salinity.

# create base map used for ggplot
base_map <- ggplot() +
  borders("world", colour="gray50", fill = "gray90", 
          xlim = c(-66.24-1 ,-64.49+1), ylim = c(44.42-1,45.52+1   )) +
  coord_map(xlim = c(-66.24-1 ,-64.49+1), ylim = c(44.42-1,45.52 +1  )) +
  theme_bw() +
  scale_color_continuous(low = "white", high = "red") +
  scale_size_continuous(guide = FALSE) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        legend.position = "bottom")


base_map + 
  geom_point(data = hstudy_set, aes(x=lon, y=lat, color = temperature),
             shape = 20,size=0.1) +
  scale_color_gradientn(colours = rainbow(7),oob=scales::squish)+ 
  facet_grid(~year) + labs(colour = "Bottom temperature (°C)")

base_map + 
  geom_point(data = hstudy_set, aes(x=lon, y=lat, color = stress),
             shape = 20,size=0.1) +
  scale_color_gradientn(colours = rainbow(7),oob=scales::squish)+ 
  facet_grid(~year) + 
  labs(colour = expression("Bottom stress"* " (kg."*"m"^-1*".s"^-2*")"))

base_map + 
  geom_point(data = hstudy_set, aes(x=lon, y=lat, color =  salinity),
             shape = 20,size=0.1) +
  scale_color_gradientn(colours = rainbow(7),oob=scales::squish)  + 
  facet_grid(~year) + labs(colour = "Bottom salinity (psu)")


# Figure 2.6: DEM depth.

base_map + 
  geom_point(data = hstudy_set, aes(x=lon, y=lat, color =  depth),shape = 20,
             size=0.5) +
  scale_color_gradientn(colours = rainbow(7),oob=scales::squish)+ 
  labs(colour = "Depth (m)")


# Figure 2.7: The LWRs for scallop meat weights and shell heights in the MWSH dataset.

h1<-ggplot(mydata, aes(x =height  , y =   weight)) +
  geom_point() +labs(x = "Height ", y = "Weight") +
  geom_smooth(method = "lm")

h2<-ggplot(mydata, aes(x =log(height)  , y =  log(weight))) +
  geom_point() +labs(x = "log Height ", y = "log Weight") +
  geom_smooth(method = "lm")

grid.arrange(h1,h2, nrow = , ncol=2)


# Figure 2.8: Weights versus environmental variables in the MWSH dataset.

h3<-ggplot(mydata, aes(x =( temperature) , y =  weight)) +
  geom_point() +labs(x = "Temperature ", y = "Weight" )

h4<-ggplot(mydata, aes(x = depth , y =   weight)) +
  geom_point() +labs(x = "Depth ", y = "Weight")


h5<-ggplot(mydata, aes(x = stress , y =   weight)) +
  geom_point() +labs(x = "Stress ", y = "Weight")


h6<-ggplot(mydata, aes(x = salinity , y =   weight)) +
  geom_point() +labs(x = "Salinity ", y = "Weight")

grid.arrange(h3,h4,h5,h6, nrow = 2, ncol=2)


# Figure 2.9: Heights versus environmental variables in the SH dataset

h3<-ggplot(myheight, aes(x =( temperature) , y =  height    )) +
  geom_point() +labs(x = "Temperature ", y = "Height" )

h4<-ggplot(myheight, aes(x = depth , y =   height  )) +
  geom_point() +labs(x = "Depth ", y = "Height")

h5<-ggplot(myheight, aes(x = stress , y =   height  )) +
  geom_point() +labs(x = "Stress ", y = "Height")

h6<-ggplot(myheight, aes(x = salinity , y =   height  )) +
  geom_point() +labs(x = "Salinity ", y = "Height")

grid.arrange(h3,h4,h5,h6, nrow = 2, ncol=2)




# Figure 2.10: Weights versus environmental variables after log transformations in the MWSH dataset.
h3<-ggplot(mydata, aes(x =log( temperature) , y =  log(weight))) +
  geom_point() +labs(x = "log Temperature ", y = "log Weight ")

h4<-ggplot(mydata, aes(x = log(depth) , y =  log(weight))) +
  geom_point() +labs(x = "log Depth ", y = "log Weight")


h5<-ggplot(mydata, aes(x = log(stress) , y =   log(weight))) +
  geom_point() +labs(x = "log Stress ", y = "log Weight")


h6<-ggplot(mydata, aes(x = log(salinity) , y =  log(weight))) +
  geom_point() +labs(x = "log Salinity ", y = "log Weight")

grid.arrange(h3,h4,h5,h6, nrow = 2, ncol=2)



# Figure 2.11: Histograms of weight and log weight from the MWSH dataset.

w1=ggplot(mydata, aes(x=weight)) + 
  geom_histogram(aes(y=..density..),     
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") 

w2=ggplot(mydata, aes(x=log(weight))) + 
  geom_histogram(aes(y=..density..),     
                 binwidth=.05,
                 color="black",  fill="white") +
  geom_density(alpha=.2,color="black", fill="#FF6666") 
grid.arrange(w1,w2, nrow = 1, ncol=2)


# Figure 2.12: Histogram of height from the SH dataset.

ggplot(myheight, aes(x=height)) + 
  geom_histogram(aes(y=..density..),      
                 binwidth=.5,
                 color="black",  fill="white") +
  geom_density(alpha=.2,color="black", fill="#FF6666") 



# Figure 2.13: Mean meat weights calculated from the MWSH dataset. (Zoom in and make it wider and save it: width=1911&height=899&scale=1)

mydata=mydata%>%
  group_by( ID_TOW  ) %>% 
  mutate(mean_weight=mean(weight,na.rm=T))

base_map + 
  geom_point(data = mydata, aes(x=lon, y=lat, color = mean_weight),
             shape = 20,size=0.05) +
  scale_color_gradientn(colours = rainbow(7), limits=c(3,50),
                        oob=scales::squish)  + 
  labs(colour = "The mean meat weight (g) in each tow location")+
  facet_grid(~year) 


# Figure 2.14: Mean shell heights calculated from the SH dataset (width=1911&height=899&scale=1)

myheight=myheight%>%
  group_by(ID_TOW) %>% 
  mutate(mean_height=mean(height,na.rm=T))

base_map + 
  geom_point(data = myheight, aes(x=lon, y=lat, color = mean_height),
             shape = 20,size=0.05) +
  scale_color_gradientn(colours = rainbow(7), limits=c(30,160),
                        oob=scales::squish)  + 
  labs(colour = "The mean shell height (mm) in each tow location")+
  facet_grid(~year) 

# Figure 2.15: The PCCs between environmental variables in the MWSH dataset.

par(mfrow=c(1,2))

corr_matrix <- mydata[,c('depth','temperature','stress','salinity')] %>% 
  cor(method="pearson", use="pairwise.complete.obs")

corrplot(corr_matrix ,method="color", addCoef.col = "black", 
         mar=c(0,0,5,0), tl.offset = 1)
mtext("Environmental variable correlations", at=2.5, line=-0.5, cex=0.8)

corr_matrix <- all_set[,c('depth','temperature','stress','salinity')] %>% 
  cor(method="pearson", use="pairwise.complete.obs")

corrplot(corr_matrix ,method="color", addCoef.col = "black", 
         mar=c(0,0,5,0), tl.offset = 1)
mtext("Environmental variable correlations after transformations", 
      at=2.5, line=-0.4, cex=0.8)



corr_matrix <- mydata[,c('depth','temperature','salinity')] %>% 
  cor(method="pearson", use="pairwise.complete.obs")

corrplot(corr_matrix ,method="color", addCoef.col = "black", 
         mar=c(0,0,5,0), tl.offset = 1)
mtext("Environmental variable correlations", at=2.5, line=-0.5, cex=0.8)

corr_matrix <- all_set[,c('depth','temperature','salinity')] %>% 
  cor(method="pearson", use="pairwise.complete.obs")

corrplot(corr_matrix ,method="color", addCoef.col = "black", 
         mar=c(0,0,5,0), tl.offset = 1)
mtext("Environmental variable correlations after transformations", 
      at=2.5, line=-0.4, cex=0.8)


#Figure 2.16: The PCCs between environmental variables in the SH dataset.

par(mfrow=c(1,1))

corr_matrix <- myheight[,c('depth','temperature','stress','salinity')] %>% 
  cor(method="pearson", use="pairwise.complete.obs")

corrplot(corr_matrix ,method="color", addCoef.col = "black", 
         mar=c(0,0,5,0), tl.offset = 1)
mtext("Environmental variable correlations", at=2.5, line=-0.5, cex=0.8)





