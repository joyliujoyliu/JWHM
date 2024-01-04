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

rm(list = ls(all.names = TRUE))

# Contains Fig 4.2, 4.4, 4.6, 4.7, B1. Tab A1


# dataset preparations
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

heightdata$ID_TOW = as.factor(paste(heightdata$year,heightdata$TOW_NO,sep = '_'))

set.seed(1)

test=c()

binocoun=matrix(0,  length(heightdata[,1]),40)

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

Julyfullset$temperature=NA
Julyfullset$salinity=NA
Julyfullset$stress=NA
Julyfullset$depth=raster::extract(draster, y = cbind(Julyfullset$mid.lon , Julyfullset$mid.lat))

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

Julyfullset$temperature=c(h1$temperature,h2$temperature,h3$temperature,h4$temperature,h5$temperature,h6$temperature,h7$temperature,h8$temperature)
Julyfullset$stress=c(h1$stress,h2$stress,h3$stress,h4$stress,h5$stress,h6$stress,h7$stress,h8$stress)
Julyfullset$salinity=c(h1$salinity,h2$salinity,h3$salinity,h4$salinity,h5$salinity,h6$salinity,h7$salinity,h8$salinity)

# SH daaset

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
  stress=stress)

# UTM

all_set <- myheight %>% 
  mutate(
    depth = depth,
    temperature = temperature,
    salinity=salinity,
    stress=stress) %>%
  mutate(X=lon, Y=lat) %>%
  `attr<-`("projection", "LL") %>%
  `attr<-`("zone", "20") %>%
  PBSmapping::convUL() 


# study_set

studyarea =readOGR(dsn = path.expand("BoF_Strata_extent4Joy.shp"), 
                   layer = "BoF_Strata_extent4Joy")
st_read("BoF_Strata_extent4Joy.shp")
shape4 <- readOGR(dsn = path.expand("SPA5_polygon_NAD83.shp"), 
                  layer = "SPA5_polygon_NAD83")
studyarea <- union(studyarea,shape4)
writeSpatialShape(studyarea, "studyarea")
studyarea =st_read("studyarea.shp")

sf_use_s2(FALSE)
set.seed(100)
nc_point <- st_sample(x = studyarea, size = 100000)
nc_point=as.matrix(nc_point)
nc_point <- do.call(rbind, st_geometry(nc_point)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))

points=as.data.frame(nc_point)%>% 
  mutate(X=lon, Y=lat) %>%
  `attr<-`("projection", "LL") %>%
  `attr<-`("zone", "20") %>%
  PBSmapping::convUL() 
points=as.matrix(points)

mg=as.data.frame(nc_point)
mg$temperature=NA
mg$salinity=NA
mg$stress=NA
mg1=mg
mg2=mg
mg3=mg
mg4=mg
mg5=mg
mg6=mg
mg7=mg
mg8=mg

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

rows= c(1:nrow(mg))
times = 8
mg=mg[rep(rows, times),]

mg$year=c(rep(2012,100000),rep(2013,100000),rep(2014,100000),rep(2015,100000),rep(2016,100000),rep(2017,100000),rep(2018,100000),rep(2019,100000))
mg$temperature=c(mg1$temperature,mg2$temperature,mg3$temperature,mg4$temperature,mg5$temperature,mg6$temperature,mg7$temperature,mg8$temperature)
mg$salinity=c(mg1$salinity,mg2$salinity,mg3$salinity,mg4$salinity,mg5$salinity,mg6$salinity,mg7$salinity,mg8$salinity)
mg$stress=c(mg1$stress,mg2$stress,mg3$stress,mg4$stress,mg5$stress,mg6$stress,mg7$stress,mg8$stress)
mg$depth=raster::extract(draster, y = cbind(mg$lon, mg$lat))

mg= mg%>%filter(
  !is.na(depth),
  !is.na(temperature),
  !is.na(stress),
  !is.na(salinity)
) %>% transmute(
  year = as.factor(year),
  lon= lon, lat= lat , 
  depth = depth ,
  temperature = temperature,
  stress=stress,
  salinity=salinity)

study_set <- mg %>% 
  mutate(
    depth = as.numeric(((-depth))),
    temperature = as.numeric(((temperature))),
    stress = as.numeric(((stress))),
    salinity = as.numeric(((salinity)))) %>%
  mutate(X=lon, Y=lat) %>%
  `attr<-`("projection", "LL") %>%
  `attr<-`("zone", "20") %>%
  PBSmapping::convUL() 
study_set=na.omit(study_set)
study_set=study_set[study_set$depth>0,]



# Figure 4.2: The Delaunay triangulation used for the GMRF presence of the HMC.

mesh1 = inla.mesh.create(as.matrix(all_set[,c("X","Y")]), refine = F, extend = F)
plot(mesh1, family = "serif", cex.main = 2, main = "")
points(cbind(all_set$X, all_set$Y), col = "orange", cex = 0.4)
mesh <- make_mesh(all_set, xy_cols = c("X", "Y"),mesh=mesh1)  # add your boundary points by PPT



# Figure 4.4: The RQR plots for the HMC.

fit_sdm<- sdmTMB(
  height ~ year,
  family = gaussian(link = "identity"), data = all_set, mesh = mesh,
  time = "year", spatial = "on", spatiotemporal = "iid",
  share_range=FALSE
)

fit_sdmt <- sdmTMB(
  height ~ year,
  family = student(link = "identity",df=2), data = all_set, mesh = mesh,
  time = "year", spatial = "on", spatiotemporal = "iid",
  share_range=FALSE
)

rq_res1 <- residuals(fit_sdm)
rq_res1 <- rq_res1[is.finite(rq_res1)]
qqnorm(rq_res1,ylim = c(-8,4))
qqline(rq_res1) # Crop the title in PPT

rq_res2 <- residuals(fit_sdmt)
rq_res2 <- rq_res2[is.finite(rq_res2)]
qqnorm(rq_res2,xlab="Theoretical for t distribution with df=2",ylim = c(-8,4))
qqline(rq_res2) # Crop the title in PP


set.seed(111)
list_tow <- unique(all_set$ID_TOW)
folds_tow <- caret::createFolds(list_tow, k = 10, list = T, returnTrain = F)
folds <- lapply(folds_tow, function(x) which(all_set$ID_TOW %in% list_tow[x]))

all_set$foldID=NA
all_set$obs=c(1:length(all_set[,1]))
all_set$foldID[which(all_set$obs %in% folds$Fold01)]=1
all_set$foldID[which(all_set$obs %in% folds$Fold02)]=2
all_set$foldID[which(all_set$obs %in% folds$Fold03)]=3
all_set$foldID[which(all_set$obs %in% folds$Fold04)]=4
all_set$foldID[which(all_set$obs %in% folds$Fold05)]=5
all_set$foldID[which(all_set$obs %in% folds$Fold06)]=6
all_set$foldID[which(all_set$obs %in% folds$Fold07)]=7
all_set$foldID[which(all_set$obs %in% folds$Fold08)]=8
all_set$foldID[which(all_set$obs %in% folds$Fold09)]=9
all_set$foldID[which(all_set$obs %in% folds$Fold10)]=10
all_set$foldID=as.factor(all_set$foldID)

# Figure 4.6: Spatial distributions of observations from the SH dataset across 10 folds

ggscatter(
  all_set, x = "lon", y = "lat", 
  color = "foldID",  ellipse = TRUE, ellipse.type = "convex", shape="year",
  size = 1.5,  legend = "right", ggtheme = theme_bw(),
  xlab = paste0("lon" ),
  ylab = paste0("lat" )
) +facet_grid(~foldID) # zoom in and adjust the size


# STM-D
fit_sdmtD<- sdmTMB(
  height ~ year+depth,
  family = student(link = "identity", df = 2), data = all_set, mesh = mesh,
  time = "year", spatial = "on", spatiotemporal = "iid",
  share_range=FALSE
)



# Base map
base_map <- ggplot() +
  borders("world", colour="gray50", fill = "gray90", xlim = c(-66.24-1 ,-64.49+1), ylim = c(44.42-1,45.52+1   )) +
  coord_map(xlim = c(-66.24-1 ,-64.49+1), ylim = c(44.42-1,45.52 +1  )) +
  theme_bw() +
  scale_color_continuous(low = "white", high = "red") +
  scale_size_continuous(guide = FALSE) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        legend.position = "bottom")

Studyheightt2STMD<- predict(fit_sdmtD,study_set,type = "response",re_form = NULL) 
write.csv(Studyheightt2STMD,"Studyheightt2STMD.csv") # save it here, will use it in the Ch4 Part 2
Studyheightt2STMD=read.csv("Studyheightt2STMD.csv")

pred.interpolation <- data.frame(
  "lon"=Studyheightt2STMD$lon,
  "lat"=Studyheightt2STMD$lat,
  "Year"=Studyheightt2STMD$year,
  "STMD"=Studyheightt2STMD$est
)
pred.interpolation=na.omit(pred.interpolation)


# Figure 4.7: Shell height predictions and standard errors from the HMC

base_map + 
  geom_point(data = pred.interpolation, aes(x=lon, y=lat, color =STMD), shape=20,  size =0.05, alpha=0.5) +
  scale_color_gradientn(colours = rainbow(7),oob=scales::squish) +
  facet_grid(~Year) +
  labs(colour = "Predicted shell heights (mm) from the HMC")

p <-predict(fit_sdmtD,type = "response",re_form = NULL,newdata=study_set, nsim = 500)
predictor_dat=study_set
predictor_dat$se<- apply(p, 1, sd)

base_map + 
  geom_point(data = predictor_dat, aes(x=lon, y=lat, color =  se   ),shape = 20,size=0.05) +
  scale_color_gradientn(colours = rainbow(7),oob=scales::squish)  + 
  facet_grid(~year) + labs(colour = "Standard errors of the predicted shell heights from the HMC")


# Table A.1: HMC parameter estimation.

summary(fit_sdmtD$sd_report, select = "fixed", p.value = TRUE) 



# Figure B.1: Visualization of the spatial, and spatiotemporal effects with their sum for the HMC.

base_map  + 
  geom_point(aes(x =  lon , y = lat ,colour =(omega_s )),   size = 0.1, data =Studyheightt2STMD , alpha =.5)  + scale_color_continuous()+
  theme(text=element_text(size=11, family="serif"))+ scale_color_gradient2(low = "blue", high = "red", mid = "white") + ggtitle("Spatial effect")

base_map +facet_wrap(year ~ ., ncol = 8) + 
  geom_point(aes(x =  lon , y = lat ,colour =(epsilon_st )),   size = 0.1, data =Studyheightt2STMD , alpha =.5)  + scale_color_continuous()+
  theme(text=element_text(size=11, family="serif"))+ scale_color_gradient2(low = "blue", high = "red", mid = "white") + ggtitle("Spatiotemporal effect")

base_map  + facet_wrap(year ~ ., ncol = 8) +
  geom_point(aes(x =  lon , y = lat ,colour =( est_rf  )),   size = 0.1, data =Studyheightt2STMD , alpha =.5)  + scale_color_continuous()+
  theme(text=element_text(size=11, family="serif"))+ scale_color_gradient2(low = "blue", high = "red", mid = "white") + ggtitle("Spatial + Spatiotemporal effect")
