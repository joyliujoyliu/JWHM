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
# Contains Fig 4.1, 4.3, 4.5, 4.8, B2. Tab A2

# Run Part 1 first, the JWHM needs the height prediction results from Part 1

# dataset preparations

scallops=read.csv("bof.mwsh.JuneJuly.2012to2019.clean.csv")
scallops=scallops%>% filter(month ==7)
scallops = scallops[!scallops$CRUISE == "RF2012",]
mydata= scallops%>% transmute(weight = WET_MEAT_WGT, 
                              height = HEIGHT,
                              year = as.factor(year),
                              TOW_NO =TOW_NO ,
                              lon= mid.lon, lat= mid.lat 
)

draster=raster("BathyCHS_GEBCO_SEAM_mixedData_BOF_ExtentClip_100m_LatLong.asc")
mydata$depth=raster::extract(draster, y = cbind(mydata$lon , mydata$lat))

obs1=mydata%>% filter(year ==2012)
obs2=mydata%>% filter(year ==2013)
obs3=mydata%>% filter(year ==2014)
obs4=mydata%>% filter(year ==2015)
obs5=mydata%>% filter(year ==2016)
obs6=mydata%>% filter(year ==2017)
obs7=mydata%>% filter(year ==2018)
obs8=mydata%>% filter(year ==2019)

a2012=raster("BtmSalinity_Jul_2012.asc")
a2013=raster("BtmSalinity_Jul_2013.asc")
a2014=raster("BtmSalinity_Jul_2014.asc")
a2015=raster("BtmSalinity_Jul_2015.asc")
a2016=raster("BtmSalinity_Jul_2016.asc")
a2017=raster("BtmSalinity_Jul_2017.asc")
a2018=raster("BtmSalinity_Jul_2018.asc")
a2019=raster("BtmSalinity_Jul_2019.asc")

obs1$salinity=raster::extract(a2012, y = cbind(obs1$lon, obs1$lat))
obs2$salinity=raster::extract(a2013, y = cbind(obs2$lon, obs2$lat))
obs3$salinity=raster::extract(a2014, y = cbind(obs3$lon, obs3$lat))
obs4$salinity=raster::extract(a2015, y = cbind(obs4$lon, obs4$lat))
obs5$salinity=raster::extract(a2016, y = cbind(obs5$lon, obs5$lat))
obs6$salinity=raster::extract(a2017, y = cbind(obs6$lon, obs6$lat))
obs7$salinity=raster::extract(a2018, y = cbind(obs7$lon, obs7$lat))
obs8$salinity=raster::extract(a2019, y = cbind(obs8$lon, obs8$lat))

mydata$salinity=c(obs1$salinity,obs2$salinity,obs3$salinity,obs4$salinity,obs5$salinity,obs6$salinity,obs7$salinity,obs8$salinity)

r2012=raster("BtmTemp_Jul_2012.asc")
r2013=raster("BtmTemp_Jul_2013.asc")
r2014=raster("BtmTemp_Jul_2014.asc")
r2015=raster("BtmTemp_Jul_2015.asc")
r2016=raster("BtmTemp_Jul_2016.asc")
r2017=raster("BtmTemp_Jul_2017.asc")
r2018=raster("BtmTemp_Jul_2018.asc")
r2019=raster("BtmTemp_Jul_2019.asc")

obs1$temperature=raster::extract(r2012, y = cbind(obs1$lon, obs1$lat))
obs2$temperature=raster::extract(r2013, y = cbind(obs2$lon, obs2$lat))
obs3$temperature=raster::extract(r2014, y = cbind(obs3$lon, obs3$lat))
obs4$temperature=raster::extract(r2015, y = cbind(obs4$lon, obs4$lat))
obs5$temperature=raster::extract(r2016, y = cbind(obs5$lon, obs5$lat))
obs6$temperature=raster::extract(r2017, y = cbind(obs6$lon, obs6$lat))
obs7$temperature=raster::extract(r2018, y = cbind(obs7$lon, obs7$lat))
obs8$temperature=raster::extract(r2019, y = cbind(obs8$lon, obs8$lat))

mydata$temperature=c(obs1$temperature,obs2$temperature,obs3$temperature,obs4$temperature,obs5$temperature,obs6$temperature,obs7$temperature,obs8$temperature)

s2012=raster("BtmStress_Jul_2012.asc")
s2013=raster("BtmStress_Jul_2013.asc")
s2014=raster("BtmStress_Jul_2014.asc")
s2015=raster("BtmStress_Jul_2015.asc")
s2016=raster("BtmStress_Jul_2016.asc")
s2017=raster("BtmStress_Jul_2017.asc")
s2018=raster("BtmStress_Jul_2018.asc")
s2019=raster("BtmStress_Jul_2019.asc")

obs1$stress=raster::extract(s2012, y = cbind(obs1$lon, obs1$lat))
obs2$stress=raster::extract(s2013, y = cbind(obs2$lon, obs2$lat))
obs3$stress=raster::extract(s2014, y = cbind(obs3$lon, obs3$lat))
obs4$stress=raster::extract(s2015, y = cbind(obs4$lon, obs4$lat))
obs5$stress=raster::extract(s2016, y = cbind(obs5$lon, obs5$lat))
obs6$stress=raster::extract(s2017, y = cbind(obs6$lon, obs6$lat))
obs7$stress=raster::extract(s2018, y = cbind(obs7$lon, obs7$lat))
obs8$stress=raster::extract(s2019, y = cbind(obs8$lon, obs8$lat))

mydata$stress=c(obs1$stress,obs2$stress,obs3$stress,obs4$stress,obs5$stress,obs6$stress,obs7$stress,obs8$stress)

# MWSH dataset

mydata= mydata%>%filter(
  !is.na(depth),
  !is.na(temperature),
  !is.na(stress),
  !is.na(salinity)
) %>% transmute(weight = weight, 
                height = height,
                year = as.factor(year),
                lon= lon, lat= lat , 
                ID_TOW = as.factor(paste(year,TOW_NO,sep = '_')),
                depth = -depth ,
                temperature = temperature,
                stress=stress,
                salinity=salinity)


# UTM

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
mg$height=c(rep((mean(mydata$height) )))
mg$temperature=c(mg1$temperature,mg2$temperature,mg3$temperature,mg4$temperature,mg5$temperature,mg6$temperature,mg7$temperature,mg8$temperature)
mg$salinity=c(mg1$salinity,mg2$salinity,mg3$salinity,mg4$salinity,mg5$salinity,mg6$salinity,mg7$salinity,mg8$salinity)
mg$stress=c(mg1$stress,mg2$stress,mg3$stress,mg4$stress,mg5$stress,mg6$stress,mg7$stress,mg8$stress)
mg$depth=raster::extract(draster, y = cbind(mg$lon, mg$lat))

# In the mg the height is 114
mg= mg%>%filter(
  !is.na(depth),
  !is.na(temperature),
  !is.na(stress),
  !is.na(salinity)
) %>% transmute(
  height = height,
  year = as.factor(year),
  lon= lon, lat= lat , 
  depth = -depth ,
  temperature = temperature,
  stress=stress,
  salinity=salinity)
mg=mg[mg$depth>0,]


# In the study_set the height is a constant
study_set <- mg %>% 
  mutate(height = as.numeric((log(height))),
         depth = as.numeric((log(depth))),
         temperature = as.numeric((log(temperature))),
         stress = as.numeric((log(stress))),
         salinity = as.numeric((log(salinity)))) %>%
  mutate(X=lon, Y=lat) %>%
  `attr<-`("projection", "LL") %>%
  `attr<-`("zone", "20") %>%
  PBSmapping::convUL() 
study_set=na.omit(study_set)


# Figure 4.1: The Delaunay triangulation used for the GMRF presence of the WMC.

mesh1 = inla.mesh.create(as.matrix(all_set[,c("X","Y")]), refine = F, extend = F)
plot(mesh1, family = "serif", cex.main = 2, main = "")
points(cbind(all_set$X, all_set$Y), col = "orange", cex = 0.4)
mesh <- make_mesh(all_set, xy_cols = c("X", "Y"),mesh=mesh1)


# Figure 4.3: The RQR plots for the wMC.

weight_sdm <- sdmTMB(
  weight ~ year+height,
  family = gaussian(link = "log"), data = all_set, mesh = mesh,
  time = "year", spatial = "on", spatiotemporal = "iid",
  share_range=FALSE
)

weight_sdmt <- sdmTMB(
  weight ~ year+height,
  family = student(link = "log",df=2), data = all_set, mesh = mesh,
  time = "year", spatial = "on", spatiotemporal = "iid",
  share_range=FALSE
)


rq_res1 <- residuals(weight_sdm)
rq_res1 <- rq_res1[is.finite(rq_res1)]
qqnorm(rq_res1,ylim = c(-8,8))
qqline(rq_res1)  # Crop the title in PPT

rq_res2 <- residuals(weight_sdmt)
rq_res2 <- rq_res2[is.finite(rq_res2)]
qqnorm(rq_res2,xlab="Theoretical for t distribution with df=2",ylim = c(-8,8))
qqline(rq_res2)  # Crop the title in PPT


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


# Figure 4.5: Spatial distributions of observations from the MWSH dataset across 10 folds by stratified sampling

ggscatter(
  all_set, x = "lon", y = "lat", 
  color = "foldID",  ellipse = TRUE, ellipse.type = "convex", shape="year",
  size = 1.5,  legend = "right", ggtheme = theme_bw(),
  xlab = paste0("lon" ),
  ylab = paste0("lat" )
) +facet_grid(~foldID)


# STM-D

weight_sdmtD <- sdmTMB(
weight ~ year+height+depth,
  family = student(link = "log",df=2), data = all_set, mesh = mesh,
  time = "year", spatial = "on", spatiotemporal = "iid",
  share_range=FALSE
)


# WMC with fixed height

Studyweightt2STMD<- predict(weight_sdmtD,study_set,type = "response",re_form = NULL)
write.csv(Studyweightt2STMD,"Studyweightt2STMD.csv")
Studyweightt2STMD=read.csv("Studyweightt2STMD.csv")

# JWHM with predicted heights from the HMC

Studyheightt2STMD=read.csv("Studyheightt2STMD.csv") # Run Part 1 first
colnames(Studyheightt2STMD)[colnames(Studyheightt2STMD) == 'est'] <- 'height'
Studyheightt2STMD=Studyheightt2STMD[,c(2:11)]
Studyheightt2STMD$year=as.factor(Studyheightt2STMD$year)
Studyheightt2STMD=Studyheightt2STMD%>% 
  mutate(height = as.numeric((log(height))),
         depth = as.numeric((log(depth))),
         temperature = as.numeric((log(temperature))),
         stress = as.numeric((log(stress))),
         salinity = as.numeric((log(salinity)))) %>%
  mutate(X=lon, Y=lat) %>%
  `attr<-`("projection", "LL") %>%
  `attr<-`("zone", "20") %>%
  PBSmapping::convUL() 
Studyheightt2STMD=na.omit(Studyheightt2STMD)
Studywh2STMD<- predict(weight_sdmtD,Studyheightt2STMD,type = "response",re_form = NULL) 
write.csv(Studywh2STMD,"Studywh2STMD.csv")

pred.interpolation <- data.frame(
  "lon"=Studyweightt2STMD$lon,
  "lat"=Studyweightt2STMD$lat,
  "Year"=Studyweightt2STMD$year,
  "weightSTMD"=Studyweightt2STMD$est
) 

whpred.interpolation <- data.frame(
  "lon"=Studywh2STMD$lon,
  "lat"=Studywh2STMD$lat,
  "Year"=Studywh2STMD$year,
  "STMD"=Studywh2STMD$est
) 

# meat weight prediction comparison
pred.interpolation.comp <- data.frame(
  "lon"=pred.interpolation$lon,
  "lat"= pred.interpolation$lat,
  "Year"=pred.interpolation$Year,
  "WMC"=pred.interpolation$weightSTMD ,
  "JWHM"=whpred.interpolation$STMD
) %>%
  gather(model,pmw,-lon,-lat,-Year) %>%
  mutate(model = factor(model, 
                        levels = c("WMC", "JWHM")))


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


# Figure 4.8: Comparison of meat weight predictions and standard errors between the WMC and JWHM.
# Run Part 1 first

base_map + 
  geom_point(data = pred.interpolation.comp, aes(x=lon, y=lat, color = pmw), shape=20,  size =0.05, alpha=0.5) +
  scale_color_gradientn(colours = rainbow(7),oob=scales::squish) +
  facet_grid(model~Year) +
  labs(colour = "Predicted meat weights (g)")


pw <-predict(weight_sdmtD,type = "response",re_form = NULL, newdata = study_set, nsim = 500)
wpredictor_dat=study_set
wpredictor_dat$se<- apply(pw, 1, sd)
colnames(wpredictor_dat)[11] <- "WMC"


pwh <-predict(weight_sdmtD,type = "response",re_form = NULL, newdata = Studyheightt2STMD, nsim = 500)
whpredictor_dat=Studyheightt2STMD
whpredictor_dat$se<- apply(pwh, 1, sd)
colnames(whpredictor_dat)[11] <- "JWHM"

sedata <- data.frame(
  "lon"=whpredictor_dat$lon,
  "lat"=whpredictor_dat$lat,
  "Year"=whpredictor_dat$year,
  "WMC"=wpredictor_dat$WMC ,
  "JWHM"=whpredictor_dat$JWHM
) %>%
  gather(model,se,-lon,-lat,-Year) %>%
  mutate(model = factor(model, 
                        levels = c("WMC", "JWHM")))

base_map + 
  geom_point(data = sedata, aes(x=lon, y=lat, color = se), shape=20,  size =0.05, alpha=0.5) +
  scale_color_gradientn(colours = rainbow(7),oob=scales::squish) +
  facet_grid(model~Year) +
  labs(colour = "Standard errors of the predicted meat weight")


# Table A.2: WMC parameter estimation.

summary(weight_sdmtD$sd_report, select = "fixed", p.value = TRUE) 


# Figure B.2: Visualization of the spatial, and spatiotemporal effects with their sum for the WMC.

base_map  + 
  geom_point(aes(x =  lon , y = lat ,colour =(omega_s )),   size = 0.1, data =Studyweightt2STMD , alpha =.5)  + scale_color_continuous()+
  theme(text=element_text(size=11, family="serif"))+ scale_color_gradient2(low = "blue", high = "red", mid = "white") + ggtitle("Spatial effect")

base_map +facet_wrap(year ~ ., ncol = 8) + 
  geom_point(aes(x =  lon , y = lat ,colour =(epsilon_st )),   size = 0.1, data =Studyweightt2STMD , alpha =.5)  + scale_color_continuous()+
  theme(text=element_text(size=11, family="serif"))+ scale_color_gradient2(low = "blue", high = "red", mid = "white") + ggtitle("Spatiotemporal effect")

base_map  + facet_wrap(year ~ ., ncol = 8) +
  geom_point(aes(x =  lon , y = lat ,colour =( est_rf  )),   size = 0.1, data =Studyweightt2STMD , alpha =.5)  + scale_color_continuous()+
  theme(text=element_text(size=11, family="serif"))+ scale_color_gradient2(low = "blue", high = "red", mid = "white") + ggtitle("Spatial + Spatiotemporal effect")





