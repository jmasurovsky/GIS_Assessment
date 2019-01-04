install.packages("shiny")
library(shiny)
shinyjs::runExample('sandbox')

qtm(OpportunityMap, fill = "AvgGCSE", fill.palette="YlOrRd", midpoint=NA)
qtm(OpportunityMap, fill = "PctLvl4Qual", fill.palette="YlOrRd", midpoint=NA)
qtm(OpportunityMap, fill = "UnauthAbsc", fill.palette="YlOrRd", midpoint=NA)
qtm(OpportunityMap, fill = "Employment", fill.palette="YlOrRd", midpoint=NA)
qtm(OpportunityMap, fill = "JSA_Claim", fill.palette="YlOrRd", midpoint=NA)
qtm(OpportunityMap, fill = "PctHousingOwned", fill.palette="YlOrRd", midpoint=NA)
qtm(OpportunityMap, fill = "CrimRate", fill.palette="YlOrRd", midpoint=NA)
qtm(OpportunityMap, fill = "No_Jobs", fill.palette="YlOrRd", midpoint=NA)
tm_shape(OpportunityMap) +
  tm_polygons("AvgGCSE",
              style="quantile",
              breaks=breaks_clusters1,
              palette="YlOrRd",
              midpoint=NA,
              popup.vars = "WardName",
              title="AvgGCSE scores")

library(maptools)
library(RColorBrewer)
library(classInt)
library(OpenStreetMap)
library(sp)
library(rgeos)
library(tmap)
library(tmaptools)
library(sf)
library(rgdal)
library(geojsonio)
library(plotly)
library(methods)
library(rgdal)
library(sf)
library(plyr)

install.packages("RCurl")
install.packages("RJSONIO")
install.packages("plyr")
install.packages("tidyverse")
library(RCurl)
library(RJSONIO)
library(plyr)
library(tidyverse)

Add <- LondonSub


LondonWards <- readOGR("~/Desktop/NewLondonWard/NewLondonWard.shp", layer="NewLondonWard")

LondonWardsMap <- read_shape("~/Desktop/GIS Final Project/England_wa_2011/england_wa_2011.shp", as.sf = TRUE)
qtm(LondonWardsMap_new)

LondonWardsMapSF <- st_as_sf(LondonWardsMap)

city <- LondonWardsMapSF[1:25,]
city$agg  <- 1
LondonWardsMap_dis <- aggregate(LondonWardsSF, by = LondonWardsSF, FUN = first)
LondonWardsMap_new <- rbind(LondonWardsMap_dis, LondonWardsSF[26:649,])
qtm(LondonWardsMap_new)
LondonWardsMap_new[1,2] <- as.character("E09000001")
LondonWardsMap_new[1,3] <- as.character("00AA")

LondonWardsMap_new[1,4] <- as.character("City of London")


LondonData<- read.csv("~/Desktop/LondonData.csv", header = TRUE, sep = ",")
LondonData2 <- read_csv("https://files.datapress.com/london/dataset/ward-profiles-and-atlas/2015-09-24T14:21:24/ward-profiles-excel-version.csv", na = "n/a")
names(LondonData2)
LondonBoroughs<-LondonData2[626:658,]
LondonBoroughs <- LondonData2[grep("^E09",LondonData[,3]),]
LondonBoroughs <- LondonBoroughs[2:34,]
names(LondonBoroughs)[1] <- c("Borough Name")
#londonboroughmap
EW <- geojson_read("http://geoportal.statistics.gov.uk/datasets/8edafbe3276d4b56aec60991cbddda50_2.geojson", what = "sp")
LondonMap <- EW[grep("^E09",EW@data$lad15cd),]
#plot it using the base plot function
qtm(LondonMap)

BoroughMapSF <- readOGR("~/Downloads/england_lad_2011.shp", as.sf = TRUE)
BoroughMapSP <- LondonMap
qtm(LondonWardsSF)


newSF <- st_as_sf(LondonMap)

LondonMap <- as(newSF, "Spatial")


LondonMap@data <- data.frame(LondonMap@data,LondonBoroughs[match(LondonMap@data[,"code"],LondonBoroughs[,"New code"]),])

datatypelist <- data.frame(cbind(lapply(LondonWardFinal,class)))

LondonWardFinal <- edit(LondonWardFinal)

LondonWardFinal <- data.frame(LondonWardFinal)

LondonWardsSF2 <- merge(LondonWardsSF, LondonWardFinal, by.x = "WD11CD", by.y = "New code")

LondonWardsMap <- read_shape("~/Desktop/GIS Final Project/England_wa_2011/england_wa_2011.shp", as.sf = TRUE)
LondonWardsMapSF <- st_as_sf(LondonWardsMap)
LondonSub <- LondonWardsSF[,c(1:73,83:86)]

#Create dataframes for subsetted variables for use in index analysis
EducationalVariables <- data.frame(LondonSub$WD11CD, LondonSub$AvgGCSE2011, LondonSub$PctLev4Qua, LondonSub$UnauthAbsenceSchools11)
EconomicVariables <- data.frame(LondonSub$WD11CD, LondonSub$PopCensus2, LondonSub$Employment, LondonSub$NoJobs2011, LondonSub$JSAClaim_1)
Neighborhood_Housing_Quality <- data.frame(LondonSub$WD11CD, LondonSub$PopCensus2, LondonSub$PctOwned20, LondonSub$CrimeRate1)
#need to add more variables^

#Calculate z-scores (test)
GCSE_Z <- data.frame(EducationalVariables$LondonSub.WD11CD, mean(EducationalVariables$LondonSub.AvgGCSE2011), sqrt(var(EducationalVariables$LondonSub.AvgGCSE2011)))
GCSE_Z_Scale <- data.frame(EducationalVariables$LondonSub.WD11CD, scale(EducationalVariables$LondonSub.AvgGCSE2011, center = TRUE, scale = FALSE))

#Normalize function
a <- data.frame(Neighborhood_Housing_Quality$LondonSub.CrimeRate1/Neighborhood_Housing_Quality$LondonSub.PopCensus2)

normalize <- function(input, column, input2, column2){
  
  x <- input$column/input2$column2
  
  return(x)
}

Normal <- normalize(Neighborhood_Housing_Quality, LondonSub.CrimeRate1, Neighborhood_Housing_Quality, LondonSub.PopCensus2)

#Normalize No Jobs
No_Jobs_Normalized <- data.frame(EconomicVariables$LondonSub.NoJobs2011/EconomicVariables$LondonSub.PopCensus2)

#Z-score of No Jobs Normalized
No_Jobs_Z <- 

#Calculate z-scores

mean <- mean(OpportunityMap$Employment)
sd <- sd(OpportunityMap$Employment)

for(i in 1:nrow(a)){
  
  z <-(OpportunityMap$Employment[i] - mean)/sd
  
  print(z)
}



z_score <- function(input, colID){
  
  attach(input)
  
  data <- colID
  
  mean <- mean(data)
  sd <- sd(data)
  
  for(i in 1:length(data)){
    
    data[i] <-(data[i] - mean)/sd
    
  }
  
  return(input <- cbind(data))
}

Index <- data.frame()
Index$Emlpoyment_Z_score <- as.numeric(z_score(OpportunityMap, Employment))
Index$data <- NULL
vnames <- names(OpportunityMap)

Index4 <- data.frame(scale(OpportunityMap$PctLvl4Qual, center = TRUE, scale = TRUE))

Z_scoresFunct <- data.frame()

Index2$Test <- NULL

Index2 <- mutate(Index2,
                 Lvl4Qual_Zscore =  as.numeric(z_score(OpportunityMap, PctLvl4Qual)),
                 AvgGCSE_Zscore = as.numeric(z_score(OpportunityMap, AvgGCSE)),
                 UnauthAbsc_Zscore = as.numeric(z_score(OpportunityMap, UnauthAbsc)),
                 Employment_Zscore = as.numeric(z_score(OpportunityMap, Employment)),
                 JSA_Claim_Zscore = as.numeric(z_score(OpportunityMap, JSA_Claim)),
                 HousingOwned_Zscore = as.numeric(z_score(OpportunityMap, PctHousingOwned)),
                 CrimeRate_Zscore = as.numeric(z_score(OpportunityMap, Normalized_CrimeRate)),
                 NoJobs_Zscore = as.numeric(z_score(OpportunityMap, Normalized_NoJobs))
)

Index2 <- mutate(Index2,
                 WardCode = OpportunityMap$WardCode,
                 WardName = OpportunityMap$WardName
)

Index2 <- mutate(Index2,
                 Geometry = OpportunityMap$geometry
)

Index2 <- mutate(Index2,
                 Index = (Index2$Lvl4Qual_Zscore + Index2$AvgGCSE_Zscore + Index2$UnauthAbsc_Zscore + Index2$Employment_Zscore + Index2$JSA_Claim_Zscore + Index2$HousingOwned_Zscore + Index2$CrimeRate_Zscore + Index2$NoJobs_Zscore)/8
)

Index3 <- NULL

#Index distribution
breaks2 <- classIntervals(Index2$Index, n=5, style="quantile")
ggplot(data = Index2) +
  geom_density(aes(x = Index)) + 
  xlim(-4,4) +
  labs(x = "Values", y = "Density", title = "Opportunity index divided into quintiles") + 
  geom_vline(xintercept = breaks2$brks)


Index_Hist <- ggplot(Index2, aes(x=Index)) + geom_histogram(aes(y = Index)) + geom_density(colour="red", size=1, adjust=1)
Index_Hist + facet_wrap(~ Index, scales="free")

plot(density(Index2$Index))

CrimeRateLogged <- ggplot(OpportunityMap, aes(x=CrimeRate)) + geom_histogram(aes(y = CrimeRate)) + geom_density(colour="red", size=1, adjust=1)
CrimeRateLogged + facet_wrap(~ Index, scales="free")
plot(CrimeRateLogged)
#Log Crime rate Distribution
plot(hist(CrimeRateLogged), 
     main="Opp Map", 
     xlab="Index", 
     border="blue", 
     col="green",
     xlim=c(-1,4),
     las=1, 
     breaks=20)

plot(hist(log10(Index2$NoJobs_Zscore), 
          main="NoJobsLogged", 
          xlab="Index", 
          border="blue", 
          col="green",
          xlim=c(-4,4),
          las=1, 
          breaks=20))

#Map the Index (Average of Z_scores for all variables) using tm_shape
class(OpportunityMap)
Index2 <- st_as_sf(Index2)
?qtm
qtm(Index2, fill = "Index", fill.palette="YlOrRd", midpoint=NA,)
tm_shape(OpportunityMap2) +
  tm_polygons("AvgGCSE",
              style="jenks",
              palette="YlOrRd",
              midpoint=NA,
              title="AvgGCSE Scores London Wards")
OpportunityMap2 <- merge(OpportunityMap1, b, by.x = "Ward.Code", by.y = "Code")
summary(Index2$Index)
?merge

#Opportunity Variables Index
Opportunity <- data.frame(LondonSub$WD11CD, LondonSub$geometry, LondonSub$WD11NM, LondonSub$PopCensus2, LondonSub$AvgGCSE2011, LondonSub$PctLev4Qua, LondonSub$UnauthAbsenceSchools11, LondonSub$Employment, LondonSub$NoJobs2011, LondonSub$JSAClaim_1, LondonSub$PctOwned20, LondonSub$CrimeRate1)

Opportunity <- mutate(Opportunity,
       Normalized_NoJobs = Opportunity$LondonSub.NoJobs2011/Opportunity$LondonSub.PopCensus2,
       Normalized_CrimeRate = Opportunity$LondonSub.CrimeRate1/Opportunity$LondonSub.PopCensus2
       )

#Visualize Summary Stats Table of Opportunity Variables
library(xtable)
sumtmp <- OpportunityMap
sumtmp$geometry <- NULL
summary(sumtmp[4:13])

#Visualize Distributions as Histograms
ForMelting <- OpportunityMap[,c(4:12)]
ForMelting$geometry <- NULL
LondonMelt <- melt(ForMelting, id.vars = 0:1)
attach(LondonMelt)
hist2 <- ggplot(LondonMelt, aes(x=value)) + geom_histogram(aes(y = ..density..)) + geom_density(colour="red", size=1, adjust=1)
hist2 + facet_wrap(~ variable, scales="free")


ggplot(data = OpportunityMap) +
  geom_histogram(mapping = aes(x = AvgGCSE), binwidth = 1)

#attribute mapping

qtm(LondonSub, fill = "PopCensus2")


#Interactive Leaflet Mapping for Opportunity index variables
library(leaflet)
library(sf)
library(sp)
library(magrittr)
library(classInt)

colours<- brewer.pal(5, "Blues")

breaks<-classIntervals(Index2$Index, n=5, style="jenks")
graphics::plot(breaks, pal=colours)
summary(breaks)
breaks <- breaks$brks

qtm(Opportunity$geometry)

pal <- colorBin(palette = "YlOrRd", 
                domain = Index2$Index,
                bins = breaks)

Index2 <- Index2$Geometry %>%
  st_transform(crs = 4326) %>%
  as("Spatial")

OpportunityMap <- st_as_sf(OpportunityMap)

OpportunityMap <- mutate(OpportunityMap,
                         WardCode = Opportunity$LondonSub.WD11CD,
                         WardName = Opportunity$LondonSub.WD11NM,
                         Population = Opportunity$LondonSub.PopCensus2,
                         AvgGCSE = Opportunity$LondonSub.AvgGCSE2011,
                         PctLvl4Qual = Opportunity$LondonSub.PctLev4Qua,
                         UnauthAbsc = Opportunity$LondonSub.UnauthAbsenceSchools11,
                         Employment = Opportunity$LondonSub.Employment,
                         No_Jobs = Opportunity$LondonSub.NoJobs2011,
                         JSA_Claim = Opportunity$LondonSub.JSAClaim_1,
                         PctHousingOwned = Opportunity$LondonSub.PctOwned20,
                         CrimeRate = Opportunity$LondonSub.CrimeRate1,
                         Normalized_CrimeRate = Opportunity$Normalized_CrimeRate,
                         Normalized_NoJobs = Opportunity$Normalized_NoJobs)


#Opportunity Map using interactive Leaflet
leaflet(Index2) %>%
  addPolygons(stroke = FALSE, 
              fillOpacity = 0.7, 
              smoothFactor = 0.5,
              color = ~pal(Index),
              popup = ~WardName
  ) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend("bottomright", 
            pal= pal, 
            values = ~Index, 
            title = "Opportunity Map", 
            labFormat = labelFormat(suffix = ""),
            opacity = 1
  )

#shiny
install.packages("shinyjs")
library(shinyjs)
tmaptools::palette_explorer()
tmap_mode("view")
tm_shape(OpportunityMap) +
  tm_polygons(c("AvgGCSE", "PctLvl4Qual"),
              style=c("jenks", "pretty"),
              palette=list("YlGn", "Reds"),
              midpoint=NA,
              title=c("Avg GCSE Scores", "% Level 4 Qualification"))

qtm(OpportunityMap, fill = "PctLvl4Qual")


#**Correlation Matrix Analysis
install.packages("corrplot")
library(corrplot)
  #Opp Map
OpportunityMap2 <- OpportunityMap
OpportunityMap2$CrimeRate <- NULL
OpportunityMap2$No_Jobs <- NULL
OppMapDF <- st_set_geometry(OpportunityMap2,NULL)
cormat <- cor(OppMapDF[,4:11], use="complete.obs", method="pearson")
corrplot(cormat)
  #zcores
Index2DF <- st_set_geometry(Index2,NULL)
cormat2 <- cor(Index2DF[,1:8], use="complete.obs", method="pearson")
corrplot(cormat2)
xtable(round(corrplot(cormat2)), size = "small")

Index3 <- Index2[1:8]
Index3$Geometry <- NULL

Index3 <- data.frame(as.numeric(Index3$Lvl4Qual_Zscore, Index3$AvgGCSE_Zscore, Index3$UnauthAbsc_Zscore, Index3$Employment_Zscore, Index3$JSA_Claim_Zscore, Index3$HousingOwned_Zscore, Index3$CrimeRate_Zscore, Index3$NoJobs_Zscore))

print(xtable(round(cor(Index2[1:8], use = "na.or.complete"),2)), 
      type = "html", size = "small")


#GWR Analysis

library(broom)
library(corrplot)

model_final <- lm(Employment ~ UnauthAbsc + AvgGCSE + PctHousingOwned + JSA_Claim, data = OpportunityMap)
summary(model_final)

OpportunityMap$model_final_res <- as.numeric(model_final$residuals)

plot(model_final)

qtm(OpportunityMap, fill = "model_final_res", midpoint = NA)

#**Spatial Weights Matrix + Lisa Moran I Test
coordsW <- coordinates(Moran)
plot(coordsW)

#Now we need to generate a spatial weights matrix (remember from the lecture). We'll start with a simple binary matrix of queen's case neighbours
#create a neighbours list
LWard_nb <- poly2nb(Moran, queen=T)
#plot them
plot(LWard_nb, coordinates(coordsW), col="red")
#add a map underneath
plot(Moran, add=T)
#create a spatial weights object from these weights
Lward.lw <- nb2listw(LWard_nb, style="C")
head(Lward.lw$neighbours)

OpportunityMap$Index <- Index2$Index
library(knitr)

huntaddresses <- read.csv("https://www.dropbox.com/s/2cbu2ux9ddy9c0l/huntaddresses.csv?raw=1")

write_(huntaddresses, "~/Desktop/TreasureHuntLocations.csv")

install.packages("spdep")
library(spdep)
Moran2 <- as(OpportunityMap,"Spatial")
moran.test(Moran@data$model_final_res, Lward.lw)
geary.test(Moran2@data$model_final_res, Lward.lw2)
LocalMoran <- localmoran(Moran@data$model_final_res, Lward.lw)
Moran@data$BLocIRz <- LocalMoran[,4]

breaks5<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
#create a new diverging colour brewer palette and reverse the order using rev so higher values correspond to red
MoranColours<- rev(brewer.pal(8, "RdGy"))

#now plot on an interactive map
tm_shape(Moran) +
  tm_polygons("BLocIRz",
              style="fixed",
              breaks=breaks5,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, Some Index Variables in London")




#*****************Using INDEX2
#GWR Analysis

library(broom)
library(corrplot)

model_final3 <- lm(Index ~ Lvl4Qual_Zscore + AvgGCSE_Zscore + UnauthAbsc_Zscore + JSA_Claim_Zscore + Employment_Zscore + HousingOwned_Zscore + CrimeRate_Zscore, data = For_clustering)
summary(model_final3)

Index3$model_final_res <- model_final3$residuals

plot(model_final3)
summary(model_final3)
plot(model2_dummy)

qtm(Index3, fill = "model_final_res")

#**Spatial Weights Matrix + Lisa Moran I Test
coordsW3 <- coordinates(Moran)
plot(coordsW3)

#Now we need to generate a spatial weights matrix (remember from the lecture). We'll start with a simple binary matrix of queen's case neighbours
#create a neighbours list
LWard_nb3 <- poly2nb(Moran, queen=T)
#plot them
plot(LWard_nb3, coordinates(coordsW3), col="red")
#add a map underneath
plot(Moran, add=T)
#create a spatial weights object from these weights
Lward.lw3 <- nb2listw(LWard_nb3, style="C")
head(Lward.lw3$neighbours)

OpportunityMap$Index <- Index2$Index

For_clustering$geometry <- Index2$Geometry

For_clustering$model_final_res <- as.numeric(model_final3$residuals)


install.packages("spdep")
library(spdep)
Moran <- as(Index2,"Spatial")
moran.test(Moran@data$Index, Lward.lw3)
LocalMoran <- localmoran(Moran@data$Index, Lward.lw3)

Moran@data$BLocIRz <- LocalMoran[,4]

breaks5<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
#create a new diverging colour brewer palette and reverse the order using rev so higher values correspond to red
MoranColours<- rev(brewer.pal(8, "RdGy"))

#now plot on an interactive map
tm_shape(Moran) +
  tm_polygons("BLocIRz",
              style="fixed",
              breaks=breaks5,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I: Index")
install.packages("GWmodel")
install.packages("spgwr")
install.packages("spDataLarge")
library(spData)
library(spgwr)
GWRbandwidth <- gwr.sel(Index ~ Lvl4Qual_Zscore + AvgGCSE_Zscore + UnauthAbsc_Zscore + JSA_Claim_Zscore + Employment_Zscore + HousingOwned_Zscore + CrimeRate_Zscore, data = Index2, coords=cbind(x,y),adapt=T) 
install.packages("summarytools")
library(summarytools)
xtable(summary(Index2[11:11]))
gwr.model <-  gwr(Index ~ Lvl4Qual_Zscore + AvgGCSE_Zscore + UnauthAbsc_Zscore + JSA_Claim_Zscore + Employment_Zscore + HousingOwned_Zscore + CrimeRate_Zscore, data = Index2, coords=cbind(x,y), adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE)

gwr.model

results <- as.data.frame(gwr.model$SDF)
head(results)

Index2@data$coefUnauthAbse<-results$UnauthAbse
Index2@data$coefEmployment<-results$Employment


#Geary C
geary.test(Moran3@data$model_final_res, Lward.lw3)
G_LWard_Global_Density <- globalG.test(Moran3@data$model_final_res, Lward.lw3)

#***K-means Test
z_score <- Index2[,1:8]
z_score$Geometry <- NULL
z_score$Index <- Index2$Index
install.packages("factoextra")
library(factoextra)
# Compute k-means with k = 4
set.seed(123)
km.res <- kmeans(z_score, 6, nstart = 25)
# Print the results
print(km.res)
fviz_cluster(km.res, z_score, ellipse.type = "norm")
fviz_nbclust(z_score, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2)



print(xtable(aggregate(z_score, by=list(cluster=km.res$cluster), mean)), include.rownames = F, size = "small")
#Add the clusters as a new column to z_scores 
z_score_k_means <- cbind(z_score, cluster = km.res$cluster)
z_score_k_means$WardName <- Index2$WardName
#Cluster Size
km.res$size
#Centroids
km.res$centers

#Map Clusters?
z_score_k_meansSF <- z_score_k_means
z_score_k_meansSF$Geometry <- Index2$Geometry
z_score_k_meansSF$WardCode <- Index2$WardCode
z_score_k_meansSF <- st_as_sf(z_score_k_meansSF)
breaks_clusters <- classIntervals(z_score_k_meansSF$cluster, n=5, style="quantile")
breaks_clusters <- breaks_clusters$brks
tm_shape(z_score_k_meansSF) +
  tm_polygons("cluster",
              style="quantile",
              breaks=breaks_clusters,
              palette="YlOrRd",
              midpoint=NA,
              popup.vars = "WardName",
              title="Opportunity Map")
tm_shape(z_score_k_meansSF) +
  tm_polygons(c("cluster", "Index"),
              style=c("quantile", "quantile"),
              breaks=breaks_clusters,
              pallette = list("Reds", "YlOrRd"),
              midpoint=NA,
              popup.vars = "WardName",
              title=c("Clusters", "Opportunity Map"))

#PCA
OppMapVariables <- Opportunity
pca1 = prcomp(na.omit(Index2DF[,1:8]), scale. = T)
print(xtable(summary(pca1)), type = "html")
plot(prcomp(na.omit(Index2DF[,1:8]), scale = T), main = "Principal components")
#Weights
rotation <- data.frame(pca1$rotation[,1])
qplot(data = rotation, x = pca1.rotation...1., y = row.names(rotation)) + 
  labs(x = "Weight in first principal component", y = NULL)

rotation2 <- data.frame(pca1$rotation[,2])
qplot(data = rotation2, x = pca1.rotation...2., y = row.names(rotation)) + 
  labs(x = "Weight in second principal component", y = NULL)









#copy
#Visualize Summary Stats Table of Opportunity Variables
library(xtable)
sumtmp <- OpportunityMap
sumtmp$geometry <- NULL
print(xtable(summary(sumtmp[4:13])), type = "html", include.rownames = F, size = "small")


#Map the Index (Average of Z_scores for all variables) using tm_shape
Index2 <- st_as_sf(Index2)

tm_shape(Index2) +
  tm_polygons("Index",
              style="jenks",
              palette="YlOrRd",
              midpoint=NA,
              title="Opportunity Map")

#Interactive Leaflet Mapping for Opportunity index variables
library(leaflet)
library(sf)
library(sp)
library(magrittr)
library(classInt)

colours<- brewer.pal(5, "Blues")

breaks<-classIntervals(Index2$Index, n=5, style="jenks")
graphics::plot(breaks, pal=colours)
summary(breaks)
breaks <- breaks$brks

pal <- colorBin(palette = "YlOrRd", 
                domain = Index2$Index,
                bins = breaks)

Index2 <- Index2$Geometry %>%
  st_transform(crs = 4326) %>%
  as("Spatial")

#Opportunity Map using interactive Leaflet
leaflet(Index2) %>%
  addPolygons(stroke = FALSE, 
              fillOpacity = 0.7, 
              smoothFactor = 0.5,
              color = ~pal(Index),
              popup = ~WardName
  ) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend("bottomright", 
            pal= pal, 
            values = ~Index, 
            title = "Opportunity Map", 
            labFormat = labelFormat(suffix = ""),
            opacity = 1
  )

#**Correlation Matrix Analysis
install.packages("corrplot")
library(corrplot)
#Opp Map
Index2DF <- st_set_geometry(Index2,NULL)
cormat2 <- cor(Index2DF[,1:8], use="complete.obs", method="pearson")
corrplot(cormat2)
print(xtable(round(corrplot(cormat2))),  type = "html", size = "small")

#GWR Analysis

library(broom)
library(corrplot)

model2_dummy <- lm(Index ~ Lvl4Qual_Zscore + AvgGCSE_Zscore + UnauthAbsc_Zscore + JSA_Claim_Zscore + Employment_Zscore + HousingOwned_Zscore + CrimeRate_Zscore, data = Index2)
print(xtable(summary(model2_dummy)), size = "small")

model_final <- lm(Employment ~ UnauthAbsc + AvgGCSE + PctHousingOwned + JSA_Claim, data = OpportunityMap)
summary(model_final)

OpportunityMap$model_final_res <- as.numeric(model_final$residuals)

plot(model_final)

qtm(OpportunityMap, fill = "model_final_res", fill.palette = "div", midpoint = NA)


?qtm










#rmarkdown
## Including Plots

You can also embed plots, for example:
  
  ```{r echo = TRUE, warning = FALSE, message = FALSE, results ='asis'}
#Visualize Summary Stats Table of Opportunity Variables


#Map the Index (Average of Z_scores for all variables) using tm_shape
Index2 <- st_as_sf(Index2)

tm_shape(Index2) +
  tm_polygons("Index",
              style="jenks",
              palette="YlOrRd",
              midpoint=NA,
              title="Opportunity Map")

#Interactive Leaflet Mapping for Opportunity index variables
library(leaflet)
library(sf)
library(sp)
library(magrittr)
library(classInt)

colours<- brewer.pal(5, "Blues")

breaks<-classIntervals(Index2$Index, n=5, style="jenks")
graphics::plot(breaks, pal=colours)
summary(breaks)
breaks <- breaks$brks

pal <- colorBin(palette = "YlOrRd", 
                domain = Index2$Index,
                bins = breaks)


#Opportunity Map using interactive Leaflet
leaflet(Index2) %>%
  addPolygons(stroke = FALSE, 
              fillOpacity = 0.7, 
              smoothFactor = 0.5,
              color = ~pal(Index),
              popup = ~WardName
  ) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend("bottomright", 
            pal= pal, 
            values = ~Index, 
            title = "Opportunity Map", 
            labFormat = labelFormat(suffix = ""),
            opacity = 1
  )

#Correlation Matrix Analysis
library(xtable)
library(corrplot)
#Opp Map
Index2DF <- st_set_geometry(Index2,NULL)
cormat2 <- cor(Index2DF[,1:8], use="complete.obs", method="pearson")
corrplot(cormat2)

```
```{r echo = FALSE, warning = FALSE, message = FALSE, results ='asis'}
library(maptools)
library(RColorBrewer)
library(classInt)
library(sp)
library(rgeos)
library(tmap)
library(tmaptools)
library(sf)
library(rgdal)
library(geojsonio)
library(plotly)
library(methods)
library(rgdal)
library(sf)
library(plyr)
library(RCurl)
library(RJSONIO)
library(plyr)
library(tidyverse)


LondonWards <- readOGR("~/Desktop/NewLondonWard/NewLondonWard.shp", layer="NewLondonWard")
LondonWardsSF <- st_as_sf(LondonWards)
extradata <- read_csv("https://www.dropbox.com/s/qay9q1jwpffxcqj/LondonAdditionalDataFixed.csv?raw=1")
LondonWardsSF <- merge(LondonWardsSF, extradata, by.x = "WD11CD", by.y = "Wardcode")
LondonSub <- LondonWardsSF[,c(1:73,83:86)]
b$Ward.Code <- b$Code

EducationalVariables <- data.frame(LondonSub$WD11CD, 
                                   LondonSub$AvgGCSE2011,
                                   LondonSub$PctLev4Qua,
                                   LondonSub$UnauthAbsenceSchools11)
EconomicVariables <- data.frame(LondonSub$WD11CD, 
                                LondonSub$PopCensus2, 
                                LondonSub$Employment, 
                                LondonSub$NoJobs2011, 
                                LondonSub$JSAClaim_1)
Neighborhood_Housing_Quality <- data.frame(LondonSub$WD11CD, 
                                           LondonSub$PopCensus2, 
                                           LondonSub$PctOwned20, 
                                           LondonSub$CrimeRate1)

No_Jobs_Normalized <- data.frame(EconomicVariables$LondonSub.NoJobs2011/EconomicVariables$LondonSub.PopCensus2)

Opportunity <- data.frame(LondonSub$WD11CD, 
                          LondonSub$geometry, 
                          LondonSub$WD11NM, 
                          LondonSub$PopCensus2, 
                          LondonSub$AvgGCSE2011, 
                          LondonSub$PctLev4Qua, 
                          LondonSub$UnauthAbsenceSchools11, 
                          LondonSub$Employment, 
                          LondonSub$NoJobs2011, 
                          LondonSub$JSAClaim_1, 
                          LondonSub$PctOwned20, 
                          LondonSub$CrimeRate1)

Opportunity <- mutate(Opportunity,
                      Normalized_NoJobs = Opportunity$LondonSub.NoJobs2011/Opportunity$LondonSub.PopCensus2,
                      Normalized_CrimeRate = Opportunity$LondonSub.CrimeRate1/Opportunity$LondonSub.PopCensus2
)

OpportunityMap <- Opportunity$geometry %>%
  st_transform(crs = 4326) %>%
  as("Spatial")

OpportunityMap <- st_as_sf(OpportunityMap)

OpportunityMap <- mutate(OpportunityMap,
                         WardCode = Opportunity$LondonSub.WD11CD,
                         WardName = Opportunity$LondonSub.WD11NM,
                         Population = Opportunity$LondonSub.PopCensus2,
                         AvgGCSE = Opportunity$LondonSub.AvgGCSE2011,
                         PctLvl4Qual = Opportunity$LondonSub.PctLev4Qua,
                         UnauthAbsc = Opportunity$LondonSub.UnauthAbsenceSchools11,
                         Employment = Opportunity$LondonSub.Employment,
                         No_Jobs = Opportunity$LondonSub.NoJobs2011,
                         JSA_Claim = Opportunity$LondonSub.JSAClaim_1,
                         PctHousingOwned = Opportunity$LondonSub.PctOwned20,
                         CrimeRate = Opportunity$LondonSub.CrimeRate1,
                         Normalized_CrimeRate = Opportunity$Normalized_CrimeRate,
                         Normalized_NoJobs = Opportunity$Normalized_NoJobs)

#Calculate z-scores

mean <- mean(OpportunityMap$Employment)
sd <- sd(OpportunityMap$Employment)

z_score <- function(input, colID){
  
  attach(input)
  
  data <- colID
  
  mean <- mean(data)
  sd <- sd(data)
  
  for(i in 1:length(data)){
    
    data[i] <-(data[i] - mean)/sd
    
  }
  
  return(input <- cbind(data))
}

Index2 <- data.frame(scale(OpportunityMap$PctLvl4Qual, center = TRUE, scale = TRUE))
Index2$scale.OpportunityMap.PctLvl4Qual..center...TRUE..scale...TRUE. <- NULL

Index2 <- mutate(Index2,
                 Lvl4Qual_Zscore =  as.numeric(z_score(OpportunityMap, PctLvl4Qual)),
                 AvgGCSE_Zscore = as.numeric(z_score(OpportunityMap, AvgGCSE)),
                 UnauthAbsc_Zscore = as.numeric(z_score(OpportunityMap, UnauthAbsc)),
                 Employment_Zscore = as.numeric(z_score(OpportunityMap, Employment)),
                 JSA_Claim_Zscore = as.numeric(z_score(OpportunityMap, JSA_Claim)),
                 HousingOwned_Zscore = as.numeric(z_score(OpportunityMap, PctHousingOwned)),
                 CrimeRate_Zscore = as.numeric(z_score(OpportunityMap, Normalized_CrimeRate)),
                 NoJobs_Zscore = as.numeric(z_score(OpportunityMap, Normalized_NoJobs))
)

Index2 <- mutate(Index2,
                 WardCode = OpportunityMap$WardCode,
                 WardName = OpportunityMap$WardName
)

Index2 <- mutate(Index2,
                 Geometry = OpportunityMap$geometry
)

Index2 <- mutate(Index2,
                 Index = (Index2$Lvl4Qual_Zscore + Index2$AvgGCSE_Zscore + Index2$UnauthAbsc_Zscore + Index2$Employment_Zscore + Index2$JSA_Claim_Zscore + Index2$HousingOwned_Zscore + Index2$CrimeRate_Zscore + Index2$NoJobs_Zscore)/8
)

#Index distribution
breaks2 <- classIntervals(Index2$Index, n=5, style="quantile")
ggplot(data = Index2) +
  geom_density(aes(x = Index)) + 
  xlim(-4,4) +
  labs(x = "Values", y = "Density", title = "Opportunity index divided into quintiles") + 
  geom_vline(xintercept = breaks2$brks)




```


Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.


colnames(sumtmp)
colnames(sumtmp)[4:13] <- vnames
vnames <- colnames(sumtmp)[4:13]
vnames
colnames(sumtmp)[4:11]
sumtmp$WardCode <- NULL
sumtmp$WardName <- NULL
colnames(sumtmp)[9]
print(xtable(summary(sumtmp[7:11])), include.rownames = F, size = "small")





#Cluster Analysis
z_score <- Index2[,1:8]
z_score$Geometry <- NULL
z_score$Index <- Index2$Index
For_clustering <- Index2[,1:8]
For_clustering$Geometry <- NULL
For_clustering$Index <- Index2$Index
For_clustering <- For_clustering[ -c(619,624,625), ]
View(For_clustering)

set.seed(123)
km.res2 <- kmeans(For_clustering, 5, nstart = 35)
# Print the results
print(km.res2)
fviz_cluster(km.res2, For_clustering, ellipse.type = "norm")
fviz_nbclust(For_clustering, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2)



print(xtable(aggregate(For_clustering, by=list(cluster=km.res2$cluster), mean)), include.rownames = F, size = "small")
#Add the clusters as a new column to z_scores 
For_clustering_k_means <- cbind(For_clustering, cluster = km.res2$cluster)
For_clustering_k_means$WardName <- Index2$WardName
#Cluster Size
km.res$size
#Centroids
km.res$centers

#Map Clusters?
z_score_k_meansSF <- z_score_k_means
z_score_k_meansSF$Geometry <- Index2$Geometry
z_score_k_meansSF$WardCode <- Index2$WardCode
z_score_k_meansSF <- st_as_sf(z_score_k_meansSF)
breaks_clusters <- classIntervals(z_score_k_meansSF$cluster, n=5, style="quantile")
breaks_clusters <- breaks_clusters$brks
tm_shape(z_score_k_meansSF) +
  tm_polygons("cluster",
              style="quantile",
              breaks=breaks_clusters,
              palette="YlOrRd",
              midpoint=NA,
              popup.vars = "WardName",
              title="Opportunity Map")


#race dot-density map

library(maptools)
library(rgeos)
library(tidyverse)
library(rgdal)
install.packages("ggthemes")
library(ggthemes)

ldnoa <- readOGR(dsn = "~/Desktop/GIS Final Project/statistical-gis-boundaries-london/ESRI/OA_2011_London_gen_MHW.shp") %>%
  spTransform(CRS("+proj=longlat +datum=WGS84"))

ldnoa <- readOGR("~/Desktop/NewLondonWard/NewLondonWard.shp", layer="NewLondonWard")
ldnoaDF <- data.frame(ldnoa)

ldnb <- readOGR(dsn = "~/Desktop/GIS Final Project/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp") %>%
  spTransform(CRS("+proj=longlat +datum=WGS84"))

library(sf)

uk <- st_read("../../data/blog_data/uk_650_wpc_2017_full_res_v1.8.shp", stringsAsFactors = FALSE, quiet = TRUE) %>% 
  st_transform(4326) %>% 
  filter(REGN == "London") %>% 
  select(ons_id = PCONCODE)

par(mar = c(0,0,0,0))
qtm(ldnoa)

plot(ldnb)

ldnoa <- readOGR("~/Desktop/NewLondonWard/NewLondonWard.shp", layer="NewLondonWard")
ldnoaSF <- st_as_sf(LondonWards)
ldnoaSub <- ldnoaSF[,c(1:73,83:86)]
ldnoaSF$


ldndata <- ldndata[!duplicated(ldndata), ]

ldneth <- read.csv("~/Desktop/GIS Final Project/ethnic-group-ward.csv", stringsAsFactors = F, check.names = F) %>%
  select(WardCode, White, Asian, Black, Mixed, Chinese/Other)

ldndata <- merge(Index2, ldneth, by.x="WardName", by.y="Ward")
class(ldndata)
num.dots <- select(ldndata$White, ldndata$Asian, ldndata$Black, ldndata$Mixed, ldndata$Chinese.Other) / 10

random_round <- function(x) {
  v=as.integer(x)
  r=x-v
  test=runif(length(r), 0.0, 1.0)
  add=rep(as.integer(0),length(r))
  add[r>test] <- as.integer(1)
  value=v+add
  ifelse(is.na(value) | value<0,0,value)
  return(value)
}

num.dots <- data.frame(ldndata) %>%
  select(White:Chinese.Other) %>% 
  mutate_all(funs(./10)) %>% 
  mutate_all(random_round)



library(sf)
install.packages("purrr")
library(purrr)
purrr::map_df
sf_dots <- map_df(names(num.dots), 
                  ~ st_sample(ldnata, size = num.dots[,.x], type = "random") %>% 
                    st_cast("POINT") %>%                                         
                    st_coordinates() %>%                                          
                    as_tibble() %>%
                    setNames(c("lon","lat")) %>%
                    mutate(Race = .x)
) %>% 
  slice(sample(1:n()))
?st_sample

sp.dfs <- lapply(num.dots), function(x) {
  dotsInPolys(ldnoa, as.integer(num.dots[, x]), f="random")
}
sp.dfs
# Base Plot --------------------------------------------------------------


pal2 <- colorBin(palette = c("#8dd3c7", "#ffffb3", "#fb8072", "#bebada", "#80b1d3"))


# first plot the london boroughs as a base layer with hidden borders to keep a blank canvas
# then plot the dots
par(mar = c(0,0,0,0))
plot(ldnoa, lwd = 0.01, border = "white")
for (i in 1:length(sp.dfs)) {
  plot(sp.dfs[[i]], add = T, pch = 16, cex = 0.1, col = pal2[i])
}




# Add Boundary Labels -------------------------------------------------

# get the centre point of each borough (polygon)
centroids <- data.frame(gCentroid(ldnb, byid = TRUE))

# plot the name of the borough on the centre points
par(mar = c(0,0,0,0))
plot(ldnb, lwd = 0.01, border = "white")
for (i in 1:length(sp.dfs)) {
  plot(sp.dfs[[i]], add = T, pch = 16, cex = 0.1, col = pal2[i])
}
text(x = centroids$x, y = centroids$y, labels = as.character(ldnb$NAME), 
     cex = .8, family = "sans", font = 2)


# Zoom to a specific location ------------------------------------------

xlim <- c(-0.145044, 0.004463)
ylim <- c(51.480955,51.55274)

par(mar = c(0,0,0,0))
plot(ldnb, lwd = 0.01, border = "white", xlim = xlim, ylim = ylim)
for (i in 1:length(sp.dfs)) {
  plot(sp.dfs[[i]], add = T, pch = 16, cex = 0.5, col = pal[i])
} # increase cex (dot size) to compensate for zoom
text(x = centroids$x, y = centroids$y, labels = as.character(ldnb$NAME), 
     cex = 1, family = "sans", font = 2) # also increase text text size
