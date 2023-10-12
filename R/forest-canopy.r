

libs <- c("tidyverse", "BIOMASS", "hrbrthemes", "gridExtra", "openxlsx", "stats", "wrMisc", "vegan", "BiodiversityR", "bbplot", "here", "MetBrewer", "raster", "rgdal", "rgeos", "raster", "sf", "stars", "rasterVis")
invisible(lapply(libs, library, character.only = T))
stringsAsFactors = TRUE

#Ser WD
setwd("C:/Users/Rene/Documents/R/earth data")

#
# get list of all tifs
list.files("data/week-06/landsat/LC80340322016205-SC20170127160728/crop")

all_landsat_bands <- list.files("data/week-06/landsat/LC80340322016205-SC20170127160728/crop",
           pattern = glob2rx("*band*.tif$"),
           full.names = TRUE)

# stack the data
landsat_stack_csf <- stack(all_landsat_bands)
# then turn it into a brick
landsat_csf_br <- brick(landsat_stack_csf)
# view stack attributes
landsat_csf_br

# remove the filename from each band name for pretty plotting
names(landsat_csf_br) <- gsub(pattern = "LC80340322016189LGN00_sr_", replacement = "", names(landsat_csf_br))

#Normalized Difference Vegetation Index (NDVI) 
ndvi <- (landsat_csf_br[[5]]-landsat_csf_br[[4]])/(landsat_csf_br[[5]]+landsat_csf_br[[4]])
plot(ndvi)

# advanced vegetation index (AVI)
avi <- (landsat_csf_br[[5]] * (1 - landsat_csf_br[[4]]) * (landsat_csf_br[[5]] - landsat_csf_br[[4]]))^(1/3)

plot(avi)

#Bar Soil Index (bsi)

# bsi <- ((landsat_csf_br[[4]] + landsat_csf_br[[2]])-landsat_csf_br[[3]])/((landsat_csf_br[[4]] + landsat_csf_br[[2]]) + landsat_csf_br[[3]])

# plot(bi)

bsi <- ((landsat_csf_br[[6]] + landsat_csf_br[[4]]) - (landsat_csf_br[[5]] + landsat_csf_br[[2]]))/((landsat_csf_br[[6]] + landsat_csf_br[[4]]) + (landsat_csf_br[[5]] + landsat_csf_br[[2]]))

plot(bsi)

# Shadow Index (SI)
si <- sqrt((256 - landsat_csf_br[[3]]) * (256 - landsat_csf_br[[4]]))
plot(si)

#ssi
si_min <- minValue(si)
si_max <- maxValue(si)
ssi <- (si - si_min) / (si_max - si_min) * 100

plot(ssi)

#mndvi
mndvi <- (ndvi* landsat_csf_br[[5]]-landsat_csf_br[[4]])/(ndvi * landsat_csf_br[[5]]+landsat_csf_br[[4]])



minValue(mndvi)
plot(mndvi)

# Define the minimum and maximum values of the original range
min_original <- minValue(mndvi)
max_original <- maxValue(mndvi)

# Define the new minimum and maximum values for the 0-100 percent range
min_new <- 0
max_new <- 100

# Calculate the rescaled MNDVI values
r_mndvi <- ((mndvi - min_original) / (max_original - min_original)) * (max_new - min_new) + min_new

plot(r_mndvi)

fcd <- sqrt(r_mndvi * ssi + 1) -1
plot(fcd)

#####################################################

all_landsat_bands <- list.files("C:/Users/Rene/Documents/R/earth data/data/week-06/landsat/LE07_L1TP_169055_20011215_20170202_01_T1/crop",
           pattern = ".tif$",
           full.names = TRUE)


# Boucle pour extraire et nommer les éléments
band <- list()
for (i in 1:length(all_landsat_bands)) {
  # Renommer l'élément et l'ajouter à la liste "band"
  band[[paste0("b", i)]] <- all_landsat_bands[[i]]
}

band <- lapply(band, raster)


band[[2]]
#Normalized Difference Vegetation Index (NDVI) 
ndvi <- (band[[2]] - band[[3]])/(band[[2]] + band[[3]])
plot(ndvi)

# advanced vegetation index (AVI)
avi <- (band[[4]] * (1 - band[[3]]) * (band[[4]] - band[[3]]))^(1/3)

plot(avi)

#Bar Soil Index (bsi)

# bsi <- ((landsat_csf_br[[4]] + landsat_csf_br[[2]])-landsat_csf_br[[3]])/((landsat_csf_br[[4]] + landsat_csf_br[[2]]) + landsat_csf_br[[3]])

# plot(bi)

bsi <- ((landsat_csf_br[[6]] + landsat_csf_br[[4]]) - (landsat_csf_br[[5]] + landsat_csf_br[[2]]))/((landsat_csf_br[[6]] + landsat_csf_br[[4]]) + (landsat_csf_br[[5]] + landsat_csf_br[[2]]))

bsi <- ((band[[5]] + band[[3]]) - (band[[4]] + band[[1]])) / ((band[[5]] + band[[3]]) + (band[[4]] + band[[1]]))

plot(bsi)

# Shadow Index (SI)
si <- sqrt((256 - band[[2]]) * (256 - band[[3]]))
plot(si)

#ssi
si_min <- minValue(si)
si_max <- maxValue(si)
ssi <- (si - si_min) / (si_max - si_min) * 100

plot(ssi)

#mndvi
mndvi <- (ndvi* band[[4]] - band[[3]])/(ndvi * band[[4]]+ band[[3]])

minValue(mndvi)
plot(mndvi)

# Define the minimum and maximum values of the original range
min_original <- minValue(mndvi)
max_original <- maxValue(mndvi)

# Define the new minimum and maximum values for the 0-100 percent range
min_new <- 0
max_new <- 100

# Calculate the rescaled MNDVI values
r_mndvi <- ((mndvi - min_original) / (max_original - min_original)) * (max_new - min_new) + min_new

plot(r_mndvi)

#FCD

fcd <- sqrt(r_mndvi * ssi + 1) -1
plot(fcd)

# Classifiez les valeurs de la variable layer
df$layer_category <- cut(df$layer, breaks = c(0, 5, 41, 60, 71, 100), labels = c("No", "Low", "subMiddle" "Middle", "High"))


# Utilisez ggplot2 pour créer le graphique
ggplot(df, aes(x = x, y = y, fill = layer)) +
  geom_raster() +
scale_fill_gradientn(name = "% percentage", colours = terrain.colors(10), trans = "reverse") +
guides(fill=guide_legend(
            direction = "horizontal",
            keyheight = unit(1.15, units = "mm"),
            keywidth = unit(15, units = "mm"),
            title.position = 'top',
            title.hjust = 0.5,
            label.hjust = .5,
            nrow = 1,
            byrow = T,
            reverse = F,
            label.position = "bottom"
          )
    ) +
theme_minimal()  +
theme(panel.background = element_blank(),
legend.background = element_blank(),
legend.position = c(.45, .04),
panel.border = element_blank(),
panel.grid.minor = element_blank(),
panel.grid.major = element_line(color = "white", size = 0.2),
plot.title = element_text(size=20, color="#6c2d83", hjust=0.5, vjust=-10),
plot.subtitle = element_text(size=14, color="#bd5288", hjust=0.5, vjust=-15, face="bold"),
plot.caption = element_text(size=9, color="grey60", hjust=0.5, vjust=9),
axis.title.x = element_text(size=7, color="grey60", hjust=0.5, vjust=5),
legend.text = element_text(size=10, color="grey20"),
legend.title = element_text(size=11, color="grey20"),
strip.text = element_text(size=12),
plot.margin = unit(c(t=-2, r=-2, b=-2, l=-2),"lines"), #added these narrower margins to enlarge map
axis.title.y = element_blank(),
axis.ticks = element_blank(),
axis.text.x = element_blank(),
axis.text.y = element_blank())

