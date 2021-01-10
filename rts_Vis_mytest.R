library(rtsVis)
library(dplyr)
library(raster)
library(sf)
library(moveVis)
library(magrittr) 
library(ggplot2) 
library(gganimate) 
library(scales) 
library(viridis)
library(hrbrthemes)

# load as Raster stack 
clip10_st <- stack("./ROM1/clip10.tif")
clip11_st <- stack("./ROM1/clip11.tif")
clip13_st <- stack("./ROM1/clip13.tif")
clip14_st <- stack("./ROM1/clip14_2.tif")
clip15_st <- stack("./ROM1/clip15.tif")
clip13_drop <- dropLayer(clip13_st, 1) # because landsat 8 has 7 bands, so remove B1
clip14_drop <- dropLayer(clip14_st, 1)
clip15_drop <- dropLayer(clip15_st, 1)

# crop the too big landsat images
shpcrop <- read_sf("./ROM1/shp_ROM1_32635.shp") %>% st_transform(crs = st_crs(clip10_st))
crop10 <- crop(clip10_st, extent(shpcrop))
raster10 <- stack(crop10) #back to stack 
plotRGB(raster15, 4,3,2, stretch="lin")
crop11 <- crop(clip11_st, extent(shpcrop))
raster11 <- stack(crop11)
crop13 <- crop(clip13_drop, extent(shpcrop))
raster13 <- stack(crop13)
crop14 <- crop(clip14_drop, extent(shpcrop))
raster14 <- stack(crop14)
crop15 <- crop(clip15_drop, extent(shpcrop))
raster15 <- stack(crop15)

# save as RDS 
rom_crop_list <- list(raster10, raster11, raster13, raster14, raster15)
saveRDS(rom_crop_list, "rom_crop_list.rds")

# fill NA
rom_crop_filled <- rtsVis::ts_fill_na(rom_crop_list) 

#save as RDS again (just in case when R session aborted..)
saveRDS(rom_crop_filled, "rom_crop_filled.rds")
rom_crop_filled <- readRDS("./rom_crop_filled.rds")

# give names 
head(names(rom_crop_filled))
names(rom_crop_filled) <- c("2010-10-22", "2011-08-22", "2013-10-30", "2014-08-14", "2015-09-18")

#Input times
in_dates <- as.POSIXct(names(rom_crop_filled))
head(in_dates)

#Output times
out_dates <-seq.POSIXt(from = in_dates[2],
                       to = in_dates[length(in_dates)],
                       length.out = length(in_dates)*2)
head(out_dates)

#interpolating 
rom_crop_filled_interpolated_2 <- rtsVis::ts_raster(r_list = rom_crop_filled,
                                                    r_times = in_dates,
                                                    out_times = out_dates,
                                                    fade_raster = T) 

head(rtsVis:::.ts_get_frametimes(rom_crop_filled_interpolated)) #check 

#save RDS (R session loves being aborted)
saveRDS(rom_crop_filled_interpolated_2, "rom_crop_filled_interpolated_2.rds")

rom_crop_filled_interpolated_2 <- readRDS("./ROM1/rom_crop_filled_interpolated_2.rds")


# load shapefiles (deforestation area [ha]) and set polygon coordinate 
shp10 <- read_sf("./ROM1/shp10.shp") %>% 
  st_transform(crs = st_crs(rom_crop_filled_interpolated_2[[1]]))
shp11 <- read_sf("./ROM1/shp11.shp") %>% 
  st_transform(crs = st_crs(rom_crop_filled_interpolated_2[[1]]))
shp13 <- read_sf("./ROM1/shp13.shp") %>% 
  st_transform(crs = st_crs(rom_crop_filled_interpolated_2[[1]]))
shp14 <- read_sf("./ROM1/shp14.shp") %>% 
  st_transform(crs = st_crs(rom_crop_filled_interpolated_2[[1]]))
shp15 <- read_sf("./ROM1/shp15.shp") %>% 
  st_transform(crs = st_crs(rom_crop_filled_interpolated_2[[1]]))

#just check the image 
plotRGB(rom_crop_filled_interpolated_2[[5]], 4,3,2, stretch="lin")
plot(st_geometry(shp15),add=T,col="yellow",alpha=0.5)

#Creating the Spatial Frames
r_frames <- rtsVis::ts_makeframes(x_list = rom_crop_filled_interpolated_2,
                                  l_indices = c(4,3,2),
                                  minq = 0.05,
                                  maxq = 0.9)

r_frames[[1]]   # Examine a single frame
r_frames[[2]]
r_frames[[3]]
r_frames[[4]]
r_frames[[5]]

#Adding additional Elements to the Frames
r_frames_styled <- r_frames %>%
  moveVis::add_labels(x = "Longitude", y = "Latitude")%>% 
  moveVis::add_northarrow(colour = "white", position = "upperright", label_margin = 1.5) %>% 
  moveVis::add_timestamps(type = "label") %>% 
  moveVis::add_progress()

r_frames_styled[[5]]  #check a single image

# I want to add my polygon data on the landsat images but I could not find how to do it
# with this package so I just did it manually... 

r1 <- r_frames_styled[[1]] + geom_sf(data=shp10, color = "white", fill =NA, alpha = 0.4) 
r2 <- r_frames_styled[[2]] + geom_sf(data=shp10, color = "white", fill =NA, alpha = 0.4)
r3 <- r_frames_styled[[3]] + geom_sf(data=shp11, color = "white", fill =NA, alpha = 0.4) 
r4 <- r_frames_styled[[4]] + geom_sf(data=shp11, color = "white", fill =NA, alpha = 0.4) 
r5 <- r_frames_styled[[5]] + geom_sf(data=shp13, color = "white", fill =NA, alpha = 0.4) 
r6 <- r_frames_styled[[6]] + geom_sf(data=shp13, color = "white", fill =NA, alpha = 0.4) 
r7 <- r_frames_styled[[7]] + geom_sf(data=shp14, color = "white", fill =NA, alpha = 0.4) 
r8 <- r_frames_styled[[8]] + geom_sf(data=shp14, color = "white", fill =NA, alpha = 0.4) 
r9 <- r_frames_styled[[9]] + geom_sf(data=shp15, color = "white", fill =NA, alpha = 0.4)
r10 <- r_frames_styled[[10]] + geom_sf(data=shp15, color = "white", fill =NA, alpha = 0.4)
frames <- list(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10) #this package loves list

for(i in 1:length(frames)) {
  r <- frames[[i]] + labs(title = "Deforestation area in Carpathian",
                   subtitle = "Ucisoara valley, Uceamare Valley, Sambata Valley \n (changing polygons show the deforestation area)",
                   caption = "Data source: USGS.GOV") +
    theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          plot.caption = element_text(color = "gray", face = "italic"))
  frames[[i]] <- r
}

frames[[2]]

# check how the images with polygons will be seen 
moveVis::animate_frames(frames,
                        out_file = "TEST6.gif",
                        width=1600,
                        fps = 2) # i like it 


# try: line plot from the package  
pnt <-read_sf("./ROM1/pnt.shp") %>% 
  st_transform(crs = st_crs(rom_crop_filled_interpolated_2[[1]]))
line_frames <- rtsVis::ts_flow_frames(r_list = rom_crop_filled_interpolated_2,
                                      positions = pnt,
                                      plot_function = "line")
line_frames[[length(line_frames)]]
point_joined<- moveVis::join_frames(frames_lists = list(frames, plot_list))
moveVis::animate_frames(point_joined,
                        out_file = "test7.gif",
                        fps = 2,
                        width=1600)



## create a deforestation [ha] plot 

# dataframe for the plot 
df <- data.frame(y = c(sum(shp10$area_ha),
                       sum(shp11$area_ha),
                       sum(shp13$area_ha),
                       sum(shp14$area_ha),
                       sum(shp15$area_ha)),
                 x = c(2011, 2012, 2013, 2014, 2015) )
y <- c()
for(i in 1:9){
  if( (i%%2) == 1){
    y[i] <- df$y[(i-1)/2 +1 ]
  }
  else {
    y[i] <- mean(df$y[c((i/2),(i/2)+1)])
  }
}

df2 <- data.frame(x = seq(2011,2015.5,by=0.5), 
                  y = c(y,900))

# create a list for join_frames 

plot_list <- list()
for(i in 1:10){
  p1 <- ggplot(aes(x=x, y=y, size=y), data = slice(df2, c(1:i))) +
    geom_segment(aes(x=x, xend=x, y=0, yend=y), data = slice(df2, c(1:i)),
                 size=1, color="gray", linetype="dotdash") +
    geom_point(color="palegreen3", alpha=0.9, shape=21, stroke=2) +
    geom_text(aes(label = y), size=3.5) +
    #scale_size_manual(values = c(200, 400, 600, 800, 1000), name="deforestration area (ha)")+
    scale_size(range = c(3, 20), name="deforestration area (ha)", breaks = c(200, 400, 600, 800, 1000)) + 
    #scale_fill_viridis(guide=F, option="D") +
    theme_ipsum() +
    theme(legend.position="bottom")+
    ylab("deforestration area (ha)") +
    xlab("year") +
    #theme(legend.position = "none") + 
    coord_cartesian(xlim = c(2011.0, 2015.5), ylim = c(100, 1000)) 
  
  plot_list[[i]] <- p1
}

joined <- moveVis::join_frames(frames_lists = list(frames, plot_list))
moveVis::animate_frames(joined,
                        out_file = "test14.gif",
                        fps = 2,
                        width=1600, 
                        height = 450)

