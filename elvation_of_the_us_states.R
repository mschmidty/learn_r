library(tidyverse)
library(sf)
library(raster)
library(elevatr)
library(rnaturalearth)
library(viridis)

usa<-ne_states(country = "united states of america", returnclass = "sf")

usa_cl<-usa%>%
  dplyr::select(name)%>%
  filter(name!="Guam" & name!="Alaska" &name!="Hawaii")

elev<-get_elev_raster(usa_cl, z=3)

plot(elev)

?raster::extract

elevation_by_state<-extract(elev, as(usa_cl, "Spatial"), df=T)

avg_elev<-elevation_by_state%>%
  rename(elevation=2)%>%
  group_by(ID)%>%
  summarize(avg_elevation = mean(elevation, na.rm=T))%>%
  ungroup()

theme_set(theme_void())

usa_cl%>%
  bind_cols(avg_elev)%>%
  ggplot(aes(fill=avg_elevation))+
  geom_sf(color="white")+
  scale_fill_viridis()+
  labs(
    title="Average Elevation of Each State in the US", 
    caption = "by Mike | data: elevatr and rnaturalearth",
    fill= "Avg. Elevation"
  )+
  theme(
    plot.title = element_text(face="bold", size=16, hjust = 0.5),
    plot.caption = element_text(color="gray50"),
    legend.position = "bottom", 
    legend.key.height = unit(0.1, "in"),
    legend.key.width = unit(1, "in")
  )

