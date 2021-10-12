library(rnaturalearth)
library(sf)
library(raster)
library(tidyverse)
library(elevatr)

theme_set(theme_void())
usa<-ne_states(country="united states of america", returnclass="sf")

usa%>%
  dplyr::select(name)%>%
  plot()

low_f <- usa%>%
  filter(name!="Alaska" & name!="Hawaii" & name!="Guam")%>%
  dplyr::select(name)

elev <- get_elev_raster(as(low_f, "Spatial"), z=3)

state_elevation <- raster::extract(elev, low_f, df=TRUE, factors=TRUE)

avg_elevation<-state_elevation%>%
  as_tibble()%>%
  group_by(ID)%>%
  summarize(avg_elevation = mean(layer))

low_f%>%
  bind_cols(avg_elevation)%>%
  dplyr::select(-ID)%>%
  ggplot(aes(fill=avg_elevation))+
  geom_sf( color="white")+
  scale_fill_distiller(palette="Spectral")+
  labs(
    title = "Average Elevation by State",
    fill="Elevation (m)"
  )+
  theme(
    plot.title = element_text(hjust=0.5, size=14, face="bold"),
    legend.position = "bottom"
  )


