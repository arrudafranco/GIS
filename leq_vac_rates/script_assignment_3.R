library(tidyverse)
library(sf)
library(classInt)
library(ggspatial)
library(RColorBrewer)
library(tmap)

boundaries <-
  st_read("Boundaries - ZIP Codes/geo_export_76c46036-6a6b-4948-9f9f-6637ba51fc50.shp")
vac_cov <-
  st_read("COVID-19 Vaccination Locations/geo_export_95213cc3-accf-43f9-a343-352961808bc8.shp")
imi_covid <-
  read_csv("Chicago Health Atlas Data Cleaned - ZIP Codes.csv")
imi_covid <- mutate(imi_covid, zip = as.character(GEOID))


chi_covid <- boundaries %>% left_join(imi_covid, by = "zip") %>%
  st_as_sf() %>% st_transform(crs = 3435)
st_write(chi_covid, "chi_covid_vars.shp")
vac_cov <- st_transform(vac_cov, crs = 3435)
# Buffer is approximately 0.5 miles.
vac_cov_buffer <- st_buffer(vac_cov, 2640)
st_write(vac_cov_buffer, "vac_cov_buffer.shp")


brks_vac <- classIntervals((chi_covid %>% drop_na(COVIV_2021))$COVIV_2021, style = "jenks")
ggplot() +
  geom_sf(chi_covid, mapping = aes(geometry = geometry, fill = COVIV_2021)) +
  geom_sf(vac_cov_buffer, mapping = aes(geometry = geometry), fill = 'Orange', alpha = 0.2) +
  theme_void() + scale_fill_stepsn(breaks = brks_vac$brks, colors = brewer.pal(8, 'PuBuGn')) +
  annotation_scale() +
  labs(title = 'Vaccination Rates in Chicago (%)',
       fill = "Vaccination Completion with Jenks Breaks")

brks_leq <- classIntervals((chi_covid %>% drop_na(`LEQ_2015_2019`))$`LEQ_2015_2019`, style = "jenks")
ggplot() +
  geom_sf(chi_covid, mapping = aes(geometry = geometry, fill = `LEQ_2015_2019`)) +
  geom_sf(vac_cov_buffer, mapping = aes(geometry = geometry), fill = 'Green', alpha = 0.3) +
  theme_void() + scale_fill_stepsn(breaks = brks_leq$brks, colors = brewer.pal(8, 'BuPu')) +
  annotation_scale()

brks_for <- classIntervals((chi_covid %>% drop_na(`FOR_2015_2019`))$`FOR_2015_2019`, style = "jenks")
ggplot() +
  geom_sf(chi_covid, mapping = aes(geometry = geometry, fill = `FOR_2015_2019`)) +
  geom_sf(vac_cov_buffer, mapping = aes(geometry = geometry), fill = 'Purple', alpha = 0.3) +
  theme_void() + scale_fill_stepsn(breaks = brks_leq$brks, colors = brewer.pal(8, 'BuGn')) +
  annotation_scale()

####


# Point in polygon. Gives the points the attributes of the polygons that they are in
pip <- st_join(vac_cov, chi_covid, join = st_within) %>% st_drop_geometry() %>%
  group_by(zip) %>% count() %>% full_join(chi_covid) %>% st_as_sf() %>%
  mutate(pip_prop = n / POP_2015_2019)

st_write(pip, 'pip_vac_pop.shp')


###

# Point in polygon. Gives the points the attributes of the polygons that they are in
pip <- st_join(vac_cov, chi_covid, join = st_within) %>% st_drop_geometry() %>%
  group_by(zip) %>% count() %>% full_join(chi_covid) %>% st_as_sf() %>%
  mutate(pip_prop = n / POP_2015_2019)

st_write(pip, 'pip_vac_pop.shp')

