library(tidyverse)
library(tidycensus)
library(RColorBrewer)
library(tmap)

census_api_key("8936f3beab8113abc6e9e9df6b82630cee84c904",
               install=TRUE, overwrite = TRUE)

PA_county.sp <- get_acs(geography = "county", 
              variables = c(medincome = "B19013_001"), 
              state = "PA", 
              year = 2018,
              geometry = TRUE)
PA_county_map <- tm_shape(PA_county.sp) + tm_fill("estimate",
                                 style = "jenks",
                                 palette = "YlGnBu",
                                 title ="Median Income by County")

PA_tract.sp <- get_acs(geography = "tract", 
                     variables = c(medincome = "B19013_001"), 
                     state = "PA", 
                     year = 2018,
                     geometry = TRUE)
PA_tract_map <- tm_shape(PA_tract.sp) + tm_fill("estimate",
                                 style = "jenks",
                                 palette = "YlGnBu",
                                 title ="Median Income by Tract")

tmap_arrange(PA_county_map, PA_tract_map)
st_write(PA_county.sp, "PA_county.shp")
st_write(PA_tract.sp, "PA_tract.shp")