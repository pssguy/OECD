library(shiny)

library(shinydashboard)

library(dplyr)

# more for collecting initial data?
library(rsdmx)
library(readr)
library(plotly)
#library(crosstalk)
library(countrycode)
library(tidyr)
library(DT)

# 
# library(readr)
# 
# 
#  library(leaflet)  # for interactive maps (NOT leafletR here)
# 
# 
# library(tigris)
# library(acs)
# library(stringr) # to pad fips codes

## 
countries <- countrycode_data

## population  Data #http://data.worldbank.org/indicator/SP.POP.TOTL?page=6

countryPops <- read_csv("data/countryPops.csv")
glimpse(countryPops)

# migration 2000-2013 - might want to switxch to just migration code
#migration_df <- readRDS("data/migration.rds")
migration_df <- readRDS("data/totalMIG.rds") # swift enough to be a shiny
migration_cats <- read_csv("data/categoriesMIG.csv")

migCats <- migration_cats$id[1]
names(migCats) <- migration_cats$label[1]

minYear <- as.integer(min(migration_df$obsTime))
maxYear <- as.integer(max(migration_df$obsTime))

### some of this will need to be moved to reactive

# sel_data <-migration_df %>% 
#   select(-TIME_FORMAT, -OBS_STATUS) %>%  # remove unnecessary columns
#   filter(VAR=="B11") %>% # reduce to variable of interest
#   spread(GEN,obsValue) %>%  # obtain male data
#   mutate(MEN=ifelse(is.na(WMN),TOT,TOT-WMN)) %>% # some women data is NA
#   gather(key=GEN,value=obsValue,TOT,MEN,WMN) %>% 
#   mutate(Year=as.integer(obsTime)) %>%  # further tidying up
#   select(Year,From=CO2,Gender=GEN,To=COU,Count=obsValue) #
# 
# 
# 
# 
# allImmsbyCountry2000 <-sel_data %>% 
#   tbl_df() %>% 
#   filter(Gender=="TOT"&Year==2000) %>% 
#   group_by(To) %>% 
#   summarize(Total=sum(Count,na.rm=T))
# 
# 
# allImmsbyCountry2000 <- allImmsbyCountry2000 %>% 
#   left_join(countries,by=c("To"="iso3c"))
# 
# allImmsbyCountry2000$hover <- with(allImmsbyCountry2000, paste(country.name, '<br>', "Immigrants", Total))

## could be used to separate countries but not sure it is better than default def at world level
l <- list(color = toRGB("grey"), width = 2)


g <- list(
  scope = 'world',
  projection = list(type = 'robinson'),
  showlakes = FALSE#,
  # lakecolor = toRGB('blue')
)