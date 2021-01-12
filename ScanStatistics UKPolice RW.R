#Launch R Version 3.5.0

#Load relevant Rtools for devtools

install.packages(c("tidyverse","sf","lubridate","sp","magrittr","modelr"))

devtools::install_github("benjak/scanstatistics", ref = "develop")

library(scanstatistics)
library(tidyverse)
library(sf)
library(lubridate)
library(sp)
library(magrittr)
library(readxl)
library(modelr)


# Read data ---------------------------------------------------------------

LSOA_r_w <- read_rds("./LSOA RW.rds") %>%
  st_as_sf(crs = 4326)

ward_r_w <- read_rds("./Wards RW.rds")

crimes_r_w <- read_rds("./Crimes RW.rds") %>%
  data.frame() %>%
  mutate(month = as.Date(paste0(month,"-01"))) %>%
  st_as_sf(crs = 4326) %>%
  st_join(LSOA_r_w, join = st_within) %>%
  filter(!is.na(LSOA11CD)) %>%
  as.data.frame() %>%
  group_by(month, LSOA11CD) %>%
  summarise(Crimes = n()) 

temp <- tempfile()
download.file("https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2flowersuperoutputareamidyearpopulationestimates%2fmid2019sape22dt2/sape22dt2mid2019lsoasyoaestimatesunformatted.zip",temp)
file <- utils::unzip(temp, "SAPE22DT2-mid-2019-lsoa-syoa-estimates-unformatted.xlsx")
data <- read_xlsx(file, sheet = "Mid-2019 Persons", skip = 3)

pop_data <- data %>% select(`LSOA Code`, `LSOA Name`, Population = `All Ages`)

rm(temp, data, file)
  
# Process crime data into LSOA lookup -------------------------------------

crime_input <- data.frame(expand.grid(unique(crimes_r_w$month), unique(LSOA_r_w$LSOA11CD))) %>%
  select(LSOA11CD = 2, month = 1) %>%
  left_join(pop_data, by = c("LSOA11CD"="LSOA Code")) %>%
  left_join(crimes_r_w, by = c("LSOA11CD"="LSOA11CD", "month"="month")) %>%
  mutate(I = lubridate::interval(max(crimes_r_w$month)-months(6),month) %/% months(1),
         Crimes = case_when(is.na(Crimes) ~ 0, 
                            T ~ as.double(Crimes)))

ggplot() + geom_histogram(data = crime_input, mapping = aes(x = Crimes))

ggplot() + geom_histogram(data = crime_input %>% distinct(LSOA11CD, Population), mapping = aes(x = log(Population)))
  

# Create objects for function arguments -----------------------------------

#Create count of crimes by area for months observing

count <- crime_input %>%
  filter(month >= as.Date(max(crime_input$month)-months(6))) %>%
  df_to_matrix(time_col = "month", location_col = "LSOA11CD", value_col = "Crimes")


zone <- LSOA_r_w %>%
  data.frame() %>%
  select(LONG, LAT) %>%
  as.matrix() %>%
  spDists(x = ., y = ., longlat = TRUE) %>%
  dist_to_knn(k = 15) %>%
  knn_zones()


mod <- glm(Crimes ~ log(Population) + I,
           family = poisson(link = "log"),
           data = crime_input %>% filter(month < as.Date(max(crime_input$month)-months(6)))
           )


ebp_baseline <- crime_input %>% 
  filter(month >= as.Date(max(crime_input$month)-months(6)))

ebp_baseline$Crime_prediction <- predict(mod, newdata = data.frame(ebp_baseline), type = "response")

ebp_baseline <- ebp_baseline %>%
 df_to_matrix(time_col = "month", location_col = "LSOA11CD", value_col = "Crime_prediction")



# Run scanstatistic -------------------------------------------------------

set.seed(1)

poisson_result <- scan_eb_poisson(counts = count, 
                                  zones = zone, 
                                  baselines = ebp_baseline,
                                  n_mcsim = 999)


LSOAs <- as.character(LSOA_r_w$LSOA11NM)
LSOAs[79]



# Score Outputs -----------------------------------------------------------


# Calculate scores and add column with county names
score <- score_locations(poisson_result, zone) %>%
  mutate(LSOA = factor(LSOAs[1:length(LSOAs)])) %>%
  left_join(LSOA_r_w, by = c("LSOA"="LSOA11NM"))

write_rds(score, "Crime SS.rds")
