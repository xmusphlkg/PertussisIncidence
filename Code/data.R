
# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)

# data --------------------------------------------------------------------

DataPop <- read.xlsx("./Data/Pertussis incidence.xlsx", sheet = 'Population')
Countries <- unique(DataPop$Name)

DataInc <- map(Countries,
               ~read.xlsx(paste0("./Data/Pertussis incidence.xlsx"),
                          sheet = .x,
                          detectDates = T))
DataInc <- do.call(rbind, DataInc) |> 
     left_join(DataPop, by = c('Country' = 'Name', 'Year' = 'Year')) |> 
     mutate(Incidence = Cases / Population)

## check the data is complete
DataInc |> 
     group_by(Country, Year) |> 
     summarise(Complete = n(),
               .groups = 'drop') |> 
     pivot_wider(names_from = Year, values_from = Complete)

write.csv(DataInc, './Outcome/Table S1.csv', row.names = F)

# map data ----------------------------------------------------------------

DataNews <- read.csv('./Data/HealthmapData.csv')
# extract the country name from the place_name, if contains a ", " then the country name is after the ", ", else the country name is the place_name
DataNews <- DataNews |> 
     mutate(country = ifelse(grepl(", ", place_name), 
                             str_trim(str_extract(place_name, "[^,]+$")),
                            place_name)) |> 
     select(country, issue_date = date) |> 
     mutate(issue_date = as.Date(issue_date),
            start_issue_date = as.numeric(difftime(issue_date, as.Date('2023-1-1'), units = 'days')))

DataNews <- DataNews |> 
     # replace Hong Kong, Macau, and Taiwan with China
     mutate(country = ifelse(country %in% c('Hong Kong', 'Macau', 'Taiwan'), 'China', country),
            # replace Guam [USA] with United States
            country = ifelse(country == 'Guam [USA]', 'United States', country))
write.csv(DataNews, './Outcome/Table S2.csv', row.names = F)

