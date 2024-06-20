
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
     mutate(Incidence = Cases / Population) |> 
     filter(Date < as.Date('2024/5/1'))

## check the data is complete
DataInc |> 
     group_by(Country, Year) |> 
     summarise(Complete = n(),
               .groups = 'drop') |> 
     pivot_wider(names_from = Year, values_from = Complete)

# save data ---------------------------------------------------------------

write.csv(DataInc, './Outcome/Table S1.csv', row.names = F)
