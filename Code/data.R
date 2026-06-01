
# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(lubridate)

# data --------------------------------------------------------------------

DataFile <- "./Data/Pertussis incidence.xlsx"

DataPop <- read.xlsx(DataFile, sheet = 'Population') |> 
     mutate(Name = str_trim(Name)) |>
     rename(PopulationCountry = any_of("Country"))
Countries <- unique(DataPop$Name)
SheetNames <- getSheetNames(DataFile)

DataInc <- map(Countries, function(country){
     sheet <- SheetNames[str_trim(SheetNames) == country][1]
     if (is.na(sheet)) {
          stop(paste("Cannot find sheet named", country))
     }
     
     read.xlsx(DataFile,
               sheet = sheet,
               detectDates = T) |> 
          mutate(Date = as.Date(Date),
                 Country = str_trim(Country),
                 Year = suppressWarnings(as.integer(as.character(Year))),
                 Year = if_else((is.na(Year) | Year < 1900) & !is.na(Date) & !is.na(Week),
                                isoyear(Date),
                                Year),
                 Year = if_else((is.na(Year) | Year < 1900) & !is.na(Date),
                                year(Date),
                                Year))
})
DataInc <- do.call(rbind, DataInc) |> 
     mutate(Date = case_when(
          Country %in% c('NZ', 'SE') & Year == 2017 & Month == 4 ~ as.Date('2017-04-01'),
          Country == 'SG' & Year == 2023 & Week == 3 ~ as.Date('2023-01-15'),
          TRUE ~ Date
     )) |>
     group_by(Country, Year, Month, Week, Date) |>
     slice_tail(n = 1) |>
     ungroup() |>
     left_join(DataPop, by = c('Country' = 'Name', 'Year' = 'Year')) |> 
     mutate(CasesRaw = Cases,
            RawIncidence = CasesRaw / Population)

AnnualOverrides <- read.csv('./Data/annual_case_overrides.csv')

DataInc <- DataInc |>
     left_join(AnnualOverrides, by = c('Country', 'Year')) |>
     group_by(Country, Year) |>
     mutate(AnnualRawCases = sum(CasesRaw, na.rm = TRUE),
            AdjustmentFactor = case_when(
                 !is.na(OfficialAnnualCases) & AnnualRawCases > 0 ~ OfficialAnnualCases / AnnualRawCases,
                 TRUE ~ 1
            ),
            AnalysisCases = CasesRaw * AdjustmentFactor,
            AnalysisIncidence = AnalysisCases / Population,
            AnnualCaseSource = if_else(is.na(AnnualCaseSource),
                                       'Reporting-interval sum',
                                       AnnualCaseSource),
            AnnualCaseNotes = replace_na(AnnualCaseNotes, ''),
            Cases = CasesRaw,
            Incidence = RawIncidence) |>
     ungroup()

## check the data is complete
DataInc |> 
     group_by(Country, Year) |> 
     summarise(Complete = n(),
               .groups = 'drop') |> 
     pivot_wider(names_from = Year, values_from = Complete)

write.csv(DataInc, './Outcome/Table S1.csv', row.names = F)

# map data ----------------------------------------------------------------

DataNews <- read.csv('./Data/HealthmapData.csv')
if (!'url' %in% names(DataNews)) {
     DataNews$url <- NA_character_
}
# extract the country name from the place_name, if contains a ", " then the country name is after the ", ", else the country name is the place_name
DataNews <- DataNews |> 
     mutate(country = ifelse(grepl(", ", place_name), 
                             str_trim(str_extract(place_name, "[^,]+$")),
                            place_name)) |> 
     select(country, issue_date = date, alert_title = title, alert_url = url, raw_place_name = place_name) |> 
     mutate(issue_date = as.Date(issue_date),
            start_issue_date = as.numeric(difftime(issue_date, as.Date('2023-1-1'), units = 'days')))

DataNews <- DataNews |> 
     # replace Hong Kong, Macau, and Taiwan with China
     mutate(country = ifelse(country %in% c('Hong Kong', 'Macau', 'Taiwan'), 'China', country),
            # replace Guam [USA] with United States
            country = ifelse(country == 'Guam [USA]', 'United States', country))
write.csv(DataNews, './Outcome/Table S2.csv', row.names = F)

