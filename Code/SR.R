
library(tidyverse)
library(lubridate)
library(patchwork)

df_raw <- read.csv("sr_data.csv", encoding = "UTF-8")
names(df_raw) <- c('gender', 'birth_date', 'class', 'class2', 'onset_date', 'diag_date',
                   'death_date', 'report_date', 'report_location', 'delete_date',
                   'delete_user', 'delete_location', 'delete_reason', 'note')
df_clean <- df_raw
df_clean[df_clean == "."] <- NA
df_clean <- df_clean |> 
     mutate(across(contains('date'), as.Date)) |>
     filter(is.na(delete_date))


# fig 1 -------------------------------------------------------------------

data <- df_clean |>
     mutate(onset_year = year(onset_date),
            month = month(onset_date),
            group_date = as.Date(paste0(onset_year, "-", month, "-01")),
            age = round((onset_date - birth_date)/365, 1),
            age_group = case_when(
                 age < 1 ~ "00-01",
                 age < 4 ~ "02-04",
                 age < 10 ~ "05-09",
                 age < 16 ~ "10-15",
                 TRUE ~ "16+"
            )) |>
     group_by(group_date, age_group) |>
     summarise(n = n())
fig1 <- ggplot(data, aes(x = group_date, y = n)) +
    geom_col() +
     scale_y_continuous(expand = c(0, 0))+
    labs(title = "a",
         x = "Date",
         y = "Monthly incidence") +
    theme_classic()

# fig 2 -------------------------------------------------------------------

fig2 <- data |> 
     mutate(year = year(group_date)) |>
     group_by(year, age_group) |>
     summarise(n = sum(n)) |>
     ggplot(aes(x = year, y = n, fill = age_group)) +
    geom_col(position = "stack") +
     scale_y_continuous(expand = c(0, 0)) +
     scale_x_continuous(breaks = 2014:2024) +
    labs(title = "b",
         x = "Year",
         fill = 'Age group',
         y = "Yearly incidence") +
    theme_classic()+
     theme(legend.position = "bottom")

# fig 3 -------------------------------------------------------------------

fig3 <- data |> 
     mutate(year = year(group_date)) |>
     group_by(year, age_group) |>
     summarise(n = sum(n)) |>
     ggplot(aes(x = year, y = n, fill = age_group)) +
     geom_col(position = "fill") +
     scale_y_continuous(labels = scales::percent_format(),
                        expand = c(0, 0)) +
     scale_x_continuous(breaks = 2014:2024) +
     labs(title = "c",
          x = "Year",
          fill = 'Age group',
          y = "Yearly incidence") +
     theme_classic()+
     theme(legend.position = "bottom")

ggsave("SR.png", 
       plot = fig1 + fig2 + fig3 + plot_layout(ncol = 1),
       width = 10, height = 10)
