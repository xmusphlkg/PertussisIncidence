
library(tidyverse)
library(lubridate)
library(paletteer)
library(patchwork)
library(openxlsx)

fill_color <- paletteer_d("ggsci::nrc_npg")

# Load data---------------------------------------------------------

scientific_10 <- function(x) {
     ifelse(x == 0, 0, parse(text = gsub("[+]", "", gsub("e", "%*%10^", scales::scientific_format()(x)))))
}

DataNews <- read.csv('./Data/HealthmapData.csv') |> 
     filter(alert_tag %in% c('NDR', 'Breaking')) |> 
     select(country) |>
     left_join(read.csv('./Data/iso3.csv'), by = c('country' = 'Country')) |> 
     unique()

df_clean <- read.csv('./Outcome/Table S1.csv') |> 
     group_by(Country) |>
     mutate(Date = as.Date(Date),
            AnnualizedInci = case_when(all(is.na(Month)) ~ Incidence * 52.14,
                                       all(!is.na(Month)) ~ Incidence * 12))

df_year <- df_clean |> 
     group_by(Country, Year) |>
     summarise(Cases = sum(Cases, na.rm = TRUE),
               Population = mean(Population, na.rm = TRUE),
               .groups = 'drop') |> 
     mutate(Incidence = Cases / Population)  |> 
     filter(Year != 2024) |> 
     mutate(Date = as.Date(paste0(Year, '-01-01')))

# plot ---------------------------------------------------------------------

fig <- ggplot(df_clean, aes(x = Date, y = Cases, color = Country)) +
     geom_line() +
     scale_x_date(date_labels = '%Y') +
     scale_y_continuous(labels = scales::comma) +
     facet_wrap(~Country, scales = 'free_y', nrow = 3) +
     labs(title = 'Epidemic curve of pertussis cases',
          x = 'Date',
          y = 'Cases',
          color = 'Country') +
     theme_bw() +
     theme(legend.position = 'none')

ggsave(filename = './preview.png',
       plot = fig,
       width = 12,
       height = 10)

# df_clean <- df_clean |> 
#      filter(Country %in% DataNews$ISO2)

# figure 1 ----------------------------------------------------------------

country_list <- c('US', 'GB', 'SE', 'CN', 'JP',
                  'SG', 'AU', 'NZ')

split_dates <- c(as.Date(c("2015/1/1", "2020/1/1", "2023/7/1")), max(df_clean$Date))
split_periods <- c("2015 Jan to 2019 Dec", "2020 Jan to 2023 Jun", "2023 Jun onwards")
datafile_rect <- data.frame(Period = split_periods,
                            start = split_dates[1:3],
                            end = split_dates[2:4])

i <- 1

plot_epidemic <- function(i){
     data_year <- df_year |> 
          filter(Country == country_list[i]) |> 
          select(Date, Incidence) |> 
          mutate(Type = 'Annual') |> 
          rename(AnnualizedInci = Incidence)
     data_year <- data_year |> 
          mutate(Date = Date + years(1) - days(1)) |> 
          rbind(data_year) |> 
          arrange(Date)
     
     data <- df_clean |> 
          filter(Country == country_list[i]) |> 
          ungroup() |> 
          select(Date, AnnualizedInci) |> 
          mutate(Type = 'Annualized') |> 
          rbind(data_year)
     
     plot_breaks <- pretty(c(0, data$AnnualizedInci))
     plot_range <- range(plot_breaks)
     
     fig_1 <- ggplot(data)+
          geom_rect(data = datafile_rect,
                    aes(xmin = start, xmax = end, fill = Period),
                    ymin = -Inf,
                    ymax = Inf,
                    alpha = 0.2,
                    show.legend = T) +
          geom_line(aes(x = Date, y = AnnualizedInci, color = Type)) +
          scale_x_date(date_labels = '%Y',
                       date_breaks = 'year',
                       limits = range(df_clean$Date),
                       expand = expansion(add = c(0, 0))) +
          scale_y_continuous(limits = plot_range,
                             breaks = plot_breaks,
                             expand = expansion(mult = c(0, 0))) +
          scale_fill_manual(values = fill_color[c(5:7)]) +
          scale_color_manual(values = fill_color[c(3, 1)]) +
          theme_bw() +
          theme(
               plot.title.position = "plot",
               plot.caption.position = "plot",
               plot.title = element_text(face = "bold", size = 14, hjust = 0),
               panel.grid.major.y = element_blank(),
               panel.grid.minor = element_blank(),
               legend.text = element_text(face = "bold", size = 12),
               legend.title = element_text(face = "bold", size = 12),
               legend.box.background = element_rect(fill = "transparent", colour = "transparent"),
               legend.background = element_rect(fill = "transparent", colour = "transparent"),
               axis.title = element_text(face = "bold", size = 12, color = "black"),
               axis.text.y = element_text(size = 12, color = "black"),
               axis.text.x = element_text(size = 12, color = "black", hjust = 0),
               plot.background = element_blank()
          ) +
          labs(title = paste0(LETTERS[i], ': ', country_list[i]),
               y = 'Incidence rate',
               x = 'Date',
               color = 'Incidence rate',
               fill = 'Stage')+
          guides(fill = guide_legend(nrow = 1,
                                     title.position = 'left'))
     
     fig_1
}

outcome <- lapply(1:length(country_list), plot_epidemic)
# outcome[[length(outcome) + 1]] <- guide_area()

fig <- outcome |> 
     wrap_plots(ncol = 2, guides = 'collect')&
     theme(legend.position = 'bottom')

ggsave(filename = './Outcome/Fig 2.pdf',
       plot = fig,
       width = 12,
       height = 10, 
       device = cairo_pdf,
       family = 'Times New Roman')

write.xlsx(df_year,
          './Outcome/fig data/fig 2.xlsx')
