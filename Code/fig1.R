
# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(sf)
library(lubridate)
library(paletteer)
library(patchwork)

rm(list = ls())

Sys.setlocale("LC_TIME", "en")

# data --------------------------------------------------------------------

fill_color <- paletteer_d("ggsci::nrc_npg")

# Load data
DataAll <- read.csv('./Outcome/Table S1.csv') |> 
     mutate(Date = as.Date(Date))

DataMap <- st_read("./Data/world.zh.json") |> 
     filter(iso_a3  != "ATA")

DataNews <- read.csv('./Outcome/Table S2.csv') |> 
     left_join(read.csv('./Data/iso3.csv'), by = c('country' = 'Country')) |> 
     mutate(issue_date = as.Date(issue_date))

DataMapPlot <- DataNews |> 
     group_by(ISO3) |>
     summarise(issue_date = min(issue_date),
               .groups = 'drop') |> 
     mutate(start_issue_date = as.numeric(difftime(issue_date, as.Date('2023-5-1'), units = 'days')),
            yearmonth = format(issue_date, "%Y %m"),
            monthyear = format(issue_date, "%b %Y")) |> 
     arrange(start_issue_date)

DataNews <- DataNews |> 
     mutate(yearmonth = format(issue_date, "%Y %m")) |> 
     group_by(yearmonth) |>
     count() |> 
     arrange(yearmonth) |> 
     ungroup() |>
     # add breaks for each month
     complete(yearmonth = seq.Date(from = as.Date('2023-05-01'),
                                   to = as.Date('2025-10-01'),
                                   by = 'month') |> format('%Y %m'),
              fill = list(n = 0)) |>
     arrange(yearmonth) |>
     mutate(issue_date = as.Date(paste0(yearmonth, ' 01'), '%Y %m %d'),
            monthyear = format(issue_date, "%b %Y"))

DataCountry <- DataMapPlot |> 
     rownames_to_column(var = "ID") |>
     group_by(yearmonth, monthyear) |>
     summarise(n = max(as.integer(ID)),
               .groups = 'drop') |> 
     arrange(yearmonth) |> 
     # complete the month
     complete(yearmonth = DataNews$yearmonth,
              fill = list(n = NA)) |>
     arrange(yearmonth) |> 
     # using last observation carried forward to fill in missing values
     mutate(n = zoo::na.locf(n, na.rm = FALSE),
            issue_date = as.Date(paste0(yearmonth, ' 01'), '%Y %m %d'),
            monthyear = format(issue_date, "%b %Y"))

# fig 1 -----------------------------------------------------------------

fill_color <- c("#E64B35FF", "#00A087FF")

fig1 <- ggplot(DataNews)+
     geom_col(aes(x = issue_date, y = n),
              fill = fill_color[1],
              color = 'white')+
     scale_x_date(limits = c(as.Date('2023-5-1'), as.Date('2025-10-31')),
                  date_labels = "%b %Y",
                  expand = expansion(add = c(0, 0)))+
     scale_y_continuous(expand = expansion(mult = c(0, 0)),
                        limits = c(0, 100))+
     theme_bw()+
     theme(panel.grid = element_blank(),
           plot.title = element_text(face = "bold", size = 14, hjust = 0),
           axis.text.y = element_text(color = 'black', face = 'plain', size = 12),
           axis.text.x = element_blank(),
           axis.title = element_text(face = "bold", size = 12, color = "black"),
           legend.title = element_text(face = "bold", size = 12),
           legend.position = c(0, 1),
           legend.justification = c(0, 1),
           legend.background = element_blank(),
           plot.title.position = 'plot')+
     labs(title = "A", x = NULL, y = 'Number of alert news', fill = 'Alert Tag')

fig2 <- ggplot(DataNews)+
     geom_col(data = DataCountry,
              aes(x = issue_date, y = n),
              fill = fill_color[2])+
     scale_x_date(limits = c(as.Date('2023-5-1'), as.Date('2025-10-31')),
                  date_labels = "%b %Y",
                  expand = expansion(add = c(0, 0)))+
     scale_y_continuous(expand = expansion(mult = c(0, 0)),
                        limits = c(0, 60))+
     theme_bw()+
     theme(panel.grid = element_blank(),
           plot.title = element_text(face = "bold", size = 14, hjust = 0),
           axis.text.y = element_text(size = 12, color = "black"),
           axis.text.x = element_text(size = 12, color = "black", angle = 45, hjust = 1),
           axis.title = element_text(face = "bold", size = 12, color = "black"),
           legend.title = element_text(face = "bold", size = 12),
           legend.position = c(0, 1),
           legend.justification = c(0, 1),
           legend.background = element_blank(),
           plot.title.position = 'plot')+
     labs(title = "B", x = 'Alert issued date', y = 'Cumlative number of alert country', fill = 'Alert Tag')

# fig 3 ----------------------------------------------------------------

fill_color <- c("#6B200CFF", "#973D21FF", "#DA6C42FF", "#EE956AFF", "#FBC2A9FF", "#BAD6F9FF", "#7DB0EAFF", "#447FDDFF", "#225BB2FF", "#133E7EFF")

DataMapPlot <- DataMap |> 
     left_join(DataMapPlot, by = c('iso_a3' = 'ISO3'))

fig3 <- ggplot(data = DataMapPlot) +
     geom_sf(aes(fill = start_issue_date)) +
     # add x, y tick labels
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8)) +
     scale_x_continuous(limits = c(-180, 180),
                        expand = c(0, 0)) + 
     scale_y_continuous(limits = c(-70, 75)) +
     scale_fill_gradientn(colors = fill_color,
                          limits = c(0, 900),
                          breaks = seq.Date(as.Date('2023-5-1'), as.Date('2025-10-16'), by = '2 month') - as.Date('2023-5-1'),
                          labels = c('May\n2023', 'Jul', 'Sep', 'Nov',
                                     'Jan\n2024', 'Mar', 'May', 'Jul', 'Sep', 'Nov',
                                     'Jan\n2025', 'Mar', 'May', 'Jul', 'Sep'),
                          na.value = "white")+
     theme_bw() +
     theme(panel.grid = element_blank(),
           plot.title = element_text(face = "bold", size = 14, hjust = 0),
           panel.background = element_rect(fill = "#C1CDCD", color = NA),
           axis.text = element_text(size = 12, color = "black"),
           axis.title = element_text(face = "bold", size = 12, color = "black"),
           legend.position = 'bottom',
           legend.title = element_text(face = "bold", size = 12),
           legend.text = element_text(size = 12),
           legend.background = element_rect(fill = "white"),
           plot.title.position = 'plot') +
     labs(title = "C", x = NULL, y = NULL, fill = 'First alert issued date') +
     guides(fill = guide_legend(direction = "horizontal",
                                keyheight = 0.5,
                                title.position = 'left',
                                title.hjust = 0.5,
                                title.vjust = 1,
                                label.hjust = 0.5,
                                nrow = 1,
                                byrow = TRUE,
                                label.position = "bottom"))

# focus on europe --------------------------------------------------------
fig3_minor <- ggplot(data = DataMapPlot) +
     geom_sf(aes(fill = start_issue_date),
             show.legend = F) +
     # add x, y tick labels
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8)) +
     scale_x_continuous(limits = c(-10, 40),
                        expand = c(0, 0)) + 
     scale_y_continuous(limits = c(35, 70)) +
     scale_fill_gradientn(colors = fill_color,
                          limits = c(0, 900),
                          breaks = seq.Date(as.Date('2023-5-1'), as.Date('2025-10-16'), by = '2 month') - as.Date('2023-5-1'),
                          labels = c('May\n2023', 'Jul', 'Sep', 'Nov',
                                     'Jan\n2024', 'Mar', 'May', 'Jul', 'Sep', 'Nov',
                                     'Jan\n2025', 'Mar', 'May', 'Jul', 'Sep'),
                          na.value = "white")+
     theme_bw() +
     theme(panel.grid = element_blank(),
           plot.title = element_text(face = "bold", size = 14, hjust = 0),
           panel.background = element_rect(fill = "#C1CDCD", color = NA),
           axis.text = element_blank(),
           axis.title = element_text(face = "bold", size = 12, color = "black"),
           plot.background = element_rect(fill = "#C1CDCD", color = NA),
           axis.ticks = element_blank()) +
     labs(x = NULL, y = NULL, fill = NULL, title = NULL)

fig3 <- fig3 + inset_element(fig3_minor, left = 0, bottom = 0.01, right = 0.2, top = 0.5)

# save --------------------------------------------------------------------

fig_1 <- cowplot::plot_grid(fig1/fig2 + plot_layout(heights = c(0.4, 0.6)),
                            fig3,
                            nrow = 1, rel_widths = c(0.3, 1))

ggsave(filename = './Outcome/Fig 1.pdf',
       plot = fig_1,
       width = 14,
       height = 6, 
       device = cairo_pdf,
       family = 'Times New Roman')
