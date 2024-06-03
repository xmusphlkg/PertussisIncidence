
# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(sf)
library(lubridate)
library(paletteer)

Sys.setlocale("LC_TIME", "en")

# data --------------------------------------------------------------------

fill_color <- paletteer_d("ggsci::nrc_npg")

# Load data
DataAll <- read.csv('./Outcome/Table S1.csv') |> 
     mutate(Date = as.Date(Date))

DataMap <- st_read("./Data/world.zh.json") |> 
     filter(iso_a3  != "ATA")

DataNews <- read.csv('./Data/HealthmapData.csv') |> 
     select(country, issue_date, alert_tag) |>
     left_join(read.csv('./Data/iso3.csv'), by = c('country' = 'Country')) |> 
     mutate(issue_date = mdy_hm(issue_date)) |> 
     filter(alert_tag %in% c('NDR', 'Breaking'))

DataMapPlot <- DataNews |> 
     group_by(ISO3) |>
     summarise(start_issue_date = min(issue_date),
               .groups = 'drop') |> 
     mutate(start_issue_date = as.numeric(difftime(start_issue_date, as.Date('2023-5-1'), units = 'days')))
DataMapPlot <- DataMap |> 
     left_join(DataMapPlot, by = c('iso_a3' = 'ISO3'))

# fig 1 -----------------------------------------------------------------

fill_color <- c("#E64B35FF", "#00A087FF")

fig_1 <- ggplot(DataNews)+
     geom_histogram(aes(x = issue_date, fill = alert_tag))+
     scale_fill_manual(values = fill_color)+
     scale_x_datetime(date_breaks = "1 month", date_labels = "%b %Y")+
     scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
                        limits = c(0, NA))+
     theme_bw()+
     theme(panel.grid = element_blank(),
           plot.title = element_text(face = "bold", size = 14, hjust = 0),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.position = c(0, 1),
           legend.justification = c(0, 1),
           legend.background = element_blank(),
           plot.title.position = 'plot')+
     labs(title = "A", x = NULL, y = 'News event', fill = 'Alert Tag')

# fig 2 ----------------------------------------------------------------

fill_color <- rev(c("#1D3141FF", "#096168FF", "#209478FF", "#75C56EFF", "#E2EE5EFF"))

fig_2 <- ggplot(data = DataMapPlot) +
     geom_sf(aes(fill = start_issue_date)) +
     # add x, y tick labels
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8)) +
     scale_x_continuous(limits = c(-180, 180),
                        expand = c(0, 0)) + 
     scale_y_continuous(limits = c(-70, 75)) +
     scale_fill_gradientn(colors = fill_color,
                          limits = c(0, 365),
                          breaks = seq.Date(as.Date('2023-5-1'), as.Date('2024-5-1'), by = '1 month') - as.Date('2023-5-1'),
                          labels = format(seq.Date(as.Date('2023-5-1'), as.Date('2024-5-1'), by = '1 month'), '%b %Y'),
                          na.value = "white")+
     theme_bw() +
     theme(panel.grid = element_blank(),
           plot.title = element_text(face = "bold", size = 14, hjust = 0),
           panel.background = element_rect(fill = "#C1CDCD", color = NA),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.position = 'bottom',
           legend.background = element_rect(fill = "white"),
           plot.title.position = 'plot') +
     labs(title = "B", x = NULL, y = NULL, fill = 'Firsted issued date') +
     guides(fill = guide_legend(direction = "horizontal",
                                keyheight = 0.5, 
                                keywidth = 3/length(labels),
                                title.position = 'top',
                                title.hjust = 0.5,
                                label.hjust = 0.5,
                                nrow = 1,
                                byrow = TRUE,
                                label.position = "bottom"))

# save --------------------------------------------------------------------

fig <- cowplot::plot_grid(fig_1, fig_2, ncol = 1, rel_heights = c(1, 1.5))

ggsave(filename = './Outcome/Fig 1.pdf',
       plot = fig,
       width = 8,
       height = 8, 
       device = cairo_pdf,
       family = 'Times New Roman')
