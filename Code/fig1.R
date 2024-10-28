
# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(sf)
library(lubridate)
library(paletteer)
library(patchwork)

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
     mutate(yearmonth = format(issue_date, "%Y %m"),
            monthyear = format(issue_date, "%b %Y")) |> 
     group_by(yearmonth, monthyear) |>
     count() |> 
     arrange(yearmonth)

DataCountry <- DataMapPlot |> 
     rownames_to_column(var = "ID") |>
     group_by(yearmonth, monthyear) |>
     summarise(n = max(as.integer(ID)),
               .groups = 'drop')

# fig 1 -----------------------------------------------------------------

fill_color <- c("#E64B35FF", "#00A087FF")

fig1 <- ggplot(DataNews)+
     geom_col(aes(x = monthyear, y = n),
              fill = fill_color[1],
              color = 'white')+
     geom_line(data = DataCountry,
               aes(x = monthyear, y = n*2, group = 1),
               color = fill_color[2],
               linewidth = 1)+
     geom_point(data = DataCountry,
               aes(x = monthyear, y = n*2, group = 1),
               color = fill_color[2],
               size = 1)+
     scale_x_discrete(limits = DataNews$monthyear)+
     scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
                        sec.axis = sec_axis(~ . * 0.5, name = "Cumlative number of alert country"),
                        limits = c(0, NA))+
     theme_bw()+
     theme(panel.grid = element_blank(),
           plot.title = element_text(face = "bold", size = 14, hjust = 0),
           axis.text.y = element_text(color = 'black', face = 'plain'),
           axis.text.x = element_text(face = "plain", color = "black", angle = 45, vjust = 1, hjust = 1),
           axis.title = element_text(face = "bold", size = 12, color = "black"),
           legend.title = element_text(face = "bold", size = 12),
           legend.position = c(0, 1),
           legend.justification = c(0, 1),
           legend.background = element_blank(),
           plot.title.position = 'plot')+
     labs(title = "A", x = NULL, y = 'Number of alert news', fill = 'Alert Tag')

# fig 2 ----------------------------------------------------------------

fill_color <- c("#EF8A47FF", "#F7AA58FF", "#FFD06FFF", "#FFE6B7FF", "#AADCE0FF", "#72BCD5FF", "#528FADFF", "#376795FF", "#1E466EFF")

DataMapPlot <- DataMap |> 
     left_join(DataMapPlot, by = c('iso_a3' = 'ISO3'))

fig2 <- ggplot(data = DataMapPlot) +
     geom_sf(aes(fill = start_issue_date)) +
     # add x, y tick labels
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8)) +
     scale_x_continuous(limits = c(-180, 180),
                        expand = c(0, 0)) + 
     scale_y_continuous(limits = c(-70, 75)) +
     scale_fill_gradientn(colors = fill_color,
                          limits = c(0, 600),
                          breaks = seq.Date(as.Date('2023-7-1'), as.Date('2024-11-1'), by = '2 month') - as.Date('2023-5-1'),
                          labels = c('May - Jun 2023', 'Jul - Aug 2023', 'Sep - Oct 2023', 'Nov - Dec 2023',
                                     'Jan - Feb 2024', 'Mar - Apr 2024', 'May - Jun 2024', 'Jul - Aug 2024',
                                     'Sep - Oct 2024'),
                          na.value = "white")+
     theme_bw() +
     theme(panel.grid = element_blank(),
           plot.title = element_text(face = "bold", size = 14, hjust = 0),
           panel.background = element_rect(fill = "#C1CDCD", color = NA),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(face = "bold", size = 12, color = "black"),
           legend.position = 'bottom',
           legend.title = element_text(face = "bold", size = 12),
           legend.background = element_rect(fill = "white"),
           plot.title.position = 'plot') +
     labs(title = "B", x = NULL, y = NULL, fill = 'First alert issued date') +
     guides(fill = guide_legend(direction = "horizontal",
                                keyheight = 0.5,
                                title.position = 'top',
                                title.hjust = 0.5,
                                label.hjust = 0.5,
                                nrow = 1,
                                byrow = TRUE,
                                label.position = "bottom"))

# focus on europe --------------------------------------------------------
fig2_minor <- ggplot(data = DataMapPlot) +
     geom_sf(aes(fill = start_issue_date),
             show.legend = F) +
     # add x, y tick labels
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8)) +
     scale_x_continuous(limits = c(-10, 40),
                        expand = c(0, 0)) + 
     scale_y_continuous(limits = c(35, 70)) +
     scale_fill_gradientn(colors = fill_color,
                          limits = c(0, 600),
                          breaks = seq.Date(as.Date('2023-7-1'), as.Date('2024-11-1'), by = '2 month') - as.Date('2023-5-1'),
                          labels = c('May - Jun 2023', 'Jul - Aug 2023', 'Sep - Oct 2023', 'Nov - Dec 2023',
                                     'Jan - Feb 2024', 'Mar - Apr 2024', 'May - Jun 2024', 'Jul - Aug 2024',
                                     'Sep - Oct 2024'),
                          na.value = "white")+
     theme_bw() +
     theme(panel.grid = element_blank(),
           plot.title = element_text(face = "bold", size = 14, hjust = 0),
           panel.background = element_rect(fill = "#C1CDCD", color = NA),
           axis.text = element_blank(),
           plot.background = element_rect(fill = "#C1CDCD", color = NA),
           axis.ticks = element_blank()) +
     labs(x = NULL, y = NULL, fill = NULL, title = NULL)

fig2 <- fig2 + inset_element(fig2_minor, left = 0, bottom = 0.01, right = 0.2, top = 0.5)

# save --------------------------------------------------------------------

fig_1 <- cowplot::plot_grid(fig1, fig2, ncol = 1, rel_heights = c(1, 1.5))

ggsave(filename = './Outcome/Fig 1.pdf',
       plot = fig_1,
       width = 9,
       height = 9, 
       device = cairo_pdf,
       family = 'Times New Roman')
