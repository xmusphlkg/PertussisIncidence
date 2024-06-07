
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

DataNews <- read.csv('./Data/HealthmapData.csv') |> 
     select(country, issue_date, alert_tag) |>
     left_join(read.csv('./Data/iso3.csv'), by = c('country' = 'Country')) |> 
     mutate(issue_date = mdy_hm(issue_date)) |> 
     filter(alert_tag %in% c('NDR', 'Breaking'))

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

DataPHSM <- read.csv('./Data/OxCGRT_simplified_v1 (1).csv')
DataPHSM <- DataPHSM |> 
     filter(CountryCode %in% DataMapPlot$ISO3 & Jurisdiction == 'NAT_TOTAL') |> 
     mutate(Date = as.Date(as.character(Date), format = "%Y%m%d")) |> 
     filter(Date >= as.Date('2022-01-01')) |> 
     select(CountryCode, Date, StringencyIndex_Average) |> 
     arrange(CountryCode, Date)
# find the date reach the point at the end of the data
last_day_data <- DataPHSM |> 
     group_by(CountryCode) |> 
     summarize(LastDate = max(Date),
               LastSI = last(StringencyIndex_Average), .groups = 'drop')

DataPHSM <- DataPHSM |> 
     left_join(last_day_data, by = "CountryCode") |> 
     filter(StringencyIndex_Average == LastSI & Date < LastDate) |> 
     group_by(CountryCode) |> 
     summarize(EarliestMatchingDate = min(Date), .groups = 'drop') |> 
     left_join(DataMapPlot, by = c('CountryCode' = 'ISO3')) |> 
     left_join(read.csv('./Data/VaccineData.csv'), by = c('CountryCode' = 'CODE'))

DataMapPlot <- DataMap |> 
     left_join(DataMapPlot, by = c('iso_a3' = 'ISO3'))

# fig 3 -------------------------------------------------------------------

fill_color <- c("#E64B35FF", "#00A087FF")

DataPHSM <- DataPHSM |> 
     mutate(MatchingDate = as.numeric(difftime(EarliestMatchingDate, as.Date('2023-5-1'), units = 'days')))

# relation between the first alert and the first PHSM
test_result <- cor.test(DataPHSM$start_issue_date, DataPHSM$MatchingDate, method = "pearson", use = "complete.obs")
cor_coef <- test_result$estimate
p_value <- test_result$p.value

fig3 <- ggplot(DataPHSM)+
     geom_point(aes(x = start_issue_date, y = MatchingDate),
                color = fill_color[1])+
     geom_smooth(aes(x = start_issue_date, y = MatchingDate),
                 method = 'lm',
                 color = fill_color[2],
                 se = FALSE) +
     annotate("text", x = Inf, y = Inf,
              hjust = 1.1, vjust = 5, size = 5, color = "black",
              label = bquote(~ italic(r) == .(round(cor_coef, 2)) ~ "," ~ italic(P) == .(round(p_value, 2)))) +
     scale_y_continuous(limits = c(-450, -120),
                        breaks = as.numeric(difftime(seq.Date(as.Date('2022-3-1'), as.Date('2023-1-1'), by = '2 month'),
                                                     as.Date('2023-5-1'),
                                                     units = 'days')),
                        labels = format(seq.Date(as.Date('2022-3-1'), as.Date('2023-1-1'), by = '2 month'), '%b %Y'))+
     scale_x_continuous(limits = c(0, 365),
                        breaks = seq(0, 365, 90),
                        labels = format(seq.Date(as.Date('2023-5-1'), as.Date('2024-5-1'), by = '3 month'), '%b %Y'))+
     theme_bw()+
     theme(panel.grid = element_blank(),
           plot.title = element_text(face = "bold", size = 14, hjust = 0),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.position = c(0, 1),
           legend.justification = c(0, 1),
           legend.background = element_blank(),
           plot.background = element_blank(),
           plot.title.position = 'plot')+
     labs(title = "C", x = 'First alert issued date', y = 'PHSM ended date')

# fig 4-8 -----------------------------------------------------------------

axis_labels <- c('Recommended vaccine (dose)',
                 'Recommended vaccine (dose),\n<2 years',
                 'Recommended vaccine (dose),\n2 years+',
                 'DTP vaccination coverage,\n1st dose',
                 'DTP vaccination coverage,\n3rd dose')

i <- 1
plot_realtion <- function(i){
     data <- DataPHSM |> 
          select(start_issue_date, i + 6)
     names(data) <- c('start_issue_date', 'n')
     
     # relation between the first alert
     test_result <- cor.test(data$start_issue_date, data$n, method = "pearson", use = "complete.obs")
     cor_coef <- test_result$estimate
     p_value <- test_result$p.value
     
     fig <- ggplot(data)+
          geom_point(aes(x = start_issue_date, y = n),
                     color = fill_color[1])+
          geom_smooth(aes(x = start_issue_date, y = n),
                      method = 'lm',
                      color = fill_color[2],
                      se = FALSE) +
          annotate("text", x = Inf, y = Inf,
                   hjust = 1.1, vjust = 5, size = 5, color = "black",
                   label = bquote(~ italic(r) == .(round(cor_coef, 2)) ~ "," ~ italic(P) == .(round(p_value, 2)))) +
          scale_x_continuous(limits = c(0, 365),
                             breaks = seq(0, 365, 90),
                             labels = format(seq.Date(as.Date('2023-5-1'), as.Date('2024-5-1'), by = '3 month'), '%b %Y'))+
          theme_bw()+
          theme(panel.grid = element_blank(),
                plot.title = element_text(face = "bold", size = 14, hjust = 0),
                axis.text = element_text(color = 'black', face = 'plain'),
                axis.title = element_text(color = 'black', face = 'plain'),
                legend.position = c(0, 1),
                legend.justification = c(0, 1),
                plot.background = element_blank(),
                legend.background = element_blank(),
                plot.title.position = 'plot')+
          labs(title = LETTERS[i+3],
               x = 'First alert issued date',
               y = axis_labels[i])
}

fig4 <- lapply(1:5, plot_realtion)

# fig 1 -----------------------------------------------------------------

fig1 <- ggplot(DataNews)+
     geom_col(aes(x = monthyear, y = n),
              fill = fill_color[1],
              color = 'white')+
     geom_line(data = DataCountry,
               aes(x = monthyear, y = n*2, group = 1),
               color = fill_color[2],
               size = 1)+
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
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.position = c(0, 1),
           legend.justification = c(0, 1),
           legend.background = element_blank(),
           plot.title.position = 'plot')+
     labs(title = "A", x = NULL, y = 'Number of alert news', fill = 'Alert Tag')

# fig 2 ----------------------------------------------------------------

fill_color <- c("#E76254FF", "#EF8A47FF", "#F7AA58FF", "#FFD06FFF", "#FFE6B7FF", "#AADCE0FF", "#72BCD5FF", "#528FADFF", "#376795FF", "#1E466EFF")

fig2 <- ggplot(data = DataMapPlot) +
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
     labs(title = "B", x = NULL, y = NULL, fill = 'First alert issued date') +
     guides(fill = guide_legend(direction = "horizontal",
                                keyheight = 0.5, 
                                keywidth = 2.8,
                                title.position = 'top',
                                title.hjust = 0.5,
                                label.hjust = 0.5,
                                nrow = 1,
                                byrow = TRUE,
                                label.position = "bottom"))

# save --------------------------------------------------------------------

fig_1 <- cowplot::plot_grid(fig1, fig2, ncol = 1, rel_heights = c(1, 1.5))

fig_2 <- fig3 + fig4[[1]] + fig4[[2]] + fig4[[3]] + fig4[[4]] + fig4[[5]]+
     plot_layout(ncol = 2)

fig <- cowplot::plot_grid(fig_1, fig_2, ncol = 2)

ggsave(filename = './Outcome/Fig 1.pdf',
       plot = fig,
       width = 14,
       height = 8, 
       device = cairo_pdf,
       family = 'Times New Roman')

