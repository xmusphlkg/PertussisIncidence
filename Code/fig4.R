
# This script generates Figure 4 in the manuscript.

# packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(patchwork)
library(openxlsx)
library(ggridges)
library(paletteer)
library(grid)
library(gtable)
library(sf)

fill_color <- c("#E76254FF", "#EF8A47FF", "#F7AA58FF", "#FFD06FFF", "#FFE6B7FF", "#AADCE0FF", "#72BCD5FF", "#528FADFF", "#376795FF", "#1E466EFF")

country_names <- c('US', 'GB', 'SE', 'CN', 'JP',
                   'SG', 'AU', 'NZ')

# figure --------------------------------------------------------------------

i <- 4

data_clean <- function(i){
     data <- read.xlsx('./Data/Pertussis case year age.xlsx',
                       sheet = country_names[i], detectDates = T)
     names(data)[1] <- 'Year'
     data[is.na(data)] <- 0
     data <- data |> 
          pivot_longer(cols = -Year, names_to = 'Age', values_to = 'Incidence') |> 
          mutate(StartAge = case_when(
               grepl("-", Age) ~ as.numeric(sub("-.*", "", Age)),
               grepl("\\+", Age) ~ as.numeric(sub("\\+.*", "", Age)),
               TRUE ~ NA_real_),
               EndAge = case_when(
                    grepl("-", Age) ~ as.numeric(sub(".*-", "", Age)),
                    grepl("\\+", Age) ~ 100,
                    TRUE ~ NA_real_)) |> 
          rowwise() |> 
          mutate(AgeList = if (is.na(StartAge)) list(NA_real_) else list(seq(StartAge, EndAge))) |> 
          unnest(AgeList) |> 
          filter(!is.na(AgeList)) |> 
          group_by(Year, AgeList) |> 
          mutate(AverageIncidence = Incidence / (EndAge - StartAge + 1)) |> 
          ungroup() %>%
          select(Year, Age = AgeList, AverageIncidence) |> 
          group_by(Year) |>
          mutate(Weight = AverageIncidence/sum(AverageIncidence),
                 Weight = case_when(
                      is.na(Weight) ~ 0,
                      TRUE ~ Weight
                 )) |> 
          group_by(Year) |> 
          do({
               dens <- density(.$Age, weights = .$Weight, adjust = 0.5, from = 0)
               data.frame(Age = dens$x, Density = dens$y, Year = unique(.$Year))
          })  |> 
          ungroup() |> 
          mutate(country = country_names[i])
     
     return(data)
}

plot_ridges <- function(i){
     data <- DataAll |> 
          filter(country == country_names[i])
     
     fig <- ggplot(data) +
          geom_density_ridges_gradient(mapping = aes(x = Age,
                                                     y = Year,
                                                     group = Year,
                                                     fill = Age,
                                                     height = Density),
                                       scale = 1.2,
                                       stat = "identity",
                                       rel_min_height = 0.01) +
          scale_x_continuous(limits = c(0, 100),
                             expand = c(0, 0),
                             breaks = seq(0, 100, 10)) +
          scale_y_continuous(breaks = seq(2015, 2023, 2),
                             limits = c(2015, NA),
                             expand = expansion(mult = c(0, 0.04)))+
          scale_fill_gradientn(colours = fill_color[c(1:4, 7:10)],
                               breaks = seq(0, 100, 10),
                               limits = c(0, 100))+
          labs(title = paste(LETTERS[ifelse(i>5, i+2, i)], country_names[i], sep = ": "),
               x = 'Age',
               y = 'Year',
               fill = "Age") +
          theme_bw()+
          theme(panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                plot.title = element_text(face = "bold", size = 14, hjust = 0),
                axis.text.y = element_text(color = 'black', face = 'plain'),
                axis.text.x = element_text(color = 'black', face = 'plain', hjust = 0.5),
                axis.title = element_text(color = 'black', face = 'plain'),
                legend.box = 'horizontal',
                plot.title.position = 'plot',
                plot.background = element_blank(),
                legend.position = if_else(i == 7, 'bottom', 'none'))+
          guides(fill = guide_colorbar(barwidth = 30,
                                       vjust = 0))
     fig
}

DataAll <- bind_rows(map(1:length(country_names), data_clean))

fig_min <- map(1:length(country_names), plot_ridges)

# map ---------------------------------------------------------------------

data <- DataAll |>
     group_by(country) |>
     arrange(Age) |>
     mutate(cum_weight = cumsum(Density)) |>
     summarise(MedianAge = Age[min(which(cum_weight >= sum(Density) / 2))],
               .groups = 'drop')

DataMap <- st_read("./Data/world.zh.json") |> 
     filter(iso_a3  != "ATA") |> 
     left_join(data, by = c("iso_a2" = "country"))

fig_1 <- ggplot(data = DataMap) +
     geom_sf(aes(fill = MedianAge)) +
     # add x, y tick labels
     theme(axis.text.x = element_text(size = 8),
           axis.text.y = element_text(size = 8)) +
     scale_x_continuous(limits = c(-180, 180),
                        expand = c(0, 0)) + 
     scale_y_continuous(limits = c(-60, 75)) +
     scale_fill_gradientn(colours = fill_color[c(1:4, 7:10)],
                          na.value = 'grey',
                          limits = c(0, 100))+
     theme_bw() +
     theme(panel.grid = element_blank(),
           plot.title.position = 'plot',
           plot.title = element_text(face = "bold", size = 14, hjust = 0),
           panel.background = element_rect(fill = "#C1CDCD", color = NA),
           axis.text = element_text(color = 'black', face = 'plain'),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.position = 'none') +
     labs(title = "G", x = NULL, y = NULL, fill = '')+
     guides(fill = guide_legend(nrow = 2))

# line --------------------------------------------------------------------

fill_color <- paletteer_d("ggsci::nrc_npg")

DataYear <- DataAll |> 
     group_by(country, Year) |>
     arrange(Age) |>
     mutate(cum_weight = cumsum(Density)) |>
     summarise(MedianAge = Age[min(which(cum_weight >= sum(Density) / 2))],
               .groups = 'drop')

fig_2 <- ggplot(data = DataYear) +
     geom_point(aes(x = Year, y = MedianAge, color = country)) +
     geom_line(aes(x = Year, y = MedianAge, color = country)) +
     scale_color_manual(values = fill_color) +
     scale_y_continuous(trans = 'log10',
                        breaks = c(3, 5, 10, 20, 40, 60),
                        limits = c(3, 60)) +
     scale_x_continuous(breaks = seq(2015, 2023, 2),
                        limits = c(2015, 2023)) +
     theme_bw()+
     theme(panel.grid.major.x = element_blank(),
           plot.title = element_text(face = "bold", size = 14, hjust = 0),
           panel.grid.minor.x = element_blank(),
           axis.text.y = element_text(color = 'black', face = 'plain'),
           axis.text.x = element_text(color = 'black', face = 'plain', hjust = 0.5),
           axis.title = element_text(color = 'black', face = 'plain'),
           legend.box = 'horizontal',
           plot.title.position = 'plot',
           legend.position = "bottom") +
     labs(title = 'J',
          color = NULL,
          x = "Year",
          y = "Median age")+
     guides(color = guide_legend(nrow = 1))

# save --------------------------------------------------------------------

design <- "
ABCDE
FFFFF
GGHIJ
"

fig <- fig_min[[1]] + fig_min[[2]] + fig_min[[3]] + fig_min[[4]] + fig_min[[5]] + 
     fig_1 + 
     fig_2 + fig_min[[6]] + fig_min[[7]] + fig_min[[8]] + 
     plot_layout(design = design, heights = c(1, 3, 1))

ggsave("./Outcome/fig4.pdf",
       fig,
       width = 12,
       height = 11,
       device = cairo_pdf,
       family = "Times New Roman")

# appendix ----------------------------------------------------------------

write.xlsx(DataYear,
           './Outcome/fig data/fig 4.xlsx')
