
library(tidyverse)
library(lubridate)
library(ggsci)
library(patchwork)
library(openxlsx)
library(paletteer)

library(lme4)
library(lmerTest)
library(emmeans)
library(ggpubr)

# data --------------------------------------------------------------------

fill_color <- paletteer_d("ggsci::nrc_npg")

# Load data
df_clean <- read.csv('./Outcome/Table S1.csv') |> 
     mutate(Date = as.Date(Date))

country_list <- c('US', 'GB', 'SE', 'CN', 'JP',
                  'SG', 'AU', 'NZ')

# fig ----------------------------------------------------------------------

i <- 3

plot_compare <- function(i){
     data <- df_clean |> 
          filter(Country == country_list[i]) |> 
          mutate(stage = ifelse(Date < as.Date('2020-01-01'), '2015 Jan to 2019 Dec',
                                ifelse(Date < as.Date('2023-07-01'), '2020 Jan to 2023 Jun',
                                       '2023 Jun onwards')),
                 AnnualizedInci = case_when(all(is.na(Month)) ~ Incidence * 52.14,
                                            all(!is.na(Month)) ~ Incidence * 12)) |> 
          select(stage, AnnualizedInci, Week, Month)
     if (!all(is.na(data$Month))){
          xaxis_values <- c(7:12, 1:6)
          xaxis_labels <- month.abb[xaxis_values]
          xaxis_breaks <- 101:112
          data <- data |> 
               mutate(xaxis = factor(Month,
                                     levels = xaxis_values,
                                     labels = xaxis_breaks),
                      xaxis = as.integer(as.character(xaxis)))
     } else {
          xaxis_values <- c(26:52, 1:25)
          xaxis_labels <- c(26:52, 1:25)
          xaxis_breaks <- 201:252
          data <- data |> 
               mutate(xaxis = factor(Week,
                                     levels = xaxis_values,
                                     labels = xaxis_breaks),
                      xaxis = as.integer(as.character(xaxis)))
     }
     
     plot_breaks <- pretty(c(0, data$AnnualizedInci))
     plot_range <- range(plot_breaks)
     
     data_2023 <- data |> 
          filter(stage == '2023 Jun onwards')
     data_2022 <- data |> 
          filter(stage != '2023 Jun onwards')

     results <- lmer(AnnualizedInci ~ stage + (1|xaxis), data = data)
     emm <- emmeans(results, ~ stage)
     pairs <- pairs(emm)
     pairs_summary <- summary(pairs, adjust = "bonferroni") |> 
          as.data.frame() |>
          separate(contrast, c('stage1', 'stage2'), sep = ' \\- ') |> 
          rename('group1' = 'stage1',
                 'group2' = 'stage2') |> 
          mutate(y.position = plot_range[2] * c(0.7, 0.8, 0.9),
                 p.value = ifelse(p.value < 0.001, '***', format(round(p.value, 3), nsmall = 3)))
     
     fig1_1 <- ggplot(data_2022, aes(x = xaxis, y = AnnualizedInci)) +
          geom_smooth(aes(color = stage, fill = stage),
                      method = 'gam',
                      show.legend = F,
                      linewidth = 1,
                      se = T) +
          geom_line(data = data_2023, aes(color = stage),
                    linewidth = 1) +
          scale_x_continuous(breaks = xaxis_breaks[seq(1, length(xaxis_breaks), by = length(xaxis_breaks) /12)],
                             labels = xaxis_labels[seq(1, length(xaxis_breaks), by = length(xaxis_breaks) /12)],
                             expand = c(0, 0)) +
          scale_y_continuous(expand = c(0, 0),
                             breaks = plot_breaks,
                             limits = plot_range) +
          labs(title = paste0(LETTERS[2*i-1]),
               x = ifelse(all(is.na(data$Month)), 'Epidemiological week', 'Month'),
               y = 'Annualized incidence rate',
               color = 'Stage', fill = 'Stage') +
          scale_color_manual(values = fill_color) +
          scale_fill_manual(values = fill_color) +
          theme_bw() +
          theme(plot.title.position = "plot",
                plot.caption.position = "plot",
                plot.title = element_text(face = "bold", size = 14, hjust = 0),
                panel.grid.major.y = element_blank(),
                panel.grid.minor = element_blank(),
                legend.text = element_text(face = "bold", size = 12),
                legend.title = element_text(face = "bold", size = 12),
                legend.box.background = element_rect(fill = "transparent", colour = "transparent"),
                legend.background = element_rect(fill = "transparent", colour = "transparent"),
                legend.position = 'bottom',
                axis.title = element_text(face = "bold", size = 12, color = "black"),
                axis.text.y = element_text(size = 12, color = "black"),
                axis.text.x = element_text(size = 12, color = "black"),
                plot.background = element_blank())+
          guides(fill = guide_legend(nrow = 1,
                                     title.position = 'left'))
     
     fig1_2 <- ggplot(data, aes(x = stage, y = AnnualizedInci)) +
          geom_boxplot(aes(color = stage),
                       show.legend = F)+
          stat_pvalue_manual(pairs_summary,
                             hide.ns = F) +
          coord_cartesian(clip = "off")+
          scale_y_continuous(expand = expansion(mult = c(0, 0)),
                             breaks = plot_breaks,
                             limits = plot_range) +
          labs(title = LETTERS[2*i], x = NULL, y = NULL) +
          scale_color_manual(values = fill_color) +
          theme_bw() +
          theme(plot.title.position = "plot",
                plot.caption.position = "plot",
                plot.title = element_text(face = "bold", size = 14, hjust = 0),
                panel.grid.major.y = element_blank(),
                panel.grid.minor = element_blank(),
                legend.text = element_text(face = "bold", size = 12),
                legend.title = element_text(face = "bold", size = 12),
                legend.box.background = element_rect(fill = "transparent", colour = "transparent"),
                legend.background = element_rect(fill = "transparent", colour = "transparent"),
                axis.title = element_text(face = "bold", size = 12, color = "black"),
                axis.text = element_blank(),
                plot.background = element_blank())
     
     return(fig1_1 + fig1_2 + plot_layout(widths = c(3, 1)))
}

fig <- lapply(1:length(country_list), plot_compare) |> 
     wrap_plots(ncol = 2, widths = c(1, 1), guides = 'collect')&
     theme(legend.position = 'bottom')

ggsave(filename = './Outcome/Fig3.pdf',
       plot = fig,
       width = 12,
       height = 12, 
       device = cairo_pdf,
       family = 'Times New Roman')

# appendix ----------------------------------------------------------------

data_2022 <- map(1:length(country_list), function(i){fig[[i]][[1]]$data})
data_2022 <- do.call('rbind', data_2022)

data_2023 <- map(1:length(country_list), function(i){fig[[i]][[2]]$data})
data_2023 <- do.call('rbind', data_2023)

data <- rbind(data_2022, data_2023)

write.csv(data, './Outcome/fig data/fig 3.xlsx')
