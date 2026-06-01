
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

rm(list = ls())

# data --------------------------------------------------------------------

fill_color <- paletteer_d("ggsci::default_nejm")

# Load data
df_clean <- read.csv('./Outcome/Table S1.csv') |> 
     mutate(Date = as.Date(Date),
            PlotCases = if_else(is.na(AnalysisCases), Cases, AnalysisCases),
            PlotIncidence = if_else(is.na(AnalysisIncidence), Incidence, AnalysisIncidence))

country_list <- c('US', 'GB',
                  'JP', 'SG',
                  'SE', 'CN',
                  'AU', 'NZ')

display_years <- sort(unique(df_clean$Year[df_clean$Year >= 2024]))
comparison_years <- sort(unique(df_clean$Year[df_clean$Year %in% 2024:2025]))
stage_levels <- c('2015 to 2019',
                  '2020 to 2023',
                  as.character(display_years))

format_jmir_p <- function(p) {
     case_when(
          is.na(p) ~ NA_character_,
          p < 0.001 ~ "P<.001",
          p > 0.99 ~ "P>.99",
          TRUE ~ paste0("P=", str_replace(format(round(p, 3), nsmall = 3), "^0", ""))
     )
}

pairwise_results <- list()
plot_data_results <- list()

# fig ----------------------------------------------------------------------

i <- 3

plot_compare <- function(i){
     data <- df_clean |> 
          filter(Country == country_list[i]) |> 
          mutate(stage = case_when(Year < 2020 ~ '2015 to 2019',
                                   Year %in% 2020:2023 ~ '2020 to 2023',
                                   TRUE ~ as.character(Year)),
                 stage = factor(stage,
                                levels = stage_levels)) |> 
          select(Country, Date, stage, Incidence = PlotIncidence, RawIncidence,
                 Cases, AnalysisCases, AdjustmentFactor, AnnualCaseSource,
                 Week, Month, Year)
     
     if (!all(is.na(data$Month))){
          xaxis_values <- 1:12
          xaxis_labels <- month.abb[xaxis_values]
          xaxis_breaks <- 101:112
          labs_y <- 'Monthly incidence'
          complete_years <- data |>
               filter(Year >= 2024) |>
               group_by(Year) |>
               summarise(interval_n = n_distinct(Month), .groups = 'drop') |>
               filter(interval_n == 12) |>
               pull(Year)
          data <- data |> 
               mutate(xaxis = factor(Month,
                                     levels = xaxis_values,
                                     labels = xaxis_breaks),
                      xaxis = as.integer(as.character(xaxis)))
     } else {
          xaxis_values <- 1:52
          xaxis_labels <- 1:52
          xaxis_breaks <- 201:252
          labs_y <- 'Weekly incidence'
          complete_years <- data |>
               filter(Year >= 2024) |>
               group_by(Year) |>
               summarise(interval_n = n_distinct(Week), .groups = 'drop') |>
               filter(interval_n >= 52) |>
               pull(Year)
          data <- data |> 
               mutate(xaxis = factor(Week,
                                     levels = xaxis_values,
                                     labels = xaxis_breaks),
                      xaxis = as.integer(as.character(xaxis)))
     }
     
     plot_breaks <- pretty(c(0, data$Incidence))
     plot_range <- range(plot_breaks)
     
     data_stage_recent <- data |> 
          filter(stage %in% as.character(display_years))
     data_stage_1_2 <- data |> 
          filter(!stage %in% as.character(display_years))

     model_data <- data |>
          filter(stage %in% c('2015 to 2019',
                              '2020 to 2023',
                              as.character(complete_years))) |>
          filter(!is.na(xaxis), !is.na(Incidence)) |>
          droplevels()

     plot_data_results[[country_list[i]]] <<- data

     results <- lmer(Incidence ~ stage + (1|xaxis), data = model_data)
     emm <- emmeans(results, ~ stage)
     pairs <- pairs(emm)
     pairs_summary <- summary(pairs, adjust = "bonferroni") |> 
          as.data.frame() |>
          separate(contrast, c('stage1', 'stage2'), sep = ' \\- ') |> 
          rename('group1' = 'stage1',
                 'group2' = 'stage2') |> 
          filter(group1 == '2015 to 2019') |>
          mutate(y.position = seq(plot_range[2] * 0.65,
                                  plot_range[2] * 0.95,
                                  length.out = n()),
                 p_label = format_jmir_p(p.value))

     pairwise_results[[country_list[i]]] <<- pairs_summary |>
          mutate(Country = country_list[i],
                 adjusted_p_label = p_label) |>
          select(Country, group1, group2, estimate, SE, df, t.ratio, p.value, adjusted_p_label)
     
     fig1_1 <- ggplot(data_stage_1_2, aes(x = xaxis, y = Incidence)) +
          geom_point(aes(group = Year, color = stage),
                     alpha = 0.5) +
          geom_path(aes(group = Year, color = stage),
                    alpha = 0.5) +
          geom_smooth(aes(color = stage, fill = stage),
                      method = 'gam',
                      formula = y ~ s(x, bs = 'cs'),
                      show.legend = F,
                      linewidth = 1,
                      se = T) +
          geom_path(data = data_stage_recent, aes(color = stage),
                    linewidth = 1) +
          scale_x_continuous(breaks = xaxis_breaks[seq(1, length(xaxis_breaks), by = length(xaxis_breaks) /12)],
                             labels = xaxis_labels[seq(1, length(xaxis_breaks), by = length(xaxis_breaks) /12)],
                             expand = c(0, 0)) +
          scale_y_continuous(expand = c(0, 0),
                             breaks = plot_breaks) +
          coord_cartesian(ylim = plot_range) +
          labs(title = paste(LETTERS[i], country_list[i], sep = ': '),
               x = ifelse(all(is.na(data$Month)), 'Epidemiological week', 'Month'),
               y = labs_y,
               color = 'Stage', fill = 'Stage') +
          scale_color_manual(values = fill_color) +
          scale_fill_manual(values = fill_color) +
          theme_bw() +
          theme(plot.title.position = "plot",
                plot.caption.position = "plot",
                plot.title = element_text(face = "bold", size = 14, hjust = 0),
                panel.grid.major.y = element_blank(),
                panel.grid.minor = element_blank(),
                legend.text = element_text(size = 12),
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
     
     fig1_2 <- ggplot(data, aes(x = stage, y = Incidence)) +
          geom_boxplot(aes(color = stage),
                       show.legend = F)+
          stat_pvalue_manual(pairs_summary,
                             label = "p_label",
                             hide.ns = F) +
          coord_cartesian(clip = "off")+
          scale_y_continuous(expand = expansion(mult = c(0, 0)),
                             breaks = plot_breaks,
                             limits = plot_range) +
          labs(x = NULL, y = NULL) +
          scale_color_manual(values = fill_color) +
          theme_bw() +
          theme(plot.title.position = "plot",
                plot.caption.position = "plot",
                plot.title = element_text(face = "bold", size = 14, hjust = 0),
                panel.grid.major.y = element_blank(),
                panel.grid.minor = element_blank(),
                legend.title = element_text(face = "bold", size = 12),
                legend.text = element_text(size = 12),
                legend.box.background = element_rect(fill = "transparent", colour = "transparent"),
                legend.background = element_rect(fill = "transparent", colour = "transparent"),
                axis.title = element_text(face = "bold", size = 12, color = "black"),
                axis.text = element_blank(),
                axis.ticks.x = element_blank(),
                plot.background = element_blank())
     
     return(fig1_1 + fig1_2 + plot_layout(widths = c(3, 1)))
}

fig <- lapply(1:length(country_list), plot_compare) |> 
     wrap_plots(ncol = 2, widths = c(1, 1), guides = 'collect')&
     theme(legend.position = 'bottom')

ggsave(filename = './Outcome/Fig 3.pdf',
       plot = fig,
       width = 12,
       height = 12, 
       device = cairo_pdf,
       family = 'Times New Roman')

ggsave(filename = './Outcome/Fig 3.png',
       plot = fig,
       width = 12,
       height = 12,
       dpi = 300,
       bg = 'white')

# appendix ----------------------------------------------------------------

data_2022 <- map(1:length(country_list), function(i){fig[[i]][[1]]$data})
data_2022 <- bind_rows(data_2022)

data_2023 <- map(1:length(country_list), function(i){fig[[i]][[2]]$data})
data_2023 <- bind_rows(data_2023)

data <- bind_rows(plot_data_results)

write.xlsx(list(fig3_plot_data = data,
                fig3_panel_left_data = data_2022,
                fig3_panel_right_data = data_2023,
                lmm_pairwise_pre_pandemic = bind_rows(pairwise_results)),
           './Outcome/fig data/fig 3.xlsx')
