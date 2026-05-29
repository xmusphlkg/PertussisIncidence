
library(tidyverse)
library(lubridate)
library(paletteer)
library(patchwork)
library(openxlsx)

rm(list = ls())

fill_color <- paletteer_d("ggsci::default_nejm")

# Load data---------------------------------------------------------

scientific_10 <- function(x) {
     ifelse(x == 0, 0, parse(text = gsub("[+]", "", gsub("e", "%*%10^", scales::scientific_format()(x)))))
}

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
     mutate(Date = as.Date(paste0(Year, '-01-01')))

min_year <- min(df_clean$Year, na.rm = TRUE)
max_year <- max(df_clean$Year, na.rm = TRUE)

# find peak time
df_clean |> 
     filter(Date >= as.Date('2020-01-01')) |> 
     group_by(Country) |>
     slice_max(order_by = Cases, n = 1) |> 
     select(Country, Date, Cases, Incidence) |> 
     arrange(desc(Date))

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

country_list <- c('US', 'GB',
                  'JP', 'SG',
                  'SE', 'CN',
                  'AU', 'NZ')

# figure 1 ----------------------------------------------------------------

df_count <- df_clean |> 
     group_by(Country, Year) |>
     summarise(Cases = sum(Cases, na.rm = TRUE),
               .groups = 'drop') |> 
     group_by(Year) |>
     mutate(CasesPercent = round(Cases / sum(Cases, na.rm = TRUE), 4))

fig1 <- ggplot(df_count, aes(x = Year, y = Cases, fill = Country)) +
     # percent stacked bar chart
     geom_col(position = 'fill', color = 'white', width = 0.85) +
     scale_fill_manual(values = fill_color,
                       breaks = country_list) +
     scale_x_continuous(breaks = seq(min_year, max_year, 1),
                        expand = expansion(add = c(0.2, 0.2))) +
     scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                        breaks = seq(0, 1, 0.2),
                        expand = expansion(mult = c(0, 0))) +
     theme_bw() +
     labs(title = 'A',
          x = 'Year',
          y = 'Percentage of cases',
          color = 'Country')+
     guides(fill = guide_legend(nrow = 1,
                                title.position = 'top'))

# figure 2 ----------------------------------------------------------------


split_dates <- c(as.Date(c("2015/1/1", "2020/1/1", "2024/1/1")), max(df_clean$Date))
split_periods <- c("2015 Jan to 2019 Dec", "2020 Jan to 2023 Dec", "2024 Jan onwards")
datafile_rect <- data.frame(Period = split_periods,
                            start = split_dates[1:3],
                            end = split_dates[2:4])

i <- 1

plot_epidemic <- function(i){
     # detact monthly or weekly data
     if (!all(is.na(df_clean$Month[df_clean$Country == country_list[i]]))) {
          labs_y <- 'Monthly incidence'
     } else {
          labs_y <- 'Weekly incidence'
     }
     
     # annual incidence data
     data_year <- df_year |> 
          filter(Country == country_list[i]) |> 
          select(Date, Incidence) |> 
          mutate(Type = 'Annual incidence')
     data_year <- data_year |> 
          mutate(Date = Date + years(1) - days(1)) |> 
          rbind(data_year) |> 
          arrange(Date)
     
     # monthly/weekly incidence data
     data_main <- df_clean |> 
          filter(Country == country_list[i]) |> 
          ungroup() |> 
          select(Date, Incidence) |> 
          mutate(Type = labs_y)
     
     # ratio for second axis
     plot_breaks_main <- pretty(c(0, data_main$Incidence))
     plot_breaks_year <- pretty(c(0, data_year$Incidence))
     
     ratio <- plot_breaks_main[2] / plot_breaks_year[2]
     
     # rbind data
     data <- data_year |>
          mutate(Incidence = Incidence * ratio) |> 
          rbind(data_main) |> 
          mutate(Type = factor(Type, levels = c('Weekly incidence', 'Monthly incidence', 'Annual incidence')))
     
     plot_breaks <- pretty(c(0, data$Incidence), n=4)
     
     # figure
     fig_1 <- ggplot(data = data) +
          geom_rect(data = datafile_rect,
                    aes(xmin = start, xmax = end, fill = Period),
                    ymin = -Inf,
                    ymax = Inf,
                    alpha = 0.2,
                    show.legend = TRUE) +
          geom_line(aes(x = Date, y = Incidence, color = Type),
                    show.legend = T) +
          coord_cartesian(xlim = range(data_main$Date)) +
          scale_x_date(date_labels = '%Y',
                       date_breaks = 'year',
                       expand = expansion(mult = c(0, 0))) +
          scale_y_continuous(
               name = labs_y,
               expand = expansion(mult = c(0, 0)),
               limits = range(plot_breaks),
               breaks = plot_breaks,
               sec.axis = sec_axis(~ . / ratio, name = "Annual incidence")  # 双y轴
          ) +
          scale_fill_manual(values = fill_color[5:7]) +
          scale_color_manual(values = fill_color[3:1],
                             breaks = c('Annual incidence', 'Monthly incidence', 'Weekly incidence'),
                             drop = FALSE) +
          theme_bw() +
          labs(
               title = paste(LETTERS[i + 1], country_list[i], sep = ': '),
               x = 'Date',
               color = 'Incidence rate',
               fill = 'Stage'
          ) +
          guides(
               fill = guide_legend(nrow = 1, title.position = 'top'),
               color = guide_legend(nrow = 1, title.position = 'top')
          )
     
     fig_1
}

outcome <- lapply(1:length(country_list), plot_epidemic)
# insert fig1 as first plot
outcome <- append(list(fig1), outcome)

fig <- outcome |> 
     wrap_plots(ncol = 3, guides = 'collect')&
     theme(plot.title.position = "plot",
           plot.caption.position = "plot",
           plot.title = element_text(face = "bold", size = 14, hjust = 0),
           panel.grid.major.y = element_blank(),
           panel.grid.minor = element_blank(),
           legend.position = 'bottom',
           legend.title = element_text(face = "bold", size = 12),
           legend.text = element_text(size = 12),
           legend.box.background = element_rect(fill = "transparent", colour = "transparent"),
           axis.text = element_text(size = 12, color = "black"),
           axis.title = element_text(face = "bold", size = 12, color = "black"),
           legend.background = element_rect(fill = "transparent", colour = "transparent"),
           plot.background = element_blank())

ggsave(filename = './Outcome/Fig 2.pdf',
       plot = fig,
       width = 16,
       height = 9, 
       device = cairo_pdf,
       family = 'Times New Roman')

write.xlsx(list(df_clean = df_clean, df_year = df_year),
          './Outcome/fig data/fig 2.xlsx')
