library(tidyverse)
library(lubridate)
library(geofacet)
library(forcats)
library(showtext)
library(spiralize)


# load data
drought_fips <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought-fips.csv')

palette <- scico::scico(palette = "lajolla", n = 10)


df <-
  drought_fips %>%
  filter(State == "CA") %>%
  group_by(State, date) %>%
  summarize(value = mean(DSCI)) %>%
  ungroup() %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  mutate(week = row_number()) %>%
  filter(week < 53) %>%
  ungroup()

df <- 
  df %>%
  rbind(df %>%
        filter(week == 52) %>%
        mutate(week = 0,
               year = year + 1)
  ) %>%
  mutate(r = rank(date)) %>%
  arrange(year, week)

size_factor = 0.1

df %>%
  ggplot() +
  geom_rect(data = . %>%
              head(-1),
            aes(xmin = week, xmax = week + 1, ymin = r, ymax = r + ((value) * size_factor), group = year, fill = value), show.legend = TRUE) +
  geom_line(aes(x = week, y = r, group = year), alpha = 0.7) +
  coord_polar() + 
  scale_x_continuous(limits = c(0, 52),
                     breaks = seq(0, 52-(52/12), 52/12),
                     labels = c( "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
  ) +
  scale_fill_gradientn(colors = palette, limits = c(0, 500), breaks = c(0, 500), labels = c("Normal", "Extreme")) +
  labs(
    title = "Californian droughts evolution",
    subtitle = "California is going through one of the most serious droughts\nin its history, and it is the second in the last ten years",
    caption = "Author: Francesco Olivo\nData: drought.gov",
    x = "",
    y = ""
  ) +
  theme_minimal(base_size=9, base_family="Consolas") +
  theme(
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = 'ghostwhite', color = "ghostwhite"),
    plot.title.position = 'plot',
    plot.title = element_text(face = 'bold', size = 15, hjust = 0),
    plot.subtitle = element_text(margin = margin(5, 0, 5, 0), lineheight = 1.2, hjust = 0),
    plot.caption = element_text(lineheight = 1.2, margin = margin(10, 0, 0, 0), hjust = 1),
    plot.margin = margin(10, 10, 10, 10),
    panel.grid.major.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.margin = margin(0, 0, 0, 0),
  )

ggsave("2022/06-14/california-drougths.png", w = 6, h = 7.5, dpi = 300)
