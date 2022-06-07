library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(forcats)
library(ggstream)
library(scales)
library(prismatic)
library(ggtext)

tuesdata <- tidytuesdayR::tt_load('2022-06-07')

list2env(tuesdata, envir = .GlobalEnv)


ranking <-
  contribution_data_all_states %>%
  filter(`Pride?` == TRUE, !is.na(Date)) %>%
  group_by(Company) %>%
  summarize(total = sum(Amount)) %>%
    ungroup() %>%
    mutate(
         rank = rank(-total, ties.method = "max"),
         comp = if_else(rank <= 7, Company, "Others"),
         comp = factor(comp, levels = c("Others", "Toyota", "State Farm", "Southern Company (Georgia Power, Alabama Power, Mississippi Power)", "FedEx", "Comcast", "AT&T", "Amazon"),
                       labels = c("Others", "Toyota", "State Farm", "Southern Company", "FedEx", "Comcast", "AT&T", "Amazon"))) %>%
    arrange(rank)

df <-
  contribution_data_all_states %>%
  filter(`Pride?` == TRUE, !is.na(Date)) %>%
  inner_join(ranking, by = "Company") %>%
  mutate(year = year(Date)) %>%
  group_by(year) %>%
  mutate(total = sum(Amount)) %>%
  group_by(comp, year, total) %>%
  summarize(sum = sum(Amount)) %>%
  mutate(perc = sum / total)

title = "Are they 
    <span style='color:#FF0000;'>r</span><span style='color:#FFDB00;'>a</span><span style='color:#49FF00;'>i</span><span style='color:#00FF92;'>n</span><span style='color:#0092FF;'>b</span><span style='color:#4900FF;'>o</span><span style='color:#FF00DB;'>w</span>-<span style='color:#FF0000;'>w</span><span style='color:#FFDB00;'>a</span><span style='color:#49FF00;'>s</span><span style='color:#00FF92;'>h</span><span style='color:#0092FF;'>i</span><span style='color:#4900FF;'>n</span><span style='color:#FF00DB;'>g</span> you? "

df %>%
  ggplot(aes(x = year, y = sum, fill = fct_rev(comp))) +
  geom_stream(aes(color = after_scale(clr_darken(fill, 0.5))), type = 'ridge', alpha = .75, show.legend = FALSE) +
  #legend
  geom_point(aes(x = -83, y = 40, fill = fct_rev(comp)), size=4, alpha = .8, shape = 21) +
  scale_fill_manual(values = c(rainbow(7), "black")) +
  scale_x_continuous(breaks = seq(2013, 2022, 1), limits = c(2013, 2022)) +
  scale_y_continuous(breaks = seq(0, 1200000, 200000), labels = c("0k", "20k", "40k", "60k", "80k", "100k", "120k")) +
  labs(
    title = title,
    subtitle = "In the last five years, many companies which sponsor prides across\nthe US increased their donations to anti-LGBTQ politicians.",
    caption = "Author: Francesco Olivo\nData: Data For Progress",
    x = "",
    y = "Dollars donated to anti-LGBTQ politicians"
  ) +
  theme_minimal(base_size=9, base_family="Consolas") +
  theme(
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = 'ghostwhite', color = "ghostwhite"),
    plot.title.position = 'plot',
    plot.title = element_markdown(face = 'bold', size = 15, hjust = 0),
    plot.subtitle = element_text(margin = margin(5, 0, 5, 0), lineheight = 1.2, hjust = 0),
    plot.caption = element_text(lineheight = 1.2, margin = margin(10, 0, 0, 0), hjust = 1),
    plot.margin = margin(10, 15, 10, 10),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(color = "gray10"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 8)
  ) +
  guides(fill=guide_legend(nrow = 2, byrow = FALSE))

ggsave("2022/06-07/rainbow-washing.png", w = 6, h = 6, dpi = 300)

