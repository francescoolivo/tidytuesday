library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(forcats)
library(ggchicklet)
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
         comp = factor(comp,
                       levels = c("Amazon", "AT&T", "Comcast", "FedEx", "Southern Company (Georgia Power, Alabama Power, Mississippi Power)", "State Farm", "Toyota", "Others"),
                       labels = c("Amazon", "AT&T", "Comcast", "FedEx", "Southern Company", "State Farm", "Toyota", "Others"))) %>%
    arrange(rank)

df <-
  contribution_data_all_states %>%
  filter(`Pride?` == TRUE, !is.na(Date)) %>%
  inner_join(ranking, by = "Company") %>%
  mutate(year = year(Date)) %>%
  group_by(comp, year) %>%
  summarize(sum = sum(Amount))


title = "Are they 
    <span style='color:#FF0000;'>r</span><span style='color:#FFDB00;'>a</span><span style='color:#49FF00;'>i</span><span style='color:#00FF92;'>n</span><span style='color:#0092FF;'>b</span><span style='color:#4900FF;'>o</span><span style='color:#FF00DB;'>w</span>-<span style='color:#FF0000;'>w</span><span style='color:#FFDB00;'>a</span><span style='color:#49FF00;'>s</span><span style='color:#00FF92;'>h</span><span style='color:#0092FF;'>i</span><span style='color:#4900FF;'>n</span><span style='color:#FF00DB;'>g</span> you? "

df %>%
  ggplot(aes(x = year, y = sum, fill = fct_rev(comp))) +
  geom_chicklet(aes(color = after_scale(clr_darken(fill, 0.5))), alpha = .75, show.legend = FALSE) +
  geom_point(aes(x = -83, y = 40, fill = comp), size=4, alpha = .8, shape = 21) +
  scale_fill_manual(values = c("black", rev(rainbow(7)), "black")) +
  scale_x_continuous(breaks = seq(2013, 2022, 1), limits = c(2013, 2022)) +
  scale_y_continuous(breaks = seq(0, 600000, 100000), 
                     labels = c("0", "100K", "200K", "300K", "400K", "500K", "600K")
                     ) +
  labs(
    title = title,
    subtitle = "In the last five years, many companies that sponsor prides across\nthe US increased their donations to anti-LGBTQ politicians.",
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
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 8)
  ) +
  guides(fill=guide_legend(nrow = 2, byrow = FALSE, reverse = TRUE))

ggsave("2022/06-07/rainbow-washing.png", w = 6, h = 6, dpi = 300)
