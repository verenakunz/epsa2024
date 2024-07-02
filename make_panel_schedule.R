## Load required packages
library(tidyverse)
library(ggtext)
library(showtext)
font_add_google("Fira Sans", "Fira Sans")

## Automatically use showtext to render text
showtext_auto()

## Import panel details
panel_details <- read_rds("data/panel_details.rds")

## Make monochrome schedule for Thursday
panel_details %>% 
  filter(str_detect(panel_day_time, "^Thursday")) %>%
  ggplot(aes(x = str_c("**", panel_day_time, "**"), 
             y = panel_room)) + 
  geom_tile(fill = "#1a3b99") + 
  geom_richtext(aes(label = str_c(section, "<br>", "**", panel_title, "**")), 
                colour = "white", 
                fill = NA, 
                label.colour = NA, 
                size = 2.9) + 
  geom_hline(yintercept = seq(1, length(panel_details$panel_day_time), 1) +.5, color="white") + 
  geom_vline(xintercept = seq(1, length(panel_details$panel_room), 1) +.5, color="white") + 
  theme_minimal() + 
  scale_x_discrete(position = "top") + 
  scale_y_discrete(limits = rev) + 
  labs(x = "", 
       y = "") + 
  theme(panel.grid.major = element_blank(), 
        text = element_text(family = "Fira Sans"), 
        axis.text.x = element_markdown(size = 12))

ggsave("Thu.pdf", 
       width = 16, 
       height = 9)

## Make monochrome schedule for Friday
panel_details %>% 
  filter(str_detect(panel_day_time, "^Friday")) %>%
  ggplot(aes(x = str_c("**", panel_day_time, "**"), 
             y = panel_room)) + 
  geom_tile(fill = "#1a3b99") + 
  geom_richtext(aes(label = str_c(section, "<br>", "**", panel_title, "**")), 
                colour = "white", 
                fill = NA, 
                label.colour = NA, 
                size = 2.9) + 
  geom_hline(yintercept = seq(1, length(panel_details$panel_day_time), 1) +.5, color="white") + 
  geom_vline(xintercept = seq(1, length(panel_details$panel_room), 1) +.5, color="white") + 
  theme_minimal() + 
  scale_x_discrete(position = "top") + 
  scale_y_discrete(limits = rev) + 
  labs(x = "", 
       y = "") + 
  theme(panel.grid.major = element_blank(), 
        text = element_text(family = "Fira Sans"), 
        axis.text.x = element_markdown(size = 12))

ggsave("Fri.pdf", 
       width = 16, 
       height = 9)

## Make monochrome schedule for Saturday
panel_details %>% 
  filter(str_detect(panel_day_time, "^Saturday")) %>%
  ggplot(aes(x = str_c("**", panel_day_time, "**"), 
             y = panel_room)) + 
  geom_tile(fill = "#1a3b99") + 
  geom_richtext(aes(label = str_c(section, "<br>", "**", panel_title, "**")), 
                colour = "white", 
                fill = NA, 
                label.colour = NA, 
                size = 2.9) + 
  geom_hline(yintercept = seq(1, length(panel_details$panel_day_time), 1) +.5, color="white") + 
  geom_vline(xintercept = seq(1, length(panel_details$panel_room), 1) +.5, color="white") + 
  theme_minimal() + 
  scale_x_discrete(position = "top") + 
  scale_y_discrete(limits = rev) + 
  labs(x = "", 
       y = "") + 
  theme(panel.grid.major = element_blank(), 
        text = element_text(family = "Fira Sans"), 
        axis.text.x = element_markdown(size = 12))

ggsave("Sat.pdf", 
       width = 16, 
       height = 9)