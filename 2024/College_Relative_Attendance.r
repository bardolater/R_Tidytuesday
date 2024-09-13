
library(tidyverse)
library(scales)
library(here)
library(sysfonts)
library(showtext)
library(ggtext)
library(gghighlight)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)
font_add_google("Roboto Condensed")
font <- "Roboto Condensed"
tuesdata <- tidytuesdayR::tt_load(2024, week = 37)

college_admissions <- tuesdata$college_admissions

college_admissions <- college_admissions |>   dplyr::mutate(
    Public = as.numeric(public),
    flagship = as.numeric(flagship)
  )

selectivity <- college_admissions |> 
  group_by(tier, par_income_bin) |>
  summarise(xx = mean(rel_att_cond_app)) |> 
  ungroup()

title="At the top of the income distribution, more attendees per applicants attend Ivy Plus schools<br>"
subtitle="This plot of the average ratio of relative attendance rate to the relative application rate on the y axis and income percentile<br>on the x axis shows that the richest families are more often attending the most exclusive group of schools (Ivy Plus).<br>The other gray lines represent the average ratio for other, less selective tiers of colleges and universities."
selectivity |> 
  ggplot(aes(x= par_income_bin, y= xx, group=tier)) +
  geom_line()+
  gghighlight::gghighlight(tier==c("Ivy Plus"), label_params = list(size = 3, nudge_y=0.1, color="sienna"), line_label_type = "ggrepel_label", unhighlighted_params = list(linewidth = 1, colour = alpha("gray20", 0.4)))+
  scale_x_continuous(breaks = c(10, 30, 50, 65, 75, 85, 92.5, 95.5,  97.5, 100))+
  geom_point() +
  labs(title = title,
       subtitle =subtitle, caption= "Plot by @DataAngler@vis.social | TidyTuesday 2024 Wk. 37",
       y = "Avg. Relative Attendance", x="Percentile of Parent Household Income")+
  theme(legend.position="none",
        legend.title = element_blank(),
        plot.title = element_markdown(size=12, family = font),
        plot.subtitle = element_markdown(size=9, family = font),
        axis.text.y = element_text(size=8, family = font),
        axis.text.x = element_text(size=8, family = font,angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5),
        axis.title.x = element_text(size=9, family = font),
        plot.caption = element_text(size= 7, family = font),
        plot.title.position = "panel")
ggsave(here("Attendance_Rate_by_Income_Band.png"), dpi=300, height=5, width=7)
