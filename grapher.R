library(tidyverse)
library(lubridate)
library(ggthemes)
library(wesanderson)

data <-  read_csv("FLU_RSV_COVID Chart - Main Data.csv")

data2 <- data %>%
  mutate(max = case_when(
    is.na(max) ~ high,
    TRUE ~ max
  ),
  min = case_when(
    is.na(min) ~ low,
    TRUE ~ min
  )) %>%
  mutate(
    Characteristic = 
      factor(
        Characteristic,
        levels = c(
          "Outpatient Treatment",
          "Antigen Testing",
          "Symptoms",
          "Contagious",
          "Exposure"

        )
      )
  )

data2 %>%
  ggplot(aes(x=Characteristic,fill=Pathogen))+
 # geom_errorbar(aes(ymin=min,ymax=max)) +
  geom_hline(yintercept = 0,
             size = 1,
             linetype=3)+
  geom_boxplot(aes(ymin=min,
                   lower = low,
                   middle = mean(low,high,trim=0),
                   upper = high,
                   ymax=max),
               stat = "identity",
               alpha=0.8)+
  coord_flip()+
  ylab("Days Since Symptom Onset") +
  xlab("") +
  scale_fill_manual(values = wes_palette("AsteroidCity1"))+
  theme_clean()+
  labs(title = "Relative Timing of Respiratory Virus Epidemiologic and Clinical Characteristics",
       subtitle = "Boxes indicate most likely values, whiskers indicate commonly described or plausible ranges",
       caption = "NOTES: \n
       Testing ranges for RSV are based on viral culture positivity.\n
       Treatment guidance varies by disease and individual patient risk status. \n
       Information on Omicron COVID strains circulating in 2023 is used where available. \n
       Source data and citations available at https://tinyurl.com/RespVirTime"
       )+
  theme(    legend.position = c(.25, .45),
            legend.justification = c("right", "top"),
            legend.box.just = "left",
            legend.margin = margin(6, 6, 6, 6),
            plot.caption = element_text(hjust = 0))+
  guides(fill = guide_legend(reverse = TRUE))

ggsave("Respiratory Virus Timing.png")

