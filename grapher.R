library(tidyverse)
library(lubridate)
library(ggthemes)

data <-  read_csv("FLU_RSV_COVID_data.csv") %>%
  mutate(max = case_when(
    is.na(max) ~ high,
    TRUE ~ max
  ),
  min = case_when(
    is.na(min) ~ low,
    TRUE ~ min
  ))

data %>%
  ggplot(aes(x=Characteristic,fill=Pathogen))+
 # geom_errorbar(aes(ymin=min,ymax=max)) +
  geom_boxplot(aes(ymin=min,
                   lower = low,
                   middle = mean(low,high,trim=0),
                   upper = high,
                   ymax=max),
               stat = "identity")+
  coord_flip()+
  ylab("Days since symptom onset")

