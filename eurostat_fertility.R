library(eurostat)
library(tidyverse)


f1 <- get_eurostat("demo_faeduc")

f2 <- f1 %>% filter(geo == "ES", age %in% c("Y15-19" ,"Y20-24", "Y25-29", "Y30-34", 
                                            "Y35-39", "Y40-44", "Y45-49")) %>% 
  mutate(age2 = gsub("Y", "", age))


ggplot(f2 %>% filter(isced11 != "TOTAL"), aes(x = age2, y = values, color = isced11, group = isced11)) +
  geom_line() +
  facet_wrap(~TIME_PERIOD) +
  theme_bw()

