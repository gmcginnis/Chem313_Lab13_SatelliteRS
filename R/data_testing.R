library(tidyverse)

au_dust <- read_csv("data/au_dust.csv", skip = 7) %>%
  rename("mean" = 2)

ggplot(au_dust, aes(x = time, y = mean))+
  geom_path(color = "red")+
  geom_point(alpha=0.2, color = "red")+
  geom_smooth()


la_no2 <- read_csv("data/la_no2.csv", skip=7) %>%
  rename("mean" = 2)

ggplot(la_no2, aes(x = time, y = mean))+
  geom_path()+
  stat_smooth()

la_no2_filtered <- la_no2 %>%
  filter(mean > 0)
  

ggplot(la_no2_filtered, aes(x = time, y = mean))+
  geom_path(alpha=0.2, color="red")+
  geom_point(alpha=0.2, color="red")+
  stat_smooth()
