library(tidyverse)
library(wesanderson)
library(ggthemes)

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
  #geom_point()+
  stat_smooth()

la_no2_filtered <- la_no2 %>%
  filter(mean > 0)

#library(lubridate)
# la_no2_averages <- la_no2_filtered %>%
#   mutate(date = seq.Date(by="month"))
#   #from=as.Date("2018-11-30", "%Y-%m-%d")
# la_no2_averages <- la_no2_filtered %>%
#   group_by(month=floor_date(date, "month"))
#   summarize(date = )

emergency <- as.Date("2020-03-04")
home <- as.Date("2020-03-19")

ggplot(la_no2_filtered, aes(x = time, y = mean))+
  #geom_path(alpha=0.2, color="red")+
  geom_vline(xintercept = emergency, color = "red", linetype = "dotted")+
  geom_vline(xintercept = home, color = "red", linetype = "longdash")+
  geom_line()+
  geom_point(alpha=0.2, color="green")+
  stat_smooth()

# start_date <- as.Date("2018-11-30")
# 
# la_no2_grouped <- la_no2_filtered %>%
#   mutate(year = seq.Date(from = start_date, by = "year"))

la_no2_grouped <- la_no2_filtered %>%
  #mutate(year = format(date, format="%Y-%m-%d", "%Y")) %>%
  #mutate(year = format(as.Date(date, format = "%Y-%m-%d"), "%Y"))
  mutate(year = format(time, format="%Y"),
         month = format(time, format="%m"),
         day = format(time, format="%d"),
         m_d = format(time, format="%m-%d"),
         m_d_mod = as.Date(m_d, format="%m-%d"),
         d = as.Date(day, format="%d"))

ggplot(la_no2_grouped, aes(x = m_d, y = mean, color = year))+
  geom_point()

ggplot(la_no2_grouped, aes(x = m_d_mod, y = mean, color = year))+
  geom_point()+
  geom_path()+
  stat_smooth()+
  scale_x_date(date_breaks = "1 month", date_labels="%m")+
  facet_wrap(~year)

ggplot(la_no2_grouped, aes(x=d, y = mean, color = year))+
  geom_point()+
  geom_path()+
  stat_smooth()+
  facet_wrap(~month)


la_no2_mod <- la_no2 %>%
  filter(mean > 0) %>%
  mutate(year = format(time, format = "%Y"),
         y = as.Date(year, format = "%Y"),
         month = format(time, format = "%m"),
         m = as.Date(month, format = "%m"),
         day = format(time, format = "%d"),
         d = as.Date(day, format = "%d"),
         month_day = format(time, format="%m-%d"),
         m_d = as.Date(month_day, format="%m-%d")) %>%
  filter(year != "2018")


#here!
ggplot(la_no2_mod, aes(x = m_d, y = mean, color = year))+
  geom_line(alpha=0.3)+
  stat_smooth()+
  scale_x_date(date_breaks = "1 month", date_labels="%b")+
  theme_few()+
  theme(axis.text.x = element_text(angle = 30,
                                   vjust = 0.7))+
  labs(x = "Time", y = "Mean", color = "Year")

ggplot(la_no2_mod, aes(month, day, fill=mean))+
  facet_wrap(~year)+
  geom_tile()+
  scale_fill_gradientn(colors = wes_palette(name = "Zissou1", type="continuous"))+
  theme_few()

ggplot(la_no2_mod, aes(month, day, fill=mean))+
  facet_grid(year~month)+
  geom_tile()+
  scale_fill_gradientn(colors = wes_palette(name = "Zissou1", type="continuous"))+
  theme_few()

ggplot(la_no2_mod, aes(month, day, fill=mean))+
  facet_wrap(~year)+
  geom_tile()+
  scale_fill_gradientn(colors = wes_palette(name = "Zissou1", type="continuous"))+
  theme_few()

ggplot(la_no2_mod, aes(x = d, y = mean))+
  facet_grid(year~month)+
  geom_point(alpha=0.3)+
  geom_line()+
  coord_flip()+
  theme_few()

la_no2_mod2 <- la_no2_mod %>%
  group_by(year, month) %>%
  summarize(mean = mean(mean))

ggplot(la_no2_mod2, aes(x = month, y = mean, color = year, group = year))+
  geom_point()+
  geom_line()

ggplot(la_no2_mod, aes(x = month, y = mean, color=year))+
  facet_wrap(~year)+
  geom_violin()+
  stat_smooth()
