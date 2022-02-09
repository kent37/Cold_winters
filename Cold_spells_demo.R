# Question: Have extended cold snaps in Boston become rarer?

library(tidyverse) # Always :-)

# Download 10 years of weather data for 
# Boston Logan Airport from NOAA
# https://www.ncdc.noaa.gov/cdo-web/datasets/LCD/stations/WBAN:14739/detail

path = 'Logan_weather_12-2012_3-2021.csv'

df = read_csv(path)
View(df)

# Yikes

names(df)

# Daily[Average|Maximum|Minimum]Dry Bulb Temperature looks
# promising, but why so many NAs??

# Look at the data dictionary. 
# https://www1.ncdc.noaa.gov/pub/data/cdo/documentation/LCD_documentation.pdf
# Report Type: Indicates type of weather observation. 
# See ISD Documentation for
# further details (positions 42-46 in Control Data Section).
# https://www1.ncdc.noaa.gov/pub/data/ish/ish-format-document.pdf
# SOD = Summary of day report from U.S. ASOS or AWOS station

df %>% count(REPORT_TYPE...3)

# OK, let's try to get some usable data
# We want the date and the dry bulb temperatures
temps = df %>% 
  filter(REPORT_TYPE...3=='SOD') %>% 
  select(DATE, matches('Daily.*DryBulbTemperature'))
View(temps)

# Better. Convert the date and make some shorter names
library(lubridate)
temps = df %>% 
  filter(REPORT_TYPE...3=='SOD') %>% 
  select(DATE, matches('Daily.*DryBulbTemperature')) %>% 
  mutate(date=ymd_hms(DATE),
         year=year(date)) %>% 
  select(year, date,
         min="DailyMinimumDryBulbTemperature",
         max="DailyMaximumDryBulbTemperature",
         avg="DailyAverageDryBulbTemperature")
View(temps)

# Ok, what does that look like?
ggplot(temps, aes(date, avg)) +
  geom_line() +
  facet_wrap(~year, ncol=1)

# Hmmm
theme_set(theme_minimal())

# We only want winter months and we want the dates to overlap
winter = temps %>% 
  filter(month(date) %in% c(12, 1, 2, 3)) %>% 
  mutate(yday=yday(date)) # yday is the day in the year, 1-366

ggplot(winter, aes(yday, avg)) +
  geom_line() +
  facet_wrap(~year, ncol=1)

# Still not quite right, we want to show each season together
# Number the season with the number of the Jan-March year
# Hack yday to put December first
# More wrangling...
# What is the minimum value of yday in December?
winter %>% 
  filter(month(date)==12) %>% 
  pull(yday) %>% 
  min()
# 335

# So wrap values >= 335 to negative numbers
winter = winter %>% 
  mutate(season=year(date+duration(1, 'month')),
         season_label=paste(season-1, season, sep='-'),
         season_day = if_else(yday>=335, yday-365, yday))

ggplot(winter, aes(season_day, avg)) +
  geom_line() +
  facet_wrap(~season_label, ncol=1)

# Better!
# Some reference lines would be nice
ggplot(winter, aes(season_day, avg)) +
  geom_hline(yintercept=c(20, 32), color='red') +
  geom_line() +
  facet_wrap(~season_label, ncol=1)

# Make the lines smaller
ggplot(winter, aes(season_day, max)) +
  geom_hline(yintercept=c(20, 32), color='red', size=0.2) +
  geom_line() +
  facet_wrap(~season_label, ncol=1)

# Do we want to look at the minimum, average or maximum?
ggplot(winter, aes(season_day, min)) +
  geom_hline(yintercept=c(20, 32), color='red', size=0.2) +
  geom_line() +
  facet_wrap(~season_label, ncol=1)

ggplot(winter, aes(season_day, max)) +
  geom_hline(yintercept=c(20, 32), color='red', size=0.2) +
  geom_line() +
  facet_wrap(~season_label, ncol=1)

# Maybe both?
ggplot(winter, aes(season_day)) +
  geom_hline(yintercept=c(20, 32), color='red', size=0.2) +
  geom_ribbon(aes(ymin=min, ymax=max), alpha=0.7) +
  facet_wrap(~season_label, ncol=1)

# How long are the cold spells? Show weeks on the x-axis
ggplot(winter, aes(season_day)) +
  geom_hline(yintercept=c(20, 32), color='red', size=0.2) +
  geom_ribbon(aes(ymin=min, ymax=max), alpha=0.7) +
  scale_x_continuous(breaks=seq(-28, 91, 14))+
  facet_wrap(~season_label, ncol=1)

# Better labels and axes
ggplot(winter, aes(season_day)) +
  geom_hline(yintercept=c(20, 32), color='red', size=0.2) +
  geom_ribbon(aes(ymin=min, ymax=max), alpha=0.7) +
  scale_x_continuous(breaks=seq(-28, 91, 14),
                     labels=seq(-28, 91, 14)+28)+
  labs(x='Week', y='Daily temperature range',
       title='Winter temperature range in Boston',
       subtitle='Red lines show 20° and 32°F',
       caption='NOAA data for Boston Logan Airport') +
  facet_wrap(~season_label, ncol=1) +
  theme(axis.text.y=element_blank()) + 
  NULL

# It kind of looks like there hasn't been an extended
# cold snap since winter of 2017-2018

# Can we quantify that? Maybe look at sequential days
# with a max temp below freezing?
# rle() computes run lengths
# I'm not going to bore you with all the thrashing it took me
# to get this to work...
runs = winter %>% 
  group_by(season_label) %>% 
  nest(data=-season_label) %>% 
  mutate(cold=map(data, ~as.data.frame(unclass((rle(.x$max<32)))))) %>% 
  select(-data) %>% 
  unnest(cold) %>% 
  filter(values==TRUE)

# How do the run lengths vary by year?
ggplot(runs, aes(lengths)) +
  geom_histogram() +
  facet_wrap(~season_label, ncol=1)

# Always choose binwidth!
ggplot(runs, aes(lengths)) +
  geom_histogram(binwidth=1) +
  facet_wrap(~season_label, ncol=1)

# Make it a bit prettier...
ggplot(runs, aes(lengths)) +
  geom_histogram(binwidth=1) +
  scale_x_continuous(breaks=c(7, 14), minor_breaks=NULL) +
  labs(title='Consecutive days of below-freezing weather in Boston',
       x='Consecutive days of cold', y='Occurrences',
       caption='NOAA data for Boston Logan Airport') +
  facet_wrap(~season_label, ncol=1) +
  theme(axis.text.y=element_blank(),
        panel.grid.minor.y=element_blank()) +
  NULL
  
get_ecdf = function(data) {
  d = environment(ecdf(data$max))
  data.frame(x=d$x, y=d$y)
}

distr = winter %>% 
  group_by(season_label) %>% 
  nest(data=-season_label) %>% 
  mutate(distr=map(data, get_ecdf)) %>% 
  select(-data) %>% 
  unnest(distr)

distr = nest(group_by(winter, season_label), data=-season_label)

p = ggplot(distr, aes(x, y)) +
  geom_vline(xintercept=32, color='red', size=0.2) +
  geom_line(aes(color=season_label))

plotly::ggplotly(p)
