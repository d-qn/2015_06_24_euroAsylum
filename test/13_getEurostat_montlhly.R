###
### 'tps00189' ## get the recent monthly asylum seeker only
###

library(eurostat)
library(dplyr)
library(ggplot2)

#search_eurostat("sylum")




id <- 'tps00189'
dat <- get_eurostat(id)
datl <- label_eurostat(dat)

# add iso2 code
datl$iso2 <- dat$geo



# This line under is slow!
data.m1 <- datl


data.m1 <- data.m1 %>% filter(asyl_app == 'Asylum applicant') %>% select(one_of(c('citizen', 'geo', 'time', 'values', 'iso2')))


ggplot(filter(data.m1, iso2 != 'EU28'), aes(time, values)) + geom_line(aes(group = geo)) + facet_wrap(~ geo)
ggplot(filter(data.m1, !iso2 %in% c('EU28', 'DE')), aes(time, values)) + geom_line(aes(group = geo)) + facet_wrap(~ geo)


id <- 'tps00191'
dat <- get_eurostat(id)
datl <- label_eurostat(dat)
# add iso2 code
datl$iso2 <- dat$geo

data.m1 <- datl %>% filter(asyl_app == 'Asylum applicant') %>% select(one_of(c('citizen', 'geo', 'time', 'values', 'iso2')))

ggplot(filter(data.m1, iso2 != 'EU28'), aes(time, values)) + geom_line(aes(group = geo)) + facet_wrap(~ geo)
