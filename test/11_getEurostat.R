###
### 'migr_asyappctza' ## get all the country of origin by year !!!!
###


library(eurostat)
library(dplyr)
library(ggplot2)

#search_eurostat("sylum")




id <- 'migr_asyappctza'
dat <- get_eurostat(id, time_format = "raw", cache = F)
datl <- label_eurostat(dat)
# add iso2 code
datl$iso2 <- dat$geo

# transform dates efficiently!
datl$time <- as.numeric(as.character(datl$time))




# This line under is slow!
data.m1 <- datl

#save(data.m1, file="data/eurostat.Rdata")

data.m1 <- data.m1 %>% filter(asyl_app == 'Asylum applicant') %>% filter(sex == 'Total') %>% filter(age == 'Total') %>%
	select(one_of(c('citizen', 'geo', 'time', 'values', 'iso2')))

data.n1 <- data.m1 %>% group_by(iso2, time) %>% summarise(sum = sum(values)) %>% ungroup()
ggplot(filter(data.n1, !iso2 %in% c('EU28', 'TOTAL')), aes(time, sum)) + geom_line(aes(group = iso2)) + facet_wrap(~ iso2)




datam.m2 <- asylumts(tablesID[2])


ggplot(filter(data.m1, iso2 != 'EU28'), aes(time, values)) + geom_line(aes(group = geo)) + facet_wrap(~ geo)
ggplot(filter(data.m2, iso2 != 'EU28'), aes(time, values)) + geom_line(aes(group = geo)) + facet_wrap(~ geo)


