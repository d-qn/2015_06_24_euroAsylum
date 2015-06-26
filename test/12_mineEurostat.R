###### This download the very large monthly data with country of origin!


library(eurostat)
library(dplyr)
library(ggplot2)

search_eurostat("sylum")

tablesID <- c(
	'migr_asyappctzm', # Asylum and new asylum applicants - monthly data from 2008
	'migr_asyappctzm',  # Asylum and first time asylum applicants by citizenship, age and sex Monthly
	'tps00194' # Asylum applicants considered to be unaccompanied minors - annual data
	)


asylumts <- function(id) {
	dat <- get_eurostat(id, time_format = "raw", cache = F )
	datl <- label_eurostat(dat)
	# add iso2 code
	datl$iso2 <- dat$geo

	# transform dates efficiently!
	times <- unique(datl$time)
	times <- structure(eurostat:::eurotime2date(times, last = FALSE), names = as.character(times))
	datl$time <- times[match(datl$time, names(times))]
	datl
}


# This line under is slow!
data.m1 <- asylumts(tablesID[1])

#save(data.m1, file="data/eurostat.Rdata")

data.m1 <- data.m1 %>% filter(asyl_app == 'Asylum applicant') %>% filter(sex == 'Total') %>% filter(age == 'Total') %>%
select(one_of(c('citizen', 'geo', 'time', 'values', 'iso2')))

data.n1 <- data.m1 %>% group_by(iso2, time) %>% summarise(sum = sum(values)) %>% ungroup()


ggplot(data.n1 %>% filter(!iso2 %in% c('EU28', 'TOTAL', 'DE')) %>% filter(time >= as.Date("2012-01-01")), aes(time, sum)) +
	geom_line(aes(group = iso2)) + facet_wrap(~ iso2)




datam.m2 <- asylumts(tablesID[2])


ggplot(filter(data.m1, iso2 != 'EU28'), aes(time, values)) + geom_line(aes(group = geo)) + facet_wrap(~ geo)
ggplot(filter(data.m2, iso2 != 'EU28'), aes(time, values)) + geom_line(aes(group = geo)) + facet_wrap(~ geo)


