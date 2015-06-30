###### This download the very large monthly data with country of origin!
library(eurostat)
library(dplyr)
library(ggplot2)
library(swiRcharts)
library(swiTheme)

############################################################################################
###		Get data
############################################################################################


citizenAgg <- c('Total', 'Extra EU-28', 'European Union (28 countries)')
id <- c('migr_asyappctzm')

getYearlyData <- function(id) {
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
datl1 <- getYearlyData(id[1])
save(datl1, file = "data/02_citizenMonthly.Rdata")


# subset the data for year 2015
data.m <- datl1 %>% filter(asyl_app == 'Asylum applicant', sex == 'Total', age == 'Total') %>%
	select(one_of(c('citizen', 'geo', 'time', 'values', 'iso2')))
data <- data.m %>% filter(time >= as.Date("2015-01-01"))


	### CHECK ###
## check CH value with http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=migr_asyappctzm&lang=en
data %>% filter(iso2 == 'CH', citizen == "Total")

### ANALYSIS

## TO do compare the 2015 monthly statistics with the previous year!
data.m %>% filter(iso2 == 'CH', citizen == "Total", time < as.Date("2015-01-01"), time >= as.Date("2014-01-01"))


############################################################################################
###		WAFFLE chart by origin country of others
############################################################################################
library(waffle)

unit <- 100

## Get the total since the beginning of the year
sumUpToNow <- unlist(data %>% filter(iso2 == 'CH', citizen == "Total") %>% group_by(citizen) %>% summarise(sum = sum(values, na.rm =T)) %>% ungroup())['sum']

citSum <- data %>% filter(iso2 == 'CH', !citizen %in% citizenAgg) %>% group_by(citizen) %>% summarise(sum = sum(values, na.rm =T))  %>% ungroup()
citSum <- citSum %>% mutate(red = round(sum / unit)) %>% filter(red >= 3)


wf <- structure(citSum$red, names = as.character(citSum$citizen))
wf <- c(wf, `autres` = round(sumUpToNow / unit) - sum(wf))

waffle(wf, rows = 10, colors = swi_rpal[1:length(wf)], xlab = paste("1 carré = ", unit, "demandes d'asile")) +
	theme(legend.key.size = unit(10, "points"))








