###### This download the yearly data with country of origin!
library(eurostat)
library(WDI)

library(dplyr)
library(ggplot2)
library(swiTheme)


############################################################################################
###		Get asylum data
############################################################################################
# Asylum and first time asylum applicants by citizenship, age and sex Annual aggregated data (rounded) [migr_asyappctza]
# First instance decisions on applications by citizenship, age and sex Annual aggregated data (rounded) [migr_asydcfsta]

id <- c('migr_asyappctza', 'migr_asydcfsta' )

getYearlyData <- function(id) {
	dat <- get_eurostat(id, time_format = "raw", cache = F )
	datl <- label_eurostat(dat)
	# add iso2 code
	datl$iso2 <- dat$geo

	# transform dates efficiently!
	datl$time <- as.numeric(as.character(datl$time))
	datl
}

dat.asap <- getYearlyData(id[1])
dat.asap <- filter(dat.asap, time == max(dat.asap$time), sex == "Total", age == "Total", asyl_app == 'Asylum applicant')
dat.asap <- filter(dat.asap, citizen == "Total", geo != "Total", iso2 != 'EU28') %>% select(one_of(c('geo', 'values', 'iso2')))


dat.asde <- getYearlyData(id[2])
dat.asde <- dat.asde %>%  filter(time == max(dat.asde$time), sex == "Total", age == "Total", decision == "Total positive decisions")
dat.asde <- filter(dat.asde, citizen == "Total", geo != "Total", iso2 != 'EU28')  %>% select(one_of(c('geo', 'values', 'iso2')))

############################################################################################
###		Get Eurostat population data
############################################################################################

dat <- get_eurostat('tps00001')
datl <- label_eurostat(dat)
datl$iso2 <- dat$geo
max(datl$time)

pop <- datl %>% filter(time == max(datl$time), iso2 %in% as.character(unique(dat.asap$iso2))) %>%
	select(one_of(c('geo','values', 'iso2')))

############################################################################################
###		Get World Bank data
############################################################################################

isoES2WB <- structure(as.character(unique(dat.asap$iso2)), names = as.character(unique(dat.asap$iso2)))
# EL and UK are not used by the WB!
isoES2WB[which(isoES2WB == 'EL')] <- 'GR'
isoES2WB[which(isoES2WB == 'UK')] <- 'GB'

# GDP per capita, PPP (current international $) http://data.worldbank.org/indicator/NY.GDP.PCAP.PP.CD
# Unemployment, total (% of total labor force) (modeled ILO estimate) http://data.worldbank.org/indicator/SL.UEM.TOTL.ZS

idw <- c('NY.GDP.PCAP.PP.CD', 'SL.UEM.TOTL.ZS')

getwbData <- function (ind, countries = isoES2WB) {
	indicator <- WDI(indicator = ind, country = countries, start = 2013, end = 2015)
	colnames(indicator)[3] <- 'values'
	do.call(rbind, by(indicator, indicator$iso2c, function(ii) {
		rowx <- !is.na(ii[,3])
		if(all(!rowx)) {
			ii[1,]
		} else if (all(rowx)){
			ii[which.max(ii$year),]
		} else {
			ii[which(rowx),]
		}
	}))
}

gdp <- getwbData(idw[1], isoES2WB)
une <- getwbData(idw[2], isoES2WB)


############################################################################################
###		Combine the data
############################################################################################

# demande d'asile
asylum.df <- dat.asap
colnames(asylum.df)[2] <- "demandes d'asiles en 2014"

stopifnot(match(asylum.df$iso2, names(isoES2WB)) == 1:length(isoES2WB))

asylum.df <- cbind (asylum.df, population = pop[match(asylum.df$iso2, pop$iso2), 'values'],
	gdpPerCapitaPPP = gdp[match(isoES2WB, gdp$iso2c), 'values'],
	unemployment  = une[match(isoES2WB, une$iso2c), 'values']
	)

asylum.df$demandeParMillion <- round((asylum.df$`demandes d'asiles en 2014` / asylum.df$population) * 10^6)
asylum.df$demandeParGDP <- round((asylum.df$`demandes d'asiles en 2014` / asylum.df$gdpPerCapitaPPP) * 10^3)
asylum.df$demandeParUnemployment <- round(asylum.df$`demandes d'asiles en 2014` / asylum.df$unemployment)

write.csv(
	asylum.df %>% select(one_of(c("geo", "demandes d'asiles en 2014",
	'demandeParMillion', 'demandeParGDP', 'demandeParUnemployment'))), file = "prod/04a_asylumSeekerByIndicator.csv", row.names = F)




posi.df <- dat.asde
colnames(posi.df)[2] <- "Décisions positives en 2014"
stopifnot(match(posi.df$iso2, names(isoES2WB)) == 1:length(isoES2WB))
posi.df <- cbind (posi.df, population = pop[match(posi.df$iso2, pop$iso2), 'values'],
	gdpPerCapitaPPP = gdp[match(isoES2WB, gdp$iso2c), 'values'],
	unemployment  = une[match(isoES2WB, une$iso2c), 'values']
	)
posi.df$demandeParMillion <- round((posi.df$`Décisions positives en 2014` / posi.df$population) * 10^6)
posi.df$demandeParGDP <- round((posi.df$`Décisions positives en 2014` / posi.df$gdpPerCapitaPPP) * 10^3)
posi.df$demandeParUnemployment <- round(posi.df$`Décisions positives en 2014` / posi.df$unemployment)

write.csv(posi.df %>% select(one_of(c("geo", "Décisions positives en 2014",
	'demandeParMillion', 'demandeParGDP', 'demandeParUnemployment'))), file = "prod/04b_positiveDecByIndicator.csv", row.names = F)

