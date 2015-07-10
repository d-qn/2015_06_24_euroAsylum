###### This download the yearly data with country of origin!
library(eurostat)
library(WDI)
library(dplyr)
library(tidyr)
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

dat.asde1 <- dat.asde %>%  filter(time == max(dat.asde$time), sex == "Total", age == "Total", decision == "Total positive decisions")
dat.asde1 <- filter(dat.asde1, citizen == "Total", geo != "Total", iso2 != 'EU28')  %>% select(one_of(c('geo', 'values', 'iso2')))


dat.asde2 <- dat.asde %>%  filter(time == max(dat.asde$time), sex == "Total", age == "Total",
	decision %in% c("Total positive decisions", "Total")) %>% filter(citizen == "Total", geo != "Total", iso2 != 'EU28')
dat.asde2 <- spread(dat.asde2, decision, values) %>% select(one_of(c('geo', 'iso2', 'Total', 'Total positive decisions')))
dat.asde2$tauxDeReconnaissance <- (dat.asde2$`Total positive decisions` / dat.asde2$Total) * 100

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

## COMBINE ASYLUM ABSOLUTE NUMBERS
# demande d'asile
asylum.df <- dat.asap %>% select(iso2, geo, values)
colnames(asylum.df)[3] <- "Demandes d'asiles en 2014"

stopifnot(dat.asde1$geo == dat.asde2$geo)

asylum.df <- cbind(asylum.df,
	`Décisions positives en première instance en 2014` = dat.asde1$values,
	`Taux de décision positive en première instance 2014` = round(dat.asde2$tauxDeReconnaissance, 1)
)

## match indicators to result data.frame
stopifnot(match(asylum.df$iso2, names(isoES2WB)) == 1:length(isoES2WB))

asy.pop <- pop[match(asylum.df$iso2, pop$iso2), 'values']
asy.gdp <- round(gdp[match(isoES2WB, gdp$iso2c), 'values'])

asylum.df$`Demandes d'asiles en 2014 par million d'habitants` <- round((asylum.df$`Demandes d'asiles en 2014` / asy.pop) * 10^6)
asylum.df$`Nombre de demandeurs d'asile par 1 USD du PIB par habitant` <- round((asylum.df$`Demandes d'asiles en 2014` / asy.gdp ), 3)


asylum.df$`Décisions positives en première instance par million d'habitants` <- round((asylum.df$`Décisions positives en première instance en 2014` / asy.pop) * 10^6)
asylum.df$`Décisions positives en première instance par 1 USD du PIB par habitant` <-
	round( asylum.df$`Décisions positives en première instance en 2014`  / asy.gdp , 3)

write.csv(asylum.df, file = "prod/04_asylumDW.csv", row.names = F)





#
#
# write.csv(
# 	asylum.df %>% select(one_of(c("geo", "demandes d'asiles en 2014",
# 	'demandeParMillion', 'demandeParGDP', 'demandeParUnemployment'))), file = "prod/04a_asylumSeekerByIndicator.csv", row.names = F)
#
#
#
#
# posi.df <- dat.asde
# colnames(posi.df)[2] <- "Décisions positives en 2014"
# stopifnot(match(posi.df$iso2, names(isoES2WB)) == 1:length(isoES2WB))
# posi.df <- cbind (posi.df, population = pop[match(posi.df$iso2, pop$iso2), 'values'],
# 	gdpPerCapitaPPP = gdp[match(isoES2WB, gdp$iso2c), 'values'],
# 	unemployment  = une[match(isoES2WB, une$iso2c), 'values']
# 	)
# posi.df$demandeParMillion <- round((posi.df$`Décisions positives en 2014` / posi.df$population) * 10^6)
# posi.df$demandeParGDP <- round((posi.df$`Décisions positives en 2014` / posi.df$gdpPerCapitaPPP) * 10^3)
# posi.df$demandeParUnemployment <- round(posi.df$`Décisions positives en 2014` / posi.df$unemployment)
#
# write.csv(posi.df %>% select(one_of(c("geo", "Décisions positives en 2014",
# 	'demandeParMillion', 'demandeParGDP', 'demandeParUnemployment'))), file = "prod/04b_positiveDecByIndicator.csv", row.names = F)
#
