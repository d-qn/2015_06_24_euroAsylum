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

id <- c('migr_asydcfsta')
citizenAgg <- c("Total", "European Union (28 countries)", "Extra EU-28")

getYearlyData <- function(id) {
	dat <- get_eurostat(id, time_format = "raw", cache = F )
	datl <- label_eurostat(dat)
	# add iso2 code
	datl$iso2 <- dat$geo

	# transform dates efficiently!
	datl$time <- as.numeric(as.character(datl$time))
	datl
}

dat.asde <- getYearlyData(id)
dat.asde <- dat.asde %>%  filter(time == max(dat.asde$time), sex == "Total", age == "Total")

save(dat.asde, file = "data/05_asylumByCitAndGeo.Rdata")



iso2.subset <- c('CH', 'DE', 'UK', 'FR', 'IT', 'SP', 'SE', 'PT')

citot <- dat.asde %>% filter(iso2 == 'TOTAL', !citizen %in% citizenAgg, decision == "Total positive decisions")
citot2 <- dat.asde %>% filter(iso2 == 'TOTAL', !citizen %in% citizenAgg, decision == "Total")
cit.top <- unique(c(as.character(head(citot2[order(citot2$values, decreasing = T),'citizen'], 10)),
	as.character(head(citot[order(citot$values, decreasing = T),'citizen'], 10))))









dat <- filter(dat.asde, !citizen %in% citizenAgg, iso2 %in% iso2.subset)  %>%
	select(one_of(c('citizen', 'decision', 'geo', 'values', 'iso2')))

