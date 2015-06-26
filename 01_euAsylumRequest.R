###### This download the yearly data with country of origin!

library(eurostat)
library(dplyr)
library(ggplot2)
library(swiRcharts)
library(swiTheme)


############################################################################################
###		Get data
############################################################################################

id <- c('migr_asyctz', 'migr_asyappctza')

getYearlyData <- function(id) {
	dat <- get_eurostat(id, time_format = "raw", cache = F )
	datl <- label_eurostat(dat)
	# add iso2 code
	datl$iso2 <- dat$geo

	# transform dates efficiently!
	datl$time <- as.numeric(as.character(datl$time))
	datl
}
datl1 <- getYearlyData(id[1])
datl2 <- getYearlyData(id[2])
save(datl1, datl2, file = "data/01_areaChart.Rdata")


data.m1 <- datl1 %>% select(one_of(c('citizen', 'geo', 'time', 'values', 'iso2')))
data.m2 <- datl2 %>% filter(asyl_app == 'Asylum applicant', sex == 'Total', age == 'Total') %>%
	select(one_of(c('citizen', 'geo', 'time', 'values', 'iso2')))

	### CHECK ###
## check CH value with http://ec.europa.eu/eurostat/tgm/refreshTableAction.do?tab=table&plugin=1&pcode=tps00191&language=en
data.m2 %>% filter(iso2 == 'CH', citizen == "Total")

# merge both timeseries !

data.mm <- rbind(data.m1, data.m2)

############################################################################################
###		1 AREA chart by host country
############################################################################################

## Get for each country the total value, exclude regions iso2 and uncomplete year
data <- data.mm %>% filter(citizen == "Total", ! iso2 %in% c('EU27', 'EU28', 'TOTAL'), time >= 1986)

ggplot(data = data %>% group_by(time) %>% summarise(sum = sum(values, na.rm = T)) %>% ungroup()) +
	geom_line(aes(time, sum))

	### EXPLORE
# ggplot(data = data %>% group_by(time, iso2) %>% summarise(sum = sum(values, na.rm = T)) %>% ungroup()) +
# 	geom_line(aes(time, sum)) + facet_wrap(~iso2)

# find the n largest countries during the last year
dev <- data %>% filter(time == max(time)) %>% group_by(iso2) %>% summarise(sum = sum(values, na.rm = T)) %>% ungroup()
iso2.top <- unlist(head(as.data.frame(dev[order(dev$sum, decreasing = T),'iso2']), 9))

### Merge not top countries
dd <- data %>% select(one_of(c('geo', 'time', 'values', 'iso2')))
dd$iso2 <- ifelse(as.character(dd$iso2) %in% iso2.top, as.character(dd$iso2), 'autres')

dd.1 <- dd %>% group_by(time, iso2) %>% mutate (y = sum(values, na.rm = T)) %>% ungroup()
dd <- dd %>% group_by(time, iso2) %>% summarise (y = sum(values, na.rm = T)) %>% ungroup()


# add back the country names to merged iso2
dd$geo <- as.character(data[match(dd$iso2, data$iso2),'geo'])
dd[which(dd$iso2 == "autres"), 'geo'] <- "autres"

collapsedCountries <- paste(as.character(unique(data$geo)[!unique(data$geo) %in% dd$geo]), collapse = ", ")


# plotLines
#http://stackoverflow.com/questions/30703979/rcharts-highcharts-plotlines




### CHART AREA
a <- Highcharts$new()
a$chart(zoomType = "xy", type = 'area', height = 500, spacing = 5)
hSeries <- hSeries2(data.frame(x = dd$time, y = dd$y, name = dd$iso2, series = dd$geo), "series")
a$series(hSeries)
a$colors(swi_pal)










h <- hPlot(y ~ time, data = dd ,type = c('area'), group = 'iso2', visible = 'visible')
h$colors(swi_pal)
h$plotOptions(area = list(stacking = "normal", marker = list(enabled = FALSE, symbol = "circle", radius = 0.1)))
h$legend(borderWidth= 0)
h$xAxis(title = list(text = ""), max = max(dd$time), min = min(dd$time),
	plotLines = list(list(color = "grey", value = 1992, width = 1, zIndex = -10,
	label = list(text = "Conflit Ex-Yougoslavie", rotation = 0,
	style = list( color = 'lightgrey')))))
h$lang( numericSymbols= NULL)
h$yAxis(title = list(text = "Demandeurs d'asile en milliers"),
	labels = list(formatter = "#! function () {return this.value / 1000;} !#"))
h

# plotLines !!!!!!

############################################################################################
###		2 AREA chart by origin country
############################################################################################
data.m2 <- datl %>% filter(asyl_app == 'Asylum applicant', sex == 'Total', age == 'Total') %>%
	select(one_of(c('citizen', 'geo', 'time', 'values', 'iso2')))

	### CHECK ###
## check CH value with http://ec.europa.eu/eurostat/tgm/refreshTableAction.do?tab=table&plugin=1&pcode=tps00191&language=en
data.m2 %>% filter(iso2 == 'TOTAL', citizen == "Syria")

## Get for each country the total value   !!!!!!!!!!
data <- data.m1 %>% filter(citizen == "Total", ! iso2 %in% c('EU28', 'TOTAL'))
ggplot(data = data %>% group_by(time) %>% summarise(sum = sum(values, na.rm = T)) %>% ungroup()) +
	geom_line(aes(time, sum))







############################################################################################
###		Prior 2008
############################################################################################



