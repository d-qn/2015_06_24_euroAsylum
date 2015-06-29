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

# Display which countries were not in the first timeseries
unique(data.m2$geo)[!unique(data.m2$geo) %in% unique(data.m1$geo)]
#Croatia                       Liechtenstein

############################################################################################
###		1 AREA chart by host country
############################################################################################

## Get for each country the total value, exclude regions iso2 and uncomplete year
data <- data.mm %>% filter(citizen == "Total", ! iso2 %in% c('EU27', 'EU28', 'TOTAL'), time >= 1986)


tot <- data %>% group_by(time) %>% summarise(sum = sum(values, na.rm = T)) %>% ungroup()
ggplot(data = tot) + geom_line(aes(time, sum))

	### EXPLORE
# ggplot(data = data %>% group_by(time, iso2) %>% summarise(sum = sum(values, na.rm = T)) %>% ungroup()) +
# 	geom_line(aes(time, sum)) + facet_wrap(~iso2)

# find the n largest countries during the last year
dev <- data %>% filter(time == max(time)) %>% group_by(iso2) %>% summarise(sum = sum(values, na.rm = T)) %>% ungroup()
iso2.top <- unlist(head(as.data.frame(dev[order(dev$sum, decreasing = T),'iso2']), 9))

### Merge not top countries
dd <- data %>% select(one_of(c('geo', 'time', 'values', 'iso2')))
dd$iso2 <- ifelse(as.character(dd$iso2) %in% iso2.top, as.character(dd$iso2), 'autres')

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
hSeries <- hSeries2(data.frame(x = dd$time, y = dd$y, name = dd$geo, series = dd$iso2), "series")

a$series(hSeries)
a$colors(swi_pal)
a$plotOptions(area = list(stacking = "normal", lineWidth = 0, marker = list(enabled = FALSE, symbol = "circle", radius = 0.5)))
a$legend(borderWidth= 0)
a$xAxis(title = list(text = ""), max = max(dd$time), min = min(dd$time),
	plotLines = list(list(color = "grey", value = 1992, width = 1, zIndex = -10,
	label = list(text = "Conflit Ex-Yougoslavie", rotation = 0,
	style = list( color = 'lightgrey')))))
a$lang( numericSymbols= NULL)
a$yAxis(title = list(text = "Demandeurs d'asile en milliers"),
	labels = list(formatter = "#! function () {return this.value / 1000;} !#"))

	## TO DO tooltip!



save(dd, collapsedCountries, file = "prod/01_areaChart.Rdata")

############################################################################################
###		2 AREA chart by origin country
############################################################################################

# There is only precise country of origin across EU since 2008

data.m2 %>% filter(iso2 == "TOTAL", time == 2014) %>% top_n(20, values)

data <- data.m2 %>% filter(iso2 == "TOTAL", ! citizen %in% c('Extra EU-28', 'European Union (28 countries)', 'Total'))

tot2 <- data %>% group_by(time) %>% summarise(sum = sum(values, na.rm = T)) %>% ungroup()
# check: compare with as.data.frame(tot) !!

# find the n largest countries during the last year
dev <- data %>% filter(time == max(time)) %>% group_by(citizen) %>% summarise(sum = sum(values, na.rm = T)) %>% ungroup()
cit.top <- unlist(head(as.data.frame(dev[order(dev$sum, decreasing = T),'citizen']), 10))


### Merge not top countries
ddd <- data %>% select(one_of(c('citizen', 'time', 'values')))
ddd$citizen <- ifelse(as.character(ddd$citizen) %in% cit.top, as.character(ddd$citizen), 'autres')

ddd <- ddd %>% group_by(time, citizen) %>% summarise (y = sum(values, na.rm = T)) %>% ungroup()



b <- Highcharts$new()
b$chart(zoomType = "xy", type = 'area', height = 500, spacing = 5)
hSeries <- hSeries2(data.frame(x = ddd$time, y = ddd$y, name = ddd$citizen, series = ddd$citizen), "series")

b$series(hSeries)
b$colors(swi_pal)
b$plotOptions(area = list(stacking = "normal", lineWidth = 0, marker = list(enabled = FALSE, symbol = "circle", radius = 0.5)))
b$legend(borderWidth= 0)
b$xAxis(title = list(text = ""), max = max(ddd$time), min = min(ddd$time))
b$lang( numericSymbols= NULL)
b$yAxis(title = list(text = "Demandeurs d'asile en milliers"),
	labels = list(formatter = "#! function () {return this.value / 1000;} !#"))


############################################################################################
###		3 WAFFLE chart by origin country of others
############################################################################################
library(waffle)

ee <- data %>% filter(time == 2014) %>% select(one_of(c('citizen', 'values')))

ee$pc <- ee$values / sum(ee$values) * 100

ww <- ee[which(ee$pc >= 1.5),]
ww$n <- round(ww$pc)
wf <- structure(ww$n, names = as.character(ww$citizen))
wf<- c(wf, `autres` = 100-sum(wf))

waffle(wf, rows = 20)



