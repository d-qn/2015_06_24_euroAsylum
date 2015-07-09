###### This download the yearly data with country of origin!
library(eurostat)
library(WDI)
library(tidyr)
library(dplyr)
library(ggplot2)
library(swiTheme)
library(swiRcharts)

getData <- F

############################################################################################
###		Get asylum data
############################################################################################
# Asylum and first time asylum applicants by citizenship, age and sex Annual aggregated data (rounded) [migr_asyappctza]
# First instance decisions on applications by citizenship, age and sex Annual aggregated data (rounded) [migr_asydcfsta]

if(getData) {
	id <- c('migr_asydcfsta')


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
} else {
	load(file = "data/05_asylumByCitAndGeo.Rdata")
}
citizenAgg <- c("Total", "European Union (28 countries)", "Extra EU-28")

############################################################################################
###		Compute the postitive decision rate for all geos
############################################################################################

tot.pos <- dat.asde %>% group_by(iso2) %>% filter( citizen == 'Total', decision == "Total positive decisions") %>% ungroup()
tot.dec <- dat.asde %>% group_by(iso2) %>% filter( citizen == 'Total', decision == "Total") %>% ungroup()
stopifnot(tot.pos$iso2 ==  tot.dec$iso2)

data.frame(iso2 = tot.pos$iso2, geo = tot.pos$geo, rate = (tot.pos$values / tot.dec$values) * 100)

############################################################################################
###		Create a subset data with a few geos to plot
############################################################################################

iso2.subset <- c('CH', 'DE', 'UK', 'FR', 'IT', 'SP', 'SE', 'HU')

citot <- dat.asde %>% filter(iso2 == 'TOTAL', !citizen %in% citizenAgg, decision == "Total positive decisions")
citot2 <- dat.asde %>% filter(iso2 == 'TOTAL', !citizen %in% citizenAgg, decision == "Total")

cit.top <- unique(c(as.character(head(citot2[order(citot2$values, decreasing = T),'citizen'], 6)),
	as.character(head(citot[order(citot$values, decreasing = T),'citizen'], 5))))

cit.top <- cit.top[!cit.top %in% c("Albania", "Serbia", "Unknown")]

dat <- filter(dat.asde, !citizen %in% citizenAgg, decision %in% c("Total positive decisions",  "Total"),
	!geo %in% c('EU28', 'TOTAL'))  %>% select(one_of(c('citizen', 'decision', 'geo', 'values', 'iso2')))

dat <- dat %>% filter(iso2 %in% iso2.subset) %>% mutate(CIT = ifelse(citizen %in% cit.top, as.character(citizen), "autres pays"))


## spread - make data wide for col decision & collapse autre pays! Killer line, go dplyr
data <- spread(dat, decision, values) %>% group_by(CIT, geo, iso2) %>% summarise(totDec = sum(Total, na.rm = T),
	totPos = sum(`Total positive decisions`, na.rm = T)) %>% ungroup()

##### check ###### !!!
data %>% group_by(iso2) %>% summarise(gTotDec = sum(totDec, na.rm =T), gTotPos = sum(totPos, na.rm = T )) %>% ungroup()
tot.pos
tot.dec

# ALMOST THE SAME ?!?!?
data$posRate <- round((data$totPos / data$totDec) * 100)

# Order the country by the average posRate:
ordered.iso2subset <- data %>% group_by(iso2) %>% summarise(meanPosRate = mean(posRate, na.rm =T )) %>% select(iso2) %>% ungroup()

############################################################################################
###		Bubble chart
############################################################################################






data$iso2 <- factor(data$iso2, levels = as.character(unlist(ordered.iso2subset)))
#ggplot(data = data) + geom_line(aes(x = iso2, y = posRate, group = CIT, colour = CIT))

# change CIT to series, drop unuse factors
data <- rename(data, series = CIT)
data$geo <- factor(data$geo)
citColors <- structure(swi_rpal[1:length(unique(data$series))], names = unique(data$series))
data$color <- citColors[match(data$series, names(citColors))]


## create fancy tooltip as html table
data$name <- paste0(
	'<table cellpadding="1" style="line-height:1.4">',
    '<tr><td><div style="font-size:0.9em">', data$geo, '</td></div>',
		'<td></td><td></td></tr>',
    '<tr><td colspan="3"><div style="font-size:0.85em">',
		"Décisions en première en instance pour demandes d'asile:", " ",
		'<b>', data$series, '</b>','</div></td></tr>',
       '<tr><td colspan="3"><div style="font-size:0.85em">', data$totDec, '</div></td></tr>',
	   '<tr><td colspan="3"><div style="font-size:0.85em">',  "% décision positive :", '<b> ',
	 	 	data$posRate, '</b> ', "(", data$totPos, ")", '</div></td></tr>',
	'</table>')



a <- rCharts::Highcharts$new()
hSeries <- hSeries2(data.frame(x = as.numeric(data$geo), y = data$posRate, z = data$totDec,
	name = data$name, series = data$series), "series")
a$series(hSeries)

a$chart(zoomType = "xy", type = "bubble", height = 600)
a$xAxis(categories = c("", levels(data$geo)))
a$lang( numericSymbols= NULL)
a$plotOptions(bubble = list(minSize = 3, maxSize = 70))
a$yAxis(title = list(text = "positive decision rate on first instance (%)", style = list(fontWeight = "bold")),
	labels = list(format = '{value}'), floor = 0, ceiling = 120, gridLineWidth = 0)

a$legend(title = list(style = list(fontWeight ='normal'),
		text = paste0("Pays d'origine", ' <span style="font-size: 9px; color: #666; font-weight: normal">',
		"Cliquer pour masquer", '</span><br>')), style = list(fontStyle = 'italic'))

a$tooltip(formatter = "#! function() { return this.point.name; } !#", useHTML = T , borderWidth = 3, style = list(padding = 1.5))
a

#a

hChart.html <- tempfile("hchart_labelledBubble.html")
a$save(hChart.html)

# Example of converting a highcharts-rCharts html chart into a responsive one

hChart2responsiveHTML(hChart.html, output.html = output.html, h2 = trad['title',lang], descr = trad['descr',lang],
	source = trad['source',lang], h3 = "", author = " | swissinfo.ch")
