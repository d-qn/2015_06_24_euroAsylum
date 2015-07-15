library(WDI)
library(countrycode)
library(tidyr)
library(dplyr)
library(ggplot2)
library(swiTheme)
library(swiMap)
library(leaflet)
library(htmlwidgets)

getData <- F

trad <- read.csv("../trad/06_refugessByPopGDP.csv", check.names = F, stringsAsFactors = F, row.names = 1)

if(getData) {
	############################################################################################
	###		Load UNHCR asylum seeker data
	############################################################################################

	dat.read <- read.csv("data/unhcr_popstats_export_time_series_2015_07_10_150142_refugees.csv", check.names = F, stringsAsFactors = F)

	dat.read <- rename(dat.read, country = `Country / territory of asylum/residence`)
	dat.read$Value <- as.numeric(dat.read$Value)

	###	Aggregate values by country (drop origin)
	dat <- dat.read %>% group_by(country) %>% summarise(total = sum(Value, na.rm = T)) %>% ungroup()
	###	Aggregate values by country and origin
	dat2 <- dat.read %>% group_by(country, `Population type`) %>% summarise(total = sum(Value, na.rm = T)) %>% ungroup()

	###	get the iso country code
	dat$iso2 <- countrycode(dat$country, "country.name", "iso2c")
	dat2$iso2 <- countrycode(dat2$country, "country.name", "iso2c")

	# discard VG, NR
	dat <- dat[!dat$iso2 %in% c('VG', 'NR'),]

	############################################################################################
	###		Get world bank indicators
	############################################################################################

	# GDP per capita, PPP (current international $) http://data.worldbank.org/indicator/NY.GDP.PCAP.PP.CD
	# Population, total http://data.worldbank.org/indicator/SP.POP.TOTL

	idw <- c('NY.GDP.PCAP.PP.CD', 'SP.POP.TOTL')

	getwbData <- function (ind, countries = dat$iso2) {
		indicator <- WDI(indicator = ind, country = countries, start = 2008, end = 2015)
		colnames(indicator)[3] <- 'values'
		do.call(rbind, by(indicator, indicator$iso2c, function(ii) {
			rowx <- !is.na(ii[,3])
			if(all(!rowx)) {
				ii[1,]
			} else if (all(rowx)){
				ii[which.max(ii$year),]
			} else {
				ii[which(rowx)[1],]
			}
		}))
	}

	gdp <- getwbData(idw[1], dat$iso2)
	pop <- getwbData(idw[2], dat$iso2)

	# combine indicators
	dat$totalParMillion <- round((dat$total / pop[match(dat$iso2, pop$iso2c),'values']) * 10^6)
	dat$totalParPIB <- round((dat$total / gdp[match(dat$iso2, gdp$iso2c),'values'] ), 1)

	############################################################################################
	###		MAP
	############################################################################################
	library(ggmap)
	library(leaflet)
	library(htmlwidgets)


	# Geocode every country (concatenate country name with iso2 code)
	latlon <- geocode(paste(dat$country, dat$iso2, sep = ", country="), output = c("latlon"), messaging = F)
	data <- cbind(dat, latlon)

	# remove parenthesis and rep. abbreviations
	naCountry <- gsub("Rep\\.", "Republic", gsub(" \\(.*\\)$", "", data[is.na(data$lat),'country']))
	latlon2 <- geocode(paste(naCountry,  data[is.na(data$lat),'iso2'], sep = ", country="),  output = c("latlon"), messaging = F)
	data[is.na(data$lat), c('lon', 'lat')] <- latlon2

	# remove lines with NA lat lon
	data <- data[!is.na(data$lat),]


	## Add back the number of asylum seeker and refugees
	# add asylum and refugees numbers
	tmp <- filter(dat2, `Population type` == "Asylum-seekers")
	data$asylumSeeker <- unlist(tmp[match(data$iso2, tmp$iso2), 'total'])
	tmp <- filter(dat2, `Population type` == "Refugees (incl. refugee-like situations)")
	data$refugees <- unlist(tmp[match(data$iso2, tmp$iso2), 'total'])
	# add PIB and GDP
	data$pop <- pop[match(data$iso2, pop$iso2c),'values']
	data$gdp <- round(gdp[match(data$iso2, gdp$iso2c),'values'])

	save(data, file = "../data/06_refugeesMap.Rdata")
} else {
	load("../data/06_refugeesMap.Rdata")
}


### Set country translation
# countryTrans <- read.csv("~/swissinfo/_helpers/countrynames.csv", sep ="\t")
# write.csv(countryTrans[c(which(countryTrans$iso2 %in% data$iso2), which(!countryTrans$iso2 %in% data$iso2)),],
# 	file = "~/swissinfo/_helpers/countrynames_ordered.csv")

############################################################################################
###		map settings
############################################################################################
fontSize <- "0.9em"


i <- 1

for (i in 1:ncol(trad)) {

	lang <- colnames(trad)[i]
	output.html <- paste("06_map_refugeesAsylum_", lang, ".html", sep ="")
	data$geo <- countryTranslation(data$iso2, toupper(lang))[,2]

	groups <- c(trad['group1',lang], trad['group2',lang])

	mb_tiles <- 'http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}'
	mb_attribution <- paste0(trad['credits',lang], ' | Tiles &copy; Esri &mdash; Esri, DeLorme, NAVTEQ')

	top <- paste0('<strong>', data$geo, "</strong>", '<p style=\"font-size:', fontSize, '\">',
	  trad['tooltip.refugeesTotal', lang], ": ", "<strong>",  data$refugees, "</strong><br>",
  	  trad['tooltip.asylumTotal', lang], ": ", "<strong>",  data$asylumSeeker,"</strong><br><br>"
	  )

	popup_pop <- paste0(top,
	  trad['tooltip.population', lang], ": ", round(data$pop / 10^6, 2), " ", trad['tooltip.popMillion', lang], "<br>",
	  trad['tooltip.totalByPop', lang], ": ", "<strong>", data$totalParMillion,
	  "</strong></p>")

	popup_gdp <- paste0(top,
	  trad['tooltip.gdpPerCapita', lang],  ": ", data$gdp, "<br>",
   	  trad['tooltip.totbyGDP', lang], ": ", "<strong>", data$totalParPIB,
  	  "</strong></p>"
	)

	mn <- leaflet(data = data) %>% addTiles(urlTemplate = mb_tiles, attribution = mb_attribution) %>%
		addCircleMarkers(lng = ~lon, lat = ~lat, stroke = FALSE, fillOpacity = 0.4, fillColor = swi_rpal[1],
	    radius = ~sqrt(totalParMillion) / 12, popup = popup_pop, group=groups[1]) %>%
		addCircleMarkers(lng = ~lon, lat = ~lat, stroke = FALSE, fillOpacity = 0.4, fillColor = swi_rpal[7],
	    radius = ~sqrt(totalParPIB) * 2, popup = popup_gdp, group=groups[2]) %>%
		setView(21.824312, 39.074208, zoom = 3) %>%
		addLegend(position = "topright", title = trad['title',lang], opacity = 0, colors = NULL, labels = NULL) %>%
	    addLayersControl(
	       baseGroups = c(groups[1], groups[2]),
	       options = layersControlOptions(collapsed = FALSE)
	     ) %>% hideGroup(groups[2])

	saveWidget(mn, file = output.html, selfcontained = FALSE, libdir = "leafletjs")
}
