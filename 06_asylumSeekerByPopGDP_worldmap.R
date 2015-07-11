library(WDI)
library(countrycode)
library(tidyr)
library(dplyr)
library(ggplot2)
library(swiTheme)
library(leaflet)

getData <- F

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

	save(data, file = "prod/data/06_refugeesMap.Rdata")
} else {
	load("prod/data/06_refugeesMap.Rdata")
}


############################################################################################
###		map settings
############################################################################################
fontSize <- "0.9em"
groups <- c("par million d'habitants", "par 1 USD du PIB par habitant")
mapTitle <- "Total des réfugiés et demandeurs d'asile dans le monde en 2014"
mapSubtitle <- "Exprimez ce total en fonction de la population ou du PIB par habitant ci-dessous."
source <- "swissinfo.ch | source: UNHCR & The World Bank"


mb_tiles <- 'http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}'
mb_attribution <- paste0(source, ' | Tiles &copy; Esri &mdash; Esri, DeLorme, NAVTEQ')




country_popup1 <- paste0('<p style=\"font-size:', fontSize, '\"><strong>', data$country, "</strong><br><br>",
					  "Nombre total de réfugiés: ", "<strong>",  data$refugees,"</strong><br>",
					  "Nombre total de demandeurs d'asile: ", "<strong>",  data$asylumSeeker,"</strong><br><br>",
					  "Population", ": ", round(data$pop / 10^6, 2), " million <br><br>",
					  "Nombre de réfugiés et demandeurs d'asile par million d'habitant: ", "<strong>", data$totalParMillion,
					  "</strong></p>"
					  )

country_popup2 <- paste0('<p style=\"font-size:', fontSize, '\"><strong>', data$country, "</strong><br><br>",
					"Nombre total de réfugiés: ", "<strong>",  data$refugees,"</strong><br>",
					"Nombre total de demandeurs d'asile: ", "<strong>",  data$asylumSeeker,"</strong><br><br>",
					"PIB par habitant (à parité de pouvoir d'achat)", ": ", data$gdp, " USD<br><br>",
				  	"Nombre de réfugiés et demandeurs d'asile par 1 USD de PIB par habitant: ", "<strong>", data$totalParPIB,
				  	"</strong></p>"
				  )

mn <- leaflet(data = data) %>% addTiles(urlTemplate = mb_tiles, attribution = mb_attribution) %>%
	addCircleMarkers(lng = ~lon, lat = ~lat, stroke = FALSE, fillOpacity = 0.4, fillColor = swi_rpal[1],
    radius = ~sqrt(totalParMillion) / 12, popup = country_popup1, group=groups[1]) %>%
	addCircleMarkers(lng = ~lon, lat = ~lat, stroke = FALSE, fillOpacity = 0.4, fillColor = swi_rpal[7],
    radius = ~sqrt(totalParPIB) * 2, popup = country_popup2, group=groups[2]) %>%
	setView(21.824312, 39.074208, zoom = 3) %>%
	addLegend(position = "topright", title = mapTitle, opacity = 0, colors = "white", labels = mapSubtitle) %>%
    addLayersControl(
       baseGroups = c(groups[1], groups[2]),
       options = layersControlOptions(collapsed = FALSE)
     ) %>% hideGroup(groups[2])


saveWidget(mn, file="map_refugeesAsylum.html", selfcontained = FALSE, libdir = "leafletjs")


