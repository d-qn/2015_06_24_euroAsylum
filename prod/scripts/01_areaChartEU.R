library(swiRcharts)
library(swiTheme)

############################################################################################
###		Load data
############################################################################################

load("../data/01_areaChart.Rdata")
trad <- read.csv("../trad/01_areaChartEU.csv", check.names = F, stringsAsFactors = F, row.names = 1)

## geo country names check ##
origin <- unique(dd$geo)
idxrow.fullgeo <- grep("^full", rownames(trad))
stopifnot(all(origin %in% gsub("^full\\.", "", rownames(trad)[idxrow.fullgeo])))

############################################################################################
###		Settings chart
############################################################################################

chartHeight <- 450

# PlotLines
plotLinesColor <- "#bfa681"
styleLine <- 'LongDash'
zIndex <- -10
widthLine <- 0.5
yoffset <- 15
fontSize <-  '0.7em'
textColor <- "#bfa681"

############################################################################################
###		chart data
############################################################################################

i <- 1

for (i in 1:ncol(trad)) {

	lang <- colnames(trad)[i]
	output.html <- paste("../01_EUayslumRequests_area_", lang, ".html", sep ="")

	ddd <- dd

	## Translate country names ##
	ddd[which(ddd$iso2 == "autres"), 'iso2'] <- as.character(trad['iso.others', lang])
	# create a named vector: new geo, name old geo
	geotrad <- structure(as.character(trad[idxrow.fullgeo,lang]), names =  gsub("^full\\.", "", rownames(trad)[idxrow.fullgeo]))
	ddd$geo <- geotrad[match(ddd$geo, names(geotrad))]

	# legend order: set order for items in legend
	legendIndex <- structure(seq(0, length(unique(ddd$iso2))-1), names = c( "DE", 'AT', "FR",  "HU", "IT", "NL", "SE", "UK", 'CH', "autres pays"))

	## create fancy tooltip as html table
	ddd$name <- paste0(
		'<table cellpadding="1" style="line-height:1.4">',
	        '<tr><td><div style="font-size:0.85em"><b>', ddd$time, '</b></td></div>',
				'<td></td><td></td></tr>',
	       '<tr><td colspan="3"><div style="font-size:0.8em">', ddd$y, " ",
		   		trad["tooltip.ayslumdemand",lang], '</div></td></tr>',
	       '<tr><td align="left"><div style="font-size:0.8em"><i>', ddd$geo,'</i></td><td></td>',
		   		'<td style="text-align:right"><div style="color:#D8D8D8;font-size:0.75em">', ddd$iso2,'</div></td></tr>',
		'</table>')

	## CHART
	a <- Highcharts$new()
	a$chart(zoomType = "xy", type = 'area', height = chartHeight, spacing = 5)
	hSeries <- hSeries2(data.frame(x = ddd$time, y = ddd$y, name = ddd$name, series = ddd$geo), "series")
	h2 <- lapply(hSeries, function(series) {
		c(series, index = unname(legendIndex[ddd[match(series$name, ddd$geo),'iso2']]))
	})

	a$series(h2)
	a$colors(swi_pal)
	a$plotOptions(area = list(stacking = "normal", lineWidth = 0.1, marker = list(enabled = FALSE, symbol = "circle", radius = 1)),
		series = list(fillOpacity = 1))

	a$legend(borderWidth= 0, itemMarginTop = 3, itemMarginBottom = 5, itemHoverStyle = list(color = '#996666'),
		itemStyle = list(fontWeight = "normal", fontSize = "0.8em"),
		title = list(style = list(fontWeight ='normal'),
			text = paste0(trad['legend.country',lang], ' <span style="font-size: 9px; color: #666; font-weight: normal">',
			trad['legend.descr',lang], '</span><br>')), style = list(fontStyle = 'italic'))

	a$xAxis(title = list(text = ""), max = max(dd$time), min = min(dd$time),
		plotLines = list(
			list(color = plotLinesColor, value = 1991.4, width = widthLine, zIndex = zIndex, dashStyle = styleLine,
			label = list(text = trad['annotation.exYougoslavia',lang], rotation = 0, y = yoffset,
			style = list( color = textColor, fontSize = fontSize))),
			list(color = plotLinesColor, value = 1998.2, width = widthLine, zIndex = zIndex, dashStyle = styleLine,
			label = list(text = trad['annotation.Kosovo',lang], rotation = 0, y = yoffset * 2,
			style = list( color = textColor, fontSize = fontSize))),
			list(color = plotLinesColor, value = 2011.3, width = widthLine, zIndex = zIndex, dashStyle = styleLine,
			label = list(text = trad['annotation.Syria',lang], rotation = 0, x = -5, y = yoffset * 3, align = 'right',
			style = list( color = textColor, fontSize = fontSize)))
			)
		)

	a$lang( numericSymbols= NULL)
	a$yAxis(title = list(text = trad['y.lab',lang]), gridLineColor = "#EFEFEF",
		labels = list(formatter = "#! function () {return this.value / 1000;} !#"))

	a$tooltip(formatter = "#! function() { return this.point.name; } !#", useHTML = T , borderWidth = 3, style = list(padding = 1.5))
	#a

	hChart.html <- tempfile("hChart_area")
	a$save(hChart.html)

	# Convert highcharts-rCharts html chart into a responsive one
	hChart2responsiveHTML(hChart.html, output.html = output.html, h2 = trad['title',lang], descr = trad['descr',lang],
		source = trad['source',lang], h3 = "", author = 'Duc-Quang Nguyen | <a href = "http://www.swissinfo.ch" target="_blank">swissinfo.ch</a>')
}
