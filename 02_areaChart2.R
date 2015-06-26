############################################################################################
###		SETTINGS
############################################################################################

source("~/swissinfo/_helpers/helpers.R")
library(rCharts)


infile <- read.csv("je-f-17.02.03.03.01.csv", stringsAsFactors = F)
colnames(infile) <- gsub("^X", "", colnames(infile))
colnames(infile)[1] <- "Parti"
sapply(infile[,-1], sum, na.rm = T)

df <- reshape2::melt(infile, id = "Parti")
df$date <- as.numeric(as.POSIXct(paste(df$variable, '-01-01', sep =""))) * 1000
df$visible <- F


h3 <- hPlot(value ~ date, data = df , type = c('area'), group = 'Parti', visible = 'visible')
h3$colors(swi_22rpalette, "darkgrey", "grey")
h3$plotOptions(area = list(stacking = "normal", marker = list(enabled = FALSE)))
h3$legend(borderWidth= 0)
h3$xAxis(type = 'datetime', dateTimeLabelFormats = list(year = '%Y'), max = max(df$date), min = min(df$date))
h3$yAxis(max = 100, title = list(text = ""))

visible = c("UDC", "PS", "PDC", "PLR", "PES", "PVL")
h3$params$series = lapply(seq_along(h3$params$series), function(i){
  x = h3$params$series[[i]]
  x$visible = x$name %in% visible
  return(x)
})

h3$set(tooltip = list(formatter =
  "#! function(){
  return Highcharts.dateFormat('%Y', new Date(this.x)) + '<br>' +
  this.series.name + ' : ' + Highcharts.numberFormat(this.y, 2) + '%'
  } !#"
))

h3
h3$save("hchart_areaParty.html")
