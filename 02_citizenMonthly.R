###### This download the very large monthly data with country of origin!
library(eurostat)
library(dplyr)
library(ggplot2)
library(swiTheme)

loadData <- T
inputfile <- "data/02_citizenMonthly.Rdata"
outputfile <- "prod/data/02_citizenMonthly_waffled.Rdata"

############################################################################################
###		Get data
############################################################################################

if(loadData) {
	load(file = "data/02_citizenMonthly.Rdata")
} else {
	##### 1 Get monthly asylum data
	id <- c('migr_asyappctzm')

	getYearlyData <- function(id) {
		dat <- get_eurostat(id, time_format = "raw", cache = F )
		datl <- label_eurostat(dat)
		# add iso2 code
		datl$iso2 <- dat$geo

		# transform dates efficiently!
		times <- unique(datl$time)
		times <- structure(eurostat:::eurotime2date(times, last = FALSE), names = as.character(times))
		datl$time <- times[match(datl$time, names(times))]
		datl
	}
	datl1 <- getYearlyData(id[1])

	##### 2 Get Population
	dat <- get_eurostat('tps00001')
	datl <- label_eurostat(dat)
	datl$iso2 <- dat$geo

	pop <- datl %>% filter(time <= as.Date("2015-01-01")) %>% select(one_of(c('geo', 'time','values', 'iso2')))
	save(datl1, pop, file = "data/02_citizenMonthly.Rdata")
}


# subset the data for year 2015
data.m <- datl1 %>% filter(asyl_app == 'Asylum applicant', sex == 'Total', age == 'Total') %>%
	select(one_of(c('citizen', 'geo', 'time', 'values', 'iso2')))


# consider only the months with full data (with NA less than 10%) !!
times <- tapply(data.m$values, data.m$time, function(v) (sum(is.na(v))/length(v)) ) < 0.1
# subset by time
data <- data.m %>% filter(time >= as.Date("2015-01-01"), time <= max(as.Date(names(times))[times]))


	### CHECK ###
## check CH value with http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=migr_asyappctzm&lang=en
data %>% filter(iso2 == 'CH', citizen == "Total")
data %>% filter(iso2 == 'HU', citizen == "Total")

### ANALYSIS

## TO do compare the 2015 monthly statistics with the previous year!
data.m %>% filter(iso2 == 'CH', citizen == "Total", time < as.Date("2015-01-01"), time >= as.Date("2014-01-01"))


############################################################################################
###		WAFFLE chart by origin country of others
############################################################################################

library(waffle)


#### Adapt the original function
waffled <- function (parts, rows = 10, xlab = NULL, title = NULL, colors = NA,
    size = 2, flip = FALSE, reverse = FALSE, equal = TRUE, pad = 0)
{
    part_names <- names(parts)
    if (length(part_names) < length(parts)) {
        part_names <- c(part_names, LETTERS[1:length(parts) -
            length(part_names)])
    }
    if (all(is.na(colors))) {
        colors <- brewer.pal(length(parts), "Set2")
    }
    parts_vec <- unlist(sapply(1:length(parts), function(i) {
        rep(LETTERS[i + 1], parts[i])
    }))
    if (reverse) {
        parts_vec <- rev(parts_vec)
    }
    dat <- expand.grid(y = 1:rows, x = seq_len(pad + (ceiling(sum(parts)/rows))))
	##### My change, added factor and levels being of length parts !!!!!!!!!!!!
    dat$value <- factor(c(parts_vec, rep(NA, nrow(dat) - length(parts_vec))), levels = LETTERS[2:(length(parts)+1)])
    if (flip) {
        gg <- ggplot(dat, aes(x = y, y = x, fill = value))
    }
    else {
        gg <- ggplot(dat, aes(x = x, y = y, fill = value))
    }
    gg <- gg + geom_tile(color = "white", size = size)
    gg <- gg + labs(x = xlab, y = NULL, title = title)
    gg <- gg + scale_x_continuous(expand = c(0, 0))
    gg <- gg + scale_y_continuous(expand = c(0, 0))
    ###### Added this as well, not sure it is useful: drop = FALSE !!!!!!!!!!!!
	gg <- gg + scale_fill_manual(name = "", values = colors,
        labels = part_names, drop = FALSE)
    gg <- gg + guides(fill = guide_legend(override.aes = list(colour = NULL)))
    if (equal) {
        gg <- gg + coord_equal()
    }
    gg <- gg + theme_bw()
    gg <- gg + theme(panel.grid = element_blank())
    gg <- gg + theme(panel.border = element_blank())
    gg <- gg + theme(panel.background = element_blank())
    gg <- gg + theme(panel.margin = unit(0, "null"))
    gg <- gg + theme(axis.text = element_blank())
    gg <- gg + theme(axis.title.x = element_text(size = 10))
    gg <- gg + theme(axis.ticks = element_blank())
    gg <- gg + theme(axis.line = element_blank())
    gg <- gg + theme(axis.ticks.length = unit(0, "null"))
    gg <- gg + theme(axis.ticks.margin = unit(0, "null"))
    gg <- gg + theme(plot.title = element_text(size = 18))
    gg <- gg + theme(plot.background = element_blank())
    gg <- gg + theme(plot.margin = unit(c(0, 0, 0, 0), "null"))
    gg <- gg + theme(plot.margin = rep(unit(0, "null"), 4))
    gg
}

###	waffle settings
font <- "Open Sans"
unit <- 5 * 10^4
squareThreshold <- 3
w.row <- 10

###	COMPUTE

citizenAgg <- c("Total", "European Union (28 countries)", "Extra EU-28")

## Compute the asylum seeker by iso2 country
dd <- data %>% filter(!citizen %in% citizenAgg, iso2 != "TOTAL") %>% group_by(iso2, geo, citizen) %>% summarise(sum = sum(values, na.rm =T))  %>% ungroup()
# add population in 2014 column
dd <- cbind(dd, pop = pop[match(dd$iso2, pop$iso2),'values'])


# express asylum demand by country per inhabitant * unit
dd$perU <- (dd$sum / dd$pop) * unit
dd$sq <- round(dd$perU)

# Check Swiss results
dd %>% filter(iso2 == 'CH', perU > 0)

### Compute the square/waffle by iso2 and filter it by squareThreshold
citizen.subset <- as.character(unlist(unique(dd %>% filter(sq > squareThreshold) %>% select(citizen))))

# filter citizen
dd <- filter(dd, citizen %in% citizen.subset)

# Find the maximum of square (use citizen Total )
sumAll <- data %>% filter(citizen == "Total", !iso2 %in% c('EU28', 'TOTAL')) %>% group_by(iso2, geo, citizen) %>% summarise(sum = sum(values, na.rm =T)) %>% ungroup()
# add population in 2014 column
sumAll <- cbind(sumAll, pop = pop[match(sumAll$iso2, pop$iso2),'values'])
sumAll$sq <- round((sumAll$sum / sumAll$pop) * unit)
tmpSum <- as.data.frame(dd %>% group_by(iso2) %>% summarise(tsum = sum(sq, na.rm = T)) %>% ungroup())
sumAll$others <- sumAll$sq - tmpSum[match(sumAll$iso2, tmpSum$iso2), 2]
maxCol <- ceiling(max(sumAll$sq) / w.row)





## Bind others sum to dd
df <- do.call(rbind, by(dd, dd$iso, function(spt) {
	iso <- as.character(unique(spt$iso))
	newrow <- sumAll[sumAll$iso == iso,]
	colnames(newrow)[colnames(newrow)=='sq'] <- 'perU'
	colnames(newrow)[colnames(newrow)=='others'] <- 'sq'
	rbind(spt, newrow)
}))
# drop unsused factors
df$citizen <- factor(df$citizen)


colorV <- structure(swi_rpal[1:nlevels(df$citizen)], names = levels(df$citizen))



### SAVE
save(df, dd, maxCol, data, sumAll, unit, squareThreshold, file = outputfile)


waffleIso <- function(iso = 'CH') {
	stopifnot(length(iso) == 1)

	#topText <- paste0("En", )
	dff <- df %>% filter(iso2 == iso)

	wf <- structure(dff$sq, names = as.character(dff$citizen))

	padding <- maxCol - ceiling(sum(wf) / w.row)

	waffled(wf, rows = w.row, size = 0.7, colors = unname(colorV[match(names(wf), names(colorV))]),
		pad = padding, xlab = paste("1 carré = demandes d'asile pour ce pays par", unit, "habitant")) +
		theme(legend.position = "top", legend.key.size = unit(0.7, "line"), legend.key.height = unit(1,"line"),
		legend.key = element_rect(colour = NA),
		axis.ticks = element_blank(), axis.text = element_blank()) + ggtitle(iso) +
		guides(fill = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(colour = NULL)))
}


## CHECK CH value with http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=migr_asyappctzm&lang=en
	waffleIso('CH')
	ck <- df %>% filter(iso2 == 'CH')
	ckdf <- as.data.frame(ck %>% filter(citizen == "Total"))
	round((ckdf$sum / ckdf$pop) * unit)


	waffleIso('DE')
	ck <- df %>% filter(iso2 == 'DE')
	ckdf <- as.data.frame(ck %>% filter(citizen == "Total"))
	round((ckdf$sum / ckdf$pop) * unit)

	waffleIso('HU')

	waffleIso('IT')
	ck <- df %>% filter(iso2 == 'IT')
	ckdf <- as.data.frame(ck %>% filter(citizen == "Total"))
	round((ckdf$sum / ckdf$pop) * unit)

	waffleIso('UK')
	ck <- df %>% filter(iso2 == 'UK')
	ckdf <- as.data.frame(ck %>% filter(citizen == "Total"))
	round((ckdf$sum / ckdf$pop) * unit)

	waffleIso('ES')
	ck <- df %>% filter(iso2 == 'ES')
	ckdf <- as.data.frame(ck %>% filter(citizen == "Total"))
	round((ckdf$sum / ckdf$pop) * unit)

