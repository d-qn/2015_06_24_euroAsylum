###### This download the very large monthly data with country of origin!

library(dplyr)
library(ggplot2)
library(swiTheme)
require(gtable)
library(animation)
library(png)


inputData <- "../data/02_citizenMonthly_waffled_new.Rdata"
load(inputData)

tradFile <- "../trad/02_translation_09.csv"
trad <- read.csv(tradFile, check.names = F, stringsAsFactors = F, row.names = 1)

# load SWI logo
swiLogo <- readPNG("~/swissinfo/_helpers/SWI-RGB.png")

############################################################################################
###		HELPER
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
	##### My change 1 , added factor and levels being of length "parts" (even if some parts have 0 square) !!!!!!!!!!!!
    dat$value <- factor(c(parts_vec, rep(NA, nrow(dat) - length(parts_vec))), levels = LETTERS[2:(length(parts)+1)])
    ##### Changes 2, do not pass data in ggplot but to geom_tile
	if (flip) {
		gg <- ggplot() + geom_tile(data = dat, aes(x = y, y = x, fill = value), color = "white", size = size)
    }
    else {
		gg <- ggplot() + geom_tile(data = dat, aes(x = x, y = y, fill = value), color = "white", size = size)
    }
    #gg <- gg + geom_tile(color = "white", size = size)
    gg <- gg + labs(x = xlab, y = NULL, title = title)
    gg <- gg + scale_x_continuous(expand = c(0, 0))
    gg <- gg + scale_y_continuous(expand = c(0, 0))
    ###### 3 Added this as well, not sure it is useful: drop = FALSE !!!!!!!!!!!!
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
	## Change 4 make space on the top and comment the other line
    gg <- gg + theme(plot.margin = unit(c(2.8, 0, 0.5, 0), "lines"))
    # gg <- gg + theme(plot.margin = rep(unit(0, "null"), 4))
    gg
}


############################################################################################
### subset the whole dataset and save the production data.frame
############################################################################################

range(data$time)
tmp <- as.data.frame(df %>% filter(iso2 != 'EU28') %>% group_by(iso2) %>%
	summarise(totsq = sum(sq)) %>% filter(totsq > 2 | iso2 %in% c('PT', 'ES')) %>%
	filter(!iso2 %in% c('LI', 'IS', 'CZ', 'HR', 'LT', 'LV', 'SI', 'SK', 'BG', 'EE', 'LU', 'PT', 'BE', 'FI', 'IE', 'NO', 'NL', 'DK', 'PL')))

iso.ordered <- as.character(tmp$iso2[order(tmp$totsq)])

## Create the translation data.frame
t.df <- df %>% filter(iso2 %in% iso.ordered) %>% select(iso2, geo, citizen)

t.df <- rbind(unique(t.df[,1:2]), data.frame(iso2 = paste0("cit.", substr(unique(t.df$citizen), 1, 4)), geo = unique(t.df$citizen)))
colnames(t.df) <- c('code', 'en')
write.csv(t.df, file = "../trad/02_translation_tmp.csv", row.names = F)


### Filter out iso.ordered
df <- df %>% filter(iso2 %in% iso.ordered)

### Add the citizen code to data.frame
df <- cbind(df, cit.code = factor(t.df[match(df$citizen, t.df$en), 'code']))
rownames(df) <- NULL


### Define colors
colorV <- structure(swi_rpal[1:nlevels(df$cit.code)], names = levels(df$cit.code))
colorV['cit.Tota'] <- 'darkgrey'

write.csv(df, file = "../data/02_inputData.csv")

############################################################################################
###		Plot
############################################################################################


####### PLOT SETTINGS #######
###	waffle settings
# how many rows in waffle
w.row <- 20
w.size <- 1

legendKeySize <- unit(1, "line")
legendTextSize <- 12
legendKeyHeight <- unit(2,"line")
animationInterval <- 3.2
countryText <- 12
descrText <- 6

fig.width <- 800
fig.height <- 600

maxCol <- ceiling(max(data.frame(df %>% group_by(iso2) %>% summarise(totSq = sum(sq)))$totSq) / w.row)

iso <- 'HU' ## debugging

####### helper functions #######

waffleIso <- function(iso = 'CH', iDf = df.l, trad, lang, font = "Open Sans") {
	#browser()
	stopifnot(length(iso) == 1)
	if(lang == 'ru') descrText <- 5


	dff <- iDf %>% filter(iso2 == iso)
	dfff <- dff %>% filter(sq > 0)

	wf <- structure(dfff$sq, names = as.character(dfff$CIT))

	countryTop <- geom_text(data = data.frame(x = 0, y = w.row + 3, label = dff$GEO[1]), aes(x = x, y = y, label = label),
				family = font, fontface = "bold", alpha = 1, size = countryText, hjust = 0, vjust = 0, colour = "#aa8959")
	topText <- paste0(dff[which(dff$citizen == "Total"),'sum'], " ", trad["title.slide", lang])
	titleTop <- geom_text(data = data.frame(x = 0, y = w.row + 2, label = topText), aes(x = x, y = y, label = label),
				family = font, alpha = 1, size = descrText, hjust = 0, vjust = 0)
	text2 <- paste0(trad["subtitle1.slide", lang], " ", unit,  " ", trad["subtitle2.slide", lang], " ",
		dff[which(dff$citizen == "Total"),'perU'], " ", trad["subtitle3.slide", lang])
	subtitle <- geom_text(data = data.frame(x = 0, y = w.row + 1, label = text2), aes(x = x, y = y, label = label),
				family = font, alpha = 1, size = descrText, hjust = 0, vjust = 0)

	# Hack: set a blank waffle and display top text
	blankW <- waffled(rep(round(maxCol * w.row / length(wf)), length(wf)), rows = w.row, size = w.size, colors = rep("white", length(wf))) +
		theme(legend.position = "bottom", legend.key.size = legendKeySize, legend.key.height = legendKeyHeight,
		legend.key = element_rect(colour = NA), legend.text = element_text(colour = "white", size = legendTextSize, family = font)) +
		guides(fill = guide_legend(nrow = 2, byrow = TRUE,
		override.aes = list(colour = NULL)))
	textOnly <- blankW + countryTop +titleTop + subtitle

	textOnly2 <- ggplot_gtable(ggplot_build(textOnly))
	textOnly2$layout$clip[textOnly2$layout$name == "panel"] <- "off"
	grid.newpage()
	grid.draw(textOnly2)

	# Waffle chart
	padding <- maxCol - ceiling(sum(wf) / w.row)
	gw <- waffled(wf, rows = w.row, size = w.size, colors = unname(colorV[match(as.character(iDf[match(names(wf), iDf$CIT),'cit.code']), names(colorV))]),
		pad = padding, xlab = NULL) +
		theme(legend.position = "bottom", legend.key.size = legendKeySize, legend.key.height = legendKeyHeight,
		legend.key = element_rect(colour = NA), legend.text = element_text(size = legendTextSize, family = font),
		axis.ticks = element_blank(), axis.text = element_blank(), text = element_text(size = 14, family = font)) +
		guides(fill = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(colour = NULL)))
		# TITLE

	gw2 <- gw + countryTop + titleTop + subtitle

	## Turn off clipping and print !
	gw3 <- ggplot_gtable(ggplot_build(gw2))
	gw3$layout$clip[gw3$layout$name == "panel"] <- "off"
	grid.newpage()
	grid.draw(gw3)
	grid.newpage()
	grid.draw(gw3)
}

introText <- function(title, title2, subtitle, img = swiLogo, font = "Open Sans") {
	par(mar = c(0,0,0,0))
	plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
	text(x = 0.01, y = 0.95, title, cex = 2.5, col = "black", family = font, font = 2, adj = 0)
	text(x = 0.01, y = 0.88, title2, cex = 2.5, col = "black", family = font, font = 2, adj = 0)
	text(x = 0.01, y = 0.5, subtitle, cex = 2, col = "gray30", family = font, font = 1, adj = 0)
	rasterImage(img, .85, 0.03, 0.96, 0)
}

outroText <- function(source = "source: Eurostat",
	method = "Seuls les mois avec des données pour tous les pays européens ont été considérés.",
	method2 = "Inspiré par: Le Monde - les Décodeurs",
	lastUpdate = paste0("Dernière mise à jour: ", Sys.Date()), author = "Duc-Quang Nguyen (@duc_qn)", img = swiLogo, font = "Open Sans") {

	par(mar = c(0,0,0,0))
	plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
	text(x = 0.01, y = 0.95, source, cex = 1.8, col = "black", family = font, font = 3, adj = 0)
	text(x = 0.01, y = 0.88, method, cex = 1.2, col = "black", family = font, font = 3, adj = 0)
	text(x = 0.01, y = 0.8, method2, cex = 1.2, col = "black", family = font, font = 3, adj = 0)
	text(x = 0.01, y = 0.2, lastUpdate, cex = 1.3, col = "gray30", family = font, font = 1, adj = 0)
	text(x = 0.01, y = 0.15, author, cex = 1.3, col = "gray30", family = font, font = 1, adj = 0)
	rasterImage(img, .85, 0.03, 0.96, 0)
}


############################################################################################
###		Loop by language to create the GIF
############################################################################################

test <- F

languages <- if(test) 'en' else colnames(trad)

for(lang in languages) {

	df.l <- cbind(df, GEO = trad[match(df$iso2, rownames(trad)), lang],
		CIT = paste0(" ", trad[match(df$cit.code, rownames(trad)), lang], "  ") )

	## define font
	font <- "Open Sans"
	if(lang %in% c('ch', 'zh', 'cn')) font <- "Kai"
	if(lang %in% c('jp', 'ja')) font <- "Osaka"

	if(test){output <- "test.gif"
	}  else {
		output <- paste0("02b_EU_asylum_waffle_", lang ,"_teaser.gif")

		saveGIF({
			introText(title = trad["title.main", lang], trad["title2.main", lang], trad["title3.main", lang], font = font)
			if(test) {
				wp <- waffleIso('CH', df.l, trad, lang, font = font)
				print(wp)
				wp <- waffleIso('HU', df.l, trad, lang, font = font)
				print(wp)
			} else {
				for(iso in iso.ordered) {
					wp <- waffleIso(iso, df.l, trad, lang, font = font)
					print(wp)
				}
				outroText(source = trad["credit.source", lang], method = trad["credit.method1", lang], method2 = trad["credit.method2", lang], lastUpdate = paste0(trad["credit.lastUpdate", lang],  Sys.Date()),font = font)
				outroText(source = trad["credit.source", lang], method = trad["credit.method1", lang], method2 = trad["credit.method2", lang], lastUpdate = paste0(trad["credit.lastUpdate", lang],  Sys.Date()),font = font)
			}
		}, movie.name = output, interval = 1.1, nmax = 50, ani.width = fig.width, ani.height = fig.height, loop = TRUE, outdir = "prod")

		output <- paste0("02_EU_asylum_waffle_", lang ,".gif")
	}

	saveGIF({
		introText(title = trad["title.main", lang], trad["title2.main", lang], trad["title3.main", lang], font = font)
		if(test) {
			wp <- waffleIso('CH', df.l, trad, lang, font = font)
			print(wp)
			wp <- waffleIso('HU', df.l, trad, lang, font = font)
			print(wp)
		} else {
			for(iso in iso.ordered) {
				wp <- waffleIso(iso, df.l, trad, lang, font = font)
				print(wp)
			}
			outroText(source = trad["credit.source", lang], method = trad["credit.method1", lang], method2 = trad["credit.method2", lang], lastUpdate = paste0(trad["credit.lastUpdate", lang],  Sys.Date()),font = font)
			outroText(source = trad["credit.source", lang], method = trad["credit.method1", lang], method2 = trad["credit.method2", lang], lastUpdate = paste0(trad["credit.lastUpdate", lang],  Sys.Date()),font = font)
		}
	}, movie.name = output, interval = animationInterval, nmax = 50, ani.width = fig.width, ani.height = fig.height, loop = TRUE, outdir = "prod")
}


