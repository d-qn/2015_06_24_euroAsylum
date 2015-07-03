###### This download the very large monthly data with country of origin!

library(dplyr)
library(ggplot2)
library(swiTheme)
require(gtable)
library(animation)
library(png)


inputData <- "data/02_citizenMonthly_waffled.Rdata"
load(inputData)

tradFile <- "prod/02_translation.csv"
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
	filter(!iso2 %in% c('LI', 'IS', 'CZ', 'HR', 'LT', 'LV', 'SI', 'SK', 'EE', 'BG', 'LU', 'PT', 'BE', 'FI', 'IE')))

iso.ordered <- as.character(tmp$iso2[order(tmp$totsq)])

## Create the translation data.frame
t.df <- df %>% filter(iso2 %in% iso.ordered) %>% select(iso2, geo, citizen)

t.df <- rbind(unique(t.df[,1:2]), data.frame(iso2 = paste0("cit.", substr(unique(t.df$citizen), 1, 4)), geo = unique(t.df$citizen)))
colnames(t.df) <- c('code', 'en')
write.csv(t.df, file = "prod/02_translation_tmp.csv", row.names = F)


### Add the citizen code to data.frame
df <- cbind(df, cit.code = t.df[match(df$citizen, t.df$en), 'code'])
rownames(df) <- NULL

write.csv(df, file = "prod/02_inputData.csv")

############################################################################################
###		Plot
############################################################################################


####### PLOT SETTINGS #######
###	waffle settings
font <- "Open Sans"
# how many rows in waffle
w.row <- 10
w.size <- 3.7

legendKeySize <- unit(2, "line")
legendKeyHeight <- unit(1.6,"line")
animationInterval <- 4.2
colorV <- structure(swi_rpal[1:nlevels(df$citizen)], names = levels(df$citizen))

iso <- 'HU' ## debugging

####### helper functions #######

waffleIso <- function(iso = 'CH', df = df) {
	stopifnot(length(iso) == 1)

	dff <- df %>% filter(iso2 == iso)
	dfff <- dff %>% filter(sq > 0)
	wf <- structure(dfff$sq, names = as.character(dfff$citizen))

	countryTop <- geom_text(data = data.frame(x = 0, y = w.row + 2.1, label = dff$geo[1]), aes(x = x, y = y, label = label),
				family = font, fontface = "bold", alpha = 1, size = 11, hjust = 0, vjust = 0, colour = "#aa8959")
	topText <- paste0(dff[which(dff$citizen == "Total"),'sum'], " ",
		" demandes d'asile y ont été déposées en 2015 jusqu'à mars.")
	titleTop <- geom_text(data = data.frame(x = 0, y = w.row + 1.45, label = topText), aes(x = x, y = y, label = label),
				family = font, alpha = 1, size = 5, hjust = 0, vjust = 0)
	text2 <- paste0("Si ce pays avait", " ", unit,  " ", "habitants, il y aurait eu", " ",
		dff[which(dff$citizen == "Total"),'perU'], " ", "demandes d'asile provenant de:")
	subtitle <- geom_text(data = data.frame(x = 0, y = w.row + 0.85, label = text2), aes(x = x, y = y, label = label),
				family = font, alpha = 1, size = 5, hjust = 0, vjust = 0)

	# Hack: set a blank waffle and display top text
	blankW <- waffled(rep(round(17 * w.row / length(wf)), length(wf)), rows = w.row, size = w.size, colors = rep("white", length(wf))) +
		theme(legend.position = "bottom", legend.key.size = legendKeySize, legend.key.height = legendKeyHeight,
		legend.key = element_rect(colour = NA), legend.text = element_text(colour = "white")) +
		guides(fill = guide_legend(nrow = 2, byrow = TRUE,
		override.aes = list(colour = NULL)))
	textOnly <- blankW + countryTop +titleTop + subtitle

	textOnly2 <- ggplot_gtable(ggplot_build(textOnly))
	textOnly2$layout$clip[textOnly2$layout$name == "panel"] <- "off"
	grid.newpage()
	grid.draw(textOnly2)

	# Waffle chart
	padding <- maxCol - ceiling(sum(wf) / w.row)
	gw <- waffled(wf, rows = w.row, size = w.size, colors = unname(colorV[match(names(wf), names(colorV))]),
		pad = padding, xlab = NULL) +
		theme(legend.position = "bottom", legend.key.size = legendKeySize, legend.key.height = legendKeyHeight,
		legend.key = element_rect(colour = NA),
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

introText <- function(title, title2, subtitle, img = swiLogo) {
	par(mar = c(0,0,0,0))
	plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
	text(x = 0.01, y = 0.95, title, cex = 2.3, col = "black", family = font, font = 2, adj = 0)
	text(x = 0.01, y = 0.88, title2, cex = 2.3, col = "black", family = font, font = 2, adj = 0)
	text(x = 0.01, y = 0.5, subtitle, cex = 2.3, col = "gray30", family = font, font = 1, adj = 0)
	rasterImage(img, .85, 0.03, 0.96, 0)
}




############################################################################################
###		Loop by language to create the GIF
############################################################################################

test <- T

languages <- if(test) 'fr' else colnames(trad)


for(lang in languages) {

	if(test){output <- "test.gif"
	}  else {
		output <- paste0("02_EU_asylum_waffle_", lang ,".gif")
	}

	saveGIF({
		introText(title = "Demandes d'asile en Europe", "En 2015, jusqu'à présent", "Si tous les pays européens avait 50000 habitants...")
		if(test) {
			wp <- waffleIso('CH', df.l)
			print(wp)
			wp <- waffleIso('HU', df.l)
			print(wp)
		} else {
			for(iso in iso.ordered) {
				wp <- waffleIso(iso, df.l)
				print(wp)
			}
		}
	}, movie.name = output, interval = animationInterval, nmax = 50, ani.width = 800, ani.height = 600, loop = TRUE, outdir = "prod")
}










