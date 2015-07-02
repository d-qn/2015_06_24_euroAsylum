###### This download the very large monthly data with country of origin!

library(dplyr)
library(ggplot2)
library(swiTheme)
require(gtable)
library(animation)

inputData <- "data/02_citizenMonthly_waffled.Rdata"
load(inputData)

tradFile <- "prod/waffle"

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
    gg <- gg + theme(plot.margin = unit(c(5, 0, 0, 0), "lines"))
    # gg <- gg + theme(plot.margin = rep(unit(0, "null"), 4))
    gg
}


############################################################################################
###
############################################################################################

range(data$time)
tmp <- as.data.frame(df %>% filter(iso2 != 'EU28') %>% group_by(iso2) %>% summarise(totsq = sum(sq)) %>% filter(totsq > 2 | iso2 %in% c('PT', 'ES')))

iso.ordered <- as.character(tmp$iso2[order(tmp$totsq)])

############################################################################################
###		Plot
############################################################################################

###	waffle settings
font <- "Open Sans"
# how many rows in waffle
w.row <- 10


colorV <- structure(swi_rpal[1:nlevels(df$citizen)], names = levels(df$citizen))

iso <- 'HU'
waffleIso <- function(iso = 'CH') {
	stopifnot(length(iso) == 1)

	dff <- df %>% filter(iso2 == iso)
	topText <- paste0("En 2015", " (jusqu'en mars), ", dff[which(dff$citizen == "Total"),'sum'],
		" demandes d'asile ont été déposées en ", dff$geo[1], ".")
	text2 <- paste0("Si la ",  dff$geo[1], " avait ", unit,  " habitants...", "\n", "Il y aurait eu ",
		dff[which(dff$citizen == "Total"),'perU'], " demandes d'asile provenant de:")

	wf <- structure(dff$sq, names = as.character(dff$citizen))

	padding <- maxCol - ceiling(sum(wf) / w.row)

	gw <- waffled(wf, rows = w.row, size = 0.7, colors = unname(colorV[match(names(wf), names(colorV))]),
		pad = padding, xlab = NULL) +
		theme(legend.position = "bottom", legend.key.size = unit(0.7, "line"), legend.key.height = unit(1,"line"),
		legend.key = element_rect(colour = NA),
		axis.ticks = element_blank(), axis.text = element_blank(), text = element_text(size = 12, family = font)) +
		guides(fill = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(colour = NULL)))
		# TITLE

	gw2 <- gw + geom_text(data = data.frame(x = 0, y = w.row + 3, label = topText), aes(x = x, y = y, label = label),
		family = font, alpha = 1, size = 4, hjust = 0, vjust = 0)

	gw2 <- gw2 + geom_text(data = data.frame(x = 0, y = w.row + 1, label = text2), aes(x = x, y = y, label = label),
		family = font, fontface = "bold", alpha = 1, size = 4, hjust = 0, vjust = 0)

	## Turn off clipping and print !
	gw3 <- ggplot_gtable(ggplot_build(gw2))
	gw3$layout$clip[gw3$layout$name == "panel"] <- "off"
	grid.newpage()
	grid.draw(gw3)
}

saveGIF({
	for(iso in iso.ordered) {
		print(waffleIso(iso))
	}
}, movie.name = "EU_asylum_waffle.gif", interval = 5, nmax = 50, ani.width = 640, ani.height = 570, loop = TRUE, outdir = getwd())




