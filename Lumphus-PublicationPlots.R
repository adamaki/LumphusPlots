# Plots for publication
# Lumpfish husbandry paper
# Adam Brooker 3rd April 2019




# 1. Pen layout and segmentation plot---------------------------

#LOAD LOCATIONS CODING DATA
locations.lookup <- read.xlsx(masterfileloc, sheet = 12, startRow = 1, cols = seq(1,7)) # read in codes from Locations Coding spreadsheet
rownames(locations.lookup) <- locations.lookup$Code

pen.col <- 'black'
pen.size <- 1.5

#create hide and hydrophone location table
pen.sym <- c(x = locations.lookup['7WHNW', 'xmin']+2, y = locations.lookup['7WHNW', 'ymin']+2)
pen.sym <- rbind(pen.sym, c(x = locations.lookup['7WHSE', 'xmin']+2, y = locations.lookup['7WHSE', 'ymin']+2))
pen.sym <- rbind(pen.sym, c(x = locations.lookup['8WHSW', 'xmin']+2, y = locations.lookup['8WHSW', 'ymin']+2))
pen.sym <- rbind(pen.sym, c(x = locations.lookup['8WHNE', 'xmin']+2, y = locations.lookup['8WHNE', 'ymin']+2))
pen.sym <- rbind(pen.sym, c(x = locations.lookup['7FBSE', 'xmin']+1.5, y = locations.lookup['7FBSE', 'ymin']+1.5))
pen.sym <- rbind(pen.sym, c(x = locations.lookup['7FBNW', 'xmin']+1.5, y = locations.lookup['7FBNW', 'ymin']+1.5))
pen.sym <- rbind(pen.sym, c(x = locations.lookup['8FBSW', 'xmin']+1.5, y = locations.lookup['8FBSW', 'ymin']+1.5))
pen.sym <- rbind(pen.sym, c(x = locations.lookup['8FBNE', 'xmin']+1.5, y = locations.lookup['8FBNE', 'ymin']+1.5))
rownames(pen.sym) <- c('7WHNW', '7WHSE', '8WHSW', '8WHNE', '7FBSE', '7FBNW', '8FBSW', '8FBNE')
pen.sym <- as.data.frame(pen.sym)
pen.sym <- rbind(pen.sym, c(40, 15.7), c(66, 40), c(14.3, 14.3), c(40, 40), c(66, 14.3), c(40, 14.23), c(14.3, 40), c(40, 38.6))
pen.sym$type <- as.factor(c(rep('hide', 4), rep('feed block', 4), rep('deep hydrophone', 4), rep('shallow hydrophone', 4)))
pen.sym$type <- factor(pen.sym$type, levels = c('deep hydrophone', 'shallow hydrophone', 'hide', 'feed block'))
rownames(pen.sym) <- c('7WHNW', '7WHSE', '8WHSW', '8WHNE', '7FBSE', '7FBNW', '8FBSW', '8FBNE', 'd1', 'd2', 'd3', 'd4', 's1', 's2', 's3', 's4')
pen.sym <- as.data.frame(pen.sym)

blank <- c(rep(' ', 9)) # blank sequence for no labels on axis tick marks

ggplot() +
  geom_point(aes(x = 15, y = 15), colour = 'white') + #scale_fill_gradientn(colours=plot.col, space = 'Lab', limits = c(0, pingmax), na.value = plot.col[length(plot.col)], name = 'No. pings') +
  theme_classic() + theme(text = element_text(family = 'Times New Roman', size = 20), legend.position = c(0.15, 0.9), axis.title.x = element_text(size = 18, face = 'bold'), axis.title.y = element_text(size = 18, face = 'bold')) +
  scale_x_continuous('x-axis (m)', limits = c(10,70), breaks = seq(10, 70, 1), labels = c('10', blank, '20', blank, '30', blank, '40', blank, '50', blank, '60', blank, '70')) +
  scale_y_continuous('y-axis (m)', limits = c(10,50), breaks = seq(10, 50, 1), labels = c('10', blank, '20', blank, '30', blank, '40', blank, '50')) +
  annotate('rect', xmin = locations.lookup['8CSW', 'xmin'], xmax = locations.lookup['8CSE', 'xmax'], ymin = locations.lookup['8CSW', 'ymin'], ymax = locations.lookup['8CNE', 'ymax'], size = pen.size, colour = pen.col, alpha = 0) +
  annotate('rect', xmin = locations.lookup['7CSW', 'xmin'], xmax = locations.lookup['7CSE', 'xmax'], ymin = locations.lookup['7CSW', 'ymin'], ymax = locations.lookup['7CNE', 'ymax'], size = pen.size, colour = pen.col, alpha = 0) +
  geom_point(data = pen.sym, mapping = aes(x = x, y = y, size = type, colour = type, fill = type, shape = type, stroke = 1)) +
  annotate('text', x = 27.5, y = 27.5, label = 'Pen A', family = 'Times New Roman', size = 6) +
  annotate('text', x = 53.5, y = 27.5, label = 'Pen B', family = 'Times New Roman', size = 6) +
  scale_size_manual(name = '', values = c('hide' = 15, 'feed block' = 5, 'deep hydrophone' = 5, 'shallow hydrophone' = 5)) +
  scale_shape_manual(name = '', values = c('hide' = 21, 'feed block' = 13, 'deep hydrophone' = 22, 'shallow hydrophone' = 24)) +
  scale_colour_manual(name = '', values = c('hide' = 'black', 'feed block' = 'black', 'deep hydrophone' = 'black', 'shallow hydrophone' = 'black')) +
  scale_fill_manual(name = '', values = c('hide' = 'grey70', 'feed block' = 'grey70', 'deep hydrophone' = 'grey35', 'shallow hydrophone' = 'grey35')) +
  guides(size = guide_legend(override.aes = list(size = 5))) +
  annotation_custom(grobTree(linesGrob(x = c(0.305, 0.325), y = c(0.81, 0.9), arrow = arrow(length = unit(5, 'mm')), gp = gpar(lwd = 2.5)))) +
  annotation_custom(grobTree(textGrob('N', x = 0.330, y = 0.93, gp = gpar(fontsize = 18, fontfamily = 'Times New Roman'))))
