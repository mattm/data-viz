library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)

# Several shared values
LINE_COLOR = "#000000"
AXIS_TITLE_FONT_SIZE = 15
AXIS_TITLE_COLOR = "#000000"
TEXT_GROB_X_OFFSET = 0.008

data <- read.csv("data.csv") %>%
	mutate(
		quarter = as.Date(quarter),
		revenue = revenue / 1000000
	)

chart <- ggplot(data, aes(x = quarter, y = revenue)) +
	geom_bar(
		stat = "identity",
		fill = "#f6cb45",
		width = 60,
		position = position_nudge((x = 46))
	) +
	theme(
		panel.background = element_blank(),
		axis.title.x = element_text(size = AXIS_TITLE_FONT_SIZE, margin = margin(15, 0, 0, 0), color = AXIS_TITLE_COLOR),
		axis.title.y = element_blank(),
		axis.text = element_text(size = 16, color = "#666666", family = "Proxima Nova"),

		# The hjust value is a hacky way to mostly center the text between the ticks
		# It needs to adjusted if you change the width of the chart. Also, the text
		# isn't actually centered and some axis text may be misaligned because they
		# won't all be the same width depending on the font face
		axis.text.x = element_text(hjust = -1.1, margin = margin(20, 0, 0, 0)),

		axis.text.y = element_text(margin = margin(0, 15, 0, 0)),
		axis.line.x = element_line(color = LINE_COLOR),
		axis.ticks.x = element_line(color = LINE_COLOR),

		# The transparent value for the first value causes the tick for $0 to disappear
		# otherwise it would overlap the x-axis
		axis.ticks.y = element_line(color = c("transparent", LINE_COLOR, LINE_COLOR)),

		# The negative length causes the tick to extend upward instead of downward
		axis.ticks.length = unit(-0.3, "cm")
	) +
	labs(
		x = "QUARTERLY"
	) +
	scale_y_continuous(
		breaks = c(0, 25, 50),
		labels = c(0, 25, "$50"),
		limits = c(0, 50),
		expand = c(0, 0)
	) +
	scale_x_date(
		date_labels = "%Y"
	)

title <- textGrob("GLOBAL REVENUE",
	gp = gpar(fontsize = 26, fontface = "bold", fontfamily = "PT Sans Narrow"),
	x = unit(TEXT_GROB_X_OFFSET, "npc"),
	just = c("left", "bottom"))

y_axis_title <- textGrob("IN MILLIONS",
	gp = gpar(fontsize = AXIS_TITLE_FONT_SIZE, col = AXIS_TITLE_COLOR, fontfamily = "Proxima Nova"),
	x = unit(TEXT_GROB_X_OFFSET, "npc"),
	just = c("left", "bottom"))

source <- textGrob("SOURCE: COMPANY RESEARCH",
	gp = gpar(fontsize = 14, col = "#444444", fontfamily = "Proxima Nova"),
	x = unit(TEXT_GROB_X_OFFSET, "npc"),
	just = c("left", "bottom"))

grid.arrange(title, y_axis_title, chart, source, ncol = 1, heights = c(0.1, 0.03, 0.55, 0.05))

# We use dev.copy so we can still preview the chart in RStudio while working
dev.copy(png, "chart.png", width = 550, height = 400)
dev.off()
