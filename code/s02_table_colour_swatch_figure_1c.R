#######################
## Hex colour table
#######################

# The aim of this script is to build a visual table showing the hex triplet
# and a colour swatch for each body region sampled from the
# erythristic Natrix, alongside the values reported by Jablonski et al. (2022)
# for three other erythristic N. natrix.

# load libraries
library(ggplot2)


# load the hex values for the sampled individual
own <- read.csv("results/hex_values_natrix_ery_070926.csv",
                stringsAsFactors = FALSE)

# Jablonski et al. (2022) hex values, transcribed from the text
jablonski2022 <- data.frame(
  individual = c(rep("Slovakia", 5),
                 rep("Romania (Danube Delta)", 4),
                 rep("Greece (Nestani)", 3)),
  region = c("head part", "head part (alt.)", "eye pigment (peri-ocular)",
             "ventral scales", "dorsal scales (lateral)",
             "rostrum (lower part)", "labials", "crescents",
             "lateral spots (lower)", "rostrum", "labials", "crescents"),
  hex = c("#fe9900", "#fead30", "#dc6205", "#d78754", "#cd711e",
          "#d27534", "#ee9937", "#eb6200", "#ef870a",
          "#a25624", "#dd8022", "#df7937"),
  stringsAsFactors = FALSE
)


# tidy region names of the sampled individual
own_tab <- data.frame(source = "This study",
                      region = gsub("_", " ", own$label),
                      hex = toupper(own$hex))

# same fot Jablonski et al. (2022)
jab_tab <- data.frame(source = paste0("Jablonski et al. 2022\n",
                                      jablonski2022$individual),
                      region = jablonski2022$region,
                      hex = toupper(jablonski2022$hex))

# bind the two tables together
colour_tab <- rbind(own_tab, jab_tab)

# keep sources and regions in the input order (top to bottom)
colour_tab$source <- factor(colour_tab$source,
                            levels = unique(colour_tab$source))
colour_tab$row_id <- factor(seq_len(nrow(colour_tab)),
                            levels = rev(seq_len(nrow(colour_tab))))

# plot table
# x position of each column of the table
col_x <- c(region = 1, swatch = 3.1, hex = 4.2)

# long format for the text columns
text_df <- rbind(
  data.frame(colour_tab[, c("source", "row_id")], x = unname(col_x["region"]),
             label = colour_tab$region, hjust = 0),
  data.frame(colour_tab[, c("source", "row_id")], x = unname(col_x["hex"]),
             label = colour_tab$hex, hjust = 0.5)
)

p <- ggplot(colour_tab, aes(y = row_id)) +
  # colour swatch
  geom_tile(aes(x = unname(col_x["swatch"]), fill = hex),
            width = 0.9, height = 0.85, colour = "grey30", linewidth = 0.2) +
  scale_fill_identity() +
  # text columns
  geom_text(data = text_df, aes(x = x, label = label, hjust = hjust),
            size = 3.2) +
  # column headers
  scale_x_continuous(breaks = col_x,
                     labels = c("Region", "Colour", "Hex"),
                     position = "top",
                     limits = c(0.9, 4.8), expand = c(0, 0)) +
  # one panel per source
  facet_grid(source ~ ., scales = "free_y", space = "free_y", switch = "y") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x.top = element_text(face = "bold", colour = "black"),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0, hjust = 1, face = "bold"),
        panel.spacing.y = unit(0.6, "lines"),
        panel.border = element_rect(fill = NA, colour = "grey80"))

# save file
ggsave("results/table_colour_swatch_figure_1c.pdf",
       p, width = 6, height = 8, dpi = 300, bg = "white")