################################
## Extract hex triplet colours
################################

# The aim of this script is to extract the RGB colour profile as hex triplet 
# from the erytrhistic Natrix from Cambridge. 

# load libraries
library(magick)
library(ggplot2)

## Set of helper functions to extract hex triplets 

# Function to interactively read off pixel coordinates by clicking on the image.
# It returns a data.frame with the pixel coordinates of the points you clicked.
pick_pixel_coords <- function(img, n = 20) {
  info <- image_info(img)
  w <- info$width; h <- info$height
  
  op <- par(mar = c(0, 0, 0, 0))
  on.exit(par(op))
  plot(0, 0, type = "n", xlim = c(0, w), ylim = c(h, 0),
       xaxs = "i", yaxs = "i", asp = 1, xlab = "", ylab = "", axes = FALSE)
  rasterImage(as.raster(img), xleft = 0, ybottom = h, xright = w, ytop = 0)
  
  message("Click up to ", n, " points on the image, one at a time -- each ",
          "cross and its number appear as soon as you click. Right-click ",
          "(or press Esc / Finish) to stop early.")
  
  xs <- numeric(0)
  ys <- numeric(0)
  for (i in seq_len(n)) {
    pt <- locator(1)
    if (is.null(pt)) break  # to esc early
    
    xs <- c(xs, pt$x)
    ys <- c(ys, pt$y)
    
    # add points
    points(pt$x, pt$y, pch = 0, cex = 2, col = "green", lwd = 2)
    text(pt$x, pt$y, labels = i, pos = 4, offset = 0.6,
         col = "green", cex = 1.2, font = 2)
  }
  
  # return data frame with pixel coordinates
  data.frame(point = seq_along(xs), x = round(xs), y = round(ys))
}


# this function extracts the mean hex colour of a square patch
# around a point in an image. it reads the image as a magick-image object,
# crops a square patch of specified size (default 5x5, i.e. patch_size = 5)
# around the given pixel coordinates, and computes the mean RGB values of
# that patch. It then converts the mean RGB values to a hex
# triplet and returns a data frame with the label,
# coordinates, mean RGB values, and hex code.
sample_patch_hex <- function(img, x, y, patch_size = 5, label = NA) {
  info <- image_info(img)
  half <- floor(patch_size / 2)

  x0 <- max(0, x - half)
  y0 <- max(0, y - half)
  w  <- min(patch_size, info$width  - x0)
  h  <- min(patch_size, info$height - y0)

  geom  <- sprintf("%dx%d+%d+%d", w, h, x0, y0)
  patch <- image_crop(img, geometry = geom)
  arr <- as.integer(image_data(patch, channels = "rgb"))
  dim(arr) <- c(h, w, 3)  # height, width, channels

  mean_rgb <- round(apply(arr, 3, mean))
  hex <- rgb(mean_rgb[1], mean_rgb[2], mean_rgb[3], maxColorValue = 255)

  data.frame(
    label = label, x = x, y = y,
    R = mean_rgb[1], G = mean_rgb[2], B = mean_rgb[3],
    hex = hex,
    stringsAsFactors = FALSE
  )
}


## Execution 

# Read in the photo of the head of the erytristic Natrix.
natrix_ery <- image_read("data/raw/natrix_helvetica_ery_head.jpg")
# check fail loudly instead of silently if desaturating
stopifnot(image_info(natrix_ery)$colorspace != "Gray")

# get pixel coordinates from image
coords_ery <- pick_pixel_coords(natrix_ery)
# x, y in pixel-coordinate sorted by click order
print(coords_ery)

# save coords
saveRDS(coords_ery, "data/intermediate/coords_natrix_ery_07092026.rds")

# save marked picture
dev.copy(
  jpeg,
  filename = "data/intermediate/markers_natrix_ery_070926.jpeg",
  width = 2000,
  height = 2000,
  res = 300
)
dev.off()

# create column for labels
regions <- c("rostral", "nasal", "loreal", "preocular", "postocular",
             "upper_labial", "lower_labial", "collar", "dorsal",
             "ventral"
             )

# add regions to coords
coords_ery$label <- regions

# sample hex from the erytristic Natrix
hex_values <- do.call(rbind, lapply(seq_len(nrow(coords_ery)), function(i) {
  sample_patch_hex(natrix_ery, x = coords_ery$x[i], y = coords_ery$y[i],
                    label = coords_ery$label[i])
}))

# save hex values from the erytristic Natrix
write.csv(hex_values, "results/hex_values_natrix_ery_070926.csv",
          row.names = FALSE)
