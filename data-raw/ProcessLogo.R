# Create Sticker
#
# Erik.Leppo@tetratech.com
# 2021-04-06
#
# https://github.com/GuangchuangYu/hexSticker
# https://usethis.r-lib.org/reference/use_logo.html
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# image
img <- file.path(file.path("data-raw", "RBP2_waterdrop.png"))

# Create directory if it doesn't exist
dir_logo <- file.path("man", "figures")
ifelse(dir.exists(dir_logo) == FALSE
       , dir.create(dir_logo)
       , "Directory already exists")

# sticker
hexSticker::sticker(img
                    , package = "BioMonTools"
                    , p_size = 20
                    , p_color = "black"
                    , s_x = 1
                    , s_y = .75
                    , s_width = .35
                    , h_fill = "white"
                    , h_color = "blue"
                    , filename = "man/figures/logo.png")

# then add to README
# <img src="man/figures/logo.png" align="right" height="139" />
# then reknit README
