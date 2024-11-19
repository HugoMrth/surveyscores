library(hexSticker)


#### Pour enlever le fond d'une image
# https://www.remove.bg/fr


# Create the sticker
sticker(
  subplot = "inst/image.png",
  filename = "inst/logo.png",  # Output file name
  #img = "C:/Users/MARTHINETEX/Documents/02 - Code/logo.png",     # Path to your background image
  package = "surveyscores",                 # Package name
  p_size = 20,
  p_y = 1.45,
  p_x = 1,# Text size
  s_x = 1,                            # X position of the image
  s_y = 0.87,                         # Y position of the image
  s_width = 0.8,                      # Width of the image
  h_fill = "#FFFFFF",                 # Background color
  h_color = "#343434",                # Hex border color
  p_color = "#343434",                # Package name color
  layout = "straight",                # Text layout
  text_y = 1.2                       # Y position of the text
)
