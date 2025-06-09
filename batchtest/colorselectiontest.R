library(dplyr)
library(tidyverse)
library(magick)

#https://www.rostrum.blog/posts/2018-11-25-art-of-the-possible/ 
#so fisrt they have us essentially just print the image :)
test_path <-c("624px-Porträtt_Rudolf_II_som_Vertumnus._Guiseppe_Arcimboldo_-_Skoklosters_slott_-_87582.webp",
"13._Red_Blue_and_Yellow_Color_Palette-3893485501.png")

#I switched it up so we could do many photos (lapply applies to all)
test_images <- lapply(test_path, function(path) {
  image_read(path) %>%
  image_scale("x400")
})

# Now they want name the colors (I used https://html-color.codes to find the most common hexidecimal color)
# we could probally use this in the future to establish the base colors o the outside in order to recognize shade
cols_vec <- setNames(
  c("#261c16", "#515047", "#2c5125","#9bac62", "#ad4d42","#e0c36b","#dd967e","#a77320","#e4e6da"),
  c("black",    "blue",    "green","light green","red", "yellow","pink" ,"gold","white")
)
print(cols_vec)

# now here is the complicated part they go in and make it all work

# For each vector element (color) create a square of that color
for (i in seq_along(cols_vec)) {
  fig_name <- paste0(names(cols_vec)[i], "_square")  # create object name
  assign(
    fig_name,  # set name
    image_graph(width = 100, height = 100, res = 300)  # create magick object
  )
  
  par(mar = rep(0, 4))  # set plot margins
  plot.new()  # new graphics frame
  rect(0, 0, 1, 1, col = cols_vec[i], border = cols_vec[i])  # build rectangle
  assign(fig_name, magick::image_crop(get(fig_name), "50x50+10+10")) # crop
  dev.off()  # shut down plotting device
  rm(i, fig_name)  # clear up
}

# Generate names of the coloured square objects
col_square <- paste0(names(cols_vec), "_square")

# Combine magick objects (coloured squares)
simple_cols <- image_append(c(
  get(col_square[1]), get(col_square[2]), get(col_square[3]),
  get(col_square[4]), get(col_square[5]), get(col_square[6]), get(col_square[7]),
  get(col_square[8]),get(col_square[9])
))
print(simple_cols)

results_list <- list()
for (i in seq_along(test_images)) {
  test <- test_images[[i]]
  test_mapped <- image_map(image = test, map = simple_cols)
  
  # Animates oringinal and mapped version but removes flickering issue)
  print(image_animate(c(test, test_mapped), fps = 1))
 
  #IGNORE THIS CODE FOR RN
  #Function to count the colours (adapted from Jeroen Ooms)
  #count_colors <- function(image) {
  #data <- image_data(image) %>%
  #apply(2:3, paste, collapse = "") %>% 
  #as.vector %>% table() %>%  as.data.frame() %>% 
  #setNames(c("hex", "freq"))
  #data$hex <- paste("#", data$hex, sep="")
  #return(data)
  #}
  
# Dataframe of dominant colours 
test_col_freq <- test_mapped %>%
  count_colors() %>%
  left_join(
    enframe(cols_vec) %>% rename(hex = value),
    by = "hex"
  ) %>% 
#making up a nice table
  arrange(desc(freq)) %>% 
  mutate(percent = 100*round((freq/sum(freq)), 3),
         image = paste("Image",i))%>%

  select(
    `Image#` = image,
    `Colour name` = name,
     Hexadecimal=hex,
    `Frequency of colour` = freq,
    `Percent of image` = percent
  )

print(knitr::kable(test_col_freq))#see result
results_list[[i]] <- test_col_freq #Stores results

}
# ok so then I would want to put in in a csv
final_df <- bind_rows(results_list)
write.csv(final_df, "color_results.csv", row.names = FALSE)
