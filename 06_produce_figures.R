library(tidyverse)
library(ggrepel)
library(readxl)
library(staplr)

# Produce plots -----------------------------------------------------------

descriptors_with_anchors <- readRDS("02_workspace/descriptors_with_anchors.RData")
descriptors_witout_anchors <- readRDS("02_workspace/descriptors_witout_anchors.RData")



# Get descriptors with anchor descriptions --------------------------------

descriptors_with_anchors <- descriptors_with_anchors %>% 
  
  select(id_title, element_name_de, description_de, 
    anchor_val = `Anchor Value`,
    anchor_text = `Anchor Description German`) %>% 
  
  mutate(anchors = "yes")



descriptors_witout_anchors <- descriptors_witout_anchors %>% 
  
  select(id_title, element_name_de, description_de) %>% 
  
  mutate(anchor_val = NA_real_, anchor_text = NA_character_, anchors = "no")


descriptors <- bind_rows(descriptors_with_anchors, descriptors_witout_anchors) %>% 
  select(id_title, anchors, element_name_de, description_de, anchor_val, anchor_text)


rm(descriptors_with_anchors, descriptors_witout_anchors)


# Rescaling ---------------------------------------------------------------

# Rescale anchor values from "0 to 7" to "0 to 5", and rounding UP to the next whole number.

descriptors <- descriptors %>% 
  
  mutate(anchor_val_5 = ( (anchor_val - 0) / (7 - 0) ) * 5,
         anchor_val_5_r = ceiling(anchor_val_5))



# We identify those element names by id_title that have identical anchor values after rounding (n = 2)
# and remove the observation with the lower anchor_val_5

descriptors <- descriptors %>% 
  group_by(id_title) %>% 
  arrange(desc(anchor_val_5)) %>% 
  mutate(duplicated_anchor_val = as.integer(duplicated(anchor_val_5_r))) %>% 
  ungroup() %>% 
  filter(duplicated_anchor_val != 1) %>% 
  select(-duplicated_anchor_val)


# Format text -------------------------------------------------------------

# We break the descriptor and anchor text into lines according to specific predefined widths

descriptor_width = 80
anchor_width = 18

descriptors <- mutate(descriptors, 
                      
                      element_name_de = str_trim(element_name_de),
                      
                      
                      # Anchor descriptor width
                      
                      anchor_text = str_trim(anchor_text),
                      anchor_text = map_chr(anchor_text, ~str_c(strwrap(., width = anchor_width), collapse = "\n")),
                      
                      
                      # Descriptor width
                      
                      description_de = str_trim(description_de),
                      description_de = map_chr(description_de, ~str_c(strwrap(., width = descriptor_width), collapse = "\n")),
                      description_de = str_c(description_de, "\n")
                      
)



# Count characters and lines and adjust and define picture dimensions --------

# Define string length of element names and descriptors and use the length of the longer one
# Define proportion of each it title length compared with the longest object


# -> This is to define figure width

descriptors <- mutate(descriptors, 
                      
                      elem_nm_wid = str_split(element_name_de, "\n") %>% map(str_count) %>% map_int(max),
                      descr_wid = str_split(description_de, "\n") %>% map(str_count) %>% map_int(max),
                      max_wid_txt = if_else(descr_wid >= elem_nm_wid, descr_wid, elem_nm_wid),
                      prop_wid_txt = max_wid_txt / max(max_wid_txt),
                      prop_wid_txt = if_else(anchors == "yes", 1, prop_wid_txt) #* 
                      #* The figures with anchors will always have the same width to the scale bar
)


# Check number of lines of text of descriptors and element names

# -> This is to define figure height

descriptors <- descriptors %>% 
  
  mutate(
    
    elem_nm_len = str_split(element_name_de, "\n") %>% map_int(length),
    descr_len = str_split(description_de, "\n") %>% map_int(length),
    anchor_len = if_else(anchors == "yes", str_split(anchor_text, "\n") %>% map_int(length), 0L)
    
  ) %>% 
  
  group_by(id_title) %>% 
  mutate(anchor_len = max(anchor_len)) %>% 
  ungroup()


descriptors <- descriptors %>% 
  
  mutate(
    
    tot_len_txt = if_else(anchors == "no", elem_nm_len + descr_len, NA_integer_),
    tot_len_txt_and_anchors = if_else(anchors == "yes", elem_nm_len + descr_len + anchor_len, NA_integer_),
    
    prop_len_txt = if_else(anchors == "no", tot_len_txt / max(tot_len_txt, na.rm = TRUE), NA_real_),
    prop_len_txt_and_anchors = if_else(anchors == "yes", tot_len_txt_and_anchors / max(tot_len_txt_and_anchors, na.rm = TRUE), NA_real_)
    
  )



# descriptors <- select(descriptors, -anchor_val, -anchor_val_5, -c(elem_nm_wid:max_wid_txt), -c(elem_nm_len:tot_len_txt_and_anchors))


# Define default figure dimensions ----------------------------------------

default_pdf_width = 13

no_anchor_pdf_height = 2.9 # Applies only to figures without anchors

anchor_figures_height = 12 # Applies only to figures with anchors

descriptors <- descriptors %>% 
  mutate(pdf_width = if_else(anchors == "yes", default_pdf_width, default_pdf_width * prop_wid_txt)) %>% 
  mutate(pdf_height = if_else(anchors == "yes", anchor_figures_height * prop_len_txt_and_anchors, no_anchor_pdf_height * prop_len_txt))


# descriptors <- select(descriptors, -c(prop_wid_txt:prop_len_txt_and_anchors))

# Get some test items -----------------------------------------------------

# Item without anchors with largest width

if(F) {
  
  descriptors

  max_width_test <- descriptors %>%
    filter(anchors == "no") %>%
    filter(prop_len_txt == max(prop_len_txt)) %>%
    filter(prop_max_lines_n  == max(prop_len_txt_and_anchors))

  min_width <- descriptors %>%
    filter(anchors == "no") %>%
    filter(prop_len_txt == min(prop_len_txt)) %>%
    filter(prop_max_lines_n  == min(prop_max_lines_n))

  select(min_width, id_title, element_name_de, descriptor, prop_len_txt, prop_max_lines_n, pdf_width, pdf_height)

  max_anchor_len <- descriptors %>%
    filter(anchors == "yes") %>%
    filter(prop_max_lines_n == max(prop_max_lines_n)) %>%
    filter(anchor_wid_prop  == max(anchor_wid_prop)) %>%
    slice(1) %>%
    pull(id_title) %>%
    {filter(descriptors, id_title == .)}

  max_anchor_wid <- descriptors %>%
    filter(anchors == "yes") %>%
    filter(anchor_wid_prop  == max(anchor_wid_prop)) %>%
    filter(anchor_len_prop == max(anchor_len_prop)) %>%
    slice(1) %>%
    pull(id_title) %>%
    {filter(descriptors, id_title == .)}

  select(max_anchor_wid, id_title, element_name_de, description_de, prop_len_txt, prop_max_lines_n, pdf_width, pdf_height)

}



# Split file into descriptors ---------------------------------------------

descriptors_l <- split(descriptors, descriptors$id_title)
length(descriptors_l)



# Function to produce plots -----------------------------------------------

img_function <- function(my_scales, my_device, file_path) {
  
  pb$tick()
  

 
  
  # my_scales <- descriptors_l[[1]]
  # my_device <- "pdf"
  # file_path <- file.path("03_output", "definitions_anchors_figures")
  
  # Plots with anchors
  
  if (my_scales$anchors[1] == "no") {
    
    only_title <- ggplot(data = my_scales) + 
      labs(title = str_c(my_scales$element_name_de, ":\n")) +
      theme_void()+
      facet_grid(. ~ description_de) + 
      
      theme(
        strip.text = element_text(size = 9, lineheight = 0.9),
        plot.title = element_text(hjust = 0.5, size = 9, face = "bold")
      )
    
    dir.create(file.path(file_path, my_device), showWarnings = FALSE, recursive = TRUE)
    file_name <- str_c(file.path(file_path, my_device), "/", my_scales$id_title, ".", my_device)
    
    ggsave(file_name, 
           width = my_scales$pdf_width, 
           height = my_scales$pdf_height, 
           units = "cm",
           device = my_device,
           title = "")
    
    
    # Plots without anchors
    
  } else {
    
    
    p <- ggplot(data = my_scales)
    
    dimensions <- p + expand_limits(x = 5.2, y = c(-0.2, 2))
    
    rescaler_enlarge <- 1 / unique(my_scales$prop_len_txt_and_anchors)
    rescaler_shrink <- unique(my_scales$prop_len_txt_and_anchors)
    
    axis <- dimensions +
      
      annotate("segment", x = 0, xend = 5, y = 0, yend = 0, size = 1.3) +  # X-axis
      annotate("segment", x = seq(0, 5, by = 1), xend = seq(0, 5, by = 1), y = -0.05 * rescaler_enlarge, yend = 0.05 * rescaler_enlarge, size = 1.3) + # ticks
      annotate("text", x = seq(0, 5, by = 1), y = -0.15 * rescaler_enlarge, label = seq(0, 5, by = 1), size = 3) # labels
    
    descriptors <- axis + geom_point(aes(x = anchor_val_5_r, y = 0), color = "dodgerblue3", size = 2, shape = 19) +
      
      geom_label_repel(aes(x = anchor_val_5_r, y = 0, label = anchor_text), 
                       color = "dodgerblue3",  size = 3, nudge_y = 1.2, segment.size = 1) +
      
      theme_void()
    
    title_added <- descriptors + facet_grid(. ~ description_de) + 
      labs(title = str_c(my_scales$element_name_de, ":\n")) +
      theme(strip.text = element_text(size = 9, lineheight = 0.9),
            plot.title = element_text(hjust = 0.5, size = 9, face = "bold"))
    
    dir.create(str_c(file_path, my_device, sep = "/"), showWarnings = FALSE)
    file_name <- unique(str_c(file.path(file_path, my_device,  my_scales$id_title), ".", my_device))
    
    ggsave(file_name, 
           width = unique(my_scales$pdf_width), 
           height = unique(my_scales$pdf_height), 
           units = "cm",
           device = my_device,
           title = "")
    
  }
  
}


pb <- progress::progress_bar$new(total = length(descriptors_l))

invisible(map(descriptors_l[seq_along(descriptors_l)], img_function, my_device = "pdf", file_path = file.path("03_output", "definitions_anchors_figures")))

my_dir <- str_c("03_output/definitions_anchors_figures", "pdf", sep = "/")

staple_pdf(input_directory = str_c("03_output", "definitions_anchors_figures", "pdf", sep = "/"),
           output_filepath = str_c("03_output", "definitions_anchors_figures", "pdf", "00_all.pdf", sep = "/"))

my_device ? 



# Save picture dimensions -------------------------------------------------

descriptors %>% 
  select(id_title, pdf_width_cm = pdf_width, pdf_height_cm = pdf_height) %>% 
  mutate(pdf_width_inch = pdf_width_cm / 2.54, pdf_height_inch = pdf_height_cm / 2.54) %>% 
  distinct(.keep_all = TRUE) %>% 
  mutate(id_title = as.numeric(id_title)) %>% 
  arrange(id_title) %>% 
  write_csv2("03_output/definitions_anchors_figures/picture_dimensions.csv")



rm("anchor_figures_height", "anchor_width", "default_pdf_width", 
  "descriptor_width", "descriptors", "descriptors_l", "file_name", 
  "file_path", "img_function", "my_device", "my_scales", "no_anchor_pdf_height", 
  "only_title", "pb")

