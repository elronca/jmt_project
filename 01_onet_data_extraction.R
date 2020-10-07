## Extract O*NET data

library(tidyverse)
library(readxl)

# ---------------------------------------------------- Load and prepare data ---------------------------------------------------------------


if(FALSE) { # Download all files in the O*NET core database, in one convenient ZIP archive
  
  download.file(
    url = "https://www.onetcenter.org/dl_files/database/db_25_0_excel.zip",
    destfile = file.path("data", "onet", "o_net_files.zip"), mode = "wb")
  
}


if(FALSE) { # Unzip the files to folder data
  
  unzip(file.path("data", "onet", "o_net_files.zip"), exdir = file.path("data"))
  
}

# For onet values extraction we only need following datasets


onet_files <- c("Abilities", "Interests", "Knowledge", "Skills", 
                "Work Activities", "Work Context", "Work Styles", 
                "Work Values")

onet_files.xlsx <- str_c(onet_files, ".xlsx")

onet_file_location <- str_c(file.path("data", "onet", "db_25_0_excel", onet_files.xlsx), sep = "/")

names(onet_file_location) <- onet_files %>% str_to_lower() %>% str_replace(" ", "_")


# We load selected datafiles and save them in a list

onet_df <- map_dfr(onet_file_location, read_excel, .id = "dimension") 

names(onet_df) <- onet_df %>% 
  names() %>% 
  str_to_lower() %>% 
  str_replace_all("[:punct:]|[:space:]", "_")


# We select the variables which we need and we remove rows of the variable scale_id which we don't need

onet_df <- onet_df %>% 
  select(dimension, o_net_soc_code, title, element_name, scale_id, data_value, element_id, domain_source) %>% 
  filter(!scale_id %in% c("IH", "CXP", "CTP", "VH"))


# We transform data values into a 0 - 100 scale (according to the values on the onet webpage)

onet_df <- onet_df %>% 
  
  mutate(
    
    value_onet = case_when(
      
      scale_id %in% c("IM", "CX") ~ ((data_value - 1) / (5 - 1)) * 100,
      
      scale_id %in% c("LV") ~ ((data_value - 0) / (7 - 0)) * 100,
      
      scale_id %in% c("OI", "EX") ~ ((data_value - 1) / (7 - 1)) * 100,
      
      scale_id %in% c("CT") ~ ((data_value - 1) / (3 - 1)) * 100,
      
      TRUE ~ NA_real_)
    
  )


# We search for identical element names across different dimensions --------

ie_ad <- onet_df %>% 
  distinct(dimension, element_name) %>% 
  count(element_name) %>% 
  arrange(desc(n)) %>% 
  filter(n > 1) %>% 
  pull(element_name)

onet_df %>% 
  filter(element_name %in% ie_ad) %>% 
  distinct(dimension, element_name)

# Those element names are renamed according to the dimension

onet_df <- onet_df %>% 
  mutate(element_name = case_when(
    dimension == "knowledge" & element_name == "Mathematics" ~ "Mathematics_kn",
    dimension == "skills" & element_name == "Mathematics" ~ "Mathematics_sk",
    dimension == "work_styles" & element_name == "Independence" ~ "Independence_ws",
    dimension == "work_values" & element_name == "Independence" ~ "Independence_wv",
    TRUE ~ element_name))

rm(ie_ad)
    


# ---------------------------- Importance and Level Values are averaged across ONET categories --------------------------------------


# We calculate mean values of the onet values across jobs and element id_s

ds_avg <- onet_df %>% 
  
  group_by(o_net_soc_code, element_id) %>% 
  
  mutate(value_onet_avg = mean(value_onet)) %>% 
  
  distinct(o_net_soc_code, element_id, .keep_all = TRUE) %>%  # select unique rows
  
  mutate(scale_id = if_else(
    
    dimension %in% c("abilities", "knowledge", "skills", "work_activities") & scale_id == "IM", "AVG", scale_id)) %>% 
  
  select(-value_onet) %>% 
  
  ungroup()



# --------------------------- Kategorien, die ähnliches erfassen werden zusammengenommen und deren ONET-Werte gemittelt ----------------------


# Combination of element names that measure similar constructs (Onet values are averaged across similar constructs)


avg_similar_constr <- function(onet_data, element_names) {
  
  # Wir definieren neuen Elementnamen (Kombination aus zwei ähnlichen Konstrukten)
  
  element_new <- str_c(c(element_names), collapse = " & ")
  
  # Die folgenden Modifikationen beschränken sich auf die Zeilen im Datensatz in denen die oben erwähnten Elementnamen vorkommen
  
  onet_data[onet_data$element_name %in% element_names, ] <- onet_data[onet_data$element_name %in% element_names, ] %>% 
    
    # Die originalen Elementnamen werden durch die kombinierte Version ersetzt.
    
    mutate(element_name_comb = if_else(element_name %in% c(element_names), element_new, element_name)) %>% 
    
    # Die onet-werte werden jetzt über die kombinierten Elementnamen gemittelt.
    
    group_by(title, element_name_comb) %>% 
    mutate(value_onet_avg_temp = mean(value_onet_avg)) %>% 
    mutate(value_onet_avg = if_else(element_name_comb == element_new, value_onet_avg_temp, value_onet_avg)) %>% 
    ungroup() %>% 
    select(-value_onet_avg_temp)
  
  return(onet_data)
  
}


ds_avg$element_name_comb <- NA_character_

ds_avg <- avg_similar_constr(onet_data = ds_avg, element_names = c("Static Strength", "Dynamic Strength"))

ds_avg <- avg_similar_constr(onet_data = ds_avg, element_names = c("Manual Dexterity", "Arm-Hand Steadiness", "Wrist-Finger Speed"))

ds_avg <- avg_similar_constr(onet_data = ds_avg, element_names = c("Multilimb Coordination", "Speed of Limb Movement"))

ds_avg <- avg_similar_constr(onet_data = ds_avg, element_names = c("Response Orientation", "Rate Control", "Reaction Time"))

ds_avg <- avg_similar_constr(onet_data = ds_avg, element_names = c("Originality", "Fluency of Ideas"))

ds_avg <- avg_similar_constr(onet_data = ds_avg, element_names = c("Active Learning", "Learning Strategies"))

ds_avg <- avg_similar_constr(onet_data = ds_avg, element_names = c("Controlling Machines and Processes", "Operation and Control"))

ds_avg <- avg_similar_constr(onet_data = ds_avg, element_names = c("Repairing and Maintaining Mechanical Equipment", 
                                                                   "Equipment Maintenance", 
                                                                   "Troubleshooting",
                                                                   "Repairing"))

ds_avg <- avg_similar_constr(onet_data = ds_avg, element_names = c("Repairing and Maintaining Electronic Equipment", 
                                                                   "Equipment Maintenance", 
                                                                   "Troubleshooting",
                                                                   "Repairing"))

ds_avg <- avg_similar_constr(onet_data = ds_avg, element_names = c("Persuasion", "Selling or Influencing Others"))

ds_avg <- avg_similar_constr(onet_data = ds_avg, element_names = c("Instructing", "Learning Strategies", "Training and Teaching Others"))

ds_avg <- avg_similar_constr(onet_data = ds_avg, element_names = c("Organizing, Planning, and Prioritizing Work", 
                                                                   "Scheduling Work and Activities"))

ds_avg <- avg_similar_constr(onet_data = ds_avg, element_names = c("Systems Analysis", "Systems Evaluation"))

ds_avg <- avg_similar_constr(onet_data = ds_avg, element_names = c("Processing Information", "Analyzing Data or Information"))

ds_avg <- avg_similar_constr(onet_data = ds_avg, element_names = c("Exposed to Hazardous Conditions", 
                                                                   "Exposed to Hazardous Equipment", 
                                                                   "Exposed to Minor Burns, Cuts, Bites, or Stings"))


# We rename the element names with the new combined name

ds_avg <- ds_avg %>% 
  mutate(element_name = if_else(!is.na(element_name_comb), element_name_comb, element_name)) %>% 
  select(-element_name_comb)


# We remove duplicates in combined names

ds_avg <- ds_avg %>% distinct(title, element_name, value_onet_avg, .keep_all = TRUE)


# --------------------------- ONET Werte werden reskaliert in eine 0-6er Skala (basierend auf 6 Perzentilen pro Dimension) ----------------------

# Rescale the onet values from a scale from 0 to 100 to 0 to 5


# Funktion, welche für die jeweilige Dimension die Sextile bestimmt und für jede Dimension die Onet-Werte in diese 6 Quantile unterteilt.

calc_sextiles <- function(x) {
  
  my_cuts <- pull(x, value_onet_avg) %>% quantile(probs = seq(from = 0, to = 1, by = 1/6))
  
  sextiles <- mutate(x, 
                     sextiles = cut(value_onet_avg, breaks = my_cuts, include.lowest = TRUE, labels = 0:5), # Spalte sextiles generiert und Onet values in 6 sextiles eingeteilt mit den namem 0 bis 5
                     sextiles = as.integer(as.character(sextiles)))
  
  return(list(sextiles, my_cuts))
  
}

# Teile das Dataset in 6 Dimensionen. Dies ergibt eine Liste mit 6 Dataframes wobei jedes Dataframe die Daten einer Dimension beinhaltet. Wende die obige
# Funktion auf jedes der 6 Dataframes an.

# Behalte die Skalen

ds_avg_scales <- ds_avg %>% 
  split(.$dimension) %>% 
  map(calc_sextiles) %>% 
  map_dfr(2) %>% 
  t() %>% 
  as.data.frame()

names(ds_avg_scales) <- str_c("cut", as.character(round(seq(0, 1, by = (1/6)) * 100, 1)), sep = "_")

ds_avg_scales

write.csv2(ds_avg_scales, file.path("results", "onet", "cut_offs.csv"), row.names = FALSE)


# Füge die Skalen als zusätzliche Variable hinzu

ds_avg_rescaled <- ds_avg %>% 
  split(.$dimension) %>% 
  map(calc_sextiles) %>% 
  map(1) %>% 
  bind_rows() %>% 
  arrange(dimension, value_onet_avg) %>% 
  rename(JMT_value = sextiles)


# Schaue die Histogramme an

ds_avg_rescaled %>% 
  ggplot(aes(x = JMT_value)) + 
  geom_histogram(bins = 6) + 
  facet_wrap(~dimension, ncol = 2)



# -------------------------------------------------------- Data file from long to wide ---------------------------------------------

job_titles <- ds_avg_rescaled %>% 
  select(o_net_soc_code, title) %>% 
  distinct()

# We arrange the data so that every row is a job and the columns represent element names (onet categories)

elem_values <- ds_avg_rescaled %>% 
  select(o_net_soc_code, dimension, element_name, JMT_value) %>% 
  mutate(element_name_unique = str_c(dimension, element_name, sep = "|")) %>% 
  select(o_net_soc_code, element_name_unique, JMT_value) %>% 
  group_by(o_net_soc_code) %>% 
  tidyr::spread(element_name_unique, JMT_value) %>% 
  left_join(job_titles, by = "o_net_soc_code") %>% 
  select(o_net_soc_code, title, everything()) %>% 
  ungroup(o_net_soc_code)

# We rename the column names (by removing the dimension part)

distinct_element_names <- colnames(elem_values)

colnames(elem_values) <- colnames(elem_values) %>% str_split("\\|") %>% map_chr(tail, n = 1L)



# Save data

write.csv(elem_values, file.path("results", "onet", "o_net_data.csv"), row.names = FALSE)

saveRDS(elem_values, file = file.path("workspace", "o_net_data.RData"))

rm("avg_similar_constr", "calc_sextiles", "distinct_element_names", 
  "ds_avg", "ds_avg_rescaled", "ds_avg_scales", "elem_values", 
  "job_titles", "onet_df", "onet_file_location", "onet_files", 
  "onet_files.xlsx")
