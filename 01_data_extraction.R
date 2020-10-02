## Extract O*NET data

library(tidyverse)
library(readxl)




if(FALSE) { # Download all files in the O*NET core database, in one convenient ZIP archive
  
  download.file(
    url = "https://www.onetcenter.org/dl_files/database/db_25_0_excel.zip",
    destfile = file.path("data", "o_net_files.zip"), mode = "wb")
  
}


if(FALSE) { # Unzip the files to folder data
  
  unzip(file.path("data", "o_net_files.zip"), exdir = file.path("data"))
  
}

# For onet values extraction we only need following datasets

dir(file.path("data", "db_25_0_excel"))

onet_files <- c("Abilities", "Interests", "Knowledge", "Skills", 
                "Work Activities", "Work Context", "Work Styles", 
                "Work Values")

onet_files.xlsx <- str_c(onet_files, ".xlsx")

onet_file_location <- str_c(file.path("data", "db_25_0_excel", onet_files.xlsx), sep = "/")

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


# We do a rescaling of the data values

onet_df <- onet_df %>% 
  
  mutate(
    
    value_onet = case_when(
      
      scale_id %in% c("IM", "CX") ~ ((data_value - 1) / (5 - 1)) * 100,
      
      scale_id %in% c("LV") ~ ((data_value - 0) / (7 - 0)) * 100,
      
      scale_id %in% c("OI", "EX") ~ ((data_value - 1) / (7 - 1)) * 100,
      
      scale_id %in% c("CT") ~ ((data_value - 1) / (3 - 1)) * 100,
      
      TRUE ~ NA_real_)
    
  )


# We calculate mean values of the onet values across jobs and element id_s

ds_avg <- onet_df %>% 
  
  group_by(o_net_soc_code, element_id) %>% 
  
  mutate(value_onet_avg = mean(value_onet)) %>% 
  
  distinct(o_net_soc_code, element_id, .keep_all = TRUE) %>%  # select unique rows
  
  mutate(scale_id = if_else(
    
    dimension %in% c("abilities", "knowledge", "skills", "work_activities") & scale_id == "IM", "AVG", scale_id)) %>% 
  
  select(-value_onet) %>% 
  
  ungroup()





# Combination of element names that measure similar constructs (Onet values are averaged across similar constructs) -------------


ds_avg_2 <- ds_avg %>% 
  mutate(element_name = if_else(element_name %in% c("Static Strength", "Dynamic Strength"), "Static Strength & Dynamic Strength", element_name)) %>% 
  
  group_by(title, element_name) %>% 
  mutate(value_onet_avg_temp = mean(value_onet_avg)) %>% 
  mutate(value_onet_avg = if_else(element_name == "Static Strength & Dynamic Strength", value_onet_avg_temp, value_onet_avg)) %>% 
  ungroup() %>% 
  distinct(dimension, title, element_name, value_onet_avg, .keep_all = TRUE) %>%
  
  select(-value_onet_avg_temp)


avg_similar_constr <- function(onet_data, element_names) {
  
  element_new <- str_c(c(element_names), collapse = " & ")
  
  onet_data %>% 
    
    mutate(element_name_comb = if_else(element_name %in% c(element_names), element_new, element_name)) %>% 
    
    group_by(title, element_name_comb) %>% 
    mutate(value_onet_avg_temp = mean(value_onet_avg)) %>% 
    mutate(value_onet_avg = if_else(element_name_comb == element_new, value_onet_avg_temp, value_onet_avg)) %>% 
    ungroup() %>% 
    distinct(dimension, title, element_name_comb, value_onet_avg, .keep_all = TRUE) %>%
    
    select(-value_onet_avg_temp)
  
}

comb_el_names <- list(
  c("Static Strength", "Dynamic Strength"), 
  c("Manual Dexterity", "Arm-Hand Steadiness", "Wrist-Finger Speed")
)

map



ds_avg_new <- avg_similar_constr(onet_data = ds_avg, element_names = c("Static Strength", "Dynamic Strength"))

ds_avg_new <- avg_similar_constr(onet_data = ds_avg, element_names = c("Manual Dexterity", "Arm-Hand Steadiness", "Wrist-Finger Speed"))

ds_avg_new <- avg_similar_constr(onet_data = ds_avg, element_names = c("Multilimb Coordination", "Speed of Limb Movement"))

ds_avg_new <- avg_similar_constr(onet_data = ds_avg, element_names = c("Response Orientation", "Rate Control", "Reaction Time"))

ds_avg_new <- avg_similar_constr(onet_data = ds_avg, element_names = c("Originality", "Fluency of Ideas"))

ds_avg_new <- avg_similar_constr(onet_data = ds_avg, element_names = c("Active Learning", "Learning Strategies"))

ds_avg_new <- avg_similar_constr(onet_data = ds_avg, element_names = c("Controlling Machines and Processes", "Operation and Control"))

ds_avg_new <- avg_similar_constr(onet_data = ds_avg, element_names = c("Repairing and Maintaining Mechanical Equipment", 
                                                                       "Equipment Maintenance", 
                                                                       "Troubleshooting",
                                                                       "Repairing"))

ds_avg_new <- avg_similar_constr(onet_data = ds_avg, element_names = c("Repairing and Maintaining Electronic Equipment", 
                                                                       "Equipment Maintenance", 
                                                                       "Troubleshooting",
                                                                       "Repairing"))

ds_avg_new <- avg_similar_constr(onet_data = ds_avg, element_names = c("Persuasion", "Selling or Influencing Others"))

ds_avg_new <- avg_similar_constr(onet_data = ds_avg, element_names = c("Instructing", "Learning Strategies", "Training and Teaching Others"))

ds_avg_new <- avg_similar_constr(onet_data = ds_avg, element_names = c("Organizing, Planning, and Prioritizing Work", 
                                                                       "Scheduling Work and Activities"))

ds_avg_new <- avg_similar_constr(onet_data = ds_avg, element_names = c("Systems Analysis", "Systems Evaluation"))

ds_avg_new <- avg_similar_constr(onet_data = ds_avg, element_names = c("Processing Information", "Analyzing Data or Information"))

ds_avg_new <- avg_similar_constr(onet_data = ds_avg, element_names = c("Exposed to Hazardous Conditions", 
                                                                       "Exposed to Hazardous Equipment", 
                                                                       "Exposed to Minor Burns, Cuts, Bites, or Stings"))



# Save data

write.csv2(ds_avg, file.path("results", "dataset_avg.csv"), row.names = FALSE)

save(ds_avg, file = file.path("workspace", "o_net_data.RData"))


# Clear workspace

rm("ds_avg", "onet_df", "onet_file_location", "onet_files", "onet_files.xlsx")
