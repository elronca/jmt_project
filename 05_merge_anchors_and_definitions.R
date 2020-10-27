
library(readxl)
library(tidyverse)

# Load Excel files with translated Anchors and Descriptor Definitions
anchors <- read_excel(file.path("01_data", "Level Scale Anchors_final R.xlsx")) %>% 
  rename(element_name_en = `Element Name`, element_id = `Element ID`)

anchors %>% filter(element_name_en == "Mathematics")

anchors <- anchors %>% 
  
  mutate(element_name_en = case_when(
    element_name_en == "Mathematics" & element_id == "2.A.1.e" ~ "Mathematics_sk",
    element_name_en == "Mathematics" & element_id == "2.C.4.a" ~ "Mathematics_kn",
    TRUE ~ element_name_en)
  )


descriptor_definitions <- read_excel(file.path("01_data", "titles_2020_26_10.xlsx")) %>% 
  
  select(id_title, var, ord_bl, ord_gn, element_name_de, element_name_en, description_de, description_en)



# Merge files based on Element.ID (remove Element.Name column in one file, as we do not need it twice)

descriptors_and_anchors <- full_join(anchors, descriptor_definitions, by = "element_name_en")

#---------------------------------------------------------------------------------------------------------
# Ein File mit nur Descriptor Definitions, die Anchor Beispiele haben, erstellen

# Alle Kategorien, die bei element_id NA haben, entsprechen ONET Kategorien, die keine Ankerbeispiele haben ODER 
# sind von uns gebildete (zusammengeschlossene) Kategorien und haben so auch kein Ankerbeispiel mehr.
# Zudem: Alle, die bei id_title NA haben brauchen wir auch nicht, weil diese ONET Kategorien sind gar nicht 
# mehr in unserem Tool drin, obwohl es dazu in ONET ein Ankerbeispiel gäbe.

descriptors_with_anchors <- descriptors_and_anchors %>% 
  filter(!is.na(element_id)) %>% 
  filter(!is.na(id_title))




saveRDS(descriptors_with_anchors, file.path("02_workspace", "descriptors_with_anchors.RData"))

# Ein File mit nur Descriptor Definitions, welche keine Anchor Beispiele haben, erstellen 
# (erstelle nur ein Bild mit einer Definition ohne Ankerbeispiel)

descriptors_witout_anchors <- filter(descriptors_and_anchors, is.na(element_id))

saveRDS(descriptors_witout_anchors, file.path("02_workspace" ,"descriptors_witout_anchors.RData"))



rm("anchors", "descriptor_definitions", "descriptors_and_anchors", 
  "descriptors_with_anchors", "descriptors_witout_anchors")

