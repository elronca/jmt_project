
library(tidyverse)
library(readxl)

## Wir laden die Onet Kategorien und JMT Werte für jeden Beruf

onet <- readRDS(file = file.path("02_workspace", "o_net_data.RData"))


# Onet Liste wird mit zusätzlichen von uns hinzugefügten Job Matching Tool Kategorien ergänzt ( z.B. Geschmackssinn)

onet <- onet %>% 
  mutate(`Kontakt mit Tieren` = NA_integer_,
         `Überkopfarbeiten durchführen` = NA_integer_,
         `Sinn für Ästhetik` = NA_integer_,
         `Musikalität` = NA_integer_,
         `Geschmackssinn` = NA_integer_,
         `Geruchssinn` = NA_integer_,
         `Taktiles Wahrnehmungsvermögen` = NA_integer_,
         `Gute Allgemeinbildung` = NA_integer_,
         `Vorbildung/Zulassungsvoraussetzungen` = NA_character_
  )

# Wir identifizieren die Job Matching Tool Kategorien

all_onet_cat <- colnames(onet)[-c(1:2)]
all_onet_cat <- all_onet_cat[-length(all_onet_cat)]


# Urbans Liste mit JMT Berufen wird mit ONet-Werten ergänzt

chberufe <- read_excel(file.path("01_data", "onet_bb_abgleich", "01_SD_basic_Linking us.xlsx")) %>% 
  
  select(sortierNr:Keywords) %>% 
  
  filter(!is.na(`Tool-Titel`)) %>% 
  
  left_join(onet, by = c("ONETcode" = "o_net_soc_code"))

rm(onet)


# Liste Berufsberatungsberufe mit Anforderungen (aus berufsberatung.ch und anforderungsprofile.ch), die zu ONET-Kategorien gelinkt wurden


my_col_types <- c("skip", "text", "skip", "text", "text",  "skip", "skip", "text", "skip", 
                  "skip", "skip", "skip")


abgll <- read_excel(file.path("01_data", "onet_bb_abgleich", "BB_AP_merged_2_bereinigt_linking.xlsx"), 
  sheet = 1, na = c("", "NA"), col_types = my_col_types) %>% 
  rename(bb_linked_to_onet_cat = ONET_kategorien) %>% 
  filter(!is.na(job_title_BB2))


# Abgleichliste abgll ist älter als chberufe-Liste von Urban. D.h. in Urbans Liste hats mehr Berufe drin, zu denen ich gar keine Anforderungswerte 
# von berufsberatung.ch habe. Aktuell konnten nur 734 Berufe mit ONET werten aus chberufe-Liste mit dem CH-Kontext abgeglichen werden. Zudem: zu
# vielen Berufen in chberufe-Liste hat es sowieso keine Anforderungsinfos auf berufsberatung.ch oder anforderungsprofile.ch


abgll_long <- separate_rows(abgll, bb_linked_to_onet_cat, sep = "\\|")

chberufe_long <- pivot_longer(data = chberufe, cols = "Auditory Attention":"Gute Allgemeinbildung", names_to = "onet_cat", values_to = "JMT_value")


## Plausibility checks

# Betonwerker apapears 235 times in the dataset after pivot_longer, this corresponds with the number of onet categories 

# chberufe_long %>% 
#   filter(str_detect(chberufe_long$`Tool-Titel`, "Detailhandelsassistent/in Schuhe EBA")) %>% 
#   pull(`Tool-Titel`) %>% 
#   length()

# length(all_onet_cat)

# There is no dpulicated Onet categories in Abdichter

# chberufe_long %>% 
#   filter(`Tool-Titel` == "Betonwerker/in EFZ") %>% 
#   pull(onet_cat) %>% 
#   enframe() %>% 
#   count(value) %>% 
#   arrange(desc(n))
# 
# Schuhe <- chberufe_long %>% filter(`Tool-Titel` == "Detailhandelsassistent/in Schuhe EBA") 


# Now we want to merge in the Abgleichliste via Bbtitle, however, since there are NA this does not work properly, that is why we replace NA's with a string "do_not_match"

chberufe_long %>% filter(is.na(Bbtitle))

chberufe_long <- mutate(chberufe_long, Bbtitle = replace_na(Bbtitle, "do_not_match"))

chberufe_long <- left_join(chberufe_long, abgll_long, by = c("Bbtitle" = "job_title_BB2"))


# Schuhe <- chberufe_long %>% filter(`Tool-Titel` == "Detailhandelsassistent/in Schuhe EBA")
# nrow(Schuhe) / length(all_onet_cat)

# Fülle die Spalte Vorbildung/Zulassungsvoraussetzungen mit dem entsprechenden Text, der in berufsberatung.ch unter Anforderungen stand

chberufe_long <- chberufe_long %>% 
  mutate(`Vorbildung/Zulassungsvoraussetzungen` = if_else(bb_linked_to_onet_cat %in% "Vorbildung/Zulassungsvoraussetzungen", `Anforderungen`, `Vorbildung/Zulassungsvoraussetzungen`))

# Schuhe <- chberufe_long %>% filter(`Tool-Titel` == "Detailhandelsassistent/in Schuhe EBA")
# nrow(Schuhe) / length(all_onet_cat)


# Now we want to adjust values via Abgleichliste --------------------------

chberufe_long %>% 
  select(`Tool-Titel`, onet_cat, bb_linked_to_onet_cat) %>% 
  map_int(~sum(is.na(.)))


chberufe_long <- chberufe_long %>% 
  mutate(is_equal = as.integer(onet_cat == bb_linked_to_onet_cat),
         is_equal = if_else(is.na(is_equal), 0L, is_equal))


# Das gibt eine Übersicht über die Anzahl gegengecheckten Onet Kategorien pro Beruf

gegengecheckt <- chberufe_long %>% 
  group_by(`Tool-Titel`) %>% 
  tally(is_equal) %>% 
  arrange(desc(n))

gegengecheckt

# Schuhe <- chberufe_long %>% filter(`Tool-Titel` == "Detailhandelsassistent/in Schuhe EBA") %>% filter(is_equal == 1L | is.na(is_equal))

# Anzahl Berufe aus Urbans chberufe-Liste, bei denen CH-Anforderungen mit ONET Anforderungen gegengecheckt/abgeglichen werden konnten: 

cat(sum(gegengecheckt$n > 0), "von", nrow(gegengecheckt))


# Passe JMT_value auf 4 an, wenn gegengecheckt/is_equal gleich 1 und JMT_value unter 4 ist.

chberufe_long <- chberufe_long %>% 
  mutate(JMT_value_adjusted = if_else(is_equal == 1L & (JMT_value < 4L | is.na(JMT_value)), 4L, JMT_value)) %>% 
  mutate(was_changed = as.integer(JMT_value != JMT_value_adjusted))


# Now we remove all the extra rows that were created because we merged the Abgleichliste into the dataset

chberufe_long_distinct <- chberufe_long %>% 
  
  # We keep records for every combination of "tool title",  Onet category and was_changed (1 if JMT_value was adjusted, 0 if not). 
  # For those combinations of "tool titles" and Onet category where no JMT_value was adjusted (was_changed = 0) we keep only one record
  # For those where we have adjusted JMT values (was_changed = 0) we have multiple records in multiple `Tool-Titel` and Onet category combinations
  # However, we want only one Onet category per tool title, either with an unadjusted JMT value or with the adjusted JMT value, which will be the the record with the higher number in JMT value
  
  distinct(`Tool-Titel`, onet_cat, was_changed, .keep_all = TRUE) %>% 
  
  # We search for multiple rows of combination of "tool title" and Onet category combinations (using duplicated) and
  # filter (keep) those who either were not adjusted (-> here not duplicated) or those who were adjusted (-> those with hightest JMT value))
  
  group_by(`Tool-Titel`, onet_cat) %>% 
  mutate(duplicated_onet_cats = sum(duplicated(onet_cat))) %>% 
  filter(duplicated_onet_cats == 0L | (duplicated_onet_cats == 1L & JMT_value_adjusted == max(JMT_value_adjusted))) %>% 
  ungroup() %>% 
  
  # We keep only those bb_linked_to_onet_cat where it lead to a change
  
  mutate(bb_linked_to_onet_cat = if_else(was_changed == 1L, bb_linked_to_onet_cat, NA_character_)) %>% 
  select(-duplicated_onet_cats, -is_equal)


  

# Have a look at the onet categories that were changed

# "Betonwerker/in EFZ"

# test <- chberufe_long_distinct %>% 
#   filter(str_detect(`Tool-Titel`, "Detailhandelsassistent/in Schuhe EBA")) %>% 
#   select(`Tool-Titel`, onet_cat, JMT_value, JMT_value_adjusted, was_changed)

# onet_cats_changed_Betonwerker <- chberufe_long_distinct %>% filter(str_detect(`Tool-Titel`, "Betonwerker/in EFZ") & was_changed == 1) %>% pull(onet_cat)
# 
# Betonwerker <- chberufe_long_distinct %>% filter(`Tool-Titel` == "Betonwerker/in EFZ")
# 
# Betonwerker %>% select(`Tool-Titel`, onet_cat, JMT_value, JMT_value_adjusted, was_changed)

# chberufe_long_distinct %>% 
#   split(f = .$`Tool-Titel`) %>% 
#   map_int(nrow) %>% 
#   table()
# 
# sum(few_onet_cats)





## Wieviele Korrekturen von JMT Werten wurden vorgenommen

sum(chberufe_long_distinct$was_changed, na.rm = TRUE)


## Wieviele Korrekturen von JMT Werten wurden pro Beruf vorgenommen

# list_corrected <- chberufe_long_distinct %>% 
#   group_by(`Tool-Titel`) %>% 
#   tally(was_changed) %>% 
#   arrange(desc(n))
# 
# list_corrected





# Schmeissen unnötige Spalten raus und bringen die gesamte Liste wieder ins Wide-Format


chberufe_final <- chberufe_long_distinct %>% 
  select(-JMT_value, -Anforderungen, -was_changed, -bb_linked_to_onet_cat) %>% 
  rename(JMT_value = JMT_value_adjusted)

chberufe_final <- chberufe_final %>% 
  pivot_wider(id_cols = c(sortierNr:`Vorbildung/Zulassungsvoraussetzungen`, Swissdoc), names_from = onet_cat, values_from = JMT_value) %>% 
  add_count(`Tool-Titel`) %>% 
  filter(!(is.na(`Vorbildung/Zulassungsvoraussetzungen`) & n == 2L)) %>% 
  select(-n) %>% 
  mutate(Bbtitle = if_else(Bbtitle == "do_not_match", NA_character_, Bbtitle))


# Do some quality checks

identical(sort(chberufe$`Tool-Titel`), sort(chberufe_final$`Tool-Titel`))

chberufe_final %>% pull(Bbtitle) %>% sort() %>% str_subset("Geflügel")

chberufe_final %>% 
  select(`Kontakt mit Tieren`:`Gute Allgemeinbildung`) %>% 
  filter_all(any_vars(!is.na(.)))
