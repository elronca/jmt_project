
library(tidyverse)
library(readxl)
library(zoo)

## Wir laden die Onet Kategorien und JMT Werte fÃ¼r jeden Beruf

onet <- readRDS(file = file.path("02_workspace", "o_net_data.RData")) %>% 
  rename(ONETtitle = title)


# Onet Liste wird mit zusÃ¤tzlichen von uns hinzugefÃ¼gten Job Matching Tool Kategorien ergÃ¤nzt ( z.B. Geschmackssinn)

onet <- onet %>% 
  mutate(`Kontakt mit Tieren` = NA_integer_,
         `Überkopfarbeiten` = NA_integer_,
         `Sinn für Ästhetik` = NA_integer_,
         `Musikalität` = NA_integer_,
         `Geschmackssinn` = NA_integer_,
         `Geruchssinn` = NA_integer_,
         `Taktile Wahrnehmung` = NA_integer_,
         `Umweltbewusstsein` = NA_integer_,
         `Feuchtigkeit` = NA_integer_,
         `Belastung durch Allergene` = NA_integer_, 
         `Gute Allgemeinbildung` = NA_integer_,
         `Vorbildung_Zulassungsvoraussetzungen` = NA_character_
  )

# Wir identifizieren die Job Matching Tool Kategorien

all_onet_cat <- colnames(onet)[-c(1:2)]
all_onet_cat <- all_onet_cat[-length(all_onet_cat)]


# Urbans Liste mit JMT Berufen wird mit ONet-Werten ergänzt

chberufe <- file.path("01_data", "01_SD_basic_Linking bis 0.722 (Abgleich bis 0.623).xlsx") %>% 
  
  read_excel(sheet = "Gegencheck") %>% 
  
  select(sortierNr:Keywords, -ONETtitle) %>% 
  
  filter(!is.na(ONETcode)) %>%
  filter(!is.na(`Tool-Titel`)) %>%
  
  left_join(onet, by = c("ONETcode" = "o_net_soc_code"))

rm(onet)


# Liste Berufsberatungsberufe mit Anforderungen (aus berufsberatung.ch und anforderungsprofile.ch), die zu ONET-Kategorien gelinkt wurden


my_col_types <- c("skip", "text", "skip", "text", "text",  "skip", "skip", "text", "skip", 
                  "skip", "skip", "skip")


abgll <- read_excel(file.path("01_data", "BB_AP_merged_2_bereinigt_linking_Übergabe MN2.xlsx"), 
                    sheet = 1, na = c("", "NA"), col_types = my_col_types) %>% 
  rename(bb_linked_to_onet_cat = ONET_kategorien) %>% 
  filter(!is.na(job_title_BB2))


# Abgleichliste abgll ist Älter als chberufe-Liste von Urban. D.h. in Urbans Liste hats mehr Berufe drin, zu denen ich gar keine Anforderungswerte 
# von berufsberatung.ch habe. Aktuell konnten nur 734 Berufe mit ONET werten aus chberufe-Liste mit dem CH-Kontext abgeglichen werden. Zudem: zu
# vielen Berufen in chberufe-Liste hat es sowieso keine Anforderungsinfos auf berufsberatung.ch oder anforderungsprofile.ch


abgll_long <- separate_rows(abgll, bb_linked_to_onet_cat, sep = "\\|")

chberufe_long <- pivot_longer(data = chberufe, cols = "Auditory Attention":"Gute Allgemeinbildung", names_to = "onet_cat", values_to = "JMT_value")


# Now we want to merge in the Abgleichliste via Bbtitle, however, since there are NA this does not work properly, that is why we replace NA's with a string "do_not_match"

chberufe_long %>% filter(is.na(Bbtitle))

chberufe_long <- mutate(chberufe_long, Bbtitle = replace_na(Bbtitle, "do_not_match"))

chberufe_long <- left_join(chberufe_long, abgll_long, by = c("Bbtitle" = "job_title_BB2"))


# Fälle die Spalte Vorbildung_Zulassungsvoraussetzungen mit dem entsprechenden Text, der in berufsberatung.ch unter Anforderungen stand


chberufe_long <- chberufe_long %>% 
  mutate(`Vorbildung_Zulassungsvoraussetzungen` = if_else(bb_linked_to_onet_cat %in% "Vorbildung/Zulassungsvoraussetzungen", `Anforderungen`, `Vorbildung_Zulassungsvoraussetzungen`))


# Falls meherere Vorbildung/Zulassungsvoraussetzungen pro Beruf und Onet Kategorie, dann nehmen wir die Anforderungen in einer Zelle zusammen.

nrow(chberufe_long)

chberufe_long_vzl <- chberufe_long %>% 
  filter(bb_linked_to_onet_cat %in% "Vorbildung/Zulassungsvoraussetzungen") %>% 
  group_by(Bbtitle, onet_cat) %>% 
  mutate(Vorbildung_Zulassungsvoraussetzungen = str_c(Anforderungen, collapse = "\n")) %>%
  select(-Anforderungen) %>% 
  ungroup() %>% 
  distinct()

chberufe_long_wo_vzl <- chberufe_long %>% 
  filter(!bb_linked_to_onet_cat %in% "Vorbildung/Zulassungsvoraussetzungen") %>% 
  select(-Anforderungen)

chberufe_long <- bind_rows(chberufe_long_vzl, chberufe_long_wo_vzl) %>% 
  arrange(`Tool-Titel`, Bbtitle, sd_nr)

nrow(chberufe_long)

rm(chberufe_long_vzl, chberufe_long_wo_vzl)


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


## Wieviele Korrekturen von JMT Werten wurden vorgenommen

sum(chberufe_long_distinct$was_changed, na.rm = TRUE)



# Schmeissen unnötige Spalten raus und bringen die gesamte Liste wieder ins Wide-Format


chberufe_final <- chberufe_long_distinct %>% 
  select(-JMT_value, -was_changed, -bb_linked_to_onet_cat) %>% 
  rename(JMT_value = JMT_value_adjusted)

chberufe_final <- chberufe_final %>% 
  pivot_wider(id_cols = c(sortierNr:`Vorbildung_Zulassungsvoraussetzungen`, Swissdoc), names_from = onet_cat, values_from = JMT_value) %>% 
  add_count(`Tool-Titel`) %>% 
  filter(!(is.na(`Vorbildung_Zulassungsvoraussetzungen`) & n == 2L)) %>% 
  select(-n) %>% 
  mutate(Bbtitle = if_else(Bbtitle == "do_not_match", NA_character_, Bbtitle))


# Wir ersetzen fehlende Werte in Onet Kategorien mit dem vorangehenden oder 
# nachfolgenden Wert der Onet Kategorie innerhalb der gleichen ONET codes.

chberufe_final <- chberufe_final %>%
  group_by(ONETcode) %>% 
  mutate_at(vars("Auditory Attention":"Working Conditions"), ~(na.locf(., na.rm = FALSE))) %>% 
  mutate_at(vars("Auditory Attention":"Working Conditions"), ~(na.locf(., na.rm = FALSE, fromLast = TRUE))) %>% 
  ungroup()


# Do some quality checks

identical(sort(chberufe$`Tool-Titel`), sort(chberufe_final$`Tool-Titel`))

chberufe_final %>% pull(Bbtitle) %>% sort() %>% str_subset("Geflügel")

chberufe_final %>% 
  select(`Kontakt mit Tieren`:`Gute Allgemeinbildung`) %>% 
  filter_all(any_vars(!is.na(.)))


# ------------------------- Wir mergen Berufsberatungs und onet Daten (fÃ¼r finale Liste fÃ¼r Wolfgang zum HOchladen in JMT Datenbank --------------------------

BB_job_list <- readRDS(file.path("02_workspace", "BB_characteristics", "BB_job_list.RData")) %>% 
  filter(webID != 7163) # Beruf Lebensmittelchemiker ist auf Berufsberatung doppelt erfasst.

JMT_joblist_final <- chberufe_final %>% 
  left_join(rename(BB_job_list, Swissdoc_BB = Swissdoc), by = c("Bbtitle" = "jobTitle")) %>% 
  select(sortierNr:Bbtitle, Keywords, webID:Vorbildung, Vorbildung_Zulassungsvoraussetzungen, 
         Swissdoc, url, ONETcode, ONETtitle, `Auditory Attention`:`Gute Allgemeinbildung`) %>% 
  unite(col = "Vorbildung_Zulassungsvoraussetzungen", Vorbildung:Vorbildung_Zulassungsvoraussetzungen, sep = "\n") %>% 
  mutate(Vorbildung_Zulassungsvoraussetzungen = str_remove(Vorbildung_Zulassungsvoraussetzungen, "\\\nNA"))


# JMT Nummer wird über ONet Kategorien als Zeile eingefÃ¼gt

descriptor_definitions <- read_excel(file.path("01_data/titles_2020_26_10.xlsx")) %>% 
  
  select(one_of(c("var", "element_name_en")))


my_matches <- match(names(JMT_joblist_final), descriptor_definitions$element_name_en)

my_titles <- descriptor_definitions[my_matches, ] %>% 
  t() %>% 
  as_tibble()

names(my_titles) <- names(JMT_joblist_final)

JMT_joblist_final <- mutate_all(JMT_joblist_final, as.character)

JMT_joblist_final <- bind_rows(my_titles, JMT_joblist_final)


# Remove all soft-hyphens 
# (see https://stackoverflow.com/questions/57077145/how-to-remove-empty-string-in-string)

JMT_joblist_final <- JMT_joblist_final %>% 
  mutate(across(everything(), ~str_remove_all(., '\U00AD'))) %>% 
  mutate(across(everything(), ~replace_na(., "")))


## Diese ONet Kategorien werden in unserem Tool nicht verwendet 
# (sind bereits in anderen JMT Variabeln integriert)

JMT_joblist_final %>% 
  slice(1) %>% 
  pivot_longer(everything()) %>% 
  filter(value == "") %>% 
  pull(name) %>% 
  dput()

JMT_joblist_final <- select(JMT_joblist_final, -one_of(c(
  "Management of Personnel Resources", "Making Decisions and Solving Problems", 
  "Monitoring and Controlling Resources", "Performing General Physical Activities"))) %>% 
  slice(-2)


# Add Holland ONET-code column --------------------------------------------

holland <- JMT_joblist_final %>%

  select(`Tool-Titel`, ONETcode, Realistic, Investigative, Artistic, Social, Enterprising, Conventional) %>%

  rename(R = Realistic, I = Investigative, A = Artistic, S = Social, E = Enterprising, C = Conventional) %>%

  slice(-1) %>%

  na.omit() %>%

  arrange(ONETcode) %>%

  mutate_at(vars(R:C), as.integer)


find_top3 <- function(x) {
  
  mySeq <- unlist(sort(x, decreasing = TRUE))
  r <- rle(mySeq)
  r$values <- seq_along(r$values)
  pre_res <- sapply(split(names(mySeq), inverse.rle(r)), paste, collapse = "=")[1:3]
  res <- paste0(pre_res, collapse = ">")
  
  return(res)
  
}

holland$holland_onet <- apply(holland[,-c(1:2)], 1, find_top3)


JMT_joblist_final <- JMT_joblist_final %>% 
  left_join(select(holland, `Tool-Titel`, holland_onet), by = "Tool-Titel") %>% 
  select(sortierNr:url, holland_onet, ONETcode:`Gute Allgemeinbildung`)



# Ändere Namen von Variablen analog Wolfgangs DB --------------------------

JMT_joblist_final <- rename(JMT_joblist_final, 
         jobname = `Tool-Titel`,
         jobname_bb = Bbtitle,
         id_web = webID,
         description = shortDescription,
         keywords_de = Keywords,
         edu_type = Bildungstypen,
         jobarea = Berufsfelder,
         branch = Branchen,
         swissdoc = sd_nr,
         activity = `Tätigkeiten`,
         length2 = Dauer,
         preptraining = Vorbildung_Zulassungsvoraussetzungen,
         onet_code = ONETcode,
         onet_title = ONETtitle)




write.csv(JMT_joblist_final, file.path("03_output", "JMT_joblist_final_comma_delim.csv"), 
          row.names = FALSE)

write.csv2(JMT_joblist_final, file.path("03_output", "JMT_joblist_final_semicol_delim.csv"), 
          row.names = FALSE)

rm("abgll", "abgll_long", "all_onet_cat", "BB_job_list", "chberufe", 
     "chberufe_final", "chberufe_long", "chberufe_long_distinct", 
     "descriptor_definitions", "find_top3", "gegengecheckt", "holland", 
     "JMT_joblist_final", "my_col_types", "my_matches", "my_titles")
