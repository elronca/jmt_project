

library(tidyverse)
library(rvest)
library(progress)

# Load websites and website ids -------------------------------------------

bb_webids <- readRDS(file.path("02_workspace", "BB_characteristics", "webids_all.RData"))
bb_webpages <- readRDS(file.path("02_workspace", "BB_characteristics", "bb_webpages_all.RData")) %>% map(read_html)


# Extract short summary from websites -------------------------------------

# Function

extract_job_summary <- function(pages, id) {
  
  pb$tick() # For progress bar
  
  jobTitle <- pages %>% 
    html_nodes("h1") %>% 
    html_text() %>% 
    .[1]
  
  shortDescription <- pages %>% 
    html_nodes(".lead") %>% 
    html_text()
  
  jobSummary_categories <- pages %>%
    html_nodes("#roof-bottom") %>%
    html_nodes(".boxContent") %>%
    html_nodes("dt") %>%
    html_text()
  
  jobSummary_content <- pages %>%
    html_nodes("#roof-bottom") %>%
    html_nodes(".boxContent") %>%
    html_nodes("dd") %>%
    html_text() %>%
    str_replace_all("[\r\n]", "") %>% 
    set_names(jobSummary_categories)
  
  updated <- pages %>% 
    html_nodes("#roof-bottom") %>%
    html_nodes(".eDoc") %>% 
    html_text() %>% 
    str_remove_all("Aktualisiert ") %>% 
    as.Date("%d.%m.%Y")
  
  jobSummary <- tibble(webID = id, jobTitle, shortDescription, bind_rows(jobSummary_content), updated)
  
}


# Run function with progress bar. Collect jobs, put into table and save file


pb <- progress::progress_bar$new(total = length(bb_webids))

bb_job_summary_l <- map2(bb_webpages, bb_webids, safely(extract_job_summary))

bb_job_summary <- bb_job_summary_l %>% map("result") %>% bind_rows()

saveRDS(bb_job_summary, file.path("02_workspace", "BB_characteristics", "bb_job_summary.RData"))




# Extract website content -------------------------------------------------


# Function

get_content <- function(pages, ids) {
  
  pb$tick()
  
  
  # Create list were all information will be collected
  
  job_characteristics <- list()
  
  
  # Define the website headings under which we want to collect job data
  
  possible_headings <- c("\nTätigkeiten\n", "\nAusbildung\n", "\nVoraussetzungen\n")
  
  all_headings <- pages %>% html_nodes(".toggleBox") %>%  html_nodes(".boxTitle") %>% html_text()
  
  headings_this_job <- str_subset(all_headings, str_c(possible_headings, collapse = "|"))

  
  
  # If the website of this particular job has any of the website headings we are looking for we go on.
  # Otherwise we job to the end and write only the web id
  
  if ( length(headings_this_job) > 0 ) {
    
    
    # We collect the job content under the predefined headings and remove paragraphs
    
    content <- pages %>% 
      html_nodes(".boxContent") %>% 
      keep(all_headings %in% headings_this_job) %>% 
      set_names(str_replace_all(headings_this_job, "[\r\n]", ""))
    
    
    
    # We extract the content under the heading "Tätigkeiten if heading Tätigkeiten is present
    
    if( any(names(content) == "Tätigkeiten") ) {
      
      job_characteristics[["taetigkeiten"]] <- content[["Tätigkeiten"]] %>% html_text() %>% str_trim()
      
    }
    
    
    # If heading "Ausbildung" is present, we extract everything related to duration of "Ausbildung" below that heading.
    
    
    if( any(names(content) == "Ausbildung") ) {
      
      if( content[["Ausbildung"]] %>% html_nodes("h3") %>% html_text() %in% "Dauer" %>% any() ) {
        
        
        zeitangaben <- c("Jahre", "Jahr", "Monate", "Monat", "Wochen", "Woche", "Tage", "Tag",
                         "Stunden", "Stunde", "Kurstage", "Unterrichtstage","Semester", "Lektionen",
                         
                         "individuell gestaltbar", "individuell gestaltbar", "Unterschiedlich lang",
                         "unterschiedlich lang", "Je nach Vorbildung")
        
        
        zeitangaben <- str_c(zeitangaben, collapse = "|")
        
        job_characteristics[["Dauer"]] <- content[["Ausbildung"]] %>% 
          as.character() %>%
          str_split("<h3>|<p>|<li>") %>%
          map(~str_subset(., zeitangaben)) %>%
          map(~str_remove_all(., "</li>|</ul>|</p>|\\\n")) %>%
          unlist() %>% 
          str_trim() %>% 
          str_c(collapse = "\n")
        
      }
      
    }
    
    
    # If heading "Voraussetzungen" is present, we extract everything related to prerequisites below that heading.
    
    if( names(content) %in% "Voraussetzungen" %>% any() ) {
      
      position_vorbildung <- content[["Voraussetzungen"]] %>%
        html_nodes("h3") %>%
        html_text() %>%
        str_detect("Vorbildung")
      
      if( any(position_vorbildung) ) {
        
        job_characteristics[["Vorbildung"]] <- content[["Voraussetzungen"]] %>%
          html_nodes("ul") %>%
          html_text() %>%
          keep(position_vorbildung) %>% 
          str_trim()
        
      }
      
    }
    
    
    # We add the web id to the list of scraped headings and contents and save as tibble row 
    
    c(webID = ids, job_characteristics) %>% bind_rows()
    
  } else {
    
    # If there were no headings in the scraped website of a particular we jumped to this code directly
    # Only the webID of the job will be saved into the row. (The list job characteristics will be empty)
    
    c(webID = ids, job_characteristics) %>% bind_rows()
    
  }
  
}



# We construct the final table --------------------------------------------


# We run the function over all jobs and construct a table

pb <- progress::progress_bar$new(total = length(bb_webids))
bb_job_content_l <- map2(bb_webpages, bb_webids, safely(get_content))
bb_job_content <- bb_job_content_l %>% map("result") %>% bind_rows()


# We merge the table just created with the table created before from the job summary data
# The final table also includes the url to the job's specific page on berufsberatung.ch

urls <- tibble(webID = bb_webids, url = str_c("https://berufsberatung.ch/dyn/show/1900?id=", bb_webids))

job_characteristics <- bb_job_summary %>% 
  left_join(bb_job_content, by = "webID") %>% 
  left_join(urls, by = "webID") %>% 
  replace(is.na(.), "") %>% 
  arrange(as.integer(webID)) %>% 
  rename(`Tätigkeiten` = `taetigkeiten`)


# Check when pages were updated -------------------------------------------

job_characteristics %>% 
  mutate(updated = format(as.Date(updated), "%Y")) %>% 
  group_by(updated) %>% 
  tally() %>%
  ungroup() %>% 
  ggplot(aes(x = updated, y = n)) +
  geom_point()+
  labs(x = "Year", y = "number of updated job profiles", 
       title = "Number of updates of job profiles per year")


# Data is saved and workspace is cleared ----------------------------------

saveRDS(job_characteristics, file.path("02_workspace", "BB_characteristics", "job_characteristics.RData"))

write.csv2(job_characteristics, file = file.path("03_output", "BB", "jobTable.csv"), row.names = FALSE)

rm("bb_job_content", "bb_job_content_l", "bb_job_summary", "bb_job_summary_l", 
  "bb_webids", "bb_webpages", "extract_job_summary", "get_content", 
  "job_characteristics", "pb", "urls")
