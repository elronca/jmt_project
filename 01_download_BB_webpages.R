
## We download all the web pages with jobs from https://berufsberatung.ch
## In a second step we will then collect all the information we need from these web pages

library(tidyverse)
library(rvest)
library(progress)



# Download web pages -------------------------------------------------------

# We try to access berufsberatung.ch with the url below by checking ids from 1 to 15000.
# We checked the ids before by looking for random jobs and checking their ids.
# Since we did not want to make thousands of of website queries in a short time 
# which would be impolite we make sure we always wait for a second before we make another 
# query.

# This is function to test the urls and download them if they work

getWebPages <- function(webids) {
  
  pb$tick() # for progress bar in loop
  
  Sys.sleep(1.0) # pause of 1s.
  
  url <- str_c("https://berufsberatung.ch/dyn/show/1900?id=", webids)
  
  read_html(url, encoding = "UTF-8")
  
}


# we define the webid's to check. We do that in chunks since connections can be lost
# or a system can crash. If that happens we need to do everything again. Which is a
# problem when scrapping since we then would over-stress the website which could 
# result in a block and also might make the maintainers suspicions.

# So we start with 1000 queries from 
# https://berufsberatung.ch/dyn/show/1900?id=1 to 
# https://berufsberatung.ch/dyn/show/1900?id=1000

# First the progress bar is set (pb <- ...). 
# This line must be run together with the loop (map...)
# Then we apply the function with the 1000 webids
# The 1000 results are collected in a list called bb_webpages
# Every list element is named according to the web id


webids <- 1:1000
pb <- progress::progress_bar$new(total = length(webids))
bb_webpages <- map(webids, safely(getWebPages))
names(bb_webpages) <- as.character(webids)



# Now we have the results of the 1000 queries. However not all of the webid's
# lead to a working web page. Therefore, we select only those ones that worked
# and get information about those which did not.

# Function

get_BB_index_and_data <- function(scraped_website) {
  
  working_websites_lgl <- scraped_website %>% 
    map(2) %>% 
    map_lgl(is.null)
  
  data_working_websites <- scraped_website %>% 
    .[working_websites_lgl] %>% 
    map("result")
  
  error_msg <- scraped_website[!working_websites_lgl] %>% 
    map("error") %>% map("message") %>% 
    unlist()
  
  return(list(BB_Data = data_working_websites, error_msg = error_msg))
  
}


# We apply the function to all the 1000 elements of the list that contains the 
# downloaded web pages.

scraped_data <- get_BB_index_and_data(bb_webpages)

# We check the error messages and see how many which and how many web ids 
# did not lead to a working web page

scraped_data[["error_msg"]] %>% {print(enframe(.))} %>% nrow()


# We inspect the working web page

scraped_data[["BB_Data"]] %>% {print(names(.))} %>% length()

# The data file of the collected web pages is in a rather strange format
# and cannot be direclty saved as RData file. Therefore, we need to first
# transform every list element to a character (string) format before we
# can save the file.

scraped_data[["BB_Data"]] %>% map(as.character) %>% 
  saveRDS(file.path("02_workspace", "BB_characteristics", "bb_webpages_0001_1000.RData"))

# Here we save the working webids that lead to the working web pages that 
# were saved above.

scraped_data[["BB_Data"]] %>% names() %>% 
  saveRDS(file.path("02_workspace", "BB_characteristics", "webids_0001_1000.RData"))


# We now do that for all the webids from 1 to 15000 in smaller chunks. After,
# we did that and saved the data we reload the data and bind it together. 
# This combination of saved web pages will then be inspected and saved again
# as one list.


if(FALSE) {
  
  
  bb_webpages_all <- c(
    readRDS(file.path("02_workspace", "BB_characteristics", "bb_webpages_0001_1000.RData")) %>% map(read_html),
    readRDS(file.path("02_workspace", "BB_characteristics", "bb_webpages_1001_2000.RData")) %>% map(read_html),
    readRDS(file.path("02_workspace", "BB_characteristics", "bb_webpages_2001_3000.RData")) %>% map(read_html),
    readRDS(file.path("02_workspace", "BB_characteristics", "bb_webpages_3001_4000.RData")) %>% map(read_html),
    readRDS(file.path("02_workspace", "BB_characteristics", "bb_webpages_4001_5000.RData")) %>% map(read_html),
    readRDS(file.path("02_workspace", "BB_characteristics", "bb_webpages_5001_7000.RData")) %>% map(read_html),
    readRDS(file.path("02_workspace", "BB_characteristics", "bb_webpages_7001_9000.RData")) %>% map(read_html),
    readRDS(file.path("02_workspace", "BB_characteristics", "bb_webpages_9001_10000.RData")) %>% map(read_html),
    readRDS(file.path("02_workspace", "BB_characteristics", "bb_webpages_10001_12000.RData")) %>% map(read_html),
    readRDS(file.path("02_workspace", "BB_characteristics", "bb_webpages_12001_15000.RData")) %>% map(read_html)
  )
  
  
  
  names(bb_webpages)
  length(bb_webpages)
  
  bb_webpages_all %>% 
    map(as.character) %>% 
    saveRDS(file.path("02_workspace", "BB_characteristics",  "bb_webpages_all.RData"))
  
  bb_webids_all <- c(
    readRDS(file.path("02_workspace", "BB_characteristics", "webids_0001_1000.RData")),
    readRDS(file.path("02_workspace", "BB_characteristics", "webids_1001_2000.RData")),
    readRDS(file.path("02_workspace", "BB_characteristics", "webids_2001_3000.RData")),
    readRDS(file.path("02_workspace", "BB_characteristics", "webids_3001_4000.RData")),
    readRDS(file.path("02_workspace", "BB_characteristics", "webids_4001_5000.RData")),
    readRDS(file.path("02_workspace", "BB_characteristics", "webids_5001_7000.RData")),
    readRDS(file.path("02_workspace", "BB_characteristics", "webids_7001_9000.RData")),
    readRDS(file.path("02_workspace", "BB_characteristics", "webids_9001_10000.RData")),
    readRDS(file.path("02_workspace", "BB_characteristics", "webids_10001_12000.RData")),
    readRDS(file.path("02_workspace", "BB_characteristics", "webids_12001_15000.RData"))
  )
  
  length(bb_webids)
  tail(bb_webids)
  
  saveRDS(bb_webids, file.path("02_workspace", "BB_characteristics", "webids_all.RData"))
  
}

# In order to check whether the range of downloaded web pages makes sense
# we plot the number of working url's (that lead to a working web page) against
# the ones that were tested.

bb_webids <- readRDS(file.path("02_workspace", "BB_characteristics", "webids_all.RData"))

to_plot <- tibble(pot_web_ids = 1L:15000L, web_ids = 0L)
to_plot[as.integer(bb_webids), "web_ids"] <- 1L
to_plot <- mutate(to_plot, web_ids = cumsum(web_ids))

ggplot(to_plot, aes(x = pot_web_ids, y = web_ids)) + 
  geom_point() +
  labs(x = "tested_web_ids", y = "existing web_ids")

rm("bb_webids", "get_BB_index_and_data", "getWebPages", "to_plot")
