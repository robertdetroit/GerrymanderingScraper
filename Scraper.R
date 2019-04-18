
library(rvest)
library(tidyverse)

# code to scrape data

# 429 results
paths_allowed("https://legiscan.com/gaits/search?state=ALL&keyword=redistrict")

page1 <- read_html("https://legiscan.com/gaits/search?state=ALL&keyword=redistrict")
page2 <- read_html("https://legiscan.com/gaits/search?page=1&state=ALL&keyword=redistrict")
page3 <- read_html("https://legiscan.com/gaits/search?page=2&state=ALL&keyword=redistrict")
page4 <- read_html("https://legiscan.com/gaits/search?page=3&state=ALL&keyword=redistrict")
page5 <- read_html("https://legiscan.com/gaits/search?page=4&state=ALL&keyword=redistrict")
page6 <- read_html("https://legiscan.com/gaits/search?page=5&state=ALL&keyword=redistrict")
page7 <- read_html("https://legiscan.com/gaits/search?page=6&state=ALL&keyword=redistrict")
page8 <- read_html("https://legiscan.com/gaits/search?page=7&state=ALL&keyword=redistrict")
page9 <- read_html("https://legiscan.com/gaits/search?page=8&state=ALL&keyword=redistrict")

scrape_bill_info <- function(x){
  state <- x %>%
    html_nodes("td:nth-child(2) a") %>%
    html_text(trim=TRUE)
  
  bill_name <- x %>%
    html_nodes("td~ td+ td > a:nth-child(1)") %>%
    html_text(trim=TRUE)
  
  bill_summary <- x %>%
    html_nodes("td:nth-child(5)") %>%
    html_text(trim=TRUE)
  
  bill_summary <- sapply(strsplit(bill_summary,"\\["), `[`, 1)
  
  status <- x %>%
    html_nodes("td:nth-child(4)") %>%
    html_text(trim=TRUE)
  
  bill_url <- x %>%
    html_nodes("br+ .gaits-bill-detail") %>%
    html_attr("href") %>%
    paste("https://legiscan.com", ., sep = "")
  
  lastAction_date <- x %>%
    html_nodes(".gaits-browse-date") %>%
    html_text(trim=TRUE)
  
  
  lastAction_main <- x %>%
    html_nodes(".gaits-browse-action") %>%
    html_text(trim=TRUE)
  
  tibble(state = state, 
         bill_name = bill_name, 
         bill_summary = bill_summary,
         status = status,
         bill_url = bill_url,
         lastAction_date = lastAction_date,
         lastAction_main = lastAction_main
  )
}

page1_bills <- scrape_bill_info(page1)
page2_bills <- scrape_bill_info(page2)
page3_bills <- scrape_bill_info(page3)
page4_bills <- scrape_bill_info(page4)
page5_bills <- scrape_bill_info(page5)
page6_bills <- scrape_bill_info(page6)
page7_bills <- scrape_bill_info(page7)
page8_bills <- scrape_bill_info(page8)
page9_bills <- scrape_bill_info(page9)

all_bills <- rbind(page1_bills,page2_bills,page3_bills,
                   page4_bills,page5_bills,page6_bills,
                   page7_bills,page8_bills,page9_bills)

bill_numb_by_state <- all_bills %>% count(state)

(bill_numb_by_state_max <- max(bill_numb_by_state$n))

bill_numb_by_state_porp <- bill_numb_by_state %>%
  group_by(state) %>% summarise(porp = n / bill_numb_by_state_max)

write_csv(all_bills, path = "data/all_bills.csv")
write_csv(bill_numb_by_state, path = "data/bill_numb_by_state.csv")
write_csv(bill_numb_by_state_porp, path = "data/bill_numb_by_state_porp.csv")