#scrape student accomodation names and postcodes from the student crowd website
#helpful links: 
#https://bradleyboehmke.github.io/2015/12/scraping-html-text.html
#https://cfss.uchicago.edu/notes/web-scraping/

library(tidyverse)
library(rvest)
library(glue)

#read review summary page:
review_page <- read_html("https://www.studentcrowd.com/student-accommodation-l1002555-edinburgh")

#this page lists all rated and unrated student accomodations in Edinburgh
#(those listed on this website anyway)
#we want to extract a link for each hall of residence and then
#extract the name and postcode for each site:

#some sleuthing around with the inspect element menu we found that
#we can access some links like this:
review_page %>% 
  html_nodes("ul.m0:nth-child(2) > li:nth-child(1) > div:nth-child(2) > meta:nth-child(2)") %>%
  html_attr("content")

#and links to the sliding box at the top of the page:
review_page %>%
  html_nodes("header.box > ul:nth-child(2) > li:nth-child(2)")

#found a split between the top 15 rated accomodations and the other 
#~50ish accomodations which do not have enough reviews to be ranked

#ids for top 15 accomodation
#here we have to change li:nth-child(1)
review_page %>%
  html_nodes("header.box > ul:nth-child(2) > li:nth-child(1)") %>% 
  html_node("a") %>% 
  html_attr("id")
#with these ones we have to combine it with the rest of the link:
#https://www.studentcrowd.com/

#full links for the the rest of the listed accomodation
#in this case we have to change the li:nth-child(i) node
review_page %>% 
  html_nodes("ul.m0:nth-child(5) > li:nth-child(1) > div:nth-child(2) > meta:nth-child(2)") %>%
  html_attr("content")

#for each case we write a function:
top15_scrape <- function(i, html_page){
  
  link_id <- html_page %>%
    html_nodes(glue("header.box > ul:nth-child(2) > li:nth-child({ i })")) %>% 
    html_node("a") %>% 
    html_attr("id")
  
  #create web link
  web_link <- paste0("https://www.studentcrowd.com/", link_id)
  
  #read site page:
  site_page <- read_html(web_link)
  
  #extract accomodation name:
  accomodation_name_1 <- site_page %>% 
    html_node(xpath = "/html/body/main/div[1]/div[1]/div/div/div[2]/section/div/div[2]/div[2]/div/div/div/h2/a") %>%
    html_text()
  
  accomodation_name_2 <- site_page %>% 
    html_node(xpath = "/html/body/main/div[1]/div[1]/div/div/div[2]/section/div/div[2]/div[2]/div/div/div/h2") %>%
    html_text()
  
  accomodation_name_3 <- site_page %>%
    html_node(xpath = "/html/body/main/div[1]/div[1]/div/div/div[2]/section/div/div[2]/div[3]/div/div[1]/div/h2") %>%
    html_text()
  
  accomodation_name <- unique(c(accomodation_name_1, 
                                accomodation_name_2,
                                accomodation_name_3))
  accomodation_name <- accomodation_name[!is.na(accomodation_name)]
  
  #and the postcode:
  all_text <- site_page %>%
    html_nodes("tr") %>%
    html_text()
  
  #and create output:
  output <- tibble(sitename = accomodation_name,
                   postcode = str_remove(all_text[str_detect(all_text, "Post")], "Postcode"))
  
  output
  
}

#and run over the top 15 rated edinburgh student accomodations:
top15_rated_sites <- c(1:15) %>% map_df(~top15_scrape(i = .x,
                                                      html_page = review_page))

top15_rated_sites <- top15_rated_sites %>% 
  mutate(sitename = str_trim(str_remove_all(sitename, "Information")))

#and then over the un-rated accomodations:
unrated_scrape <- function(i, html_page){
  
  ###scrape weblink directly:
  web_id <- html_page %>% 
    html_nodes(glue("ul.m0:nth-child(5) > li:nth-child({ i }) > div:nth-child(2) > meta:nth-child(2)")) %>%
    html_attr("content")
  
  #extract everything after the # to extract link to page:
  web_link <- paste0("https://www.studentcrowd.com/", str_extract(web_id, "(?<=#).*"))
  
  ###read site page:
  site_page <- read_html(web_link)
  
  ###extract accomodation name
  # #h2 tag returns all accomodation names and we index to exclude the first 15 (the rated ones)
  # #and the titles to get the ith accomodation name:
  accomodations <- site_page %>% html_nodes("h2") %>%
    html_text()
  
  accomodation_name <- str_trim(str_remove_all(accomodations[1], "Applying to"))
  
  # accomodation_name <- site_page %>% html_nodes(".lg-1 > h2:nth-child(1)") %>% html_text()
  
  ###extract the postcode
  #by brute force we take all text related to the tr tag
  #and extract when creating output:
  all_text <- site_page %>%
    html_nodes("tr") %>%
    html_text()
  
  #and create output:
  output <- tibble(sitename = accomodation_name,
                   postcode = str_remove(all_text[str_detect(all_text, "Post")], "Postcode"))
  
  output
  
}

#run over 66 values of i:
unrated_sites <- c(1:80) %>% map_df(~unrated_scrape(i = .x,
                                                    html_page = review_page))

#remove entries which do not look like a postcode:
unrated_sites <- unrated_sites %>% filter(str_detect(postcode, "[A-Z][A-Z][0-9]"))

#combine results and write to csv:
ed_student_accomodations <- bind_rows(top15_rated_sites,
                                      unrated_sites)

write.csv(ed_student_accomodations, "student-crowd-edinburgh-accomodations-2020.csv",
          row.names = FALSE)
