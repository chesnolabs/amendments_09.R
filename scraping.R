library(rvest)
library(xml2)
library(dplyr)
library(tidyr)
library(httr)
library(magrittr)
library(data.table)
library(jsonlite)
library(readr)
library(extrafont)
library(extrafontdb)
library(ggplot2)


### Start parsing ####

# Step 1. MPs' list to use for a loop
mps09 <- read.csv("https://data.rada.gov.ua/ogd/mps/skl9/mps09-data.csv", fileEncoding = "UTF-8")%>%
  #select(rada_id, full_name, date_end)%>%
  filter(date_end=="")%>%
  select(id, full_name, region_name)

# Step 2. Actually turn a dataframe into a list
mps09$id <- as.list(mps09$id)

# Step 3. A function to download 
get_amends <- function(){
  for(i in mps09$id) {
           url <- paste0("http://w1.c1.rada.gov.ua/pls/pt2/reports.dep2?PERSON=", i, "&SKL=10")
           url_table <- read_html(url, 
                      encoding = "Windows-1251")%>%
                      xml_find_all("//table") %>% 
                      html_table()
           
           # Amendments are located in a second table
           # Some tables can be empty because MPs' didn't amend yet
         
           results_2019 <- url_table[[2]]%>% 
             data.frame()%>% replace(!nzchar(.), NA)
           
           if (nrow(results_2019)>0) {
             
             name <- read_html(url, encoding = "Windows-1251")%>%
               html_node("span b")%>%
               html_text(trim = TRUE)
             
             results_2019$name <- name

            # Write data 
            write.table(results_2019, fileEncoding = "UTF-8", 
                                                  paste0("Amends_scraping",  "_28_01_2020",   #Change date
                                                         ".csv"), 
                                                  append = TRUE, col.names = FALSE, 
                                                  row.names = FALSE, sep = ";")  
           
           message(paste(i, '_downloaded'))
           }
           Sys.sleep(0.5)
  }
  }

# Step 4. Start parsing 
amends <- get_amends()


