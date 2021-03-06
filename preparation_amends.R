
# Download the list of MPs' ####
    
mps09 <- read.csv("https://data.rada.gov.ua/ogd/mps/skl9/mps09-data.csv", fileEncoding = "UTF-8")%>%
  filter(date_end=="")%>% # Filter to none to get rid of MPs' who resigned
  select(id, full_name, region_name)  

# Download the factions of MPs' ####

get_factions_open <- function(){
  posts <- read_csv("https://data.rada.gov.ua/ogd/mps/skl9/mp-posts_ids.csv")
  posts_ids <- read_tsv("https://data.rada.gov.ua/ogd/mps/skl9/mp-posts_unit.txt", 
                        locale(encoding = "windows-1251"), col_names = F, col_types = NULL) %>% 
    rename(unit_id = X1, unit = X2)
  mps <- read_csv("https://data.rada.gov.ua/ogd/mps/skl9/mps09-data.csv")
  
  factions_full <- posts %>% 
    left_join(mps[, c("rada_id" ,"id", "full_name")], by = c("mp_id" = "id")) %>% 
    left_join(posts_ids) %>% 
    filter(unit_type %in% c("grp", "fra")) %>% 
    select(mp_id, full_name, unit)
  
  factions_df <-  mps %>% 
    filter(is.na(resignation_text)) %>% 
    select(rada_id, id, full_name) %>% 
    left_join(factions_full, by = c("id" = "mp_id", "full_name")) %>% 
    mutate(unit = ifelse(is.na(unit), "Позафракційні", unit)) %>% 
    rename(faction = unit, fullname = full_name) %>% 
    mutate(faction = recode(faction,
                            `Фракція ПОЛІТИЧНОЇ ПАРТІЇ "СЛУГА НАРОДУ"` = "Слуга Народу",
                            `Фракція Політичної Партії "ГОЛОС" у Верховній Раді України дев'ятого скликання` = "ГОЛОС",
                            `Фракція ПОЛІТИЧНОЇ ПАРТІЇ "ЄВРОПЕЙСЬКА СОЛІДАРНІСТЬ"` = "ЄС",
                            `Фракція Політичної партії "ОПОЗИЦІЙНА ПЛАТФОРМА - ЗА ЖИТТЯ" у Верховній Раді України` = "ОПЗЖ",
                            `Фракція політичної партії Всеукраїнське об'єднання "Батьківщина" у Верховній Раді України дев'ятого скликання` = "Батьківщина",
                            `Група "Партія "За майбутнє"` = "За майбутнє",
                            `Група "ДОВІРА"`= "ДОВІРА"))  # The group "Dovira" was created in December 2019))
  return(factions_df)
}


factions_09 <- get_factions_open()

factions <- factions_09 %>%
    select(-rada_id, -id)



# Download the bills  ####

# Download signed acts 
bills_acts_skl9 <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_acts-skl9.csv")

# The executives of Rada-9
bills_executives_skl9 <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_executives-skl9.csv", 
                                  fileEncoding = "UTF-8")%>%
  filter(type=="mainExecutive")%>% # We need the main 
  select(-convocation, -person_id, -organization, -post)%>%
  mutate(bill_id=as.character(bill_id))

# The main file with bills' activities
bills_main_skl9 <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_main-skl9.csv", fileEncoding = "UTF-8")%>%
  #select(bill_id, number,type, rubric, subject,currentPhase_title)%>%
  mutate(number=as.character(number))

# Read a file with a downloaded data  #### amends_read
amends_read <- read_delim("Amends_scraping_28_01_2020.csv", ";", # Check your name
                                escape_double = FALSE, col_names = FALSE, 
                                col_types = cols(X1 = col_character()), 
                                trim_ws = TRUE)
                                
# Fix a leading zero which disappeared during scraping
amends_pad <- amends_read %>%
  mutate(X1=stringr::str_pad(Amends_scraping$X1, 4, pad = "0"))  # Add a zero 
  
# Fix names and add factions and MPs' list
amends_f <- amends_pad %>%
  separate(X11, c("surname", "name", "parent"), sep = " ")%>%
  unite(fullname, c("surname", "name", "parent"), sep = " ")%>%
  left_join(factions, by=c("fullname"="fullname"))%>% # Make sure you've downloaded the factions
  left_join(mps09, by=c("fullname"="full_name"))%>%
  mutate(X1=as.character(X1))

# Rename headers and join a main df with description of all bills' activities
amends_full <- amends_f %>% 
  rename(
    number = X1,
    name_bill = X2,
    reading_date = X3,
    totally = X4,
    accepted = X5,
    rejected = X6,
    partly_accepted = X7,
    redakciyno_accepted = X8,
    others = X9,
    no_conclusion = X10
  )%>%
  mutate(number=as.character(number))%>%
  left_join(bills_main_skl9, by=c("number"="number"))%>%
  mutate(bill_id=as.character(bill_id))

# Add the executives   
amends_full_description <- amends_full%>%
  left_join(bills_executives_skl9, by=c("bill_id"="bill_id"))
  
# Write files ####
library(xlsx)
library(rJava)

date_amends <- "28_01_2019" # Change 

write.xlsx(as.data.frame(amends_full), 
           file=paste0("amends_full", date_amends, ".xlsx"), # Fix names
           sheetName="amends_full", row.names=FALSE, append = FALSE)

write.xlsx(as.data.frame(amends_full_description), 
           file=paste0("amends_full", date_amends, ".xlsx"), # Fix names
           sheetName="amends_full_description", row.names=FALSE, append = TRUE)
