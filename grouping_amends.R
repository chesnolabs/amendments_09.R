##### By MPs', SUM #### 

amends_by_mps <- amends_full_description %>%
  group_by(faction, fullname, region_name)%>%
  summarise(totally=sum(totally),
            accepted=sum(accepted),
            partly_accepted=sum(partly_accepted),
            rejected=sum(rejected),
            redakciyno_accepted=sum(redakciyno_accepted),
            others=sum(others),
            no_conclusion=sum(no_conclusion))
            
##### By MPs', % #### 
            
amends_by_mps_perc <- amends_full_description %>%
  group_by(faction, fullname, region_name)%>%
  summarise(
    totally_number=sum(totally),
    perc_accepted=round(mean(accepted*100/totally), digits=1),
    perc_partly_accepted=round(mean(partly_accepted*100/totally), digits=1),
    rejected_perc=round(mean(rejected*100/totally), digits=1),
    redakciyno_accepted_perc=round(mean(redakciyno_accepted*100/totally), digits=1),
    others_perc=round(mean(others*100/totally), digits=1),
    no_conclusion_perc=round(mean(no_conclusion*100/totally), digits=1)
    )
    

### By Faction, Sum #### 
amends_by_faction <- amends_full_description%>%
  group_by(faction)%>%
  summarise(totally=sum(totally),
            accepted=sum(accepted),
            partly_accepted=sum(partly_accepted),
            rejected=sum(rejected),
            redakciyno_accepted=sum(redakciyno_accepted),
            others=sum(others),
            no_conclusion=sum(no_conclusion))

### By Faction, % #### 
amends_by_faction_perc <- amends_full_description%>%
  group_by(faction)%>%
  summarise(
    totally_number=sum(totally),
    perc_accepted=round(mean(accepted*100/totally), digits=1),
    perc_partly_accepted=round(mean(partly_accepted*100/totally), digits=1),
    rejected_perc=round(mean(rejected*100/totally), digits=1),
    redakciyno_accepted_perc=round(mean(redakciyno_accepted*100/totally), digits=1),
    others_perc=round(mean(others*100/totally), digits=1),
    no_conclusion_perc=round(mean(no_conclusion*100/totally), digits=1))%>% 
  arrange(perc_accepted, faction)
  
 amends_by_faction_perc$faction <- as.factor(amends_by_faction_perc$faction)
  
##### By region, sum ####
amends_by_region <- amends_full_description%>%
  group_by(region_name)%>%
  summarise(totally=sum(totally),
            accepted=sum(accepted),
            partly_accepted=sum(partly_accepted),
            rejected=sum(rejected),
            redakciyno_accepted=sum(redakciyno_accepted),
            others=sum(others),
            no_conclusion=sum(no_conclusion))

##### By region, % ####  
amends_by_region_perc <- amends_full_description%>%
  group_by(region_name)%>%
  summarise(
    totally_number=sum(totally),
    perc_accepted=round(mean(accepted*100/totally), digits=1),
    perc_partly_accepted=round(mean(partly_accepted*100/totally), digits=1),
    rejected_perc=round(mean(rejected*100/totally), digits=1),
    redakciyno_accepted_perc=round(mean(redakciyno_accepted*100/totally), digits=1),
    others_perc=round(mean(others*100/totally), digits=1),
    no_conclusion_perc=round(mean(no_conclusion*100/totally), digits=1))%>% 
  arrange(perc_accepted, faction)

##### By faction & department, SUM #### 
amends_by_faction_department <- amends_full_description%>%
  group_by(faction, department)%>%
  summarise(
    totally_number=sum(totally),
    perc_accepted=round(mean(accepted*100/totally), digits=1),
    perc_partly_accepted=round(mean(partly_accepted*100/totally), digits=1),
    rejected_perc=round(mean(rejected*100/totally), digits=1),
    redakciyno_accepted_perc=round(mean(redakciyno_accepted*100/totally), digits=1),
    others_perc=round(mean(others*100/totally), digits=1),
    no_conclusion_perc=round(mean(no_conclusion*100/totally), digits=1)
  )%>%
  arrange(desc(perc_accepted, faction))

##### By faction & department, % #### 
amends_by_faction_department_perc <- amends_full_description%>%
  group_by(faction, department)%>%
    summarise(totally=sum(totally),
              accepted=sum(accepted),
              partly_accepted=sum(partly_accepted),
              rejected=sum(rejected),
              redakciyno_accepted=sum(redakciyno_accepted),
              others=sum(others),
              no_conclusion=sum(no_conclusion))%>%
  arrange(desc(accepted, faction))

### By Department & Number of Bills, Sum ####
amends_depart_number <- amends_full_description%>%
  group_by(department, number)%>%
  summarise(
    totally=sum(totally),
            accepted=sum(accepted),
            partly_accepted=sum(partly_accepted),
            rejected=sum(rejected),
            redakciyno_accepted=sum(redakciyno_accepted),
            others=sum(others),
            no_conclusion=sum(no_conclusion)
    )%>%
  arrange(desc(accepted, faction))
  
### By Department & Number of Bills, % ####
amends_depart_perc <- amends_full_description%>%
  group_by(department, number)%>%
  summarise(
    totally_number=sum(totally),
    perc_accepted=round(mean(accepted*100/totally), digits=1),
    perc_partly_accepted=round(mean(partly_accepted*100/totally), digits=1),
    rejected_perc=round(mean(rejected*100/totally), digits=1),
    redakciyno_accepted_perc=round(mean(redakciyno_accepted*100/totally), digits=1),
    others_perc=round(mean(others*100/totally), digits=1),
    no_conclusion_perc=round(mean(no_conclusion*100/totally), digits=1)
    )%>%
  arrange(desc(perc_accepted, number))  

### By Department & Region Name, % ####
amends_depart_region_perc <- amends_full_description%>%
  group_by(department, region_name)%>%
  summarise(
    totally_number=sum(totally),
    perc_accepted=round(mean(accepted*100/totally), digits=1),
    perc_partly_accepted=round(mean(partly_accepted*100/totally), digits=1),
    rejected_perc=round(mean(rejected*100/totally), digits=1),
    redakciyno_accepted_perc=round(mean(redakciyno_accepted*100/totally), digits=1),
    others_perc=round(mean(others*100/totally), digits=1),
    no_conclusion_perc=round(mean(no_conclusion*100/totally), digits=1)
            )%>%
  arrange(desc(perc_accepted, faction))
  
### By Number & fullname & faction, % ####
amends_by_number_perc <- amends_full_description%>%
  group_by(number, fullname, faction)%>%
  summarise(
    totally_number=sum(totally),
    perc_accepted=round(mean(accepted*100/totally), digits=1),
    perc_partly_accepted=round(mean(partly_accepted*100/totally), digits=1),
    rejected_perc=round(mean(rejected*100/totally), digits=1),
    redakciyno_accepted_perc=round(mean(redakciyno_accepted*100/totally), digits=1),
    others_perc=round(mean(others*100/totally), digits=1),
    no_conclusion_perc=round(mean(no_conclusion*100/totally), digits=1))%>% 
  arrange(perc_accepted, faction)
  
#### Amends from not a mono coalition ####
anomalities <- amends_by_mps_perc%>%
  filter(!faction=="Слуга Народу")%>%
  arrange(desc(perc_accepted))%>%
  arrange(desc(totally_number))

#### Amends from the best MPs'from Sluga Narody####
best_mps <- amends_by_mps_perc%>%
  filter(faction=="Слуга Народу")%>%
  filter(perc_accepted==100)

#### Amends from the worst MPs' from Sluga Narody ####
worse_mps <- amends_by_mps_perc%>%
  filter(faction=="Слуга Народу")%>%
  filter(perc_accepted==0 & perc_partly_accepted==0)%>%
  arrange(desc(totally_number))
