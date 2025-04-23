################################################################################
### PACKAGES
################################################################################
##### Connection with MySQL (#available in sql.RData)
library(RMySQL)
library(tidyr)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(outliers)
library(EnvStats)
library(gt)
library(units)
library(pbapply)
library(naniar)
library(stringr)
####
dbBaki = dbConnect(MySQL(), user='root', password='cyril2024', 
                   dbname='mydb', host='localhost')
dbListTables(dbBaki)
for (i in dbListTables(dbBaki)){
  print(i)
  rsBaki = dbSendQuery(dbBaki, paste0("SELECT * FROM ",i))
  assign(i,fetch(rsBaki, n=-1))
}

dbSepsis = dbConnect(MySQL(), user='root', password='cyril2024', dbname='sepsis', host='localhost')
dbListTables(dbSepsis)
for (i in dbListTables(dbSepsis)){
  print(i)
  rsBaki = dbSendQuery(dbSepsis, paste0("SELECT * FROM ",i))
  assign(i,fetch(rsBaki, n=-1))
}


antibiotics_list <- c("cefazoline", "isoniazide","rifampicine","pyrazinamide","oxytetracycline + polymixine B sulfaat","colistimethaat natrium", "metronidazol","cefuroxim","voriconazol","linezolid","piperacilline - tazobactam", "vancomycine hydrochloride", "fluconazol", "amoxilline - clavulaanzuur","clindamycine","levofloxacine","anidulafungine","meropenem","tigecycline","flucloxacilline","ceftriaxon","ganciclovir","erythromycine","moxifloxacine","amikacine","sulfamethoxazol - trimethoprim", "ciprofloxacine","clarithromycine","tobramycine - dexamethason","posaconazole","lamivudine","chloortetracycline","teicoplanine","aztreonam","gentamicine","tobramycine","ampicilline","chinolonen","immunoglobulinen antitetanus", "difterie-anatoxine - tetanus-anatoxine","miconazole","temocilline","aciclovir","oseltamivir","azythromycine","caspofungine acetaat","benzylpenicilline","hydroxychloroquine sulfaat","mupirocine calcium","norfloxacine","primaquine","amfotericine B","tobramycine sulfaat","valganciclovir hydrochloride","oxacilline nartrium" ,"sulfadiazine zilver","nystatine","doxycycline","fucidinezuur")
#### All data ######
###################HospitalDate########################################## 

#BAKI-I
#Day1: E0 is inclusion, E1 (6pm) only if E0 before noon
#Day2: E2 (6am),E3 (6pm)
#Day3: E4 (6am), E5 (6pm)
#Day4: E6 (6am), E7 (6pm)
#Day 5: E8 (6am)
#Day 6: E9 (6am)
#Day 7: E10 (6am)

#BAKI-A
#E0 is after the induction of anaesthesia and before the start of surgery,
#E1 is the ICU admission (0h) = ICUAdmissionTime
#E2 is 2h after ICU admission
#E3 is 4h
#E4 is 6h
#E5 is 12h: day 1
#E6 is 24h: day 2
#E7 is 48h: day 3

colnames(Baki_Cytokines)[2] <- "StudyID"
HospitalDate <- data.frame(StudyID=unique(Baki_Pat$StudyID))
patient_study_ids <- unique(select(Baki_Pat,c(PatientID,StudyID)))
HospitalDate <- merge(HospitalDate, patient_study_ids, by="StudyID")
dates <- select(Baki_Pat,-c(PatientID,InclusionYear,Gender,Age, ICUAdmissionDate,))
HospitalDate <- merge(HospitalDate, dates, by="StudyID")
HospitalDate$ICUAdmissionTime <- as.POSIXct(HospitalDate$ICUAdmissionTime,"GMT")
HospitalDate$InclusionDate <- as.POSIXct(HospitalDate$InclusionDate,tz="GMT")
HospitalDate$HospitalAdmissionTime <- as.POSIXct(HospitalDate$HospitalAdmissionTime,tz="GMT")
HospitalDate$ICUDischargeTime <- as.POSIXct(HospitalDate$ICUDischargeTime,tz="GMT")
HospitalDate$HospitalDischargeTime <- as.POSIXct(HospitalDate$HospitalDischargeTime,tz="GMT")

Baki_Lab$SampleTime <- as.POSIXct(Baki_Lab$SampleTime, "GMT")
Baki_VentParams$ObsTime <- as.POSIXct(Baki_VentParams$ObsTime, "GMT")
Baki_AllVars$ObsTime<- as.POSIXct(Baki_AllVars$ObsTime, "GMT")
Baki_VasoPres$ObsDate<- as.POSIXct(Baki_VasoPres$ObsDate, "GMT")
Baki_VasoPres$ObsDate<- as.POSIXct(format(Baki_VasoPres$ObsDate, "%Y-%m-%d 12:00"), "GMT")
Baki_Medication$GivenAt<- as.POSIXct(Baki_Medication$GivenAt, "GMT")

Baki_Infections$infectionstart<- as.POSIXct(Baki_Infections$infectionstart, "GMT")
Baki_Infections$infectionend <- as.POSIXct(Baki_Infections$infectionend, "GMT")


HospitalDate$ICU_length_days <- difftime(HospitalDate$ICUDischargeTime,HospitalDate$ICUAdmissionTime,
                                         units = "days")
HospitalDate$Hospital_length_days <- difftime(HospitalDate$HospitalDischargeTime,HospitalDate$HospitalAdmissionTime,
                                              units = "days")
HospitalDate$Survival <- ifelse((HospitalDate$ICUSurvival == 0 | HospitalDate$HospitalSurvival == 0), 0, 1)


PatientGeneralInfo <- data.frame(StudyID=unique(Baki_Pat$StudyID))
patient_study_ids <- unique(select(Baki_Pat,c(PatientID,StudyID)))
PatientGeneralInfo <- merge(PatientGeneralInfo, patient_study_ids, by="StudyID")
gender <- select(Baki_Pat, c(PatientID,Gender,Age))
PatientGeneralInfo <- merge(PatientGeneralInfo, select(Baki_Comorb,c(PatientID,
                                                                     Length,
                                                                     Weight)),by="PatientID")
PatientGeneralInfo <- merge(PatientGeneralInfo,gender,by="PatientID")
PatientGeneralInfo$BMI <- round(PatientGeneralInfo$Weight/(PatientGeneralInfo$Length/100)^2,2)


baki_a_and_i <- merge(select(PatientGeneralInfo,-StudyID), HospitalDate, by="PatientID")
baki_a_and_i <- baki_a_and_i %>%
  mutate(Study = ifelse(substring(StudyID,1,1)=="I","BAKI-I","BAKI-A")) %>%
  arrange(StudyID,as.numeric(substring(StudyID,3)))

baki_a_and_i <- select(baki_a_and_i,c(PatientID,StudyID,Study,
                                      Gender,Age,Length,Weight,
                                      BMI,HospitalSurvival,ICUSurvival,Survival,
                                      HospitalAdmissionTime,HospitalDischargeTime,Hospital_length_days,	
                                      ICUAdmissionTime,	ICUDischargeTime,ICU_length_days,
                                      InclusionDate))

baki_a_and_i <- merge(baki_a_and_i,select(Baki_Cytokines,-cytokines_id), by="StudyID",all.x = T)
baki_a_and_i$temp_id <- paste0(baki_a_and_i$cytokines_e,"_",baki_a_and_i$StudyID)
Baki_GDF15$temp_id <- paste0(Baki_GDF15$gdf15_day,"_",Baki_GDF15$gdf15_patient_id)
baki_a_and_i <- merge(baki_a_and_i,Baki_GDF15, by="temp_id",all.x=T)
baki_a_and_i <- select(baki_a_and_i,-c(gdf15_patient_id,gdf15_id,gdf15_day,temp_id))
baki_i <- filter(baki_a_and_i, Study=="BAKI-I")
baki_a <- filter(baki_a_and_i, Study=="BAKI-A")

#### BAKI_I only #####

#create complete dataset, beyond the 7 ICU days
baki_i <- filter(baki_a_and_i, Study=="BAKI-I")
baki_i <- baki_i %>% #adding the day for patients with cytokine values
  mutate(day = ifelse(cytokines_e == "E0", 1,
                      ifelse(cytokines_e == "E2", 2,
                             ifelse(cytokines_e == "E4", 3,
                                    ifelse(cytokines_e == "E6", 4,
                                           ifelse(cytokines_e == "E8", 5,
                                                  ifelse(cytokines_e == "E9", 6,
                                                         ifelse(cytokines_e == "E10", 7, NA))))))))
patients_no_cytokines <- filter(baki_i, is.na(cytokines_e))$PatientID
baki_i[is.na(baki_i$day),]$day <- 1 #day for patients without cytokines_e

#calculate the number of additional rows for each patient. we want one row per day, beyond the 7 ICU days if they stayed hospitalized in the ICU.
baki_i <- baki_i %>%
  group_by(PatientID) %>%
  mutate(nr_record = n()) %>% 
  mutate(additional_rows = as.integer(round(first(ICU_length_days))-nr_record)) %>%
  mutate(additional_rows = ifelse(additional_rows > 0, additional_rows, 0))

#Duplicate rows based on the number of days stayed
df_new <- baki_i %>%
  select(c(StudyID,PatientID,Study,Gender,Age,Length,Weight,BMI,HospitalSurvival,ICUSurvival,Survival,HospitalAdmissionTime,HospitalDischargeTime,Hospital_length_days, ICUAdmissionTime, ICUDischargeTime, ICU_length_days, InclusionDate, additional_rows, day)) %>% 
  group_by(PatientID) %>%
  summarize(across(c(StudyID,Study,Gender,Age,Length,Weight,BMI,HospitalSurvival,ICUSurvival,Survival,HospitalAdmissionTime,HospitalDischargeTime,Hospital_length_days, ICUAdmissionTime, ICUDischargeTime, ICU_length_days, InclusionDate, additional_rows), first),
            last_cytokine_day=max(day)) %>%
  filter(additional_rows > 0) %>% #not adding any rows for people who have enough records
  uncount(additional_rows) %>% #duplication
  group_by(PatientID)%>%
  mutate(day = ifelse(PatientID %in% patients_no_cytokines, row_number()+1, row_number()+last_cytokine_day)) #patients without cytokines have max d=1, other have max d=7

baki_i <- bind_rows(baki_i, df_new)

#Adding the time. For BAKI-I, the samples are taken at 6am. Time frame is thus 18h00 d-1 --> 18h00 d
baki_i <- baki_i %>%
  mutate(time = InclusionDate + ((day-1)*24*3600)) %>%
  mutate(time = as.POSIXct(format(time, "%Y-%m-%d 06:00"), origin="1970-01-01",tz = "GMT")) %>%
  mutate(time_inf = as.POSIXct(format(time-(24*3600), "%Y-%m-%d 18:00"), "GMT")) %>%
  mutate(time_sup = as.POSIXct(format(time, "%Y-%m-%d 18:00"), "GMT")) %>%
  arrange(as.numeric(substring(StudyID,3)), as.numeric(substring(cytokines_e, 2)))  


####BAKI-A####
baki_a <- baki_a %>%
  mutate(day = ifelse(cytokines_e == "E5" | cytokines_e == "S5", 1,
                      ifelse(cytokines_e == "E6", 2,
                             ifelse(cytokines_e == "E7", 3, 
                                    ifelse(cytokines_e == "E0", 0, NA)))))

patients_no_cytokines <- filter(baki_a, is.na(cytokines_e))$PatientID
baki_a[is.na(baki_a$day),]$day <- 0 #day for patients without cytokines_e

#calculate the number of additional rows for each patient. we want one row per day, beyond the 7 ICU days if they stayed hospitalized.
baki_a <- baki_a %>%
  group_by(PatientID) %>%
  mutate(nr_record = n()) %>% 
  mutate(additional_rows = as.integer(round(first(ICU_length_days))-nr_record)) %>%
  mutate(additional_rows = ifelse(additional_rows > 0, additional_rows, 0))

#Duplicate rows based on the number of days stayed
df_new <- baki_a %>%
  select(c(StudyID,PatientID,Study,Gender,Age,Length,Weight,BMI,HospitalSurvival,ICUSurvival,Survival,HospitalAdmissionTime,HospitalDischargeTime,Hospital_length_days, ICUAdmissionTime, ICUDischargeTime, ICU_length_days, InclusionDate, additional_rows)) %>% 
  group_by(PatientID) %>%
  summarize(across(c(StudyID,Study,Gender,Age,Length,Weight,BMI,HospitalSurvival,ICUSurvival,Survival,HospitalAdmissionTime,HospitalDischargeTime,Hospital_length_days, ICUAdmissionTime, ICUDischargeTime, ICU_length_days, InclusionDate, additional_rows), first)) %>%
  filter(additional_rows > 0) %>% #not adding any rows for people who have enough records
  uncount(additional_rows) %>% #duplication
  group_by(PatientID)%>%
  mutate(day = ifelse(PatientID %in% patients_no_cytokines, row_number()+0, row_number()+3)) #patients without cytokines have max d=0, other have max d=3

baki_a <- bind_rows(baki_a, df_new)

baki_a <- baki_a %>%
  mutate(time = ifelse(day == 0, ICUAdmissionTime, 
                       ifelse(day==1, ICUAdmissionTime+(12*3600),
                              ifelse(day > 1,  ICUAdmissionTime+ ((day-1)*24*3600), NA)))) %>%
  mutate(time = as.POSIXct(time, origin="1970-01-01",tz = "GMT")) %>%
  mutate(time_inf = ifelse(day == 1, time - (6*3600),
                           ifelse(day == 2, time - (6*3600),
                                  ifelse(day >= 3, time - (12*3600), NA)))) %>%
  mutate(time_sup = ifelse(day == 1, time + (6*3600),
                           ifelse(day == 2, time + (12*3600),
                                  ifelse(day >= 3, time + (12*3600), NA)))) %>%
  mutate(time_inf = as.POSIXct(time_inf, origin="1970-01-01",tz = "GMT")) %>%
  mutate(time_sup = as.POSIXct(time_sup, origin="1970-01-01",tz = "GMT")) %>%
  arrange(as.numeric(substring(StudyID,3)), as.numeric(substring(cytokines_e, 2))) %>%
  filter(day > 0) #remove records before surgery


#####SOFA & AKI & Sepsis score####
baki_a_and_i <- bind_rows(baki_i, baki_a)
creat_baseline <- read.delim("original_data/jorien_data.csv",sep=";") %>% #adding creatinine baseline
  select(c(patient, scr_baseline)) %>%
  mutate(patient = paste0("I/",str_sub(patient,start=2)))
baki_a_and_i <- merge(baki_a_and_i, creat_baseline, by.x="StudyID", by.y="patient",all.x=T)

#Create database for consecutive hours below a certain threshold of urinary output
data_urine <- Baki_AllVars %>%
  select(PatientID,VariableName,ObsTime,ObsValue) %>%
  filter(VariableName == "UIT urine")
weight <- Baki_Comorb %>%
  select(PatientID, Weight)
data_urine <- data_urine %>%
  merge(weight, by="PatientID", all.x = T) %>%
  mutate(urine_ml_kg_h = ObsValue/Weight) %>%
  select(c(PatientID,ObsTime,urine_ml_kg_h)) %>%
  arrange(PatientID, ObsTime) %>%
  group_by(PatientID) %>%
  mutate(time_diff = as.numeric(difftime(ObsTime, lag(ObsTime), units = "hours"))) %>%
  ungroup()
data_urine[is.na(data_urine$time_diff),]$time_diff <- 0
data_urine <- data_urine %>%
  mutate(below_threshold_0.5 = ifelse(urine_ml_kg_h < 0.5, 1, 0)) %>%
  mutate(below_threshold_0.3 = ifelse(urine_ml_kg_h < 0.3, 1, 0)) %>%
  mutate(below_threshold_0 = ifelse(urine_ml_kg_h == 0, 1, 0)) %>%
  group_by(PatientID) %>%
  mutate(consecutive_below_threshold_0.5 = ave(time_diff*below_threshold_0.5, cumsum(below_threshold_0.5==0), FUN = cumsum)) %>% #this formulation resets the cumsum if hits zero
  mutate(consecutive_below_threshold_0.3 = ave(time_diff*below_threshold_0.3, cumsum(below_threshold_0.3==0), FUN = cumsum)) %>%
  mutate(consecutive_below_threshold_0 = ave(time_diff*below_threshold_0, cumsum(below_threshold_0==0), FUN = cumsum)) %>%
  ungroup()

#creating database for creatinine relative/absolute changes
data_creat <- Baki_Lab %>%
  select(PatientID,VariableAbbr,SampleTime,LabValue) %>%
  filter(VariableAbbr == "Creatinine-s")

baseline <- unique(select(baki_a_and_i,c(PatientID,scr_baseline)))
data_creat <- merge(data_creat,baseline,by="PatientID")

#check max increase in a window of 48h
max_creatinine_increase_48h = function(row, data_creat){
  patientid <- as.integer(row$PatientID)
  datetime <- as.POSIXct(row$SampleTime)
  samples_within_window <- between(as.numeric(difftime(datetime,data_creat[data_creat$PatientID==patientid,]$SampleTime, units="hours")), 0, 48)
  samples_within_window <- data_creat[data_creat$PatientID==patientid, ][samples_within_window,][["LabValue"]]
  
  if (length(samples_within_window)<=1){return(NA)}
  
  # find max increase in window
  result = max(apply(combn(samples_within_window,2),2,FUN=diff))  
  return(result)
}
data_creat$max_creatinine_increase_previous_48 = NA
for (i in 1:nrow(data_creat)) {
  # Call max_creatinine_increase_48h function for each row
  data_creat$max_creatinine_increase_previous_48[i] <- max_creatinine_increase_48h(data_creat[i, ], data_creat)
}

calculate_sofa_and_aki_baki <- function(data){
  aki_stages <- factor(
    c("AKI Stage 1", "AKI Stage 2", "AKI Stage 3", "No AKI",NA),
    levels = c("No AKI", "AKI Stage 1", "AKI Stage 2", "AKI Stage 3"),
    ordered = TRUE
  )
  # line per line
  pfs <- c()
  respi_sofas <- c()
  thrombos <- c()
  coagu_sofas <- c()
  bilis <- c()
  bilis_max <- c()
  liver_sofas <- c()
  SAPms <- c()
  dopamines <- c()
  dobus <- c()
  norepis <- c()
  epis <- c()
  milris <- c()
  cardio_sofas <- c()
  GCSs <- c()
  CNS_sofas <- c()
  renal_sofas <- c()
  creats <- c()
  total_sofas <- c()
  vasopresses <- c()
  lactates <- c()
  antibiotics <- c()
  ASTs_max <-c()
  ALTs_max <- c()
  tropts_max <- c()
  creats_s_max <-c()
  creats_u_max <-c()
  LDHs_max <- c()
  pfs_max <- c()
  AKIs_creat <- c()
  AKIs_u <- c()
  AKIs_creat_change <- c()
  AKIs_max <- c()
  INRs_max <- c()
  
  pb <- txtProgressBar(min=0,max=nrow(data), style=3)
  for (i in 1:nrow(data)) {
    setTxtProgressBar(pb, i)
    row <- data[i,]
    patientid <- row$PatientID
    CreatBasis <- row$scr_baseline
    time_inf <- row$time_inf
    time_sup <- row$time_sup
    if(is.na(time_inf) | is.na(time_sup)){
      pfs <- append(pfs,NA)
      respi_sofas <- append(respi_sofas,NA)
      thrombos <- append(thrombos,NA)
      coagu_sofas <- append(coagu_sofas,NA)
      bilis <- append(bilis,NA)
      bilis_max <- append(bilis_max, NA)
      liver_sofas <- append(liver_sofas,NA)
      SAPms <- append(SAPms,NA)
      dopamines <- append(dopamines,NA)
      dobus <- append(dobus,NA)
      norepis <- append(norepis,NA)
      epis <- append(epis,NA)
      milris <- append(milris, NA)
      cardio_sofas <- append(cardio_sofas,NA)
      GCSs <- append(GCSs,NA)
      CNS_sofas <- append(CNS_sofas,NA)
      renal_sofas <- append(renal_sofas,NA)
      creats <- append(creats,NA)
      vasopresses <- append(vasopresses,NA)
      lactates <- append(lactates,NA)
      antibiotics <- append(antibiotics,NA)
      ASTs_max <-append(ASTs_max,NA)
      ALTs_max <- append(ALTs_max,NA)
      tropts_max <- append(tropts_max,NA)
      creats_s_max <-append(creats_s_max,NA)
      creats_u_max <-append(creats_u_max,NA)
      pfs_max <- append(pfs_max,NA)
      AKIs_creat <- append(AKIs_creat,NA)
      AKIs_u <- append(AKIs_u,NA)
      AKIs_creat_change <- append(AKIs_creat_change,NA)
      AKIs_max <- append(AKIs_max,NA)
      LDHs_max <- append(LDHs_max,NA)
      INRs_max <- append(INRs_max,NA)
      next
    }
    #Various biomarkers to retrieve
    AST_max <- max(filter(Baki_Lab, VariableAbbr == "AST(GOT)-s" & PatientID==patientid & SampleTime <= time_sup & SampleTime >= time_inf)$LabValue, na.rm=T)
    ALT_max <- max(filter(Baki_Lab, VariableAbbr == "ALT(GPT)-s" & PatientID==patientid & SampleTime <= time_sup & SampleTime >= time_inf)$LabValue, na.rm=T)
    tropt_max <- max(filter(Baki_Lab, VariableAbbr == "TroponineT-s" & PatientID==patientid & SampleTime <= time_sup & SampleTime >= time_inf)$LabValue, na.rm=T)
    creat_s_max <- max(filter(Baki_Lab, VariableAbbr == "Creatinine-s" & PatientID==patientid & SampleTime <= time_sup & SampleTime >= time_inf)$LabValue, na.rm=T)
    creat_u_max <- max(filter(Baki_Lab, VariableAbbr == "Creatinine-u" & PatientID==patientid & SampleTime <= time_sup & SampleTime >= time_inf)$LabValue, na.rm=T)
    pf_max <- max(filter(Baki_VentParams, VariableName == "P/F" & PatientID == patientid & ObsTime <= time_sup & ObsTime >= time_inf)$ObsValue, na.rm=T)
    ASTs_max <-append(ASTs_max,AST_max)
    ALTs_max <- append(ALTs_max,ALT_max)
    tropts_max <- append(tropts_max,tropt_max)
    creats_s_max <-append(creats_s_max,creat_s_max)
    creats_u_max <-append(creats_u_max,creat_u_max)
    pfs_max <- append(pfs_max,pf_max)
    
    #respiratory sofa
    pf <- mean(filter(Baki_VentParams, VariableName == "P/F" & PatientID == patientid & ObsTime <= time_sup & ObsTime >= time_inf)$ObsValue, na.rm=T)
    
    pfs <- append(pfs,pf)
    
    respi_sofa <- ifelse(is.na(pf), NA, 
                         ifelse(pf > 400, 0,
                                ifelse(pf <= 400 & pf > 300, 1,
                                       ifelse(pf <= 300 & pf > 200, 2,
                                              ifelse(pf <= 200 & pf > 100, 3,
                                                     ifelse(pf <= 100, 4, NA))))))
    respi_sofas <- append(respi_sofas,respi_sofa)
    
    #coagulatio sofa
    thrombo <- mean(filter(Baki_Lab, VariableAbbr == "Thrombocyten" & PatientID == patientid & SampleTime <= time_sup & SampleTime >= time_inf)$LabValue, na.rm=T)
    thrombos <- append(thrombos,thrombo)
    coagu_sofa <- ifelse(is.na(thrombo), NA, 
                         ifelse(thrombo > 150, 0,
                                ifelse(thrombo <= 150 & thrombo > 100, 1,
                                       ifelse(thrombo <= 100 & thrombo > 50, 2,
                                              ifelse(thrombo <= 50 & thrombo > 20, 3,
                                                     ifelse(thrombo <= 20, 4, NA))))))
    coagu_sofas <- append(coagu_sofas,coagu_sofa)
    
    #liver sofa
    bili <- mean(filter(Baki_Lab, VariableAbbr == "Bili. tot-s" & PatientID == patientid & SampleTime <= time_sup & SampleTime >= time_inf)$LabValue, na.rm=T)
    bilis <- append(bilis,bili)
    
    bili_max  <- max(filter(Baki_Lab, VariableAbbr == "Bili. tot-s" & PatientID == patientid & SampleTime <= time_sup & SampleTime >= time_inf)$LabValue, na.rm=T)
    
    bilis_max <- append(bilis_max, bili_max)
    
    liver_sofa <- ifelse(is.na(bili), NA, 
                         ifelse(bili <=1.2, 0,
                                ifelse(bili <= 1.9 & bili > 1.2, 1,
                                       ifelse(bili <= 5.9 & bili > 2, 2,
                                              ifelse(bili <= 11.9 & bili > 6, 3,
                                                     ifelse(bili > 12, 4, NA))))))
    liver_sofas <- append(liver_sofas,liver_sofa)
    
    INR_max <- max(filter(Baki_Lab, VariableAbbr == "INR" & PatientID == patientid & SampleTime <= time_sup & SampleTime >= time_inf)$LabValue, na.rm=T)
    INRs_max <- append(INRs_max, INR_max)
    
    #Cardiovascular sofa
    calculate_vaso_data <- function() {
      SAPm <- mean(filter(Baki_AllVars, VariableName == "SAPm" & PatientID == patientid & ObsTime <= time_sup & ObsTime >= time_inf)$ObsValue, na.rm=T)
      dopamine <- mean(filter(Baki_VasoPres, Type == "Dopa" & PatientID == patientid & ObsDate <= time_sup & ObsDate >= time_inf)$AvgRes, na.rm=T)
      dopamine <- ifelse(is.na(dopamine), 0, dopamine)
      dobu <- mean(filter(Baki_VasoPres, Type == "Dobu" & PatientID == patientid & ObsDate <= time_sup & ObsDate >= time_inf)$AvgRes, na.rm=T)
      dobu <- ifelse(is.na(dobu), 0, dobu)
      norepi <- mean(filter(Baki_VasoPres, Type == "Norepi" & PatientID == patientid & ObsDate <= time_sup & ObsDate >= time_inf)$AvgRes, na.rm=T)
      norepi <- ifelse(is.na(norepi), 0, norepi)
      epi <- mean(filter(Baki_VasoPres, Type == "Epi" & PatientID == patientid & ObsDate <= time_sup & ObsDate >= time_inf)$AvgRes, na.rm=T)
      epi <- ifelse(is.na(epi), 0, epi)
      milri <-  mean(filter(Baki_VasoPres, Type == "Milri" & PatientID == patientid & ObsDate <= time_sup & ObsDate >= time_inf)$AvgRes, na.rm=T)
      milri <- ifelse(is.na(milri), 0, milri)
      vaso_data <- c(SAPm, dopamine, dobu, norepi, epi, milri)
      return(vaso_data)
    }
    vaso <- calculate_vaso_data()
    SAPms <- append(SAPms, vaso[1])
    dopamines <- append(dopamines, vaso[2])
    dobus <- append(dobus, vaso[3])
    norepis <- append(norepis, vaso[4])
    epis <- append(epis, vaso[5])
    milris <- append(milris, vaso[6])
    
    cardio_sofa <- case_when(
      vaso[2] > 15 | vaso[5] > 0.1 | vaso[4] > 0.1 ~ 4, 
      vaso[2] > 5 | (vaso[5] <= 0.1 & vaso[5]>0) | (vaso[4] <= 0.1 & vaso[4]>0) ~ 3,
      (vaso[2] <= 5 & vaso[2]>0) | vaso[3]>0 ~ 2,
      vaso[1]< 70 ~ 1,
      vaso[1]>=70 ~ 0,
      TRUE ~ NA
    )

    
    cardio_sofas <- append(cardio_sofas,cardio_sofa)
    
    #CNS sofa
    GCS <- mean(filter(Baki_AllVars,VariableName == "GCS" & PatientID == patientid & ObsTime <= time_sup & ObsTime >= time_inf)$ObsValue, na.rm=T)
    GCSs <- append(GCSs, GCS)
    CNS_sofa <- ifelse(is.na(GCS), NA,
                       ifelse(GCS == 15, 0,
                              ifelse(GCS <= 14 & GCS >= 13, 1,
                                     ifelse(GCS <= 12 & GCS >= 10, 2,
                                            ifelse(GCS <= 9 & GCS >= 6, 3,
                                                   ifelse(GCS < 6, 4, NA))))))
    CNS_sofas <- append(CNS_sofas,CNS_sofa)
    
    #Renal SOFA
    creat <- mean(filter(Baki_Lab, VariableAbbr == "Creatinine-s" & PatientID == patientid & SampleTime <= time_sup & SampleTime >= time_inf)$LabValue, na.rm=T)
    creats <- append(creats,creat)
    renal_sofa <- ifelse(is.na(creat), NA,
                         ifelse(creat < 1.2, 0,
                                ifelse(creat <= 1.9 & creat >= 1.2, 1,
                                       ifelse(creat <= 3.4 & creat >= 2.0, 2,
                                              ifelse(creat <= 4.9 & creat >= 3.5, 3,
                                                     ifelse(creat > 5.0, 4, NA))))))
    renal_sofas <- append(renal_sofas,renal_sofa)
    
    total_sofa <-  cardio_sofa+renal_sofa+respi_sofa+coagu_sofa+CNS_sofa+liver_sofa
    total_sofas <- append(total_sofas,total_sofa)
    
    #AKI
    ##Urine flow
    urine_flow <- filter(data_urine,PatientID==patientid & ObsTime <= time_sup & ObsTime >= time_inf)
    nr_hours_below_0.5 <- max(urine_flow$consecutive_below_threshold_0.5,na.rm=T)
    nr_hours_below_0.3 <- max(urine_flow$consecutive_below_threshold_0.3,na.rm=T)
    nr_hours_below_0 <- max(urine_flow$consecutive_below_threshold_0,na.rm=T)
    AKI_stage_u <- case_when(
      nrow(urine_flow) == 0 ~ last(aki_stages),
      nr_hours_below_0.3 >= 24 | nr_hours_below_0 >= 12 ~ aki_stages[3],
      nr_hours_below_0.5 >= 12 ~ aki_stages[2],
      nr_hours_below_0.5 < 12 & nr_hours_below_0.5 >= 6 ~ aki_stages[1],
      TRUE ~ aki_stages[4]
    )
    AKIs_u <- append(AKIs_u, AKI_stage_u)
    ##s_creat
    creat_max <- max(filter(Baki_Lab, VariableAbbr == "Creatinine-s" & PatientID == patientid & SampleTime <= time_sup & SampleTime >= time_inf)$LabValue, na.rm=T)
    AKI_stage_creat <- case_when(
      creat_max == -Inf ~ data.table::last(aki_stages, na.rm=T),
      creat_max >= 3*CreatBasis | creat_max >= 4 ~ aki_stages[3],
      is.na(CreatBasis) == T ~ data.table::last(aki_stages, na.rm=T),
      creat_max >= 2*CreatBasis & creat_max < 3*CreatBasis ~ aki_stages[2],
      creat_max >= 1.5*CreatBasis & creat_max < 2*CreatBasis ~ aki_stages[1],
      TRUE ~ aki_stages[4]
    )
    AKIs_creat <- append(AKIs_creat,AKI_stage_creat)
    
    ##change in creat
    creat_change <- max(filter(data_creat, PatientID == patientid & SampleTime <= time_sup & SampleTime >= time_inf)$max_creatinine_increase_previous_48, na.rm=T)
    AKI_stage_creat_change <- case_when(
      creat_change == -Inf ~ data.table::last(aki_stages, na.rm=T),
      creat_change >= 0.3 ~ aki_stages[1],
      is.na(creat_change) == T ~ data.table::last(aki_stages, na.rm=T),
      creat_change < 0.3 ~ aki_stages[4],
      TRUE ~ last(aki_stages))
    
    AKIs_creat_change <- append(AKIs_creat_change,AKI_stage_creat_change)
    
    AKI_max <- max(AKI_stage_u,AKI_stage_creat,AKI_stage_creat_change, na.rm=T)
    AKIs_max <- append(AKIs_max,AKI_max)
    
    #additional measures for sepsis
    #need of vasopressor?
    if (any(vaso[2:6]>0)){
      vasopress <- T
    } else{
      vasopress <- F
    }
    vasopresses <- append(vasopresses, vasopress)
    
    #lactate level? mg/dl
    lactate <- max(filter(Baki_Lab,(VariableAbbr == "Lactaat-s" | VariableAbbr == "Lact-BG") & PatientID == patientid & SampleTime <= time_sup & SampleTime >= time_inf)$LabValue, na.rm=T)
    lactates <- append(lactates,lactate)
    
    #LDH levels?
    LDH <- max(filter(Baki_Lab,(VariableAbbr == "LDH-s") & PatientID == patientid & SampleTime <= time_sup & SampleTime >= time_inf)$LabValue, na.rm=T)
    LDHs_max <- append(LDHs_max,LDH)

    
    #antibiotics for infection?
    if (nrow(filter(Baki_Medication, GenericName %in% antibiotics_list & PatientID == patientid & GivenAt <= time_sup & GivenAt >= time_inf))>0) { #if antibiotic has been given
      OrderNumbers <- filter(Baki_Medication, GenericName %in% antibiotics_list & PatientID == patientid & GivenAt <= time_sup & GivenAt >= time_inf)$OrderNumber #check order id
      barids <- filter(Baki_Orders_MedBars, ordernumber %in% OrderNumbers)$barid
      infectionids <- filter(Baki_InfLinks, barid %in% barids)$infectionid
      focuses <- filter(Baki_Infections, infectionid %in% infectionids)$infectionfocuskey
      
      #if all focuses are with the word "profylax..." then it's not infection
      if (all(grepl("PROFYLAXIS", focuses)) == TRUE){ #returns TRUE if all focuses have the word profylax
        antibiotic <- FALSE
      } 
      else {
        antibiotic <- TRUE 
      }
    } else { #if no antibiotic has been given
      antibiotic <- FALSE
    }
    
    antibiotics <- append(antibiotics,antibiotic)
    
  }
  
  additional_data <- data.frame(mean_pf = pfs, respi_sofa = respi_sofas,
                                mean_thrombo = thrombos, coagu_sofa = coagu_sofas,
                                mean_bili = bilis, max_bili = bilis_max, liver_sofa = liver_sofas,
                                mean_SAPm = SAPms, mean_dopamine = dopamines, mean_dobu = dobus, mean_norepi = norepis, mean_epi = epis, mean_milri = milris, cardio_sofa = cardio_sofas,
                                mean_GCS = GCSs, CNS_sofa = CNS_sofas,
                                mean_creat = creats, renal_sofa = renal_sofas,
                                vasopress = vasopresses,
                                lactate = lactates,
                                antibiotic = antibiotics,
                                AST_max = ASTs_max,
                                ALT_max = ALTs_max,
                                tropt_max = tropts_max,
                                creat_s_max = creats_s_max,
                                creat_u_max = creats_u_max,
                                pf_max = pfs_max,
                                AKI_creat_baseline_abs = AKIs_creat,
                                AKI_u = AKIs_u,
                                AKI_creat_rel = AKIs_creat_change,
                                AKI = AKIs_max,
                                LDH_max = LDHs_max,
                                INR_max = INRs_max)
  return(bind_cols(data,additional_data))
}

baki_a_and_i <- baki_a_and_i %>%
  filter(Study == "BAKI-I" & StudyID %in% Baki_Cytokines$StudyID)


baki_a_and_i_temp <- calculate_sofa_and_aki_baki(baki_a_and_i)
baki_a_and_i <- baki_a_and_i_temp

#Patient 38522 is an edge-case for which AKI on day 1 can't be automatically computed because of the way time_inf and time_sup are defined
#For the demographic table, AKI on day 1 can still be calculated manually. The patient arrived at 5pm and the first AKI diagnosis can be given at 8pm.
#This diagnosis is "No AKI" based on urinary flow and creatinine relative to baseline.
baki_a_and_i[baki_a_and_i$PatientID==38522 & baki_a_and_i$day==1,]$AKI <- as.factor("No AKI")


# Impute missing SOFAs and missing lactate
baki_a_and_i <- baki_a_and_i %>%
  ungroup() %>%
  group_by(PatientID) %>%
  arrange(day, .by_group = T)

baki_a_and_i[baki_a_and_i==-Inf] <- NA
data <- baki_a_and_i
to_be_imputed <- c("cardio_sofa", "renal_sofa",
                   "respi_sofa", "coagu_sofa", 
                   "CNS_sofa","liver_sofa", "lactate", "mean_pf")
for (id in unique(data$PatientID)) {
  current_idx <- which(data$PatientID==id)
  #check for missing values in each SOFA
  for(sofa in to_be_imputed){
    if(sum(is.na(data[[sofa]][current_idx]))>0 & length(current_idx) - sum(is.na(data[[sofa]][current_idx])) >= 2){
      #if there are missing values but at least 2 non-missing are present
      print(paste(id,sofa))
      initial_values <- data[[sofa]][current_idx]
      if(sofa=="lactate" | sofa=="mean_pf"){
        data[[sofa]][current_idx] <- round(imputeTS::na_ma(initial_values, k = 2, maxgap = 3, weighting = "linear"),2)
      } else{
        data[[sofa]][current_idx] <- round(imputeTS::na_ma(initial_values, k = 2, maxgap = 3, weighting = "linear"))
      }
      final_values <- data[[sofa]][current_idx]
      # say what happened
      print(paste("Values were changed from"))
      print(initial_values)
      print("to")
      print(final_values)
    } else if (sum(is.na(data[[sofa]][current_idx]))>0){
      print(paste("Not possible to infer missing values from", id, sofa, "with the following serie:"))
      print(data[[sofa]][current_idx]) }
    
    #if there are missing values and only 1 or less values are non-missing, we keep NA
  }
}
#visualization:
s <- gg_miss_var(baki_a_and_i[34:63],show_pct = T)
s <- gg_miss_var(data[34:63],show_pct = T)
vis_miss(baki_a_and_i)
vis_miss(data[to_be_imputed])

baki_a_and_i <- data
baki_a_and_i <- baki_a_and_i %>%
  ungroup() %>%
  mutate_at(vars(all_of(c("cardio_sofa", "renal_sofa",
                          "respi_sofa", "coagu_sofa", 
                          "CNS_sofa","liver_sofa"))), ~ifelse(. < 0, 0, .)) %>%
  mutate_at(vars(all_of(c("cardio_sofa", "renal_sofa",
                          "respi_sofa", "coagu_sofa", 
                          "CNS_sofa","liver_sofa"))), ~ifelse(. > 4, 4, .)) %>%
  mutate(total_sofa = rowSums(select(., c("cardio_sofa", "renal_sofa",
                                          "respi_sofa", "coagu_sofa", 
                                          "CNS_sofa","liver_sofa"))))

# Calculate Sepsis score
#bSuspected infection could be defined as the concomitant administration of oral or parenteral antibiotics and sampling of body fluid cultures (blood, urine, cerebrospinal fluid, peritoneal, etc). For example, if the culture is obtained, the antibiotic is required to be administered within 72 hours, whereas if the antibiotic is first, the culture is required within 24 hours.12"

calculate_sepsis_baki <- function(data){
  
  data <- data %>%
    #antibiotics 2 days before/after?
    group_by(PatientID) %>%
    mutate(
      antibiotic_past_or_in_2days = case_when(
        row_number() == 1 &  row_number() == n() ~ antibiotic,
        row_number() == 1 & row_number() == n() - 1 ~ antibiotic |  lead(antibiotic),
        row_number() == 2 & row_number() == n()  ~ antibiotic |  lag(antibiotic),
        row_number() == 1 ~ antibiotic | lead(antibiotic, n=2)| lead(antibiotic),
        row_number() == 2 ~ antibiotic | lead(antibiotic, n=2)| lead(antibiotic) | lag(antibiotic),
        row_number() == n() ~ antibiotic | lag(antibiotic) | lag(antibiotic,n=2),
        row_number() == n() - 1 ~ antibiotic | lag(antibiotic) | lag(antibiotic,n=2)| lead(antibiotic),
        TRUE ~ antibiotic | lag(antibiotic) | lag(antibiotic,n=2)| lead(antibiotic) | lead(antibiotic, n=2)
      )
    )
  data <- data %>%
    mutate(sepsis = case_when(
      is.na(total_sofa) ~ NA,
      total_sofa >= 2 & antibiotic_past_or_in_2days==1 & vasopress & lactate/9 > 2 ~ "septic_shock",
      total_sofa >= 2 & antibiotic_past_or_in_2days==1 ~ "sepsis",
      TRUE ~ "no_sepsis"
    )) 
  return(data)
}

baki_a_and_i <- calculate_sepsis_baki(baki_a_and_i)
write.csv(baki_a_and_i,"original_data/own_data.csv",row.names = F)

