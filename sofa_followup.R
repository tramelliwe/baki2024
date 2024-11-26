data_graph <- data_orig %>%
  select(c(PatientID,Days,Sofa_score)) %>%
  mutate(SOFA_Category = case_when(
    Sofa_score == 0 ~ "0",
    Sofa_score > 0 & Sofa_score <= 5 ~ "1-5",
    Sofa_score > 5 & Sofa_score <= 10 ~ "6-10",
    Sofa_score > 10 & Sofa_score <= 15 ~ "11-15",
    Sofa_score > 15 ~ ">15",
    TRUE ~ NA
  )) %>%
  ungroup()
# Apply categorization to dataframe
data_graph$SOFA_Category <- factor(data_graph$SOFA_Category, levels = c("0","1-5","6-10","11-15",">15"), ordered = T)

other <- data_graph %>%
  #filter(Days < 8) %>%
  select(c(PatientID,Days,SOFA_Category)) %>%
  group_by(PatientID) %>%
  summarize(max_Sofa = max(SOFA_Category, na.rm=T),
            Sofa_d1 = first(SOFA_Category, na.rm=T))

patient_data <- data_graph %>%
  group_by(PatientID) %>%
  slice(1:3) %>%
  summarize(max_Sofa_d123 = max(SOFA_Category, na.rm=T))

other <- merge(other,patient_data,by="PatientID")
other <- na.exclude(other)
other$d1_highest <- ifelse(other$Sofa_d1==other$max_Sofa,1,0)
other$d123_highest <- ifelse(other$max_Sofa_d123==other$max_Sofa,1,0)
other$d1_lower <- ifelse(other$Sofa_d1<other$max_Sofa,1,0)
other$d123_lower <- ifelse(other$max_Sofa_d123<other$max_Sofa,1,0)

d1_lower <- other[other$d1_lower==1,]$PatientID
d123_lower <- other[other$d123_lower==1,]$PatientID
d1_highest <- other[other$d1_highest==1,]$PatientID

data_d1_lower <- data_graph %>%
  filter(PatientID %in% d1_lower) %>%
  select(-c(PatientID,Sofa_score))
data_d123_lower <- data_graph %>%
  filter(PatientID %in% d123_lower) %>%
  select(-c(PatientID,Sofa_score))
data_d1_highest <- data_graph %>%
  filter(PatientID %in% d1_highest) %>%
  select(-c(PatientID,Sofa_score))
data_all <- data_graph %>%
  select(-c(PatientID,Sofa_score))


data_d123_lower <- as.data.frame.matrix(table(data_d123_lower))
data_d123_lower$Days <- as.numeric(row.names(data_d123_lower))
data_d123_lower <- data_d123_lower %>%
  pivot_longer(cols=c("0","1-5","6-10","11-15",">15"))
data_d123_lower$Stage <- factor(data_d123_lower$name, levels=c("0","1-5","6-10","11-15",">15"),
                                ordered = T)

data_d1_lower <- as.data.frame.matrix(table(data_d1_lower))
data_d1_lower$Days <- as.numeric(row.names(data_d1_lower))
data_d1_lower <- data_d1_lower %>%
  pivot_longer(cols=c("0","1-5","6-10","11-15",">15"))
data_d1_lower$Stage <- factor(data_d1_lower$name, levels=c("0","1-5","6-10","11-15",">15"),
                              ordered = T)

data_d1_highest <- as.data.frame.matrix(table(data_d1_highest))
data_d1_highest$Days <- as.numeric(row.names(data_d1_highest))
data_d1_highest <- data_d1_highest %>%
  pivot_longer(cols=c("0","1-5","6-10","11-15",">15"))
data_d1_highest$Stage <- factor(data_d1_highest$name, levels=c("0","1-5","6-10","11-15",">15"),
                                ordered = T)

data_all <- as.data.frame.matrix(table(data_all))
data_all$Days <- as.numeric(row.names(data_all))
data_all <- data_all %>%
  pivot_longer(cols=c("0","1-5","6-10","11-15",">15"))
data_all$Stage <- factor(data_all$name, levels=c("0","1-5","6-10","11-15",">15"),
                         ordered = T)

ggplot(data=data_d123_lower)+
  geom_stream(mapping = aes(x=Days, y=value, fill=(Stage)), type="ridge")+
  xlab("Days")+
  ylab("Amount patients")+
  ggtitle(paste0("Distribution of SOFA score over ICU days for \n patient whose max score is not in the first 3 days (n=",length(d123_lower),")"))+
  scale_fill_manual(values = palette_5_color)+
  labs(fill = "SOFA score")+
  scale_y_continuous(breaks=seq(0,length(d123_lower),10))+
  scale_x_continuous(breaks = seq(0,40,5), limits = c(0,40))+
  theme_classic()

ggplot(data=data_d1_lower)+
  geom_stream(mapping = aes(x=Days, y=value, fill=(Stage)), type="ridge")+
  xlab("Days")+
  ylab("Amount patients")+
  ggtitle(paste0("Distribution of SOFA score over ICU days for \n patient whose max score is not on day 1 (n=",length(d1_lower),")"))+
  scale_fill_manual(values = palette_5_color)+
  labs(fill = "SOFA score")+
  scale_y_continuous(breaks=seq(0,length(d1_lower),10))+
  scale_x_continuous(breaks = seq(0,40,5), limits = c(0,40))+
  theme_classic()

if (sum(data_d1_highest[data_d1_highest$Stage==">15",]$value) == 0){
  data_d1_highest <- filter(data_d1_highest, Stage != ">15")
} #bug in geom_stream if not ">15" are present
ggplot(data=data_d1_highest)+
  geom_stream(mapping = aes(x=Days, y=value, fill=(Stage)), type="ridge")+
  xlab("Days")+
  ylab("Amount patients")+
  ggtitle(paste0("Distribution of SOFA score over ICU days for \n patient whose max score is on day 1 (n=",length(d1_highest),")"))+
  scale_fill_manual(values = palette_5_color)+
  labs(fill = "SOFA score")+
  scale_y_continuous(breaks=seq(0,length(d1_highest),20))+
  scale_x_continuous(breaks = seq(0,40,5), limits = c(0,40))+
  theme_classic()
  
plot_sofa_all <- ggplot(data=data_all)+
  geom_stream(mapping = aes(x=Days, y=value, fill=(Stage)), type="ridge")+
  xlab("Days")+
  ylab("Amount patients")+
  ggtitle(paste0("Distribution of SOFA score over ICU days for \n all patients (n=",length(other$PatientID),")"))+
  scale_fill_manual(values = palette_5_color)+
  labs(fill = "SOFA score")+
  scale_y_continuous(breaks=seq(0,length(other$PatientID),20))+
  scale_x_continuous(breaks = seq(0,40,5), limits = c(0,40))+
  theme_classic()

