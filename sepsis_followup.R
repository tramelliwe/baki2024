data_graph <- data_orig %>%
  select(c(PatientID,Days,sepsis)) %>%
  ungroup()

other <- data_orig %>%
  select(c(PatientID,Days,sepsis)) %>%
  group_by(PatientID) %>%
  summarize(max_sepsis = max(sepsis, na.rm=T),
            sepsis_d1 = first(sepsis, na.rm=T))

patient_data <- data_orig %>%
  group_by(PatientID) %>%
  slice(1:3) %>%
  summarize(max_sepsis_d123 = max(sepsis, na.rm=T))

other <- merge(other,patient_data,by="PatientID")
other <- na.exclude(other)
other$d1_highest <- ifelse(other$sepsis_d1==other$max_sepsis,1,0)
other$d123_highest <- ifelse(other$max_sepsis_d123==other$max_sepsis,1,0)
other$d1_lower <- ifelse(other$sepsis_d1<other$max_sepsis,1,0)
other$d123_lower <- ifelse(other$max_sepsis_d123<other$max_sepsis,1,0)

d1_lower <- other[other$d1_lower==1,]$PatientID
d123_lower <- other[other$d123_lower==1,]$PatientID
d1_highest <- other[other$d1_highest==1,]$PatientID

data_d1_lower <- data_graph %>%
  filter(PatientID %in% d1_lower) %>%
  select(-PatientID)
data_d123_lower <- data_graph %>%
  filter(PatientID %in% d123_lower) %>%
  select(-PatientID)
data_d1_highest <- data_graph %>%
  filter(PatientID %in% d1_highest) %>%
  select(-PatientID)
data_all <- data_graph %>%
  select(-PatientID)


data_d123_lower <- as.data.frame.matrix(table(data_d123_lower))
data_d123_lower$Days <- as.numeric(row.names(data_d123_lower))
data_d123_lower <- data_d123_lower %>%
  pivot_longer(cols=c("no_sepsis","sepsis","septic_shock"))
data_d123_lower$Stage <- factor(data_d123_lower$name, levels=c("no_sepsis","sepsis","septic_shock"),
                           ordered = T)

data_d1_lower <- as.data.frame.matrix(table(data_d1_lower))
data_d1_lower$Days <- as.numeric(row.names(data_d1_lower))
data_d1_lower <- data_d1_lower %>%
  pivot_longer(cols=c("no_sepsis","sepsis","septic_shock"))
data_d1_lower$Stage <- factor(data_d1_lower$name, levels=c("no_sepsis","sepsis","septic_shock"),
                              ordered = T)

data_d1_highest <- as.data.frame.matrix(table(data_d1_highest))
data_d1_highest$Days <- as.numeric(row.names(data_d1_highest))
data_d1_highest <- data_d1_highest %>%
  pivot_longer(cols=c("no_sepsis","sepsis","septic_shock"))
data_d1_highest$Stage <- factor(data_d1_highest$name, levels=c("no_sepsis","sepsis","septic_shock"),
                                ordered = T)

data_all <- as.data.frame.matrix(table(data_all))
data_all$Days <- as.numeric(row.names(data_all))
data_all <- data_all %>%
  pivot_longer(cols=c("no_sepsis","sepsis","septic_shock"))
data_all$Stage <- factor(data_all$name, levels=c("no_sepsis","sepsis","septic_shock"),
                         ordered = T)


ggplot(data=data_d123_lower)+
  geom_stream(mapping = aes(x=Days, y=value, fill=(Stage)), type = "ridge")+
  ggtitle(paste0("Distribution of sepsis stage over ICU days for \n patient whose max stage is after 3 days (n=",length(d123_lower),")"))+
  scale_fill_manual(values = palette_tricolor)+
  labs(fill = "Sepsis stage")+
  scale_y_continuous(breaks=seq(0,length(d123_lower),10))+
  scale_x_continuous(breaks = seq(0,40,5),limits = c(0,40))+
  xlab("Days")+
  ylab("Amount patients")+
  theme_classic()

ggplot(data=data_d1_lower)+
  geom_stream(mapping = aes(x=Days, y=value, fill=(Stage)), type = "ridge")+
  xlab("Days")+
  ylab("Amount patients")+
  ggtitle(paste0("Distribution of sepsis stage over ICU days for \n patient whose max stage is not on day 1 (n=",length(d1_lower),")"))+
  scale_fill_manual(values = palette_tricolor)+
  labs(fill = "Sepsis stage")+
  scale_y_continuous(breaks=seq(0,length(d1_lower),10))+
  scale_x_continuous(breaks = seq(0,40,5), limits = c(0,40))+
  theme_classic()

ggplot(data=data_d1_highest)+
  geom_stream(mapping = aes(x=Days, y=value, fill=(Stage)), type = "ridge")+
  xlab("Days")+
  ylab("Amount patients")+
  ggtitle(paste0("Distribution of sepsis stage over ICU days for \n patient whose max stage is on day 1 (n=",length(d1_highest),")"))+
  scale_fill_manual(values = palette_tricolor)+
  labs(fill = "Sepsis stage")+
  scale_y_continuous(breaks=seq(0,length(d1_highest),20))+
  scale_x_continuous(breaks = seq(0,40,5), limits = c(0,40))+
  theme_classic()

plot_sepsis_all <- ggplot(data=data_all)+
  geom_stream(mapping = aes(x=Days, y=value, fill=(Stage)), type = "ridge")+
  xlab("Days")+
  ylab("Amount patients")+
  ggtitle(paste0("Distribution of sepsis stage over ICU days for \n all patients (n=",length(other$PatientID),")"))+
  scale_fill_manual(values = palette_tricolor)+
  labs(fill = "Sepsis stage")+
  scale_y_continuous(breaks=seq(0,length(other$PatientID),20))+
  scale_x_continuous(breaks = seq(0,40,5), limits = c(0,40))+
  theme_classic()



