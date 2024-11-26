data_graph <- data_orig %>%
  #filter(Days < 8) %>%
  ungroup() %>%
  select(c(StudyID,Days,AKI))

  


other <- data_orig %>%
  ungroup() %>%
  select(c(StudyID,Days,AKI)) %>%
  group_by(StudyID) %>%
  summarize(max_AKI = max(AKI, na.rm=T),
            AKI_d1 = first(AKI, na.rm=T))

patient_data <- data_orig %>%
  ungroup() %>%
  group_by(StudyID) %>%
  slice(1:3) %>%
  summarize(max_AKI_d123 = max(AKI, na.rm=T))

other <- merge(other,patient_data,by="StudyID")
other <- na.exclude(other)
other$d1_highest <- ifelse(other$AKI_d1==other$max_AKI,1,0)
other$d123_highest <- ifelse(other$max_AKI_d123==other$max_AKI,1,0)
other$d1_lower <- ifelse(other$AKI_d1<other$max_AKI,1,0)
other$d123_lower <- ifelse(other$max_AKI_d123<other$max_AKI,1,0)

d1_lower <- other[other$d1_lower==1,]$StudyID
d123_lower <- other[other$d123_lower==1,]$StudyID
d1_highest <- other[other$d1_highest==1,]$StudyID

data_d1_lower <- data_graph %>%
  filter(StudyID %in% d1_lower) %>%
  select(-StudyID)
data_d123_lower <- data_graph %>%
  filter(StudyID %in% d123_lower) %>%
  select(-StudyID)
data_d1_highest <- data_graph %>%
  filter(StudyID %in% d1_highest) %>%
  select(-StudyID)
data_all <- data_graph %>%
  select(-StudyID)


data_d123_lower <- as.data.frame.matrix(table(data_d123_lower))
data_d123_lower$Days <- as.numeric(row.names(data_d123_lower))
data_d123_lower <- data_d123_lower %>%
  pivot_longer(cols=c("No_AKI","AKI Stage 1","AKI Stage 2","AKI Stage 3"))
data_d123_lower$Stage <- factor(data_d123_lower$name, levels=c("No_AKI","AKI Stage 1","AKI Stage 2","AKI Stage 3"), labels = c("No_AKI", "Stage 1","Stage 2", "Stage 3"),
                                ordered = T)

data_d1_lower <- as.data.frame.matrix(table(data_d1_lower))
data_d1_lower$Days <- as.numeric(row.names(data_d1_lower))
data_d1_lower <- data_d1_lower %>%
  pivot_longer(cols=c("No_AKI","AKI Stage 1","AKI Stage 2","AKI Stage 3"))
data_d1_lower$Stage <- factor(data_d1_lower$name, levels=c("No_AKI","AKI Stage 1","AKI Stage 2","AKI Stage 3"),labels = c("No_AKI", "Stage 1","Stage 2", "Stage 3"),
                              ordered = T)

data_d1_highest <- as.data.frame.matrix(table(data_d1_highest))
data_d1_highest$Days <- as.numeric(row.names(data_d1_highest))
data_d1_highest <- data_d1_highest %>%
  pivot_longer(cols=c("No_AKI","AKI Stage 1","AKI Stage 2","AKI Stage 3"))
data_d1_highest$Stage <- factor(data_d1_highest$name, levels=c("No_AKI","AKI Stage 1","AKI Stage 2","AKI Stage 3"),labels = c("No_AKI", "Stage 1","Stage 2", "Stage 3"),
                                ordered = T)

data_all <- as.data.frame.matrix(table(data_all))
data_all$Days <- as.numeric(row.names(data_all))
data_all <- data_all %>%
  pivot_longer(cols=c("No_AKI","AKI Stage 1","AKI Stage 2","AKI Stage 3"))
data_all$Stage <- factor(data_all$name, levels=c("No_AKI","AKI Stage 1","AKI Stage 2","AKI Stage 3"),
                         ordered = T)


ggplot(data=data_d123_lower)+
  geom_stream(mapping = aes(x=Days, y=value, fill=(Stage)), type = "ridge")+
  xlab("Days")+
  ylab("Amount patients")+
  ggtitle(paste0("Distribution of AKI stage over ICU days for \n patient whose max stage is after 3 days (n=",length(d123_lower),")"))+
  scale_fill_manual(values = palette_4_color)+
  labs(fill = "AKI stage")+
  scale_y_continuous(breaks=seq(0,length(d123_lower),10))+
  scale_x_continuous(breaks = seq(0,40,5), limits = c(0,40))+
  theme_classic()

ggplot(data=data_d1_lower)+
  geom_stream(mapping = aes(x=Days, y=value, fill=(Stage)), type = "ridge")+
  xlab("Days")+
  ylab("Amount patients")+
  ggtitle(paste0("Distribution of AKI stage over ICU days for \n patient whose max stage is not on day 1 (n=",length(d1_lower),")"))+
  scale_fill_manual(values = palette_4_color)+
  labs(fill = "AKI stage")+
  scale_y_continuous(breaks=seq(0,length(d1_lower),10))+
  scale_x_continuous(breaks = seq(0,40,5), limits = c(0,40))+
  theme_classic()

ggplot(data=data_d1_highest)+
  geom_col(mapping = aes(x=Days, y=value, fill=(Stage)), type = "ridge")+
  xlab("Days")+
  ylab("Amount patients")+
  ggtitle(paste0("Distribution of AKI stage over ICU days for \n patient whose max stage is on day 1 (n=",length(d1_highest),")"))+
  scale_fill_manual(values = palette_4_color)+
  labs(fill = "AKI stage")+
  scale_y_continuous(breaks=seq(0,length(d1_highest),20))+
  scale_x_continuous(breaks = seq(0,40,5), limits = c(0,40))+
  theme_classic()

plot_AKI_all <- ggplot(data=data_all)+
  geom_stream(mapping = aes(x=Days, y=value, fill=(Stage)), type = "ridge")+
  xlab("Days")+
  ylab("Amount patients")+
  ggtitle(paste0("Distribution of AKI stage over ICU days for \n all patients (n=",length(other$StudyID),")"))+
  scale_fill_manual(values = palette_4_color)+
  labs(fill = "AKI stage")+
  scale_y_continuous(breaks=seq(0,length(other$StudyID),20))+
  scale_x_continuous(breaks = seq(0,40,5), limits = c(0,40))+
  theme_classic()

