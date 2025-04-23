#Run Packages & data + Load data & formatting from main


# Threshold stratification data formatting -----
pats <- data %>%
  select(c(PatientID, survival_30d, cytokines_il18_max_d17, cytokines_il1ra_max_d17, MDA_max_d17)) %>%
  mutate(il18_pos = ifelse(cytokines_il18_max_d17 >= 6.6, 1, 0),
         MDA_pos = ifelse(MDA_max_d17 >= 3.93, 1, 0), #based on density for deceased group
         il1ra_pos = ifelse(cytokines_il1ra_max_d17 > 0, 1, 0))
pats <- pats[complete.cases(pats$cytokines_il18_max_d17),]



il1ra_peak <- filter(data,is.na(cytokines_il1ra_max_d17)!=T)
il1ra_peak_pats <- il1ra_peak[il1ra_peak$cytokines_il1ra_max_d17>0,]$PatientID
il1ra_nopeak_pats <- il1ra_peak[il1ra_peak$cytokines_il1ra_max_d17==0,]$PatientID

max_day <- data_orig %>%
  filter(Days<8) %>%
  group_by(PatientID) %>%
  reframe(max_day = which.max(cytokines_il1ra),
          total_day = max(Days, na.rm=T))

max_day <- merge(max_day,data_orig,by="PatientID",all.y=T)
max_day <- max_day %>%
  filter(Days<8) %>%
  group_by(PatientID) %>%
  mutate(day_aj=Days-max_day) %>%
  ungroup()



max_day <- merge(max_day, pats, by="PatientID", all.x=T)
max_day$day_aj <- ifelse(max_day$il1ra_pos==0, max_day$Days-1, max_day$day_aj)
max_day$day_aj_centered <- ifelse(max_day$il1ra_pos==0, max_day$Days-round(max_day$total_day/2), max_day$day_aj)
data_graph <- max_day
data_graph$PatientID <- as.factor(data_graph$PatientID)
data_graph$day_aj_fac <- as.factor(data_graph$day_aj)
data_graph <- filter(data_graph,is.na(cytokines_il1ra)==F)

data_graph <- data_graph %>%
  group_by(PatientID) %>%
  mutate(cytokines_il18_z=(cytokines_il18-mean(cytokines_il18))/(sd(cytokines_il18))) %>%
  mutate(cytokines_il1ra_z=(cytokines_il1ra-mean(cytokines_il1ra))/(sd(cytokines_il1ra))) %>%
  mutate(MDA_z=(MDA-mean(MDA))/(sd(MDA))) %>%
  arrange(PatientID,day_aj) %>%
  ungroup()
data_graph[is.na(data_graph$cytokines_il1ra_z),]$cytokines_il1ra_z <- 0
data_graph_unique <- data_graph[!duplicated(data_graph$PatientID),]




#create il1ra/mda/il18 trajectories for specific categories with death count
plotSpecCat <- function(data, filter_conditions){
  condition_expr <- rlang::parse_expr(filter_conditions)
  data_graph_cond <- filter(data, !!condition_expr)
  
  data_graph_cond_temp <- data_graph_cond %>%
    filter(interval<=7 & survival_30d=="Deceased") %>%
    arrange(PatientID,day_aj)%>%
    group_by(PatientID) %>%
    summarize(last_day=max(day_aj))
  
  data_graph_cond <- merge(data_graph_cond, data_graph_cond_temp, by="PatientID", all.x=T)
  data_graph_cond_unique <- data_graph_cond[!duplicated(data_graph_cond$PatientID), ]
  lastdays <- data_graph_cond_unique %>%
    group_by(last_day) %>%
    summarize(count=n())
  lastdays$count <- cumsum(lastdays$count)
  
  quant <- data_graph_cond %>%
    group_by(day_aj) %>%
    reframe(across(all_of(c("cytokines_il1ra","cytokines_il18","MDA")), \(x) quantile(log2(x+1),na.rm=T)[2], .names = "{.col}-25"),
            across(all_of(c("cytokines_il1ra","cytokines_il18","MDA")), \(x) quantile(log2(x+1),na.rm=T)[3], .names = "{.col}-50"),
            across(all_of(c("cytokines_il1ra","cytokines_il18","MDA")), \(x) quantile(log2(x+1),na.rm=T)[4], .names = "{.col}-75"),
            across(all_of(c("cytokines_il1ra","cytokines_il18","MDA")), \(x) max(log2(x+1),na.rm=T), .names = "{.col}-100")) %>%
    pivot_longer(cols=-c(day_aj),
                 names_to = c("cytokine","quantile"),
                 names_sep="-") %>%
    pivot_wider(names_from = quantile,
                values_from = value,
                names_prefix = "value_")
  quant <- merge(quant, lastdays, by.x="day_aj",by.y="last_day", all.x=T)
  quant$count[1] <- 0
  while(sum(is.na(quant$count)>0)){
    quant$count <- ifelse(is.na(quant$count)==T, lag(quant$count), quant$count)
  }
  
  #graph
  p <- ggplot(quant,aes(x=day_aj))+
    geom_ribbon(aes(ymin = value_25, ymax=value_75, fill = cytokine), alpha = 0.5)+
    geom_point(data = data_graph_cond, aes(y=log2(cytokines_il1ra+1), fill="cytokines_il1ra"),col="darkgreen",alpha=0.5, size = 0.5, position = position_jitter(seed = 123, height=0.2, width=0.1))+
    geom_point(data =data_graph_cond,aes(y=log2(cytokines_il18+1), fill="cytokines_il18"),col="red",alpha=0.5, size = 0.5, position = position_jitter(seed = 123, height=0.2, width=0.1))+
    geom_point(data = data_graph_cond,aes(y=log2(MDA+1), fill="MDA"),col="blue",alpha=0.5,size = 0.5, position = position_jitter(seed = 123, height=0.2, width=0.1))+
    geom_line(data = data_graph_cond, aes(group=PatientID,y=log2(cytokines_il1ra+1), fill="cytokines_il1ra"),col="darkgreen",alpha=0.1, position = position_jitter(seed = 123, height=0.2, width=0.1))+
    geom_line(data = data_graph_cond, aes(group=PatientID,y=log2(cytokines_il18+1), fill="cytokines_il18"),col="red",alpha=0.1, position = position_jitter(seed = 123, height=0.2, width=0.1))+
    geom_line(data = data_graph_cond, aes(group=PatientID,y=log2(MDA+1), fill="MDA"),col="blue",alpha=0.1, position = position_jitter(seed = 123, height=0.2, width=0.1))+
    geom_line(aes(y=count))+
    ylab("Log2-transformed values")+
    
    ggtitle(filter_conditions)+
    scale_x_continuous("Day relative to IL1ra peak",labels=quant$day_aj, breaks=quant$day_aj)+
    scale_y_continuous(sec.axis=sec_axis(trans=~.*1, name="Cumulative number of deaths"), limits = c(-0.5, 18))+
    ggthemes::theme_few()
  
  return(p)
}







# Optimized stratification data formatting ----
longi_data <- data_graph %>%
  filter(day_aj>=0) %>%
  select(c(PatientID,day_aj,cytokines_il18_z)) %>%
  arrange(day_aj) %>%
  pivot_wider(names_from=day_aj,
              values_from=cytokines_il18_z,
              names_prefix="day")
pats <- longi_data$PatientID


sync_beginning_il18_results <- read.delim("sync_beginning_il18_results_230724-C2-1-Clusters.csv",sep=";") #1 is decreasing, 2 is increasing
#See kml3d_v2.R for the clustering methodology

sync_beginning_il18_results$idAll <- pats
colnames(sync_beginning_il18_results) <- c("PatientID","il18_pos_kml")
sync_beginning_il18_results$il18_pos_kml <- sync_beginning_il18_results$il18_pos_kml-1

no_sync_MDA_results <- read.delim("no_sync_MDA_results-C2-1-Clusters.csv",sep=";")
no_sync_MDA_results$idAll <- pats
colnames(no_sync_MDA_results) <- c("PatientID","MDA_pos_kml")

data_graph <- merge(data_graph, sync_beginning_il18_results, by="PatientID", all.x=T)
data_graph <- merge(data_graph, no_sync_MDA_results, by="PatientID", all.x=T)

#15 patients have NA cluster --> replace by classical threshold
data_graph$il18_pos_kml <- ifelse(is.na(data_graph$il18_pos_kml)==T, data_graph$il18_pos, data_graph$il18_pos_kml)
data_graph_unique <- data_graph[!duplicated(data_graph$PatientID),]

# plots ----
# Figure A: Count of patients patients per cat ----
plotCountperCat <- function(data_unique, CatVarNames){
  #CatVarNames stores in a vector the name of the three 1/0 variables
  x <- list(data_unique[pull(data_unique, CatVarNames[1])==1,]$PatientID,data_unique[pull(data_unique, CatVarNames[2])==1,]$PatientID,data_unique[pull(data_unique, CatVarNames[3])==1,]$PatientID)
  names(x) <- CatVarNames
  tripleNegCount <- nrow(data_unique[pull(data_unique, CatVarNames[1])==0 & pull(data_unique, CatVarNames[2])==0 & pull(data_unique, CatVarNames[3])==0,])
  p <- ggVennDiagram(x,label="count")+scale_fill_gradient(low="white", high="darkblue")+
    theme(legend.position="none")+
    ggtitle("Patients per category",subtitle = paste("(Excluding", tripleNegCount, "triple negative patients)"))
  return(p)
}
#plotCountperCat(data_graph_unique, c("il1ra_pos","MDA_pos","il18_pos"))
figA <- plotCountperCat(data_graph_unique, c("il1ra_pos","MDA_pos","il18_pos_kml"))

# Figure B: Mortality per cat ----
#table(select(data_graph_unique, c(survival_30d, il1ra_pos, il18_pos, MDA_pos)))
table(select(data_graph_unique, c(survival_30d, il1ra_pos, il18_pos_kml, MDA_pos)))
# Make figure manually

# Figure C & D: IL18+/- IL1ra+ trajectories
graph_il1rapos_il18pos <- plotSpecCat(data_graph, "il1ra_pos==1 & il18_pos==1")
figC <- graph_il1rapos_il18pos <- plotSpecCat(data_graph, "il1ra_pos==1 & il18_pos_kml==1")
ggsave(plot = graph_il1rapos_il18pos, "figures/il18_il1ra_classification/il18pos_il1rapos_kml.svg", width=18,height=7.5,units = "cm")
graph_il1rapos_il18neg <- plotSpecCat(data_graph, "il1ra_pos==1 & il18_pos==0")
figD <- graph_il1rapos_il18neg <- plotSpecCat(data_graph, "il1ra_pos==1 & il18_pos_kml==0")
ggsave(plot=graph_il1rapos_il18neg, "figures/il18_il1ra_classification/il18neg_il1rapos_kml.svg", width=18,height=7.5,units = "cm")






# Figure S: survival
data_cluster <- data_graph_unique
data_cluster$rapos_18pos_thresh <- ifelse(data_cluster$il1ra_pos==1 & data_cluster$il18_pos==1, 1,
                                          ifelse(data_cluster$il1ra_pos==1 & data_cluster$il18_pos==0, 2, NA))
data_cluster$rapos_18pos_kml <- ifelse(data_cluster$il1ra_pos==1 & data_cluster$il18_pos_kml==1, 1,
                                          ifelse(data_cluster$il1ra_pos==1 & data_cluster$il18_pos_kml==0, 2, NA))
survival_rapos_18pos_thresh <- survfit2(Surv(interval_ceil30, censoring_30d) ~ as.factor(rapos_18pos_thresh) , data = data_cluster) %>%
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  )+
  ylim(0,1)+
  #add_pvalue()+ #0.02601764
  ggthemes::theme_clean()
# ggsave("figures/il18_il1ra_classification/MDA_surv_thresh.svg", width=7,height=4,units = "cm")

survival_rapos_18pos_kml <- survfit2(Surv(interval_ceil30, censoring_30d) ~ as.factor(rapos_18pos_kml) , data = data_cluster) %>%
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  )+
  ylim(0,1)+
  #add_pvalue()+ #0.0001851587
  ggthemes::theme_clean()
# ggsave("figures/il18_il1ra_classification/MDA_surv_thresh.svg", width=7,height=4,units = "cm")

survival_MDA <- survfit2(Surv(interval_ceil30, censoring_30d) ~ as.factor(MDA_pos) , data = data_cluster) %>%
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  )+
  ylim(0,1)+
  add_pvalue()+
  ggthemes::theme_clean()
# ggsave("figures/il18_il1ra_classification/MDA_surv_thresh.svg", width=7,height=4,units = "cm")

survival_MDA <- survfit2(Surv(interval_ceil30, censoring_30d) ~ as.factor(MDA_pos_kml) , data = data_cluster) %>%
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  )+
  ylim(0,1)+
  #add_pvalue()+ 0.8
  ggthemes::theme_clean()
# ggsave("figures/il18_il1ra_classification/MDA_surv_kml.svg", width=7,height=4,units = "cm")


# Figure S: organ failure per category
data_graph_curr <- filter(data_graph, il1ra_pos==1 & il18_pos_kml==1 & MDA_pos==1)
data_venn <- data_graph_curr %>%
  group_by(PatientID) %>%
  reframe(PatientID=first(PatientID),
          ARF=ifelse(max(respi_sofa,na.rm=T)>=3, 1, 0), #corresponds to a PaO2/FiO2 ratio < 200
          Liver=ifelse(max(INR_max, na.rm=T)>=1.5 & max(max_bili, na.rm=T)>=3, 1, 0),
          AKI=ifelse(max(AKI, na.rm=T)!="No_AKI", 1, 0))
x <- list("ARF"=data_venn[data_venn$ARF==1,]$PatientID,"Liver"=data_venn[data_venn$Liver==1,]$PatientID,"AKI"=data_venn[data_venn$AKI==1,]$PatientID)
ggVennDiagram(x, label = "count", label_geom="text", label_alpha = 1, label_color = "white")+scale_fill_gradient(low="#e0cdd6", high="darkblue")+theme(legend.position="none")
# ggsave("organ_il1ra.svg", width=5, height=5, unit="cm")


#Mortality in IL18 positive patients but IL1Ra negative
table(filter(data_graph_unique, il1ra_pos==0 & il18_pos_kml==1)$survival_30d)
table(filter(data_graph_unique, il1ra_pos==1 & il18_pos_kml==0)$survival_30d)

