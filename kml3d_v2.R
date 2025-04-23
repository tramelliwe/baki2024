### Formatting data -----
## Peak syncing
#peak syncing in the middle and clustering the whole trajectory (patients without peak are centered on the peak)
sync_middle <- data_graph 
sync_middle$day_aj <- sync_middle$day_aj_centered
ggplot(sync_middle, aes(x=day_aj))+
  geom_line(data = sync_middle, aes(group=PatientID,y=(cytokines_il18_z), col="green"), position = position_jitter(seed = 123, height=0.2, width=0.1))+
  geom_line(data = sync_middle, aes(group=PatientID,y=(cytokines_il1ra_z), col="red"), position = position_jitter(seed = 123, height=0.2, width=0.1))+
  theme(legend.position = "none")+
  ggtitle("IL1ra peak synchronization in the middle")
#peak syncing in the beginning and clustering only from the peak onwards (patients without peak are not centered, they start at day 1)
sync_beginning <- filter(data_graph, day_aj>=0)
ggplot(sync_beginning, aes(x=day_aj))+
  geom_line(data = sync_beginning, aes(group=PatientID,y=(cytokines_il18_z), col="green"), position = position_jitter(seed = 123, height=0.2, width=0.1))+
  geom_line(data = sync_beginning, aes(group=PatientID,y=(cytokines_il1ra_z), col="red"), position = position_jitter(seed = 123, height=0.2, width=0.1))+
  theme(legend.position = "none")+
  ggtitle("IL1ra peak synchronization in the beginning")

## No peak syncing
no_sync <- data_graph
no_sync$day_aj <- no_sync$Days-1
ggplot(no_sync, aes(x=day_aj))+
  geom_line(data = no_sync, aes(group=PatientID,y=(cytokines_il18_z), col="green"), position = position_jitter(seed = 123, height=0.2, width=0.1))+
  geom_line(data = no_sync, aes(group=PatientID,y=(cytokines_il1ra_z), col="red"), position = position_jitter(seed = 123, height=0.2, width=0.1))+
  theme(legend.position = "none")+
  ggtitle("No synchronization")

### Longitudinal clustering of IL18 with all patients ----
#With sync_middle
longi_data <- sync_middle %>%
  select(c(PatientID,day_aj,cytokines_il18_z)) %>%
  filter(day_aj>=-3 & day_aj <=3) %>% #avoiding edge effect
  arrange(day_aj)
longi_data <- longi_data %>% 
  pivot_wider(names_from=day_aj,
                           values_from=cytokines_il18_z,
                           names_prefix="day")
pats <- longi_data$PatientID
longi_data <- longi_data %>%
  select(-PatientID)

traj <- matrix(as.vector(unlist(longi_data)),length(pats))


sync_middle_il18_results_obj <- clusterLongData(
  traj=traj,
  time=c(seq(-3,3))
)
kml(sync_middle_il18_results_obj,nbRedrawing = 50)
x11()
choice(sync_middle_il18_results_obj)

sync_middle_il18_results <- read.delim("sync_middle_il18_results_obj-C2-1-Clusters.csv",sep=";")
sync_middle_il18_results$idAll <- pats
colnames(sync_middle_il18_results) <- c("PatientID","clusters")
sync_middle <- merge(sync_middle,sync_middle_il18_results, by="PatientID")

ggplot(sync_middle, aes(x=day_aj, col=as.factor(clusters)))+
  geom_line(data = sync_middle, aes(group=PatientID,y=(cytokines_il18_z), alpha=0.2), position = position_jitter(seed = 123, height=0.2, width=0.1))+
  theme(legend.position = "none")+
  ylab("z-normalized IL18")+
  xlab("Days relative to IL1ra peak")+
  ggtitle("IL18 longitudinal clustering with IL1ra peak synchronization in the middle")

#With sync_beginning
longi_data <- sync_beginning %>%
  select(c(PatientID,day_aj,cytokines_il18_z)) %>%
  arrange(day_aj)
longi_data <- longi_data %>% 
  pivot_wider(names_from=day_aj,
              values_from=cytokines_il18_z,
              names_prefix="day")
pats <- longi_data$PatientID
# longi_data <- longi_data %>%
#   select(-PatientID)
# 
# traj <- matrix(as.vector(unlist(longi_data)),length(pats))
# 
# 
# sync_beginning_il18_results_230724 <- clusterLongData(
#   traj=traj,
#   time=c(seq(0,6))
# )
# set.seed(123)
# kml(sync_beginning_il18_results_230724,nbRedrawing = 150)
# x11()
# choice(sync_beginning_il18_results_230724)

sync_beginning_il18_results <- read.delim("sync_beginning_il18_results_230724-C2-1-Clusters.csv",sep=";")
sync_beginning_il18_results$idAll <- pats
colnames(sync_beginning_il18_results) <- c("PatientID","clusters")
sync_beginning <- merge(sync_beginning,sync_beginning_il18_results, by="PatientID")
ggplot(sync_beginning,  aes(x=day_aj, col=as.factor(clusters)))+
  geom_line(data = sync_beginning, aes(group=PatientID,y=(cytokines_il18_z)),alpha=0.5, position = position_jitter(seed = 123, height=0.2, width=0.1))+
  theme(legend.position = "none")+
  ylab("z-normalized IL18")+
  xlab("Days relative to IL1ra peak")+
  ggtitle("IL18 longitudinal clustering with IL1ra peak synchronization at x=0")+
  ggthemes::theme_few()
ggsave("figures/il18_il1ra_classification/kml1d_trajs.svg", width=15,height=7.5,units = "cm")


#with no sync
longi_data <- no_sync %>%
  select(c(PatientID,day_aj,cytokines_il18_z)) %>%
  arrange(day_aj)
longi_data <- longi_data %>% 
  pivot_wider(names_from=day_aj,
              values_from=cytokines_il18_z,
              names_prefix="day")
pats <- longi_data$PatientID
longi_data <- longi_data %>%
  select(-PatientID)

traj <- matrix(as.vector(unlist(longi_data)),length(pats))


no_sync_il18_results <- clusterLongData(
  traj=traj,
  time=c(seq(0,6))
)
kml(no_sync_il18_results,nbRedrawing = 200)
x11()
choice(no_sync_il18_results)

no_sync_il18_results <- read.delim("no_sync_il18_results-C4-1-Clusters.csv",sep=";")
no_sync_il18_results$idAll <- pats
colnames(no_sync_il18_results) <- c("PatientID","clusters")
no_sync <- merge(no_sync,no_sync_il18_results, by="PatientID")
ggplot(no_sync, aes(x=day_aj, col=as.factor(clusters)))+
  geom_line(data = no_sync, aes(group=PatientID,y=(cytokines_il18_z), alpha=0.2), position = position_jitter(seed = 123, height=0.2, width=0.1))+
  theme(legend.position = "none")+
  ylab("z-normalized IL18")+
  xlab("Days")+
  ggtitle("IL18 longitudinal clustering without sync")




### Longitudinal clustering of IL18 & IL1ra with all patients ----
#with sync_middle
longi_data <- select(sync_middle,c(PatientID,cytokines_il18_z,cytokines_il1ra_z,day_aj)) %>%
  filter(day_aj<=3 & day_aj>=-3) %>% #avoid edge effect
  arrange(day_aj)

longi_data <- longi_data %>% pivot_wider(names_from=day_aj,
                           values_from=c(cytokines_il18_z,cytokines_il1ra_z),
                           names_prefix="day") 
pats <- longi_data$PatientID

# il18 <- select(longi_data,starts_with("cytokines_il18"))
# il1ra <- select(longi_data,starts_with("cytokines_il1ra"))
# 
# il18 <- matrix(as.vector(unlist(il18)),length(pats))
# il1ra <- matrix(as.vector(unlist(il1ra)),length(pats)) 
# 
# traj <- array(c(il18,il1ra), dim = c(length(pats),7,2))
# 
# sync_middle_3d_results <- clusterLongData3d(
#   traj=traj,
#   idAll=as.character(pats),
#   time=c(seq(-3,3)),
#   varNames=c("IL18", "IL1ra"),
#   maxNA = 4
# )
# set.seed(123)
# kml3d(sync_middle_3d_results, nbRedrawing = 150)
# x11()
# choice(sync_middle_3d_results)

#With this clustering on 23/07/24, cluster C&D have a IL1ra peak, cluster A has decreasing il18, cluster B has increasing il18, cluster C has increasing then decreasing il18, and cluster D has decreasing then increasing il18
load("C:/Users/cwillemart/OneDrive - Universiteit Antwerpen/PhD/Topics/BAKI/BAKI_March2024/sync_middle_3d_results_230724.Rdata")
sync_middle_3d_results_230724_obj <- sync_middle_3d_results

sync_middle_3d_results <- read.delim("sync_middle_3d_results_230724-C4-1-Clusters.csv",sep=";")
sync_middle_3d_results$idAll <- pats
colnames(sync_middle_3d_results) <- c("PatientID","clusters")
sync_middle <- merge(sync_middle,sync_middle_3d_results, by="PatientID")
axx <- list(
  title = 'ICU Day'
)

axy <- list(
  title = 'Z-normalized IL18'
)

axz <- list(
  title = 'Z-normalized IL1ra'
)

p <- plot_ly(sync_middle,color=~as.factor(clusters),line = list(width=4),opacity=0.5,
             x = ~ day_aj, y = ~cytokines_il18_z, z = ~cytokines_il1ra_z, showlegend=F,borderwidth=100)

p <- add_paths(p, linetype = ~PatientID,width=100)%>% 
  layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))

p


#with sync_beginning
longi_data <- select(sync_beginning,c(PatientID,cytokines_il18_z,cytokines_il1ra_z,day_aj)) %>%
  arrange(day_aj)

longi_data <- longi_data %>% pivot_wider(names_from=day_aj,
                                         values_from=c(cytokines_il18_z,cytokines_il1ra_z),
                                         names_prefix="day") 
pats <- longi_data$PatientID
# 
# il18 <- select(longi_data,starts_with("cytokines_il18"))
# il1ra <- select(longi_data,starts_with("cytokines_il1ra"))
# 
# il18 <- matrix(as.vector(unlist(il18)),length(pats))
# il1ra <- matrix(as.vector(unlist(il1ra)),length(pats)) 
# 
# traj <- array(c(il18,il1ra), dim = c(length(pats),7,2))
# 
# sync_beginning_3d_results <- clusterLongData3d(
#   traj=traj,
#   idAll=as.character(pats),
#   time=c(seq(0,6)),
#   varNames=c("IL18", "IL1ra"),
#   maxNA = 4
# )
# set.seed(123)
# kml3d(sync_beginning_3d_results,nbRedrawing = 150)
# x11()
# choice(sync_beginning_3d_results)

#With this clustering on 23/07/24, cluster C&D have a IL1ra peak, cluster A&D have decreasing il18, cluster B&C have increasing il18
load("C:/Users/cwillemart/OneDrive - Universiteit Antwerpen/PhD/Topics/BAKI/BAKI_March2024/sync_beginning_3d_results_230724.Rdata")
sync_beginning_3d_results_obj <- sync_beginning_3d_results

sync_beginning_3d_results <- read.delim("sync_beginning_3d_results_230724-C4-1-Clusters.csv",sep=";")
sync_beginning_3d_results$idAll <- pats

colnames(sync_beginning_3d_results) <- c("PatientID","clusters")
sync_beginning <- merge(sync_beginning,sync_beginning_3d_results, by="PatientID")

axx <- list(
  title = 'ICU Day'
)

axy <- list(
  title = 'Z-normalized IL18'
)

axz <- list(
  title = 'Z-normalized IL1ra'
)

p <- plot_ly(sync_beginning,color=~as.factor(clusters),line = list(width=4),opacity=0.5,
             x = ~ day_aj, y = ~cytokines_il18_z, z = ~cytokines_il1ra_z, showlegend=F,borderwidth=100)

p <- add_paths(p, linetype = ~PatientID,width=100)%>% 
  layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))

p
#working with no-synced data here makes no sense, so not displayed






sync_beginning_mean <- sync_beginning %>%
  group_by(clusters, day_aj) %>%
  summarize(mean_il1ra=mean(cytokines_il1ra_z),
            mean_il18=mean(cytokines_il18_z))

axx <- list(
  title = 'ICU Day'
)

axy <- list(
  title = 'mean Z-normalized IL18'
)

axz <- list(
  title = 'mean Z-normalized IL1ra'
)
plot_ly(sync_beginning_mean,x=~day_aj,z=~mean_il1ra,y=~mean_il18, mode="lines",color=~as.factor(clusters), type="scatter3d",opacity=0.7,line=list(width=10))%>% 
  layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))

sync_middleg_mean <- sync_middle %>%
  group_by(clusters, day_aj) %>%
  summarize(mean_il1ra=mean(cytokines_il1ra_z),
            mean_il18=mean(cytokines_il18_z))


plot_ly(sync_middleg_mean,x=~day_aj,z=~mean_il1ra,y=~mean_il18, mode="lines",color=~as.factor(clusters), type="scatter3d",opacity=0.9,line=list(width=10))%>%
  layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))


data_cluster <- merge(data,select(sync_beginning_3d_results,c(PatientID,clusters)), by="PatientID",all.y=T)
survfit2(Surv(interval_ceil30, censoring_30d) ~ as.factor(clusters), data = data_cluster) %>%
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  )+
  ylim(0,1)+
  add_pvalue()+
  ggthemes::theme_clean()
ggsave("kml_beginning_classification.svg",width=10,height=7,units="cm")

# same graphs as stratification_thresh ------------------------------------

#With this clustering on 23/07/24, cluster C&D have a IL1ra peak, cluster A&D have decreasing il18, cluster B&C have increasing il18

data_graph <- merge(data_graph, sync_beginning_3d_results, by="PatientID",all.x=T)
data_graph[is.na(data_graph$clusters),]$clusters <- 0
data_graph$il1ra_pos_clusters <- ifelse(data_graph$clusters>=3, 1,
                                        ifelse(data_graph$clusters==1 | data_graph$clusters==2, 0, data_graph$il1ra_pos)) #clusters 3 & 4 have IL1ra peak. if na, take the threshold
data_graph$il18_pos_clusters <- ifelse(data_graph$clusters==2 | data_graph$clusters==3, 1,
                                        ifelse(data_graph$clusters==1 | data_graph$clusters==4, 0, data_graph$il18_pos)) #clusters 2 & 3 have icnreasing il18. if na, take the threshold
data_graph_unique <- data_graph[!duplicated(data_graph$PatientID),]

#IL1ra+ IL18+
data_graph_rapos_18pos <- filter(data_graph, il1ra_pos_clusters==1 & il18_pos_clusters==1)
###Calculation nr of records stopping due to death###
data_graph_curr <- data_graph_rapos_18pos
data_graph_curr_temp <- data_graph_curr %>%
  filter(interval<=7 & survival_30d=="Deceased") %>%
  arrange(PatientID,day_aj)%>%
  group_by(PatientID) %>%
  summarize(last_day=max(day_aj))
data_graph_curr <- merge(data_graph_curr, data_graph_curr_temp, by="PatientID", all.x=T)
data_graph_curr_unique <- data_graph_curr[!duplicated(data_graph_curr$PatientID), ]
lastdays <- data_graph_curr_unique %>%
  group_by(last_day) %>%
  summarize(count=n())
lastdays$count <- cumsum(lastdays$count)

quant <- data_graph_curr %>%
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
quant_18pos <- quant



#IL1ra+ IL18-
data_graph_rapos_18neg <- filter(data_graph, il1ra_pos_clusters==1 & il18_pos_clusters==0)
###Calculation nr of records stopping due to death###
data_graph_curr <- data_graph_rapos_18neg
data_graph_curr_temp <- data_graph_curr %>%
  filter(interval<=7 & survival_30d=="Deceased") %>%
  arrange(PatientID,day_aj)%>%
  group_by(PatientID) %>%
  summarize(last_day=max(day_aj))
data_graph_curr <- merge(data_graph_curr, data_graph_curr_temp, by="PatientID", all.x=T)
data_graph_curr_unique <- data_graph_curr[!duplicated(data_graph_curr$PatientID), ]
lastdays <- data_graph_curr_unique %>%
  group_by(last_day) %>%
  summarize(count=n())
lastdays$count <- cumsum(lastdays$count)

quant <- data_graph_curr %>%
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
quant_18neg <- quant


# plots ----
# Figure A: Count of patients patients per cat ----
x <- list("IL1ra+"=data_graph_unique[data_graph_unique$il1ra_pos_clusters==1,]$PatientID,"MDA+"=data_graph_unique[data_graph_unique$MDA_pos==1,]$PatientID,"IL18+"=data_graph_unique[data_graph_unique$il18_pos_clusters==1,]$PatientID)
ggVennDiagram(x,label="count")+scale_fill_gradient(low="white", high="darkblue")+
  theme(legend.position="none")+
  ggtitle("Patients per category",subtitle = "(Excluding 75 triple negative patients)")

# Figure B: Mortality per cat ----
table(select(data_graph_unique, c(survival_30d, il1ra_pos_clusters, il18_pos_clusters, MDA_pos)))
# Make figure manually

# Figure C: IL18+ IL1ra+ trajectories
ggplot(quant_18pos,aes(x=day_aj))+
  geom_ribbon(aes(ymin = value_25, ymax=value_75, fill = cytokine), alpha = 0.5)+
  geom_point(data = data_graph_rapos_18pos, aes(y=log2(cytokines_il1ra+1), fill="cytokines_il1ra"),col="darkgreen",alpha=0.5, size = 0.5, position = position_jitter(seed = 123, height=0.2, width=0.1))+
  geom_point(data =data_graph_rapos_18pos,aes(y=log2(cytokines_il18+1), fill="cytokines_il18"),col="red",alpha=0.5, size = 0.5, position = position_jitter(seed = 123, height=0.2, width=0.1))+
  geom_point(data = data_graph_rapos_18pos,aes(y=log2(MDA+1), fill="MDA"),col="blue",alpha=0.5,size = 0.5, position = position_jitter(seed = 123, height=0.2, width=0.1))+
  geom_line(data = data_graph_rapos_18pos, aes(group=PatientID,y=log2(cytokines_il1ra+1), fill="cytokines_il1ra"),col="darkgreen",alpha=0.1, position = position_jitter(seed = 123, height=0.2, width=0.1))+
  geom_line(data = data_graph_rapos_18pos, aes(group=PatientID,y=log2(cytokines_il18+1), fill="cytokines_il18"),col="red",alpha=0.1, position = position_jitter(seed = 123, height=0.2, width=0.1))+
  geom_line(data = data_graph_rapos_18pos, aes(group=PatientID,y=log2(MDA+1), fill="MDA"),col="blue",alpha=0.1, position = position_jitter(seed = 123, height=0.2, width=0.1))+
  geom_line(aes(y=count))+
  ylab("Log2-transformed values")+
  
  ggtitle("IL18+ & IL1ra+")+
  #scale_x_continuous("Day", labels=quant$day_aj, breaks=quant$day_aj)+
  scale_x_continuous("Day relative to IL1ra peak",labels=quant$day_aj, breaks=quant$day_aj)+
  scale_y_continuous(sec.axis=sec_axis(trans=~.*1, name="Cumulative number of deaths"))+
  ylim(-0.5,18)+
  ggthemes::theme_few()

ggsave("il18pos_il1rapos_kml.svg", width=15,height=7.5,units = "cm")

# Figure D: IL18- IL1ra+ trajectories
ggplot(quant_18neg,aes(x=day_aj))+
  geom_ribbon(aes(ymin = value_25, ymax=value_75, fill = cytokine), alpha = 0.5)+
  geom_point(data = data_graph_rapos_18neg, aes(y=log2(cytokines_il1ra+1), fill="cytokines_il1ra"),col="darkgreen",alpha=0.5, size = 0.5, position = position_jitter(seed = 123, height=0.2, width=0.1))+
  geom_point(data =data_graph_rapos_18neg,aes(y=log2(cytokines_il18+1), fill="cytokines_il18"),col="red",alpha=0.5, size = 0.5, position = position_jitter(seed = 123, height=0.2, width=0.1))+
  geom_point(data = data_graph_rapos_18neg,aes(y=log2(MDA+1), fill="MDA"),col="blue",alpha=0.5,size = 0.5, position = position_jitter(seed = 123, height=0.2, width=0.1))+
  geom_line(data = data_graph_rapos_18neg, aes(group=PatientID,y=log2(cytokines_il1ra+1), fill="cytokines_il1ra"),col="darkgreen",alpha=0.1, position = position_jitter(seed = 123, height=0.2, width=0.1))+
  geom_line(data = data_graph_rapos_18neg, aes(group=PatientID,y=log2(cytokines_il18+1), fill="cytokines_il18"),col="red",alpha=0.1, position = position_jitter(seed = 123, height=0.2, width=0.1))+
  geom_line(data = data_graph_rapos_18neg, aes(group=PatientID,y=log2(MDA+1), fill="MDA"),col="blue",alpha=0.1, position = position_jitter(seed = 123, height=0.2, width=0.1))+
  geom_line(aes(y=count))+
  ylab("Log2-transformed values")+
  
  ggtitle("IL18- & IL1ra+")+
  #scale_x_continuous("Day", labels=quant$day_aj, breaks=quant$day_aj)+
  scale_x_continuous("Day relative to IL1ra peak",labels=quant$day_aj, breaks=quant$day_aj)+
  scale_y_continuous(sec.axis=sec_axis(trans=~.*1, name="Cumulative number of deaths"))+
  ylim(-0.5,18)+
  ggthemes::theme_few()

ggsave("il18neg_il1rapos_kml.svg", width=15,height=7.5,units = "cm")




# Figure S: organ failure per category
data_graph_curr <- filter(data_graph, il1ra_pos_clusters==1 & il18_pos_clusters==0 & MDA_pos==0)
data_venn <- data_graph_curr %>%
  group_by(PatientID) %>%
  reframe(PatientID=first(PatientID),
          ARDS=ifelse(max(respi_sofa,na.rm=T)>=3, 1, 0),
          Liver=ifelse(max(ALT_max, na.rm=T)>=100 | max(AST_max,na.rm=T)>=100, 1, 0),
          AKI=ifelse(max(AKI, na.rm=T)!="No_AKI", 1, 0))
x <- list("ARDS"=data_venn[data_venn$ARDS==1,]$PatientID,"Liver"=data_venn[data_venn$Liver==1,]$PatientID,"AKI"=data_venn[data_venn$AKI==1,]$PatientID)
ggVennDiagram(x, label = "none")+scale_fill_gradient(low="white", high="darkblue")+theme(legend.position="none")
ggsave("organ_il1ra_kml.svg", width=5, height=5, unit="cm")



# Figure S: survival
data_cluster <- data_graph_unique
data_cluster$rapos_18pos_kml <- ifelse(data_cluster$il1ra_pos_clusters==1 & data_cluster$il18_pos_clusters==1, 1,
                                          ifelse(data_cluster$il1ra_pos_clusters==1 & data_cluster$il18_pos_clusters==0, 2, NA))
survfit2(Surv(interval_ceil30, censoring_30d) ~ as.factor(rapos_18pos_kml) , data = data_cluster) %>%
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  )+
  ylim(0,1)+
  ggthemes::theme_clean()
ggsave("il18pos_il1rapos_surv_kml.svg", width=7,height=4,units = "cm")
