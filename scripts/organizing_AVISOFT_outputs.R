#### this script is for loading sunning playback data, processing and summarizing it and adding the dyadic social
#### ties strength to the response outcome of each playback experiment

setwd("C:/Users/vdemartsev/My Cloud/Sunning_network_analysis")

library(tidyverse)
library(ggpubr)
#save headings for the AVISOFT output files - DEPENDS ON THE EXTRACTION SETTINGS. CHECK FILE STRUCTURE BEFORE RUNNING!!!!
headings <- c("#",	"label",	"start time", 	"end time",
              "fund_freq", 	"spect_centr", 	
              "entropy", 	"filename")

#load 2019 playback data
playbacks_2019 <- read.delim("2019_logfile.txt", header = F)

#add column names
colnames(playbacks_2019) <- headings

#split filename column into separate data columns
playbacks_2019<- separate(playbacks_2019, col = filename, into = c("date", "group", "pair", "file_name"), sep = "_", remove = F)
playbacks_2019 <- separate(playbacks_2019, col = pair, into = c("focal_dye", "playback_dye"), sep = "x")

#cleaning up and adding year colun
playbacks_2019$file_name <- NULL
playbacks_2019$year <- 2019

#loading IND_INFO data from 2019
Ind_info <- read.delim("ALL_INDIVIDUAL_INFO.txt")

#create unique ID column for group and dye mark combinations
Ind_info$unique_ID <- paste(Ind_info$group, Ind_info$dye, sep="_") #in IND_INFO file
playbacks_2019$uniquefocal <- paste(playbacks_2019$group, playbacks_2019$focal_dye, sep="_") #for focal individual
playbacks_2019$uniqueplayback <- paste(playbacks_2019$group, playbacks_2019$playback_dye, sep="_") #for playback individual

#pasting ID codes and sex for focal and playback individuals
for (i in 1:nrow(playbacks_2019)) 
{ codeID    <- match(playbacks_2019[i, "uniquefocal"], Ind_info$unique_ID)
  PB_codeID <- match(playbacks_2019[i, "uniqueplayback"], Ind_info$unique_ID)
  
playbacks_2019[i , "focal_code"] <- paste(Ind_info[codeID, "code"])
playbacks_2019$focal_sex[i] <- paste(Ind_info[codeID, "sex"])


playbacks_2019[i , "playback_code"] <- paste(Ind_info[PB_codeID, "code"])
playbacks_2019$playback_sex[i] <- paste(Ind_info[PB_codeID, "sex"])

}


 ### now we have the 2019 playback outputs in a complete format
 #### next step is to bring 2021 playback output into the same one####

#load 2021 playback data
playbacks_2021 <- read.delim("2021_logfile.txt", header = F)

#add column names
colnames(playbacks_2021) <- headings


#rename PB call labels in the 2021 data to match 2019 data 

playbacks_2021[which(playbacks_2021$label == "sn"), "label"] <- "pb" # rename SN into PB
playbacks_2021[which(playbacks_2021$label %in% c("call", "cal")), "label"] <- "sn" #rename call into SN
playbacks_2021[which(playbacks_2021$label == "end"), "label"] <- "stop" #rename END markers to STOP
table(playbacks_2021$label)


#split file name column into separate data columns
playbacks_2021<- separate(playbacks_2021, col = filename, into = c("take", "group", "focal_dye", "focal_code", "junk"), sep = "_", remove = F)
#clean up junk and add year column
playbacks_2021$junk <- NULL
playbacks_2021$year <- 2021

#load 2021 playback log
 log_2021_sunning_playbacks <- read.csv("2021_playback_log.csv")

 #load playback individual codes
 for (i in 1:nrow(playbacks_2021)) {
   playback_code <- log_2021_sunning_playbacks[which(log_2021_sunning_playbacks$pbrec == playbacks_2021$take[i]) , "ID_caller"]
   ifelse(length(playback_code) > 0, 
   playbacks_2021$playback_code[i] <- playback_code, 
   playbacks_2021$playback_code[i] <- NA)
   }
#add date and sex columns for consistency
 playbacks_2021$date <- NA
 playbacks_2021$sex <- NA
 

#select the required columns for both playback data frames
playbacks_2021_test <- cbind(playbacks_2021[, 1:8 ], playbacks_2021$date, playbacks_2021$year, playbacks_2021$group, 
                             playbacks_2021$focal_code, playbacks_2021$playback_code)

playbacks_2019_test <- cbind(playbacks_2019[ , 1:9], playbacks_2019$year, playbacks_2019$group, 
                             playbacks_2019$focal_code, playbacks_2019$playback_code)

#set same col-names for both and rename
col_names <- c("#", "label", "start time", "end time", "fund_freq","spect_centr","entropy","filename",      
                 "date", "year", "group", "focal_code", "playback_code" )
colnames(playbacks_2019_test) <- colnames(playbacks_2021_test) <-  col_names
 
#collect everything in one data object
all_playbacks_combined <- rbind(playbacks_2019_test, playbacks_2021_test)

#rename calls for consistency between years
all_playbacks_combined$label <-   tolower(all_playbacks_combined$label)
all_playbacks_combined[which(all_playbacks_combined$label == "alarm"), "label"] <- "al" # rename ALARM into al
all_playbacks_combined[which(all_playbacks_combined$label == "focal moved"), "label"] <- "moved" # rename focal moved into moved

table(all_playbacks_combined$label)

#remove bad trials
bad_trials <- c("30.06.19_Lazuli_RSTBLxMBLT_1006.WAV", "04.07.19_HakunaMatata_MBLTxRSTB_1007.WAV", "01.08.19_Whiskers_HLTxLTT_1001.WAV",
                "18.07.19_Lazuli_HTBxHRT_1014.WAV", "09.08.19_HakunaMatata_SHLTxLTTB_1009.WAV", "1029_MP_LSRS_VMPF048_pbrec.wav",
                "1139_LAZ_RSTB_VLF272_pbrec.wav", "1031_MP_LSLT_VMPF047_pbrec.wav","1071_MP_LSRS_VMPF048_pbrec.wav", 
              "1188_LAZ_TBLH_VLF244_pbrec.wav", "11.07.19_Lazuli_MBRSxRTTBL_1004.WAV", "30.07.19_Jaxx_HRTxRCTB_1004.WAV",
              "08.08.19_Lazuli_HMBxRTTBL_1006.WAV", "08.08.19_Lazuli_HTBxHRT_1005.WAV", "09.08.19_HakunaMatata_HTBxRSTB_1012.WAV", 
              "13.08.19_Jaxx_TBLxLSLR_1011.WAV", "18.08.19_Whiskers_MBTxMBRT_1006.WAV", "1018_MP_LRRT_VMPF035_pbrec.wav", 
              "1019_MP_LRRS_VMPM037_pbrec.wav", "1174_MP_LSRT_VMPM046_pbrec.wav", "1045_LAZ_MBLS_VHMM023_pbrec.wav", 
              "1047_LAZ_SHMB_VLM265_pbrec.wav", "1053_MP_LST_VMPM027_pbrec.wav") 
                


#all_playbacks_combined[which(all_playbacks_combined$label== "moved") , ]

stop_markers <- c("aproach", "moved", "stop")
non_call_labels <- c("grooming", "nf alarm", "not alone", "pause", "resume", "second focal")
non_sn_calls <- c("agg","al", "call",  "cc",  "growl", "soc") 

all_playbacks_combined <-  all_playbacks_combined [-which(all_playbacks_combined$label %in% non_call_labels) , ] 
all_playbacks_combined <-  all_playbacks_combined [-which(all_playbacks_combined$filename %in% bad_trials) , ] 
all_playbacks_combined <-  all_playbacks_combined [-which(all_playbacks_combined$label %in% non_sn_calls) , ] 
table(all_playbacks_combined$label)

#calculate call duration for all data
all_playbacks_combined$duration <- all_playbacks_combined$`end time` - all_playbacks_combined$`start time`


 
call_rates_abs_summary <- data.frame()
call_rates_deltas <- data.frame()
 

for (file in unique(all_playbacks_combined$filename)) {
  #select one audio file
 one_track <- all_playbacks_combined[which(all_playbacks_combined$filename == file), ]
  pb_start_t <- min(one_track[which(one_track$label == "pb"), "start time"]) #get time when playback started
  pb_end_t   <- max(one_track[which(one_track$label == "pb"), "end time"]) +1 #get time when playback ended
  
 start_t <- one_track[which(one_track$label == "start"), "start time"] #get time when experiment started
 
 if (length(start_t) == 0) {start_t <- 1} #if no marker set time to 1 sec
 
 stop_t   <- min(one_track[which(one_track$label %in% stop_markers), "end time"]) #get time when experiment stopped
 
 if (stop_t < pb_end_t) {next}
  
 #calculate call rates for the three stages CONTROL, PLAYBACK, RECOVERY
 control_phase <- one_track[which(one_track$`start time` > start_t & one_track$`start time` < pb_start_t) ,]
 recovery_phase <- one_track[which(one_track$`start time` > pb_end_t & one_track$`end time` < stop_t) ,]
 playback_phase <- one_track[which(one_track$`start time` > pb_start_t & one_track$`end time` < pb_end_t & one_track$label != "pb") ,]
 
 
 
 call_rates <- rbind(
                             c(one_track[1, 8:13], "call_rate_cont", nrow(control_phase)/(pb_start_t - start_t)),
                             c(one_track[1, 8:13], "vocal_rate_cont", sum(control_phase$duration)/(pb_start_t - start_t)),
 
                             c(one_track[1, 8:13], "call_rate_recovery", nrow(recovery_phase)/(stop_t - pb_end_t)),
                             c(one_track[1, 8:13], "vocal_rate_recovery", sum(recovery_phase$duration)/(stop_t - pb_end_t)),
 
                             c(one_track[1, 8:13], "call_rate_pb", nrow(playback_phase)/(pb_end_t - pb_start_t)),
                             c(one_track[1, 8:13], "vocal_rate_pb", sum(playback_phase$duration)/(pb_end_t - pb_start_t)))
 call_rates <- as.data.frame(call_rates)
 colnames(call_rates) <- c("file", "date", "year", "group", "focal_code", "playback_code", "type", "value")
 
 call_rates_abs_summary <- rbind(call_rates_abs_summary, call_rates)
 
 
 call_rates_delta <- rbind(
   c(one_track[1, 8:13], "call_rate_(pb-cont)", nrow(playback_phase)/(pb_end_t - pb_start_t) - nrow(control_phase)/(pb_start_t - start_t)),
   c(one_track[1, 8:13], "vocal_rate_(pb-cont)",  sum(playback_phase$duration)/(pb_end_t - pb_start_t) - sum(control_phase$duration)/(pb_start_t - start_t)),
   
   c(one_track[1, 8:13], "call_rate_(recovery-cont)",  nrow(recovery_phase)/(stop_t - pb_end_t) - nrow(control_phase)/(pb_start_t - start_t)),
   c(one_track[1, 8:13], "vocal_rate_(recovery-cont)",  sum(recovery_phase$duration)/(stop_t - pb_end_t) - sum(control_phase$duration)/(pb_start_t - start_t)))
 
 colnames(call_rates_delta) <-   c("file", "date", "year", "group", "focal_code", "playback_code", "type", "value")
 call_rates_deltas <- rbind(call_rates_deltas, call_rates_delta)
}

warnings()
call_rates_abs_summary$value <- as.numeric(call_rates_abs_summary$value)
call_rates_abs_summary$type <- as.character(call_rates_abs_summary$type)


call_rates_deltas$type <- as.character(call_rates_deltas$type)
call_rates_deltas$value <- as.numeric(call_rates_deltas$value)

#plotting the general playback effects
library(ggplot2)
ggplot(data =  call_rates_abs_summary, aes(x = type, y = value)) + geom_boxplot() 
ggplot(data =  call_rates_deltas, aes(x = type, y = value)) + geom_boxplot()

#making nicer plots
ggplot(data =  call_rates_abs_summary[which(grepl("call", call_rates_abs_summary$type)),], aes(x = type, y = value)) + geom_boxplot() +
  labs(y= "Call_rate", x = "Phase") + scale_x_discrete(labels=c("call_rate_cont" = "Control", "call_rate_pb" = "Playback",
                                                                "call_rate_recovery" = "Recovery")) + 
  theme( panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text.x = element_text(face="bold",   size=14),
        axis.text.y = element_text(face="bold",   size=14), plot.title = element_text(face="bold",   size=18)) + 
        ggtitle("Call rate increases in playback phase")

ggplot(data =  call_rates_abs_summary[which(grepl("vocal", call_rates_abs_summary$type)),], aes(x = type, y = value)) + geom_boxplot() +
  labs(y= "Calling_time", x = "Phase") + scale_x_discrete(labels=c("vocal_rate_cont" = "Control", "vocal_rate_pb" = "Playback",
                                                                "vocal_rate_recovery" = "Recovery"))
#combining the playbacks data with the social ties


#removing all rows with missing playback_codes as those are failed playbacks
call_rates_deltas <- call_rates_deltas[which(!is.na(call_rates_deltas$playback_code)) ,]

#renaming groups to short codes
call_rates_deltas[which(call_rates_deltas$group == "Zulus") , "group"] <- "ZU"
call_rates_deltas[which(call_rates_deltas$group == "Lazuli") , "group"] <- "LAZ"
call_rates_deltas[which(call_rates_deltas$group == "Jaxx") , "group"] <- "JX"
call_rates_deltas[which(call_rates_deltas$group == "Whiskers") , "group"] <- "W"
call_rates_deltas[which(call_rates_deltas$group == "HakunaMatata") , "group"] <- "HM"


#load social tie data

### here select which source of social data to use###
#network_ties <- read.csv("all_network__groom_ties.csv")
network_ties <- read.csv("all_network_ties_1.csv")
#network_ties[which(network_ties$Tie.lenght == Inf),  "Tie.lenght"] <- NA
network_ties$Tie.lenght <- as.numeric(network_ties$Tie.lenght)
network_ties$Tie.strenght <- as.numeric(network_ties$Tie.strenght)
network_ties$Tie.lenght.adj <- as.numeric(network_ties$Tie.lenght.adj)


options(stringsAsFactors=FALSE)
network_ties_norm <- data_frame()
#normalize network ties per group
for (year in unique(network_ties$year)) {
  year_select <- network_ties[which(network_ties$year == year) ,]
  for (group in unique(year_select$group)) {
  group_select <- year_select[which(year_select$group == group) ,]
  group_select$Z_Tie.strenght <- scale(group_select$Tie.strenght)
  group_select$Z_Tie.lenght <- scale(group_select$Tie.lenght)
  group_select$Z_Tie.lenght.adj <- scale(group_select$Tie.lenght.adj)
  
  network_ties_norm <- rbind(network_ties_norm, group_select)
                                       
  
  }
}

colnames(network_ties_norm)

for (i in 1:nrow(call_rates_deltas)) {
  
  Z_Tie.lenght <- network_ties_norm[which(network_ties_norm$ID1 == call_rates_deltas$focal_code[i] & network_ties_norm$ID2 == call_rates_deltas$playback_code[i] 
                                          & network_ties_norm$year == call_rates_deltas$year[i]) , "Z_Tie.lenght"]
  Z_Tie.lenght <- as.numeric(Z_Tie.lenght)
  
  
  Focal_dom <- network_ties_norm[which(network_ties_norm$ID1 == call_rates_deltas$focal_code[i]  
                                       & network_ties_norm$year == call_rates_deltas$year[i]) , "ID1_dom.status"][1]
  PB_dom <- network_ties_norm[which(network_ties_norm$ID1 == call_rates_deltas$playback_code[i]  
                                          & network_ties_norm$year == call_rates_deltas$year[i]) , "ID1_dom.status"][1]
  
  Focal_sex <- network_ties_norm[which(network_ties_norm$ID1 == call_rates_deltas$focal_code[i]  
                                       & network_ties_norm$year == call_rates_deltas$year[i]) , "ID1_sex.sex"][1]
  PB_sex <- network_ties_norm[which(network_ties_norm$ID1 == call_rates_deltas$playback_code[i]  
                                    & network_ties_norm$year == call_rates_deltas$year[i]) , "ID1_sex.sex"][1]
  
  if (is.na(Z_Tie.lenght)) {call_rates_deltas$tie_lenghts[i] <- 
    network_ties_norm[which(network_ties_norm$ID2 == call_rates_deltas$focal_code[i] & network_ties_norm$ID1 == call_rates_deltas$playback_code[i] 
                            & network_ties_norm$year == call_rates_deltas$year[i]) , "Z_Tie.lenght"]
  }else{call_rates_deltas$tie_lenghts[i] <- Z_Tie.lenght}
    
  
  Z_Tie.strenght <-  network_ties_norm[which(network_ties_norm$ID1 == call_rates_deltas$focal_code[i] & network_ties_norm$ID2 == call_rates_deltas$playback_code[i] 
                                             & network_ties_norm$year == call_rates_deltas$year[i]) , "Z_Tie.strenght"]
  
  Z_Tie.strenght <- as.numeric(Z_Tie.strenght)
  
  if(is.na(Z_Tie.strenght)) {call_rates_deltas$tie_strenght[i] <- 
    network_ties_norm[which(network_ties_norm$ID2 == call_rates_deltas$focal_code[i] & network_ties_norm$ID1 == call_rates_deltas$playback_code[i] 
                       & network_ties_norm$year == call_rates_deltas$year[i]) , "Z_Tie.strenght"]
  }else{call_rates_deltas$tie_strenght[i] <-Z_Tie.strenght}
  
  
  Z_Tie.lenght.adj <- network_ties_norm[which(network_ties_norm$ID1 == call_rates_deltas$focal_code[i] & network_ties_norm$ID2 == call_rates_deltas$playback_code[i] 
                                          & network_ties_norm$year == call_rates_deltas$year[i]) , "Z_Tie.lenght.adj"]
  Z_Tie.lenght.adj <- as.numeric(Z_Tie.lenght.adj)
  
  if (is.na(Z_Tie.lenght.adj)) {call_rates_deltas$tie_lenghts.adj[i] <- 
    network_ties_norm[which(network_ties_norm$ID2 == call_rates_deltas$focal_code[i] & network_ties_norm$ID1 == call_rates_deltas$playback_code[i] 
                            & network_ties_norm$year == call_rates_deltas$year[i]) , "Z_Tie.lenght.adj"]
  }else{call_rates_deltas$tie_lenghts.adj[i] <- Z_Tie.lenght.adj}
  
  
  
 call_rates_deltas$focal_dom[i] <- Focal_dom
  call_rates_deltas$playback_dom[i] <- PB_dom
  
  call_rates_deltas$focal_sex[i] <- Focal_sex
  call_rates_deltas$playback_sex[i] <- PB_sex
}

#correcting some typos in individia sex
call_rates_deltas[which(call_rates_deltas$focal_sex == "N"), "focal_sex"] <- "M"
call_rates_deltas[which(call_rates_deltas$playback_sex == "N"), "playback_sex"] <- "M"

#getting dom statuses into binary variable
call_rates_deltas$focal_dom_bin <- call_rates_deltas$focal_dom
call_rates_deltas$playback_dom_bin <- call_rates_deltas$playback_dom
call_rates_deltas[which(call_rates_deltas$focal_dom != "Dominant") , "focal_dom_bin"] <- "Sub" 
call_rates_deltas[which(call_rates_deltas$playback_dom != "Dominant") , "playback_dom_bin"] <- "Sub" 

call_rates_deltas$dom_paired <- paste(call_rates_deltas$playback_dom_bin, call_rates_deltas$focal_dom_bin, sep = "_")
#data.table::fwrite(call_rates_deltas, file ="playback_call_rates.csv")


ggplot(data = call_rates_deltas, aes(y = value, x = tie_lenghts )) + geom_smooth(method = "lm")+ geom_point() + 
  facet_grid( ~ type)
ggplot(data = call_rates_deltas, aes(y = value, x = tie_lenghts.adj )) + geom_smooth(method = "lm")+ geom_point() + 
  facet_grid( ~ type)
ggplot(data = call_rates_deltas, aes(y = value, x = tie_lenghts.adj, color = type)) + geom_smooth(method = "lm")+ geom_point()


#making nicer plots
new_names <- c("Playback", "Recovery")
names(new_names) <- c("call_rate_(pb-cont)", "call_rate_(recovery-cont)")
ggplot(data = call_rates_deltas[which(grepl("call", call_rates_deltas$type)), ],
       aes(y = value, x = tie_lenghts.adj, colour = dom_paired  )) + geom_smooth(method = "lm")+ geom_point() + theme_bw() + ylab("call_rate") +
  facet_grid( ~ type, labeller = labeller(type = new_names))

ggplot(data = call_rates_deltas[which(grepl("call", call_rates_deltas$type)), ],
       aes(y = value, x = tie_lenghts, colour = dom_paired  )) + geom_smooth(method = "lm")+ geom_point() + theme_bw() + ylab("call_rate") +
  facet_grid( ~ type, labeller = labeller(type = new_names))

library(gridExtra)
ggplot(data = call_rates_deltas[which(call_rates_deltas$type == "call_rate_(pb-cont)" & 
                                        call_rates_deltas$dom_paired != "Dominant_Dominant"), ],
       aes(y = value, x = tie_lenghts.adj )) + geom_smooth(method = "lm")+ geom_point(position = "jitter") + 
  theme_light() + facet_wrap( playback_dom_bin + focal_dom_bin ~ ., ncol = 2, scale = "free") + 
                              theme(strip.text.x = element_text(size = 15))


ggplot(data = call_rates_deltas[which(call_rates_deltas$type == "call_rate_(pb-cont)" & 
                                        call_rates_deltas$dom_paired != "Dominant_Dominant"), ],
       aes(y = value, x = tie_lenghts )) + geom_smooth(method = "lm")+ geom_point(position = "jitter") + 
  theme_light() + facet_wrap( playback_dom_bin + focal_dom_bin ~ ., ncol = 2, scale = "free") + 
  theme(strip.text.x = element_text(size = 15))

ggplot(data = call_rates_deltas[which(call_rates_deltas$type == "call_rate_(pb-cont)" & 
                                        call_rates_deltas$dom_paired != "Dominant_Dominant"), ],
       aes(y = value, x = dom_paired )) + geom_boxplot() + 
  theme_light() +
 #facet_wrap( playback_dom_bin + focal_dom_bin ~ ., ncol = 2) + 
  #theme(strip.text.x = element_text(size = 15))

 

library(lme4)
library(lmerTest)
library(car)

#reformat variables to characters
call_rates_deltas$group <- as.character(call_rates_deltas$group)
#call_rates_deltas$year <- as.factor(call_rates_deltas$year)
call_rates_deltas$focal_code <- as.factor(as.character(call_rates_deltas$focal_code))
call_rates_deltas$dom_paired <- as.factor(call_rates_deltas$dom_paired)

#call_rates_deltas <- call_rates_deltas[-which(call_rates_deltas$dom_paired == "Dominant_Dominant") ,]

call_rates_deltas$group_year <- paste(call_rates_deltas$group, call_rates_deltas$year, sep = "_")

model <- lmer(data = call_rates_deltas[which(call_rates_deltas$type == "call_rate_(pb-cont)") ,],
              value ~ tie_lenghts.adj*dom_paired + tie_strenght*dom_paired   + 
                 (1|focal_code/group_year))

summary(model)

vif(model)


model_red <- lmer(data = call_rates_deltas[which(call_rates_deltas$type == "call_rate_(pb-cont)" & 
                                                   call_rates_deltas$dom_paired != "Dominant_Dominant") , ],
              value ~ tie_lenghts.adj*dom_paired  + (1|focal_code/group_year))
model_red <- lmer(data = call_rates_deltas[which(call_rates_deltas$type == "vocal_rate_(pb-cont)" & 
                                                   call_rates_deltas$dom_paired != "Dominant_Dominant") , ],
                  value ~ tie_lenghts.adj*dom_paired  + (1|focal_code/group_year))

summary(model_red)

vif(model_red)


vif(model_red_2)
library(sjPlot)
library(sjmisc)
library(ggplot2)

plot_model(model_red, type = "int")

#######################################################################################################################
  ####################here we do calculations for per call level#####################################
  all_calls <- data.frame()
  for (file in unique(all_playbacks_combined$filename)) {
    #select one audio file
    one_track <- all_playbacks_combined[which(all_playbacks_combined$filename == file), ]
    pb_start_t <- min(one_track[which(one_track$label == "pb"), "start time"]) #get time when playback started
    pb_end_t   <- max(one_track[which(one_track$label == "pb"), "end time"]) +1 #get time when playback ended
    
    start_t <- one_track[which(one_track$label == "start"), "start time"] #get time when experiment started
    
    if (length(start_t) == 0) {start_t <- 1} #if no marker set time to 1 sec
    
    stop_t   <- min(one_track[which(one_track$label %in% stop_markers), "end time"]) #get time when experiment stopped
    
    if (stop_t < pb_end_t) {next}
    
    #get playback times for the track
    pb_times <- one_track[which(one_track$label =="pb"), 2:4]
    #calculate intervals between the playback calls
    for ( i in 1:nrow(pb_times)-1){
    pb_times$int[i] <- pb_times$`end time`[i+1] - pb_times$`start time`[i]
    }
    
    #remove multiple note call intervals
    for (i in 1:(nrow(pb_times)-1)){
      if(pb_times$int[i] < 0.5) {
        pb_times$`end time`[i] <- NA
        pb_times$`start time`[i+1] <- NA
      }
    }
    #get updated playback times
    pb_times_upd <- cbind(pb_times[which(!is.na(pb_times$`start time`)), "start time"], pb_times[which(!is.na(pb_times$`end time`)), "end time"])
    pb_times_upd <- as.data.frame(pb_times_upd)
    colnames(pb_times_upd) <- c("startT", "EndT")
    
    #adding one more row for defining boundary of a last PB call and adding 3 seconds for a response time
    pb_times_upd[nrow(pb_times_upd)+1 ,  ] <- pb_times_upd[nrow(pb_times_upd) , ] +3
    #calculate response latency
    
    for (i in 1:(nrow(pb_times_upd)-1)){
      
      #calculate reply latency for each call
      latency <- min(one_track[which(one_track$label != "pb" & 
                                       one_track$`start time` > pb_times_upd$startT[i] & 
                                       one_track$`start time` < pb_times_upd$startT[i+1]) ,"start time"]) - pb_times_upd$startT[i]
      
       #calculate estimated latency for a simulated call given at the same time during control phase
      latency_cont <- min(one_track[which(one_track$label != "pb" & one_track$`start time` > start_t & one_track$`start time` < pb_times_upd$startT[1] &
                                            one_track$`start time` > (start_t + pb_times_upd$startT[i] - pb_start_t) & 
                                            one_track$`start time` < (start_t + pb_times_upd$startT[i+1] - pb_start_t)) ,"start time"]) - 
                                      (start_t + pb_times_upd$startT[i] - pb_start_t)
      
      
      latency_post <- min(one_track[which(one_track$label != "pb" & one_track$`start time` > pb_end_t & one_track$`start time`< stop_t &
                                            one_track$`start time` > (pb_end_t + pb_times_upd$startT[i] - pb_start_t) & 
                                            one_track$`start time` < (pb_end_t + pb_times_upd$startT[i+1] - pb_start_t)) ,"start time"]) - 
                                      (pb_end_t + pb_times_upd$startT[i] - pb_start_t)
      #count number of calls given in response 
      n_calls <- nrow(one_track[which(one_track$label != "pb" & 
                                        one_track$`start time` > pb_times_upd$startT[i] & 
                                        one_track$`start time` < pb_times_upd$startT[i+1]) ,])
      
      n_calls_cont <- nrow(one_track[which(one_track$label != "pb" & one_track$`start time` > start_t &
                                             one_track$`start time` > (start_t + pb_times_upd$startT[i] - pb_start_t) & 
                                             one_track$`start time` < (start_t + pb_times_upd$startT[i+1] - pb_start_t)) ,])
      
      n_calls_post <- nrow(one_track[which(one_track$label != "pb" & one_track$`start time` > pb_end_t & one_track$`start time`< stop_t &
                                             one_track$`start time` > (pb_end_t + pb_times_upd$startT[i] - pb_start_t) & 
                                             one_track$`start time` < (pb_end_t + pb_times_upd$startT[i+1] - pb_start_t)) ,])
      #calculate response vocal duration
      vocal_dur <- sum(one_track[which(one_track$label != "pb" & one_track$`start time` > start_t &
                                     one_track$`start time` > pb_times_upd$startT[i] & 
                                     one_track$`start time` < pb_times_upd$startT[i+1]) , "duration"])
      vocal_dur_cont <- sum(one_track[which(one_track$label != "pb" & 
                                              one_track$`start time` > (start_t + pb_times_upd$startT[i] - pb_start_t) & 
                                              one_track$`start time` < (start_t + pb_times_upd$startT[i+1] - pb_start_t)) , "duration"])
      
      vocal_dur_post <- sum(one_track[which(one_track$label != "pb" & one_track$`start time` > pb_end_t & one_track$`start time`< stop_t &
                                              one_track$`start time` > (pb_end_t + pb_times_upd$startT[i] - pb_start_t) & 
                                              one_track$`start time` < (pb_end_t + pb_times_upd$startT[i+1] - pb_start_t)) ,"duration"])
     
       #calculate the time interval between Playback calls
      interval <- pb_times_upd$startT[i+1] - pb_times_upd$startT[i]
      
      sum_stats <- c(one_track$filename[1], one_track$date[1], one_track$year[1], one_track$group[1],one_track$focal_code[1], one_track$playback_code[1],
                     latency, n_calls, vocal_dur, interval, latency_cont, n_calls_cont,  vocal_dur_cont, latency_post, n_calls_post,  vocal_dur_post)
      all_calls <- rbind(all_calls, sum_stats)
    }
    
  }
  
  colnames(all_calls) <- c("file", "date", "year", "group", "focal_code", "playback_code", "latency", "n_calls", "vocal_dur", "interval",
                           "latency_cont", "n_calls_cont",  "vocal_dur_cont", "latency_post", "n_calls_post", "vocal_dur_post")
  all_calls[ , 7] <-  as.numeric(all_calls[, 7])
  all_calls[ , 8] <-  as.numeric(all_calls[, 8])
  all_calls[ , 9] <-  as.numeric(all_calls[, 9])
  all_calls[ , 10] <-  as.numeric(all_calls[, 10])
  all_calls[ , 11] <-  as.numeric(all_calls[, 11])
  all_calls[ , 12] <-  as.numeric(all_calls[, 12])
  all_calls[ , 13] <-  as.numeric(all_calls[, 13])
  all_calls[ , 14] <-  as.numeric(all_calls[, 14])
  all_calls[ , 15] <-  as.numeric(all_calls[, 15])
  all_calls[ , 16] <-  as.numeric(all_calls[, 16])
  
  
  all_calls$rate <- all_calls$n_calls/all_calls$interval
  all_calls$rate_cont <- all_calls$n_calls_cont/all_calls$interval
  all_calls$rate_post <- all_calls$n_calls_post/all_calls$interval
  
  
  #combining the playbacks data with the social ties
  
  #removing all rows with missing playback_codes as those are failed playbacks
  all_calls <-all_calls[which(!is.na(all_calls$playback_code)) ,]
  
  #renaming groups to short codes
  all_calls[which(all_calls$group == "Zulus") , "group"] <- "ZU"
  all_calls[which(all_calls$group == "Lazuli") , "group"] <- "LAZ"
  all_calls[which(all_calls$group == "Jaxx") , "group"] <- "JX"
  all_calls[which(all_calls$group == "Whiskers") , "group"] <- "W"
  
   
  
  for (i in 1:nrow(all_calls)) {
    
    Z_Tie.lenght <- network_ties_norm[which(network_ties_norm$ID1 == all_calls$focal_code[i] & network_ties_norm$ID2 == all_calls$playback_code[i] 
                                            & network_ties_norm$year == all_calls$year[i]) , "Z_Tie.lenght"]
    Z_Tie.lenght <- as.numeric(Z_Tie.lenght)
    if (is.na(Z_Tie.lenght)) {all_calls$tie_lenghts[i] <- 
      network_ties_norm[which(network_ties_norm$ID2 == all_calls$focal_code[i] & network_ties_norm$ID1 == all_calls$playback_code[i] 
                              & network_ties_norm$year == all_calls$year[i]) , "Z_Tie.lenght"]
    }else{all_calls$tie_lenghts[i] <- Z_Tie.lenght}
    
    
    Z_Tie.strenght <-  network_ties_norm[which(network_ties_norm$ID1 == all_calls$focal_code[i] & network_ties_norm$ID2 == all_calls$playback_code[i] 
                                               & network_ties_norm$year == all_calls$year[i]) , "Z_Tie.strenght"]
    
    Z_Tie.strenght <- as.numeric(Z_Tie.strenght)
    
    if(is.na(Z_Tie.strenght)) {all_calls$tie_strenght[i] <- 
      network_ties_norm[which(network_ties_norm$ID2 == all_calls$focal_code[i] & network_ties_norm$ID1 == all_calls$playback_code[i] 
                              & network_ties_norm$year == all_calls$year[i]) , "Z_Tie.strenght"]
    }else{all_calls$tie_strenght[i] <-Z_Tie.strenght}
    
    
    
    Z_Tie.lenght.adj <- network_ties_norm[which(network_ties_norm$ID1 == all_calls$focal_code[i] & network_ties_norm$ID2 == all_calls$playback_code[i] 
                                                & network_ties_norm$year == all_calls$year[i]) , "Z_Tie.lenght.adj"]
    Z_Tie.lenght.adj <- as.numeric(Z_Tie.lenght.adj)
    
    if (is.na(Z_Tie.lenght.adj)) {all_calls$tie_lenghts.adj[i] <- 
      network_ties_norm[which(network_ties_norm$ID2 == all_calls$focal_code[i] & network_ties_norm$ID1 == all_calls$playback_code[i] 
                              & network_ties_norm$year == all_calls$year[i]) , "Z_Tie.lenght.adj"]
    }else{all_calls$tie_lenghts.adj[i] <- Z_Tie.lenght.adj}
    
    
    
  }
  
  all_calls[which(all_calls$latency == Inf), "latency"] <- NA
  all_calls[which(all_calls$latency_cont == Inf), "latency_cont"] <- NA
  all_calls[which(all_calls$latency_post == Inf), "latency_post"] <- NA
  
  all_calls$diff_latency <- all_calls$latency - all_calls$latency_cont
  all_calls$diff_rate <- (all_calls$rate+1)/(all_calls$rate_cont+1)
  all_calls$diff_duration <- all_calls$vocal_dur-all_calls$vocal_dur_cont
  all_calls$diff_calls <- all_calls$n_calls - all_calls$n_calls_cont
  
  all_calls$diff_latency_post <- all_calls$latency_post - all_calls$latency_cont
  all_calls$diff_rate_post <- (all_calls$rate_post+1)/(all_calls$rate_cont+1)
  all_calls$diff_duration_post <- all_calls$vocal_dur_post-all_calls$vocal_dur_cont
  all_calls$diff_calls_post <- all_calls$n_calls_post - all_calls$n_calls_cont
  
  #write.csv(all_calls, "call_level_measurments.csv")
  
  diff_rate<- ggplot(data =all_calls,  aes(x = tie_lenghts.adj , y =  diff_rate)) + geom_smooth(method = "lm")+ geom_point()
  
  diff_latency<- ggplot(data = all_calls, aes(x = tie_lenghts.adj, y  = diff_latency)) + geom_smooth(method = "lm")+geom_point()
  
  diff_duration<- ggplot(data = all_calls, aes(x = tie_lenghts.adj, y  = diff_duration)) + geom_smooth(method = "lm")+geom_point()
  
  diff_calls <- ggplot(data = all_calls, aes(x = tie_lenghts.adj, y  = diff_calls)) + geom_smooth(method = "lm")+geom_point()
  
  
  ggarrange(diff_rate, diff_latency, diff_duration,   diff_calls,
            labels = c("Call_rate", "Response latency", "Vocal duration", "Call count"),
            ncol = 2, nrow = 2)
  
  
  
  diff_rate_post <- ggplot(data =all_calls, aes(y = diff_rate_post, x =  tie_lenghts.adj)) + geom_smooth(method = "lm")+ geom_point()
  
  diff_duration_post <- ggplot(data =all_calls, aes(y = diff_duration_post, x = tie_lenghts.adj)) + geom_smooth(method = "lm")+ geom_point()
  
  diff_latency_post <-ggplot(data = all_calls, aes(x = tie_lenghts.adj, y  = diff_latency_post)) + geom_smooth(method = "lm")+geom_point()
  
  diff_calls_post <- ggplot(data = all_calls, aes(x = tie_lenghts.adj, y  = diff_calls_post)) + geom_smooth(method = "lm")+geom_point()
  
  
  ggarrange(diff_rate_post, diff_latency_post, diff_duration_post,   diff_calls_post,
            labels = c("Call_rate", "Response latency", "Vocal duration", "Call count"),
            ncol = 2, nrow = 2)
  
  model <- lmer(data = all_calls,
                diff_latency  ~  tie_lenghts.adj + tie_strenght+ (1|file/focal_code)) 
  
  summary(model)
  
  
  
  ########################################################################
  #### looking at progression of the response along the playback phase ###
  ########################################################################
  

  all_calls <-  all_calls %>%
    group_by(file) %>% mutate(ord = row_number())
  
  
  all_calls$tie_lenghts.adj <- as.numeric(all_calls$tie_lenghts.adj)
  all_calls$diff_rate <- as.numeric(all_calls$diff_rate)
  all_calls$ord <- as.factor(all_calls$ord)
  
  ggplot(data = all_calls[which(all_calls$ord %in% c(1:10)),], aes(x = tie_lenghts.adj, y = diff_rate, color = ord)) + geom_point()+geom_smooth(method = "gam")
  ggplot(data = all_calls[which(all_calls$ord %in% c(1:8)),], aes(x = tie_lenghts.adj, y = diff_latency, color = ord)) + geom_point()+geom_smooth(method = "lm")
 
  ggplot(data = all_calls[which(all_calls$ord %in% c(1:5)),], aes(x = tie_lenghts, y = diff_rate)) + geom_point()+geom_smooth(method = "lm") +
  facet_wrap( ~ ord)