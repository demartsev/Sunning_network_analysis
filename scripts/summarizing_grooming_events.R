setwd("C:/Users/vdemartsev/My Cloud/Sunning_network_analysis")

grooming_all <- read.csv("Groom_events2.csv")



library(stringr)
grooming_all$year <- str_sub(grooming_all$WatchDate, 7, 8)

years <- unique(grooming_all$year)
all_events <- data.frame()
for (year in years) {
  year_select <- grooming_all[which(grooming_all$year == year),]
  
  groups <- unique(year_select$GroupName)
  for (group in groups) {
    group_select <- year_select[which(year_select$GroupName == group), ]
    group_select <- group_select[which(as.Date(group_select$WatchDate, format = "%d-%m-%y") > as.Date(paste("01-04-", group_select$year[1], sep = ""), format = "%d-%m-%y") &
                                         as.Date(group_select$WatchDate, format = "%d-%m-%y") <  as.Date(paste("01-09-", group_select$year[1], sep = ""), format = "%d-%m-%y")),]
    groom_events <- unique(group_select$EventTime)
    
    for (event in groom_events) {
      event_select <-
        group_select[which(group_select$EventTime == event), ]
      event_select <- event_select[-grep("P0|P2|P1", event_select$Code), ]
      
      event_summary <-
        c(
          event_select$WatchDate[1],
          NA,
          paste(event_select$Code, collapse = " "),
          "grooming",
          event_select$GroupAbbrev[1],
          event_select$year[1]
        )
      
      all_events <- rbind(all_events, event_summary)
    }
  }
}

add_grooming <- read.csv("aditional_grooming.csv")
add_grooming$Date <- paste(substr(add_grooming$Date, 7, 8), "-", 
                           substr(add_grooming$Date, 5, 6), "-", 
                           substr(add_grooming$Date, 3, 4), sep = "")
colnames(add_grooming) <- colnames(all_events)
all_events <- rbind(all_events, add_grooming)

write.csv(all_events, "grooming.csv") 


