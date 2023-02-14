### this is a (not so) minimal script for plotting and analysis if sunning playback data###
library(tidyverse)
library(ggpubr)
library(gridExtra)
library(lme4)
library(lmerTest)
library(car)
library(sjPlot)
library(sjmisc)
library(dplyr)
library(lemon)
library(cowplot)
library(grid)
library(performance)

#######################################################################################
#data loading
load("sunning_results.Rdata")
# you are getting two data frames:
 #call_rates_deltas is the output in the level of the trial
 #all_calls is the output on the level of each call

################## WE ARE STARTING WITH TRIAL LEVEL ANALYSIS  #######################################
#####################################################################################################

    #COLUMN EXPALNATION:
 #value = call rate (number of calls per sec) or vocal rate (time of calls per sec). Dependent on the "type" column
 #type = the index of the calculated value. pb-cont = difference between control and playback phases
 #                                          recovery-cont = difference between control and recovery phases 
 #tie_lengths = as it sounds. Higher value indicates weaker ties
 #tie_lengths_adj = adjusted to number of links
 #tie_strength = weight of the link (higher is stronger but there are zero values)
 #dom_paired = dominance pairs. First is the playback, second is the receiver

 #plotting all the data just to eyeball it. 
     #looking at tie_lengths
ggplot(data = call_rates_deltas, aes(y = value, x = tie_lenghts )) + geom_smooth(method = "lm")+ geom_point() + 
  facet_grid( ~ type)

      ##looking at adjusted tie_lengths
ggplot(data = call_rates_deltas, aes(y = value, x = tie_lenghts.adj )) + geom_smooth(method = "lm")+ geom_point() + 
  facet_grid( ~ type)

     ##looking at tie_strength
ggplot(data = call_rates_deltas, aes(y = value, x = tie_strenght)) + geom_smooth(method = "lm")+ geom_point() + 
  facet_grid( ~ type)
 
### the data looks reasonably homogeneous for the tie length values but no convincing pattern is seen ####

  ###making nicer plots###
  # we are breaking the data by dominance dyads. The DOm-Dom dyad is under-represented with only two data points
     table(call_rates_deltas$dom_paired)

  ####so we are filtering Dom-Dom from all subsequent plotting and analysis
   


#setting some names for nicer plotting
new_names_1 <- c("Playback", "Recovery", "Playback", "Recovery")
names(new_names_1) <- c("call_rate_(pb-cont)", "call_rate_(recovery-cont)", "vocal_rate_(pb-cont)", "vocal_rate_(recovery-cont)")


###         #plotting call_rate as a function of adjusted tie length for the 3 possible dominance combinations
###    ggplot(data = call_rates_deltas[which(grepl("call", call_rates_deltas$type)& 
###                                            call_rates_deltas$dom_paired != "Dominant_Dominant"), ],
###           aes(y = value, x = tie_lenghts.adj, colour = dom_paired  )) + geom_smooth(method = "lm")+ geom_point() + theme_bw() + ylab("call_rate") +
###      facet_grid( ~ type, labeller = labeller(type = new_names))
###           
###    
###       #plotting vocal_rate as a function of adjusted tie length for the 3 possible dominance combinations
###    
###    ggplot(data = call_rates_deltas[which(grepl("vocal", call_rates_deltas$type)& 
###                                            call_rates_deltas$dom_paired != "Dominant_Dominant"), ],
###           aes(y = value, x = tie_lenghts.adj, colour = dom_paired  )) + geom_smooth(method = "lm")+ geom_point() + theme_bw() + ylab("vocal_rate") +
###      facet_grid( ~ type, labeller = labeller(type = new_names_1))
###    


#plotting call rate and vocal_rate as a function of adjusted tie length for the 3 possible dominance combinations

p <- ggplot(data = call_rates_deltas[which(call_rates_deltas$dom_paired != "Dominant_Dominant"), ],
       aes(y = value, x = tie_lenghts.adj, colour = dom_paired  )) + geom_smooth(method = "lm")+ geom_point() + theme_bw() +
  ylab(NULL) +
 facet_wrap(vars(type), nrow = 2, scales = "free", labeller = labeller(type = new_names_1))

a <- textGrob('Call_rate', rot = 90,  vjust = 3 ,gp=gpar(fontsize=10))
b <- textGrob('Vocal rate', rot = 90, vjust = 3 ,gp=gpar(fontsize=10))
plot_grid(plot_grid(a, b, ncol = 1), p, rel_widths = c(0.1, 1))


             ######## LOOKS LIKE AN INTERACTION BETWEEN DOMINANCE AND tie_lenghts.adj ###########
             ######## the DOM-SUB dyad showing nice trend of higher call rate when    ###########
             ######## a subordinate is responding to a dominant with whom it is not  ###########
             ######## bonded

      #plotting the same data but on separate panels 

     #call rate and tie_lengths.adj
ggplot(data = call_rates_deltas[which(call_rates_deltas$type == "call_rate_(pb-cont)" & 
                                        call_rates_deltas$dom_paired != "Dominant_Dominant"), ],
       aes(y = value, x = tie_lenghts.adj )) + geom_smooth(method = "lm")+ geom_point(position = "jitter") + 
  labs(title = "Call rate vs adjusted tie lenght") +
  theme_light() + facet_wrap(playback_dom_bin + focal_dom_bin ~ ., ncol = 2, scale = "free") + 
  theme(strip.text.x = element_text(size = 15))


      #vocal rate and tie_lengths.adj
ggplot(data = call_rates_deltas[which(call_rates_deltas$type == "vocal_rate_(pb-cont)" & 
                                        call_rates_deltas$dom_paired != "Dominant_Dominant"), ],
       aes(y = value, x = tie_lenghts.adj )) + geom_smooth(method = "lm")+ geom_point(position = "jitter") + 
  labs(title = "Vocal rate vs adjusted tie lenght") +
  theme_light() + facet_wrap( playback_dom_bin + focal_dom_bin ~ ., ncol = 2, scale = "free") + 
  theme(strip.text.x = element_text(size = 15))


           #call rate and tie_lengths (not adjusted)
ggplot(data = call_rates_deltas[which(call_rates_deltas$type == "call_rate_(pb-cont)" & 
                                        call_rates_deltas$dom_paired != "Dominant_Dominant"), ],
       aes(y = value, x = tie_lenghts )) + geom_smooth(method = "lm")+ geom_point(position = "jitter") + 
  labs(title = "Call rate vs  tie lenght") +
  theme_light() + facet_wrap( playback_dom_bin + focal_dom_bin ~ ., ncol = 2, scale = "free") + 
  theme(strip.text.x = element_text(size = 15))



          #vocal rate and tie_lengths (not adjusted)
ggplot(data = call_rates_deltas[which(call_rates_deltas$type == "vocal_rate_(pb-cont)" & 
                                        call_rates_deltas$dom_paired != "Dominant_Dominant"), ],
       aes(y = value, x = tie_lenghts )) + geom_smooth(method = "lm")+ geom_point(position = "jitter") + 
  labs(title = "vocal rate vs tie lenght") +
  theme_light() + facet_wrap( playback_dom_bin + focal_dom_bin ~ ., ncol = 2, scale = "free") + 
  theme(strip.text.x = element_text(size = 15))


        #plotting general changes in call rate by dominance dyad
ggplot(data = call_rates_deltas[which(call_rates_deltas$type == "call_rate_(pb-cont)" & 
                                      call_rates_deltas$dom_paired != "Dominant_Dominant"), ],
       aes(y = value, x = dom_paired )) + geom_boxplot() + theme_light() + 
       labs(title = "Call rate by dominance dyad") 
       
 ### there is a general increase in call rate during playback phase, sight stronger in Dom-Sub
        ### dyads but probably not significant as an overall effect
 
#____________________________________________________________________________________________________________________  
  
  

#reformat variables to characters
call_rates_deltas$group <- as.character(call_rates_deltas$group)
call_rates_deltas$focal_code <- as.factor(as.character(call_rates_deltas$focal_code))
call_rates_deltas$dom_paired <- as.factor(call_rates_deltas$dom_paired)

 
# create group_year variable to separate trials perforemd at the same group but in diferent years
call_rates_deltas$group_year <- paste(call_rates_deltas$group, call_rates_deltas$year, sep = "_")

#####################################################################################################
#####################################################################################################
       ### STATS ###
#####################################################################################################
### building full model for call rate
call_rate <- lmer(data = call_rates_deltas[which(call_rates_deltas$type == "call_rate_(pb-cont)" & 
                                                   call_rates_deltas$dom_paired != "Dominant_Dominant") , ],
                  value ~ tie_lenghts.adj*dom_paired + tie_strenght*dom_paired + (1|focal_code))

summary(call_rate)
check_model(call_rate)
check_collinearity(call_rate) #large VIF model needs to be redesigned

#reduced model - no dom-dom trials, no tie_strength
red_call_rate <- lmer(data = call_rates_deltas[which(call_rates_deltas$type == "call_rate_(pb-cont)" & 
                                                   call_rates_deltas$dom_paired != "Dominant_Dominant") , ],
                  value ~ tie_lenghts.adj*dom_paired  + (1|focal_code))
summary(model_red_rate)
 

check_model(model_red_rate) #not looking amazing at all
 
check_collinearity(model_red_rate) #no collegiality - GOOD
check_normality(model_red_rate)    #residuals not normally distributed. But plot is not horrible
plot(check_normality(model_red_rate))
check_heteroscedasticity(model_red_rate)  #high heteroscedasity - BAD
plot(check_heteroscedasticity(model_red_rate))
plot(check_outliers(model_red_rate))     #no outlines - GOOD
plot(check_distribution(model_red_rate))  #dosn't seem to fit with any distribution


plot_model(model_red_rate, type = "int",  title = "Predicted value of Call rate")  #call rate



#plotting the interaction term of the model
plot_model(model_red_vocal, type = "int", title = "Predicted value of Vocal rate") #vocal rate
plot_model(model_red_rate, type = "int",  title = "Predicted value of Call rate")  #call rate

 ##THIS IS THE END OF TRIAL LEVEL PART ################################################
########################################################################################
########################################################################################

################## WE ARE STARTING WITH CALL LEVEL ANALYSIS  ############################
#########################################################################################
######################################################################################### 
 
#we are now working with the all_call dataframe

#COLUMN EXPALNATION:
#diff_latency = response latency to a playback call - response latency to a simulated playback call during control phase
#diff_rate    =  call rate following a playback call /  call rate following a simulated playback call during control phase
#diff_duration = vocal duration following a playback call  - vocal duration following a simulated playback call during control phase
#diff_calls   =  call count following a playback call - call count following a simulated playback call during control phase

  ##plotting four different variables as a function of tie_lengths adjusted
diff_rate<- ggplot(data =all_calls,  aes(x = tie_lenghts.adj , y =  diff_rate)) + geom_smooth(method = "lm")+ geom_point()
diff_latency<- ggplot(data = all_calls, aes(x = tie_lenghts.adj, y  = diff_latency)) + geom_smooth(method = "lm")+geom_point()
diff_duration<- ggplot(data = all_calls, aes(x = tie_lenghts.adj, y  = diff_duration)) + geom_smooth(method = "lm")+geom_point()
diff_calls <- ggplot(data = all_calls, aes(x = tie_lenghts.adj, y  = diff_calls)) + geom_smooth(method = "lm")+geom_point()


ggarrange(diff_rate, diff_latency, diff_duration,   diff_calls,
          labels = c("Call_rate", "Response latency", "Vocal duration", "Call count"),
          ncol = 2, nrow = 2)
  #### response latency is going slightly up at the longer ties

  ##### since latency is the only potentially affected variable ######
  ##### we are breaking the latency cording to dominance pairs again ######
  ##### and removing Dom-Dom again since very little data ########

#adding the dominance pairs column to the call data
for (i in 1:nrow(all_calls)) {
all_calls$dom_pairs[i] <- 
  as.character(call_rates_deltas[which(call_rates_deltas$file == all_calls$file[i]), "dom_paired"][1])} 

#plotting latency on a by_call level. Playback - Control
ggplot(data = all_calls[which(all_calls$dom_pairs != "Dominant_Dominant"),] , aes(x = tie_lenghts.adj, y  = diff_latency)) + geom_smooth(method = "lm")+ geom_point() + 
  facet_wrap(~ dom_pairs)

#plotting latency on a by_call level. Recovery - Control
ggplot(data = all_calls[which(all_calls$dom_pairs != "Dominant_Dominant"),] , aes(x = tie_lenghts.adj, y  = diff_latency_post)) + geom_smooth(method = "lm")+ geom_point() + 
  facet_wrap(~ dom_pairs)


## Additional plots exploring other variables by dominance pair

  
#plotting call_rate on a by_call level. Playback - Control
ggplot(data = all_calls[which(all_calls$dom_pairs != "Dominant_Dominant"),] , aes(x = tie_lenghts.adj, y  = diff_rate)) + geom_smooth(method = "lm")+ geom_point() + 
  facet_wrap(~ dom_pairs)
#plotting call duration on a by_call level. Playback - Control
ggplot(data = all_calls[which(all_calls$dom_pairs != "Dominant_Dominant"),] , aes(x = tie_lenghts.adj, y  = diff_duration)) + geom_smooth(method = "lm")+ geom_point() + 
  facet_wrap(~ dom_pairs)
#plotting call count on a by_call level. Playback - Control
ggplot(data = all_calls[which(all_calls$dom_pairs != "Dominant_Dominant"),] , aes(x = tie_lenghts.adj, y  = diff_calls)) + geom_smooth(method = "lm")+ geom_point() + 
  facet_wrap(~ dom_pairs)



########################################################################
#### looking at progression of the response along the playback phase ###
########################################################################

#adding call count for each call in a playback
all_calls <-  all_calls %>%
  group_by(file) %>% mutate(ord = row_number())

#add bin value separating playback calls into 4 bins
all_calls <-  all_calls %>%
  group_by(file) %>% mutate(bin = ntile( n = 4))

#making call counts as factors
all_calls$ord <- as.factor(all_calls$ord)
all_calls$bin <- as.factor(all_calls$bin)

ggplot(data = all_calls[which(all_calls$ord %in% c(1:8) & all_calls$dom_pairs != "Dominant_Dominant"),], aes(x = tie_lenghts.adj, y = diff_rate, color = dom_pairs)) + 
  geom_point()+geom_smooth(method = "lm") + facet_wrap( ~ ord)

ggplot(data = all_calls[which(all_calls$ord %in% c(1:8) & all_calls$dom_pairs != "Dominant_Dominant"),], aes(x = tie_lenghts.adj, y = diff_rate, color = dom_pairs)) + 
  geom_point()+geom_smooth(method = "lm") + facet_wrap( ~ bin)


ggplot(data = all_calls[which(all_calls$ord %in% c(1:8) & all_calls$dom_pairs != "Dominant_Dominant"),], aes(x = tie_lenghts.adj, y = diff_latency, color = dom_pairs)) + 
  geom_point()+geom_smooth(method = "lm") + facet_wrap( ~ ord)

ggplot(data = all_calls[which(all_calls$ord %in% c(1:8) & all_calls$dom_pairs != "Dominant_Dominant"),], aes(x = tie_lenghts.adj, y = diff_duration, color = dom_pairs)) + 
  geom_point()+geom_smooth(method = "lm") + facet_wrap( ~ ord)


#####################################################
#### looking at progression of the recovery phase ###
#####################################################


ggplot(data = all_calls[which(all_calls$ord %in% c(1:8) & all_calls$dom_pairs != "Dominant_Dominant"),], aes(x = tie_lenghts.adj, y = diff_rate_post, color = dom_pairs)) + 
  geom_point()+geom_smooth(method = "lm") + facet_wrap( ~ ord)

ggplot(data = all_calls[which(all_calls$ord %in% c(1:8)),], aes(x = tie_lenghts.adj, y = diff_rate_post)) + 
  geom_point() + geom_smooth()+facet_wrap( ~ ord)

ggplot(data = all_calls[which(all_calls$ord %in% c(1:8)),], aes(x = tie_lenghts.adj, y = diff_rate_post)) + 
  geom_point() + geom_smooth()+facet_wrap( ~ bin)

ggplot(data = all_calls[which(all_calls$ord %in% c(1:8)),], aes(x = tie_lenghts.adj, y = diff_duration_post)) + 
  geom_point() + geom_smooth()+facet_wrap( ~ ord)

ggplot(data = all_calls[which(all_calls$ord %in% c(1:8)),], aes(x = tie_lenghts.adj, y = diff_calls_post)) + 
  geom_point() + geom_smooth()+facet_wrap( ~ ord)


#BOTTOM LINE: something is happening and dominance is definitely playing a role as well as social bonds.
#NEXT STEP: design proper models and run grownup tests for significance

 





                      
#####   #### the next four plots are looking at the recovery phase. The same data but comparing control with the post_playback time
#####   diff_rate_post <- ggplot(data =all_calls, aes(y = diff_rate_post, x =  tie_lenghts.adj)) + geom_smooth(method = "lm")+ geom_point()
#####   diff_duration_post <- ggplot(data =all_calls, aes(y = diff_duration_post, x = tie_lenghts.adj)) + geom_smooth(method = "lm")+ geom_point()
#####   diff_latency_post <-ggplot(data = all_calls, aes(x = tie_lenghts.adj, y  = diff_latency_post)) + geom_smooth(method = "lm")+geom_point()
#####   diff_calls_post <- ggplot(data = all_calls, aes(x = tie_lenghts.adj, y  = diff_calls_post)) + geom_smooth(method = "lm")+geom_point()


#####   ggarrange(diff_rate_post, diff_latency_post, diff_duration_post,   diff_calls_post,
#####         labels = c("Call_rate", "Response latency", "Vocal duration", "Call count"),
#####         ncol = 2, nrow = 2)


