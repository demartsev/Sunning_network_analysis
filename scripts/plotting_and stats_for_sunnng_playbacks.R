### this is a (not so) minimal script for plotting and analysis if sunning playback data###
library(tidyverse)
library(ggpubr)
library(gridExtra)
library(lme4)
library(lmerTest)
library(car)
library(cowplot)
library(sjmisc)
library(dplyr)
library(lemon)
library(grid)
library(performance)
library(ggsignif)

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


             ######## LOOKS LIKE AN INTERACTION BETWEEN DOMINANCE AND tie_lenghts.adj ########
             ######## the DOM-SUB dyad showing a trend of higher call rate when       ########
             ######## a subordinate is responding to a dominant with whom it is not   ########
             ######## bonded                                                          ########
             ######## looking at recovery phase does not look promising so we will    ########
             ######## skip it for now                                                 ########

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


        #plotting general changes in call rate by dominance dyad (not looking at the actual bond strength)

my_comparisons = list(c("Dominant_Sub", "Sub_Dominant"),  c("Sub_Dominant", "Sub_Sub"), c("Dominant_Sub","Sub_Sub"))

CR <- ggplot(data = call_rates_deltas[which(call_rates_deltas$type == "call_rate_(pb-cont)" & 
                                      call_rates_deltas$dom_paired != "Dominant_Dominant"), ],
       aes(y = value, x = dom_paired )) + geom_boxplot() + theme_light() + stat_compare_means(comparisons = my_comparisons)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
       labs(title = "Call rate by dominance dyad") 

#plotting general changes in call rate by dominance dyad (not looking at the actual bond strength)
VR <- ggplot(data = call_rates_deltas[which(call_rates_deltas$type == "vocal_rate_(pb-cont)" & 
                                        call_rates_deltas$dom_paired != "Dominant_Dominant"), ],
       aes(y = value, x = dom_paired )) + geom_boxplot() + theme_light() + stat_compare_means(comparisons = my_comparisons)+
  labs(title = "vocal rate by dominance dyad") 

grid.arrange(CR, VR)
       
 ### there is a general increase in call rate during playback phase, sight stronger in Dom-Sub
 ### dyads. Might be significant above 0 but not sure if different from other 
 ### dyad combination. NOT SURE WHAT IS THE BEST WAY TO TEST IT!!!! (TODO)
 ### feels like that most of the effect is explained by dominance and the detailed bond strenghts 
 ###  are having a relatively small effect ####

 
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
 

check_model(red_call_rate) #looks reasonable
 
check_collinearity(red_call_rate) #no collegiality - GOOD
check_normality(red_call_rate)    #residuals not normally distributed. 
plot(check_normality(red_call_rate)) #But plot is rather horrible
check_heteroscedasticity(red_call_rate)  #high heteroscedasity - BAD
plot(check_heteroscedasticity(red_call_rate))
plot(check_outliers(red_call_rate))     #no outlines - GOOD
plot(check_distribution(red_call_rate))  #dosn't seem to fit with any distribution

library(sjPlot)
plot_model(red_call_rate, type = "int",  title = "Predicted value of Call rate")  #call rate
tab_model(red_call_rate)
#probably wrong model and  generally not significant. SUB_SUB dyad is showing weak negative trend
# call rate is positively correlated with tie length

#### now we repeat the same analysys but now looking at vocal rate ####

#reduced model - no dom-dom trials, no tie_strength
red_vocal_rate <- lmer(data = call_rates_deltas[which(call_rates_deltas$type == "vocal_rate_(pb-cont)" & 
                                                       call_rates_deltas$dom_paired != "Dominant_Dominant") , ],
                      value ~ tie_lenghts.adj*dom_paired  + (1|focal_code))


check_model(red_vocal_rate) #looks reasonable

check_collinearity(red_vocal_rate) #no collegiality - GOOD
check_normality(red_vocal_rate)    #residuals not normally distributed. 
plot(check_normality(red_vocal_rate)) #But plot is rather horrible
check_heteroscedasticity(red_vocal_rate)  #high heteroscedasity - BAD
plot(check_heteroscedasticity(red_vocal_rate))
plot(check_outliers(red_vocal_rate))     #no outlines - GOOD
plot(check_distribution(red_vocal_rate))  #dosn't seem to fit with any distribution

library(sjPlot)
plot_model(red_vocal_rate, type = "int",  title = "Predicted value of vocal rate")  #call rate
tab_model(red_vocal_rate)
#probably wrong model again. SUB_SUB dyad is showing significant negative trend
# call rate is positively correlated with tie length

 ##### THE MODELS NEEDS TO BE CHECKED TO RESOLVE THE ASSUMPTIONS VIOLATIONS ###



library(DHARMa)

simulationOutput <- simulateResiduals(fittedModel = red_vocal_rate)
plot(simulationOutput)
testZeroInflation(simulationOutput)
#summary(red_vocal_rate)
Anova(red_vocal_rate)

#TODO

# figure out zero inflation
#add contrast analysis and stats

 ##THIS IS THE END OF TRIAL LEVEL PART ################################################
########################################################################################
########################################################################################

################## WE ARE STARTING WITH CALL LEVEL ANALYSIS  ############################
#########################################################################################
######################################################################################### 



#we are now working with the all_calls dataframe

#COLUMN EXPALNATION:
#diff_latency = response latency to a playback call - response latency to a simulated playback call during control phase
#diff_rate    =  call rate following a playback call /  call rate following a simulated playback call during control phase
#diff_duration = vocal duration following a playback call  - vocal duration following a simulated playback call during control phase
#diff_calls   =  call count following a playback call - call count following a simulated playback call during control phase


#adding the dominance pairs column to the call data
for (i in 1:nrow(all_calls)) {
  all_calls$dom_pairs[i] <- 
    as.character(call_rates_deltas[which(call_rates_deltas$file == all_calls$file[i]), "dom_paired"][1])} 

all_calls$dom_pairs <- as.factor(all_calls$dom_pairs)
  ##plotting four different variables as a function of tie_lengths adjusted
diff_rate<- ggplot(data =all_calls,  aes(x = tie_lenghts.adj , y =  diff_rate)) + geom_smooth(method = "lm")+ geom_point()
diff_latency<- ggplot(data = all_calls, aes(x = tie_lenghts.adj, y  = diff_latency)) + geom_smooth(method = "lm")+geom_point()
diff_duration<- ggplot(data = all_calls, aes(x = tie_lenghts.adj, y  = diff_duration)) + geom_smooth(method = "lm")+geom_point()
diff_calls <- ggplot(data = all_calls, aes(x = tie_lenghts.adj, y  = diff_calls)) + geom_smooth(method = "lm")+geom_point()


ggarrange(diff_rate, diff_latency, diff_duration,   diff_calls,
          labels = c("Call_rate", "Response latency", "Vocal duration", "Call count"),
          ncol = 2, nrow = 2)
  #### response latency is shorter for shorter ties
##### since latency is the only potentially affected variable ######
##### we will look at latency acording to dominance pairs  ######
##### and removing Dom-Dom again since very little data ########


##plotting four different variables as a function of dominance dyads only
dom_diff_rate <-     ggplot(data = all_calls[which(all_calls$dom_pairs != "Dominant_Dominant"),], aes(x = dom_pairs , y =  diff_rate)) + geom_boxplot() + stat_compare_means(comparisons = my_comparisons)
dom_diff_latency <-  ggplot(data = all_calls[which(all_calls$dom_pairs != "Dominant_Dominant"),], aes(x = dom_pairs, y  = diff_latency)) + geom_boxplot() + stat_compare_means(comparisons = my_comparisons)
dom_diff_duration <- ggplot(data = all_calls[which(all_calls$dom_pairs != "Dominant_Dominant"),], aes(x = dom_pairs, y  = diff_duration)) + geom_boxplot() + stat_compare_means(comparisons = my_comparisons)
dom_diff_calls <-    ggplot(data = all_calls[which(all_calls$dom_pairs != "Dominant_Dominant"),], aes(x = dom_pairs, y  = diff_calls)) + geom_boxplot() + stat_compare_means(comparisons = my_comparisons)

ggarrange(dom_diff_rate, dom_diff_latency, dom_diff_duration,   dom_diff_calls,
          labels = c("Call_rate", "Response latency", "Vocal duration", "Call count"),
          ncol = 2, nrow = 2)

### NO CLEAR EFFECT OS VISIBLE FOR ANY OF THE VARIABLES ###

###################################################################################################
 # Here I am not sure how Latency should be treated. Does it makes sense in looking at latency after simulated calls?
 # or should latency be analyzed only within the Playback Phase

#plotting latency on a by_call level
ggplot(data = all_calls[which(all_calls$dom_pairs != "Dominant_Dominant"),] , aes(x = tie_lenghts.adj, y  = latency)) + geom_smooth(method = "lm")+ geom_point() + 
  labs(title = "Latency vs Tie lenght") + facet_wrap(~ dom_pairs)

#plotting latency on a by_call level. Playback - Control
ggplot(data = all_calls[which(all_calls$dom_pairs != "Dominant_Dominant"),] , aes(x = tie_lenghts.adj, y  = diff_latency)) + geom_smooth(method = "lm")+ geom_point() + 
  labs(title = "Diff_Latency vs Tie lenght") + facet_wrap(~ dom_pairs)


## Additional plots exploring other variables by dominance pair

  
#plotting call_rate on a by_call level. Playback - Control
ggplot(data = all_calls[which(all_calls$dom_pairs != "Dominant_Dominant"),] , aes(x = tie_lenghts.adj, y  = diff_rate)) + geom_smooth(method = "lm")+ geom_point() + 
  facet_wrap(~ dom_pairs)
#plotting call duration on a by_call level. Playback - Control
ggplot(data = all_calls[which(all_calls$dom_pairs != "Dominant_Dominant"),] , aes(x = tie_lenghts.adj, y  = diff_duration)) + geom_smooth(method = "lm")+ geom_point() + 
  facet_wrap(~ dom_pairs)


#############################################################################################################
#Addition (Vlad 210323) number of calls until first response
summ <- data.frame() 
for (trial in unique(all_calls$file)) {
   
  trial_select <- all_calls[which(all_calls$file == trial), ] 
  which(trial_select$n_calls !=0)[1]
  combine_data <- data.frame(c(trial_select[1, c(1:7, 21:23, 32)],  which(trial_select$n_calls !=0)[1]))
  colnames( combine_data) <- c("X", "file", "date", "year",  "group", "focal_code", "playback_code",  "tie_lenghts", "tie_strenght", "tie_lenghts.adj", "dom_pairs",  "first_resp")   
  summ <- rbind(summ,combine_data )
 }


summ$first_resp <- as.factor(summ$first_resp)
ggplot (data = summ, aes(x = first_resp, y = tie_lenghts.adj)) + geom_boxplot()








 ###STATS ###
        ### LATENCY
call_latency <- lmer(data = all_calls[which(all_calls$dom_pairs != "Dominant_Dominant"),],
                  latency ~ tie_lenghts.adj*dom_pairs + tie_strenght*dom_pairs + (1|focal_code))

summary(call_latency)
check_model(call_latency)
check_collinearity(call_latency) #large VIF model needs to be redesigned

#reduced model - no dom-dom trials, no tie_strength
red_call_latency <- lmer(data = all_calls[which(all_calls$dom_pairs != "Dominant_Dominant"),],
                         latency ~ tie_lenghts.adj*dom_pairs  + (1|focal_code))


check_model(red_call_latency) #looks reasonable

check_collinearity(red_call_latency) #no collegiality - GOOD
check_normality(red_call_latency)    #residuals not normally distributed. 
plot(check_normality(red_call_latency)) #But plot is rather horrible
check_heteroscedasticity(red_call_latency)  #high heteroscedasity - BAD
plot(check_heteroscedasticity(red_call_latency))
plot(check_outliers(red_call_latency))     #no outlines - GOOD
plot(check_distribution(red_call_latency))  #seem to fit tweedie distribution, whatever that is


#model summary
plot_model(red_call_latency, type = "int",  title = "Predicted value of latency")  #call latency
tab_model(red_call_latency)

#no effect##


### CALL RATE ###
red_call_rate <- lmer(data = all_calls[which(all_calls$dom_pairs != "Dominant_Dominant"),],
                     diff_rate ~ tie_lenghts.adj*dom_pairs + (1|focal_code))

summary(call_rate)
check_model(red_call_rate)

check_collinearity(red_call_rate) #no collegiality - GOOD
check_normality(red_call_rate)    #residuals not normally distributed. 
plot(check_normality(red_call_rate)) #But plot is rather horrible
check_heteroscedasticity(red_call_rate)  #high heteroscedasity - BAD
plot(check_heteroscedasticity(red_call_rate))
plot(check_outliers(red_call_rate))     #no outlines - GOOD
plot(check_distribution(red_call_rate))  #no good fit to anything


#model summary
plot_model(red_call_rate, type = "int",  title = "Predicted value of call rate")  #call rate
tab_model(red_call_rate)

#no effect##

### CALL duration ###
red_call_duration <- lmer(data = all_calls[which(all_calls$dom_pairs != "Dominant_Dominant"),],
                      diff_duration ~ tie_lenghts.adj*dom_pairs + (1|focal_code))

summary(red_call_duration)
 
#model summary
plot_model(red_call_duration, type = "int",  title = "Predicted value of call duration")  #call rate
tab_model(red_call_duration)

#small efect of interaction between tie lengths and dom_pairs


### CALL COUNT ###
red_call_count <- lmer(data = all_calls[which(all_calls$dom_pairs != "Dominant_Dominant"),],
                          diff_calls ~ tie_lenghts.adj*dom_pairs + (1|focal_code))

summary(red_call_count)

#model summary
plot_model(red_call_count, type = "int",  title = "Predicted value of call rate")  #call rate
tab_model(red_call_count)
##########################################################################################################################
#####    SUMMARY OF THIS PART
# Nothing is really happening on the call level besides the call duration which is showing similar results 
# to the full trial level of analysys. So I am thinking about dropping the per call part and only report the 
# not very impressive results on the level of trial 



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


#call rate progressionn for diferent dyads
ggplot(data = all_calls[which(all_calls$dom_pairs != "Dominant_Dominant"),],
       aes(x = bin, y = diff_rate, color = dom_pairs)) + geom_smooth(method = "lm") 
ggplot(data = all_calls[which(all_calls$dom_pairs != "Dominant_Dominant"),],
       aes(x = bin, y = diff_duration, color = dom_pairs)) + geom_smooth(method = "lm") 
ggplot(data = all_calls[which(all_calls$dom_pairs != "Dominant_Dominant"),],
      aes(x = bin, y = diff_calls, color = dom_pairs)) + geom_smooth(method = "lm") 
ggplot(data = all_calls[which(all_calls$dom_pairs != "Dominant_Dominant"),],
       aes(x = bin, y = latency, color = dom_pairs)) + geom_smooth(method = "lm") 

#call rate progression for diferent dyads in recovery phase
ggplot(data = all_calls[which(all_calls$dom_pairs != "Dominant_Dominant"),],
       aes(x = bin, y = diff_rate_post, color = dom_pairs)) + geom_smooth(method = "lm") 
ggplot(data = all_calls[which(all_calls$dom_pairs != "Dominant_Dominant"),],
       aes(x = bin, y = diff_duration_post, color = dom_pairs)) + geom_smooth(method = "lm") 
ggplot(data = all_calls[which(all_calls$dom_pairs != "Dominant_Dominant"),],
       aes(x = bin, y = diff_calls_post, color = dom_pairs)) + geom_smooth(method = "lm") 

#adding tie lengths categories

all_calls<- all_calls %>% mutate(tie_bins = cut(tie_lenghts.adj, 
                                       breaks= c(min(all_calls$tie_lenghts.adj)-1, mean(all_calls$tie_lenghts.adj), max(all_calls$tie_lenghts.adj)+1)))


lat <- ggplot(data = all_calls[which(all_calls$dom_pairs != "Dominant_Dominant"),], aes(x = bin, y = latency, col = tie_bins)) + 
  geom_smooth(method = "lm") + labs(x = "") + facet_wrap( ~ dom_pairs)
rate <- ggplot(data = all_calls[which(all_calls$dom_pairs != "Dominant_Dominant"),], aes(x = bin, y = diff_rate, col = tie_bins)) + 
  geom_smooth(method = "lm")   + labs(x = "") + facet_wrap( ~ dom_pairs)
dur <- ggplot(data = all_calls[which(all_calls$dom_pairs != "Dominant_Dominant"),], aes(x = bin, y = diff_duration, col = tie_bins)) + 
  geom_smooth(method = "lm")  + labs(x = "Playback phase progression") + facet_wrap( ~ dom_pairs)


plot <- ggarrange(lat, rate, dur,
          labels = c("Response latency", "Call rate", "Vocal duration"),
          ncol = 1, nrow = 3)
annotate_figure(plot, top = text_grob("Progression of a responce during playback stimulus. Weak and strong ties comparison", 
                                      color = "red", face = "bold", size = 14))

#####################################################
#### looking at progression of the recovery phase ###
#####################################################


calls_rec <- ggplot(data = all_calls[which(all_calls$dom_pairs != "Dominant_Dominant"),], aes(x = bin, y = diff_calls_post, col = tie_bins)) + 
  geom_smooth(method = "lm") + labs(x = "") + facet_wrap( ~ dom_pairs)
rate_rec <- ggplot(data = all_calls[which(all_calls$dom_pairs != "Dominant_Dominant"),], aes(x = bin, y = diff_rate_post, col = tie_bins)) + 
  geom_smooth(method = "lm")   + labs(x = "") + facet_wrap( ~ dom_pairs)
dur_rec <- ggplot(data = all_calls[which(all_calls$dom_pairs != "Dominant_Dominant"),], aes(x = bin, y = diff_duration_post, col = tie_bins)) + 
  geom_smooth(method = "lm")  + labs(x = "Playback phase progression") + facet_wrap( ~ dom_pairs)


plot_rec <- ggarrange(calls_rec, rate_rec, dur_rec,
                  labels = c("Recovery_call_count", "Recovery Call rate", "Recovery vocal duration"),
                  ncol = 1, nrow = 3)
annotate_figure(plot_rec, top = text_grob("Progression of a recovery from playback stimulus. Weak and strong ties comparison", 
                                      color = "red", face = "bold", size = 14))



