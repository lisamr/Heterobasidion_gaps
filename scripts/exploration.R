#LOAD DATA----

#load packages
rm(list=ls())
library(tidyverse)
library(sf)
library(scales)
library(brms)
library(tidybayes)
library(wesanderson)
theme_set(theme_tidybayes())


trees <- readxl::read_xlsx('data/Lisa_treeyr_w_plot.xlsx', col_types = c('text', 'text', 'numeric','numeric','numeric','numeric','numeric','numeric','numeric', 'text', 'numeric','numeric', 'text', 'text', 'text', 'text', 'numeric', 'numeric', 'text', 'text', 'text', 'text', 'text', 'text' ))
print(trees, width = Inf)


#lets look at just one plot. Objective: understand how distance from plot center affects mortality
Yos <- trees %>% 
  filter(Project == "Yosemite") %>% 
  mutate(L_D = ifelse(YearDead > SurveyYear | is.na(YearDead), "L", "D"))
print(Yos, width = Inf)

#plot all the trees
ggplot(Yos, aes(Distance, SurveyYear)) +
  geom_jitter(width = 0, height = 1, alpha = .5, size = 2, aes(color = L_D)) + 
  facet_wrap(~Plot)


#one of the plots
Yos %>% filter(Plot == "Yellow Pines") %>% 
  ggplot(., aes(Distance, SurveyYear)) +
  geom_jitter(width = 0, height = .5, alpha = .5, size = 2, aes(color = L_D)) + 
  facet_wrap(~Plot)

Yos %>% filter(Plot == "Yellow Pines") %>% 
  ggplot(., aes(Distance, YearDead)) +
  geom_jitter(width = 0, height = .5, alpha = .5, size = 2, aes(color = Species))

test <- Yos %>% 
  filter(!duplicated(TreeID), Plot == "Yellow Pines") %>% 
  select(TreeID, Species, Distance, YearDead, Notes)
  
test %>% 
  group_by(YearDead, Species) %>%
  summarize(newdead = n(),
            ntrees = nrow(test)) %>% 
  group_by(YearDead, Species) %>% 
  mutate(cumsum(newdead))
  mutate(p_surv = 1 - cumsum(newdead)/ntrees) %>% 
  ggplot(., aes(YearDead, p_surv)) +
  geom_point() +
  geom_line()

test %>% 
ggplot(., aes(Distance, Species)) +
  geom_jitter(width = 0, height = .1,  size = 2, aes(color = YearDead)) +
  scale_color_viridis_c()



#format data for survival analysis----
#want each row to represent individual tree at an interval. 

#start with just one Site
Yos <- trees %>% 
  filter(Project =='Yosemite') %>% 
  select(Plot, TreeID, TreeSurveyID,  SurveyYear, Species, DBH, YearDead, Disease, Distance, Notes) %>% 
  arrange(TreeID)
Yos  


#check for duplicated rows
Yos %>% group_by(TreeID) %>% filter(duplicated(SurveyYear))
Yos %>% filter(TreeID %in% c(1962, 1542, 368 ))
#delete that tree's 2011 record. Seems like a typo. 
Yos <- Yos %>% filter(!TreeSurveyID %in% c(5617, 6699, 5140))

#convert into single row per individual
Yos2 <- Yos %>% 
  group_by(TreeID) %>% 
  mutate(Year0 = min(SurveyYear), 
         dead = ifelse(is.na(YearDead), 0, 1),
         InitialDBH = min(DBH, na.rm = T),
         YearEnd = ifelse(is.na(YearDead), 2014, YearDead )) %>%
  #pivot_wider(id_cols = c(TreeID, Species, InitialDBH, Distance, dead, Year0, YearEnd, YearDead), names_from = SurveyYear, values_from = DBH, names_prefix = 'DBH_') %>% 
  mutate(Year0 = ifelse(Year0 < YearDead | is.na(YearDead), Year0, YearDead - 1),
         YearDead = NULL,
         time = YearEnd - Year0)

#some error with DBH. check it out.
which(is.infinite(Yos2$InitialDBH))
Yos2[4650,]
Yos2 %>% filter(TreeID == 1972) %>% pull(Notes)

#fix the DBH of trees with typos
Yos2[4650, 'InitialDBH'] <- 20

#lump species together besides cade, quke, pipo



#simple survival analysis----

#try out the survival analysis without time varying covariates. species, distance, plotID

#exclude species without any deaths.
Yos2.2 <- Yos2 %>% filter(Species %in% c("CADE", "PIPO", "QUKE")) #can only include species with deaths. Has estimation problems when no deaths. 


m1 <- coxph(Surv(time, dead) ~ Species*Distance + cluster(Plot), data = Yos2.2)
summary(m1)

covs <- expand_grid(Species = c('PIPO', "CADE", "QUKE"), Distance = c(1, 10, 20, 30, 50), InitialDBH = c(10, 30, 60)) %>% 
  mutate(index = as.character(1:nrow(.)))
m1pred <- summary(survfit(m1, newdata = covs, type = "aalen")) 

m1pred <- data.frame(time = m1pred$time, surv = m1pred$surv) %>% 
  pivot_longer(cols = starts_with('surv'), names_prefix = 'surv.', names_to = 'index') %>% 
  left_join(covs, by = 'index')

ggplot(m1pred, aes(time, value, group = index, color = Distance)) +
  geom_line() +
  scale_color_viridis_c() +
  facet_grid(cols = vars(Species))




#time-varying covariates----

#make each row an individual during survey interval. cutpoints determined by death years. 
library(survival)
cut.points <- unique(YP2$YearEnd)
YP3 <- survSplit(data = YP2, cut = cut.points, end = "YearEnd", start = "Year0", event = "dead") 
YP3






