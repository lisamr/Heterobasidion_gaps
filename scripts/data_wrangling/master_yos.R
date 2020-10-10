#cleaning Yosemite gap data for suvival analysis. Produce 2 datasets--one where death year is assumed exact and another where the intervals are based on survey years. 

#need to do: 1. identify which tree is the 'gapmaker' and actual date it died, 2. when these plots were surveyed, 3. utm coordinates of every tree translated from az/dist, 4. start/stop format data

rm(list=ls())

library(tidyverse)

theme_set(theme_classic())

dat <- read_csv('data/raw_data/Lisa_treeyr_w_plot.csv')
head(dat)
str(dat)

#earliest dead tree
dat %>% group_by(Plot) %>% 
  summarise(YearDead = min(YearDead, na.rm = T)) %>% 
  print(n=Inf)


#manual cleanup-----
dat$DBH[dat$TreeID==1972] <- 20 #treeID 1972 DBH==20





#just dealing with Yosemite----

PlotUTM <- dat %>% filter(Project == 'Yosemite') %>% 
  distinct(Plot, UTM_Zone, Easting, Northing)

#filter Yos
dat2 <- dat %>% 
  filter(Project == 'Yosemite', !is.na(SurveyYear)) %>% 
  select(Plot, TreeID, TreeNumber, SurveyYear, Species, Azimuth, Distance, YearDead, DBH, Notes) %>% 
  mutate(Dead = case_when(
    SurveyYear >= YearDead ~ 1,
    is.na(YearDead) ~ 0,
    T ~ 0
  )) %>% 
  arrange(TreeID) %>% 
  droplevels()

#how many unique trees? maintain this number. 
length(unique(dat2$TreeID)) #1788 trees




#get coordinates of trees----


#instead of az/distance from center, covert tree locationsto an x/y plane. will be better for estimating distances between trees. doesn't need to be UTM coordinates. 

tmp <- dat2 %>% 
  select(Plot, TreeID, Azimuth, Distance) %>% 
  distinct()

#split by plot, apply function
L <- split(tmp, f = tmp$Plot)

xycoords <- function(data){
  data %>% 
    mutate( y_multiplier = case_when(
      Azimuth >90 & Azimuth <180 ~ -1,
      Azimuth >=180 & Azimuth <270 ~ -1,
      T ~ 1),
      radians = Azimuth * pi/180,
      x = sin(radians) * Distance,
      y = sqrt(Distance^2 - x^2)*y_multiplier) %>% 
    select(-y_multiplier, -radians)
}

treecoords <- lapply(L, xycoords) %>% bind_rows()





#1 row per tree----


#concatenate notes from all years. add in later. 
Notes <- dat2 %>% 
  group_by(TreeID) %>% 
  summarise(Notes = paste0(paste(SurveyYear, Notes, sep = ':'), collapse = ', '))
 

#get max dbh for each tree. if any -Inf, check notes
maxDBH <- dat2 %>% 
  group_by(TreeID) %>% 
  summarise(maxDBH = max(DBH, na.rm = T))

#recode species
key <- c('pine', 'cedar', 'oak', 'oak', 'oak', 'fir', 'doug fir', 'oak', 'unknown', 'unknown', 'fir', 'unknown')
names(key) <- unique(dat2$Species)

#1 row per tree
dat3 <- dat2 %>% 
  select(-Notes) %>% 
  filter(Dead == 0) %>% 
  bind_rows(dat2 %>% 
              select(-Notes) %>% 
              group_by(TreeID) %>% 
              filter(Dead==1) %>% 
              filter(SurveyYear == min(SurveyYear))) %>% 
  arrange(TreeID) %>% 
  select(!Dead) %>% 
  pivot_wider(everything(), names_from = SurveyYear, values_from = DBH, names_prefix = 'DBH') %>% 
  mutate(Species2 = recode(Species, !!!key)) %>% 
  left_join(maxDBH) %>% 
  left_join(treecoords) %>% 
  left_join(Notes) 

print(dat3, width = Inf)


#identify gapmaker--oldest dead tree (for now)?
dat3 %>% 
  group_by(Plot) %>% 
  summarise(gapmakers = sum(YearDead == min(YearDead, na.rm = T), na.rm = T))





#make survival data long using exact death yrs-----

#add in start of survey year for each gap
cutpts1 <- sort(unique(dat3$YearDead)) #based on guestimated death years (death is exact)


#roundabout way of making data long...
dat4 <- dat3 %>% 
  group_by(Plot) %>% 
  mutate(YearStart = min(YearDead, na.rm = T)) %>% 
  ungroup() %>% 
  select(Plot:Species, YearStart, YearDead) 


#do it with cutpoints according to death years
mat <- matrix(NA, ncol = length(cutpts1), nrow = nrow(dat4))
colnames(mat) <- cutpts1

#for each tree, each year is 0/1 for dead/alive
for(i in 1:nrow(mat)){
  #if censored, all years are 1
  if(is.na(dat4$YearDead[i])) mat[i,] <- 1 
  else{ #if died within study
    Range <- (dat4$YearStart[i]):(dat4$YearDead[i]-1)
    mat[i,] <- as.numeric(cutpts1 %in% Range[Range[2]>Range[1]])
  }
}

#data now long :). No duplicates and live trees stay censored. 
dat5 <- dat4 %>% 
  bind_cols(as.data.frame(mat)) %>%
  pivot_longer(cols = `1965`:`2012`, values_to = 'Alive', names_to = 'Year') %>% 
  mutate(Year = as.integer(Year)) %>% 
  filter(Year <= YearDead | is.na(YearDead)) %>% 
  group_by(Plot) %>% 
  filter(Year >= YearStart) %>% 
  group_by(TreeID) %>% 
  mutate(Tstart = ifelse(is.na(lag(Year)), Year-1, lag(Year)),
         Dead = ifelse(Alive==0, 1, 0)) %>% 
  rename(Tstop = Year) %>% 
  select(Plot:Species, Deathyr = YearDead, Tstart, Tstop, Dead) 


#how to add in DBH? 
DBH <- dat2 %>% 
  select(TreeID, YearDead, Tstop = SurveyYear, DBH) %>% 
  filter(!is.na(DBH)) %>% 
  group_by(TreeID) %>% 
  #when yeardead is before any of the survey years, assign a new row of surveyyear to the yeardead.
  mutate(Tstop = case_when(
    unique(YearDead) < min(Tstop) ~ unique(YearDead),
    T ~ Tstop
    )) %>% 
  distinct(TreeID, Tstop, DBH)

#merge long data with dbh values. 
dat_long_exact <- dat5 %>% left_join(DBH) 



#make survival data long using survey intervals-----

cutpts2 <- c(1971:1980, 1983, 1984, 1986, 1989, 1996, 1998, 2012) #based on survey yrs (death is interval censored)


#do it with cutpoints according to survey intervals
mat2 <- matrix(NA, ncol = length(cutpts2), nrow = nrow(dat4))
colnames(mat2) <- paste0('x_', cutpts2)

#for each tree, each year is 0/1 for dead/alive
for(i in 1:nrow(mat2)){
  #if censored, all years are 1
  if(is.na(dat4$YearDead[i])) mat2[i,] <- 1 
  else{ #if died within study
    Range <- (cutpts2[1]):(dat4$YearDead[i]-1)
    mat2[i,] <- as.numeric(cutpts2 %in% Range[Range[2]>Range[1]])
  }
}


#assume year start is 1970 for every gap and identify trees that died before survey
dat6 <- dat4 %>% 
  mutate(left_censored = case_when(
    YearDead < cutpts2[1] ~ 1, 
    is.na(YearDead) ~ 0,
    T ~ 0),
    YearStart = 1970)
left_censored <- dat6 %>% 
  filter(left_censored==1) %>% 
  select(-YearStart, -left_censored) %>% 
  rename(Deathyr = YearDead) %>% 
  mutate(Tstart = NA,
         Tstop = cutpts2[1], 
         Dead = 1) 

#merge with data. make sure there are 1788 trees.
dat_long_intervals <- dat6 %>% 
  bind_cols(as.data.frame(mat2)) %>%
  pivot_longer(cols = contains('x_'), values_to = 'Alive', names_to = 'Year', names_prefix = 'x_') %>% 
  mutate(Year = as.integer(Year)) %>% 
  filter(lag(Year < YearDead) | Year <= YearDead | is.na(YearDead)) %>% 
  group_by(Plot) %>% 
  filter(Year >= YearStart) %>% 
  group_by(TreeID) %>% 
  mutate(Tstart = ifelse(is.na(lag(Year)), Year-1, lag(Year)),
    #Tstart = lag(Year),
    Dead = ifelse(Alive==0, 1, 0)) %>% 
  rename(Tstop = Year) %>% 
  select(Plot:Species, Deathyr = YearDead, Tstart, Tstop, Dead) %>% 
  bind_rows(left_censored) %>% 
  arrange(TreeID) %>% 
  left_join(dat2 %>% 
              select(TreeID, Tstop = SurveyYear, DBH) %>% 
              filter(!is.na(DBH)) %>% 
              distinct(TreeID, Tstop, DBH))

print(dat_long_intervals, n=50)

#add a column to dat3 to express which interval showed tree dead
dat3 <- left_join(dat3, 
          dat_long_intervals %>% 
            group_by(TreeID) %>% 
            summarise(IntervalDead = ifelse(
              sum(Dead)>0, max(Tstop), NA))
)

#the data frames----

PlotUTM #plot locations
dat3 #1 tree = 1 row
dat_long_exact #1 tree = many rows, breaks defined by guestimated death years
print(dat_long_exact, n=50)
dat_long_intervals # 1 tree = many rows, breaks defined by survey intervals




#plot trees----

# set up shapes. species get same shape every time.
shapes <- c(19, 15, 21, 22, 17, 24)
names(shapes) <- unique(dat3$Species2)

unique(dat3$IntervalDead)

#make a list of plots
plotmap <- function(i){
  d <- filter(dat3, Plot == PlotUTM$Plot[i])
  r <- rank(d$IntervalDead)
  ggplot(d, aes(x, y)) +
    geom_point(alpha = .8, 
               aes(color = IntervalDead, 
                   size = maxDBH, 
                   shape = Species2)) +
    scale_shape_manual(values = shapes) +
    scale_color_distiller(palette = 'Spectral', na.value = 'grey70', direction = 1) +
    coord_equal() +
    guides(size = FALSE) +
    theme(legend.position = 'bottom', legend.title = element_blank()) +
    labs(title = unique(d$Plot)) +
    geom_text(data = d[r==min(r),],
              aes(label = TreeNumber), size = 3)
}
plotmap(6)
Lplot <- lapply(1:length(unique(dat3$Plot)), plotmap)


# create pdf where each page is a separate plot.
pdf("figures/Yos_gap_maps.pdf")
lapply(1:length(Lplot), function(x) Lplot[[x]])
dev.off()





