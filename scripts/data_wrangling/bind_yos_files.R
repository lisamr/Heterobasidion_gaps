#merge yosemite files together 
#files are not perfect--seems some of them had damage and missing rows, so will need to manually figure out what went missing. 

temp <- list.files(path = 'data/raw_data/Yosemite/', pattern="*.csv")
temp2 <- paste0('data/raw_data/Yosemite/', temp)
myfiles <- lapply(temp2, read_csv)

#plot names
plots <- as.list(sub("nogap.csv", "", temp)) 

#not all the same number of cols. select ones you want.
selectcols <- function(df, plotname){
  res <- df %>% 
    mutate(Plot = plotname) %>% 
    select(Plot, `TR#`, SP:`71DBH`, `LIVE/YRDEAD`:`DIST(m)`, Gapmakers) 
  return(res)
}

#bind together
myfiles2 <- map2(myfiles, plots, ~selectcols(.x, .y)) %>% 
  bind_rows(myfiles2) 


myfiles2 %>% 
  group_by(Plot) %>% 
  summarise(gapmakers2 = sum(Gapmakers, na.rm = T)) 

y



