#rollmeans and SE's 

bbs_allscales = read.csv("data/BBS/bbs_allscales.csv", header = TRUE)
bbs_allscales = bbs_allscales %>% 
  dplyr::filter(logN != "NA")


central_alt = bbs_allscales %>%  
  dplyr::select(logA, pctCore) %>% 
  transmute(pctCore_m = rollapply(pctCore, width = 1, FUN = mean, na.rm = TRUE, fill = NULL),
            logA = logA) %>% 
  mutate(logA = round(logA, digits = 2)) %>%
  group_by(logA) %>%
  summarise(pctCore = mean(pctCore_m),
            sdC = sd(pctCore_m)) %>% 
  mutate(focalrte = "99999", logA = logA) %>% 
  dplyr::select(focalrte, logA, pctCore, sdC)