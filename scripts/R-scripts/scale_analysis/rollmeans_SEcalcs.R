#rollmeans and SE's 

bbs_allscales = read.csv("data/BBS/bbs_allscales.csv", header = TRUE)
bbs_allscales = bbs_allscales %>% 
  dplyr::filter(logN != "NA")


#do I calc SE's using a rollapply-esque function at the same time I calc mean, or do I do this after? 
central_altB = bbs_allscales %>%  
  dplyr::select(logA, pctCore) %>% 
  transmute(pctCore_m = rollapply(pctCore, width = 1, FUN = mean, na.rm = TRUE, fill = NULL),
            logA = logA) %>% 
  mutate(logA = round(logA, digits = 2)) %>%
  group_by(logA) %>%
  summarise(pctCore = mean(pctCore_m),
            sdC = sd(pctCore_m)) %>% #calc sd for rolled avgs bc if try to for pctCore, just get same # repeated
  mutate(focalrte = "99999", logA = logA) %>% 
  dplyr::select(focalrte, logA, pctCore, sdC)



parests = pctCore #mean pctcore estimates
vcov(mod1)
par.se = sd(x)/sqrt(sum(!is.na(x)))


par.se <- sqrt(diag(vcov(mod1)))
upper95 <- pctCore+par.se*qt(.975,df.residual(mod1))
lower95 <- pctCore-par.se*qt(.975,df.residual(mod1))