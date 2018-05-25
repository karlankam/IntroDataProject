

#Kar's Windows
finance <- read.csv("E:\\Karl\\UCSC\\IntroDataAnalytics\\elsect_main.csv", header = TRUE, sep = ",", 
                         stringsAsFactors= FALSE)
#Kar's Mac
finance <- read.csv("/Users/kkam/Documents/Personal/UCSCExtension/IntroDataAnalysis/RStudio/elsect_main.csv", 
                         header = TRUE, sep = ",", stringsAsFactors= FALSE)

attach(finance)
nrow(finance)
colnames(finance)
lapply(finance, class)
head(finance)

library(dplyr)
library(ggplot2)

finance2=finance %>% dplyr::filter(ENROLL!=0) %>%
  dplyr::mutate(TotalRevPer= TOTALREV*1000/ENROLL) %>% 
  dplyr::mutate(TotalFedRevPer= TFEDREV*1000/ENROLL) %>% 
  dplyr::mutate(TotalStateRevPer= TSTREV*1000/ENROLL) %>%
  dplyr::mutate(TotalLocalRevPer= TLOCREV*1000/ENROLL) %>%
  dplyr::mutate(TotalExpendRevPer= TOTALEXP*1000/ENROLL) %>%
  dplyr::mutate(TotalInstruExpPer= TCURINST*1000/ENROLL) %>%
  dplyr::mutate(TotalSupServExpPer= TCURSSVC*1000/ENROLL) %>%
  dplyr::mutate(TotalOtherExpPer= TCURONON*1000/ENROLL) %>%
  dplyr::mutate(TotalCapOutExpPer= TCAPOUT*1000/ENROLL)
head(finance2)


#Top and Bottom States - Total Revenue Per Student 1993-1997
mean_TotalRevPer = finance2 %>% dplyr::group_by(STATE) %>% 
  dplyr::filter(YRDATA==1993 | YRDATA==1994 | YRDATA==1995 | YRDATA==1996 | YRDATA==1997) %>% 
  dplyr::summarize(Mean = mean(TotalRevPer)) %>% dplyr::arrange(-Mean)
head(mean_TotalRevPer,10)
tail(mean_TotalRevPer,10)

#Mean Total Revenue per Student 1993-97
mean_TotalRevPer %>% dplyr::summarize(Mean2 = mean(Mean))

#Top and Bottom States - Total Revenue Per Student 2011-2015
mean_TotalRevPer = finance2 %>% dplyr::group_by(STATE) %>% 
  dplyr::filter(YRDATA==2011 | YRDATA==2012 | YRDATA==2013 | YRDATA==2014 | YRDATA==2015) %>% 
  dplyr::summarize(Mean = mean(TotalRevPer)) %>% dplyr::arrange(-Mean)
head(mean_TotalRevPer,10)
tail(mean_TotalRevPer,10)

#Mean Total Revenue per Student 2011-15
mean_TotalRevPer %>% dplyr::summarize(Mean2 = mean(Mean))


#Total Federal Revenue Per Student 1993-1997
mean_TotalFedRevPer = finance2 %>% dplyr::group_by(STATE) %>% 
  dplyr::filter(YRDATA==1993 | YRDATA==1994 | YRDATA==1995 | YRDATA==1996 | YRDATA==1997) %>% 
  dplyr::summarize(Mean = mean(TotalFedRevPer/TotalRevPer*100)) %>% dplyr::arrange(-Mean)
head(mean_TotalFedRevPer,10)
tail(mean_TotalFedRevPer,10)

#Mean Total Revenue per Student 1993-97
mean_TotalFedRevPer %>% dplyr::summarize(Mean2 = mean(Mean))




********************* Testing below

    
  
  
  
  
#Top and Bottom States - Total Federal Revenue Per Student 1993-1997
mean_TotalFedRevPer = finance2 %>% dplyr::group_by(STATE) %>% 
  dplyr::filter(YRDATA==1993 | YRDATA==1994 | YRDATA==1995 | YRDATA==1996 | YRDATA==1997) %>% 
  dplyr::summarize(Mean = mean(TotalFedRevPer/TotalRevPer*100)) %>% dplyr::arrange(-Mean)
head(mean_TotalFedRevPer,10)
tail(mean_TotalFedRevPer,10)

#Top and Bottom States - Total Federal Revenue Per Student 1993-1997
finance2 %>% 
  dplyr::filter(YRDATA==1993 | YRDATA==1994 | YRDATA==1995 | YRDATA==1996 | YRDATA==1997) %>% 
  dplyr::summarize(Mean = mean(TotalFedRevPer/TotalRevPer*100)) 


************

#Top and Bottom States - Total Federal Revenue Per Student 2011-2015
mean_TotalFedRevPer = finance2 %>% dplyr::group_by(STATE) %>% 
  dplyr::filter(YRDATA==2011 | YRDATA==2012 | YRDATA==2013 | YRDATA==2014 | YRDATA==2015) %>% 
  dplyr::summarize(Mean = mean(TotalFedRevPer)) %>% dplyr::arrange(-Mean)
head(mean_TotalFedRevPer,10)
tail(mean_TotalFedRevPer,10)

#Top and Bottom States - Total State Revenue Per Student 1993-1997
mean_TotalFedRevPer = finance2 %>% dplyr::group_by(STATE) %>% 
  dplyr::filter(YRDATA==1993 | YRDATA==1994 | YRDATA==1995 | YRDATA==1996 | YRDATA==1997) %>% 
  dplyr::summarize(Mean = mean(TotalStateRevPer)) %>% dplyr::arrange(-Mean)
head(mean_TotalFedRevPer,10)
tail(mean_TotalFedRevPer,10)

#Top and Bottom States - Total State Revenue Per Student 2011-2015
mean_TotalFedRevPer = finance2 %>% dplyr::group_by(STATE) %>% 
  dplyr::filter(YRDATA==2011 | YRDATA==2012 | YRDATA==2013 | YRDATA==2014 | YRDATA==2015) %>% 
  dplyr::summarize(Mean = mean(TotalStateRevPer)) %>% dplyr::arrange(-Mean)
head(mean_TotalFedRevPer,10)
tail(mean_TotalFedRevPer,10)

#Top and Bottom States - Total Local Revenue Per Student 1993-1997
mean_TotalFedRevPer = finance2 %>% dplyr::group_by(STATE) %>% 
  dplyr::filter(YRDATA==1993 | YRDATA==1994 | YRDATA==1995 | YRDATA==1996 | YRDATA==1997) %>% 
  dplyr::summarize(Mean = mean(TotalLocalRevPer)) %>% dplyr::arrange(-Mean)
head(mean_TotalFedRevPer,10)
tail(mean_TotalFedRevPer,10)

#Top and Bottom States - Total Local Revenue Per Student 2011-2015
mean_TotalFedRevPer = finance2 %>% dplyr::group_by(STATE) %>% 
  dplyr::filter(YRDATA==2011 | YRDATA==2012 | YRDATA==2013 | YRDATA==2014 | YRDATA==2015) %>% 
  dplyr::summarize(Mean = mean(TotalLocalRevPer)) %>% dplyr::arrange(-Mean)
head(mean_TotalFedRevPer,10)
tail(mean_TotalFedRevPer,10)

##### Testing Below #####

finance3 = finance2 %>% dplyr::filter(ENROLL!=0) %>% dplyr::filter(NAME=="ABBOTT UN FREE SCH DIST") %>% 
  dplyr::filter(YRDATA==1993 | YRDATA==1994)



finance2 %>% dplyr::filter(NAME=="ABBEVILLE CO SCH DIST") %>% 
  dplyr::filter(YRDATA==1993)

finance2 %>% dplyr::filter(NAME=="ABBOTT UN FREE SCH DIST") %>% 
  dplyr::filter(YRDATA==1993 | YRDATA==1994)

