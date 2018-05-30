

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



#Mean Total Revenue per Student 1993-97
finance2 %>% 
  dplyr::filter(YRDATA==1993 | YRDATA==1994 | YRDATA==1995 | YRDATA==1996 | YRDATA==1997) %>%
  dplyr::summarize(Avg = sum(TOTALREV*1000)/sum(ENROLL)) 


#Top and Bottom States - Total Revenue Per Student 1993-1997  
TotalRevPer_90s = finance2 %>% dplyr::group_by(STATE) %>% 
  dplyr::filter(YRDATA==1993 | YRDATA==1994 | YRDATA==1995 | YRDATA==1996 | YRDATA==1997) %>%
  dplyr::summarize(Avg = sum(TOTALREV*1000)/sum(ENROLL)) %>% dplyr::arrange(-Avg)  
head(TotalRevPer_90s,10)
tail(TotalRevPer_90s,10)


#Mean Total Revenue per Student 2011-15
finance2 %>% 
  dplyr::filter(YRDATA==2011 | YRDATA==2012 | YRDATA==2013 | YRDATA==2014 | YRDATA==2015) %>%
  dplyr::summarize(Avg = sum(TOTALREV*1000)/sum(ENROLL)) 


#Top and Bottom States - Total Revenue Per Student 2011-15  
TotalRevPer_21s = finance2 %>% dplyr::group_by(STATE) %>% 
  dplyr::filter(YRDATA==2011 | YRDATA==2012 | YRDATA==2013 | YRDATA==2014 | YRDATA==2015) %>%
  dplyr::summarize(Avg = sum(TOTALREV*1000)/sum(ENROLL)) %>% dplyr::arrange(-Avg)  
head(TotalRevPer_21s,10)
tail(TotalRevPer_21s,10)



#Greatest Jump and Drop in Ranking
count=1
State1=character(51)
Rank90=numeric(51)
Rank21=numeric(51)
Diff1=numeric(51)

for (i in 1:51)
{
  for (j in 1:51)
  { if (TotalRevPer_90s$STATE[i]==TotalRevPer_21s$STATE[j])
    {
    State1[count]=TotalRevPer_90s$STATE[i]
    Rank90[count]=i
    Rank21[count]=j
    Diff1[count]=i-j
    count=count+1
    }
  }
}
rank_diff=data.frame(State1, Rank90, Rank21, Diff1, stringsAsFactors=FALSE)
rank_diff %>% dplyr::arrange(-Diff1) 


#% funding 1993-97
finance2 %>% dplyr::group_by(STATE) %>%
  dplyr::filter(STATE=="North Dakota" | STATE=="Louisiana" | STATE=="Michigan" | STATE=="Florida")  %>%
  dplyr::filter(YRDATA==1993 | YRDATA==1994 | YRDATA==1995 | YRDATA==1996 | YRDATA==1997) %>%
  dplyr::summarize(FedPercent = sum(TFEDREV)/sum(TOTALREV)*100, 
                   StatePercent = sum(TSTREV)/sum(TOTALREV)*100,
                   LocalPercent = sum(TLOCREV)/sum(TOTALREV)*100,
                   Student = sum(ENROLL)/5 )

#% funding 2011-15
finance2 %>% dplyr::group_by(STATE) %>%
  dplyr::filter(STATE=="North Dakota" | STATE=="Louisiana" | STATE=="Michigan" | STATE=="Florida")  %>%
  dplyr::filter(YRDATA==2011 | YRDATA==2012 | YRDATA==2013 | YRDATA==2014 | YRDATA==2015) %>%
  dplyr::summarize(FedPercent = sum(TFEDREV)/sum(TOTALREV)*100, 
                   StatePercent = sum(TSTREV)/sum(TOTALREV)*100,
                   LocalPercent = sum(TLOCREV)/sum(TOTALREV)*100,
                   Student = sum(ENROLL)/5 )


#Correlation 1993-97
cor_finance_90 = finance2 %>% dplyr::group_by(STATE) %>%
  dplyr::filter(YRDATA==1993 | YRDATA==1994 | YRDATA==1995 | YRDATA==1996 | YRDATA==1997) %>%
  dplyr::summarize(TOtalRevAvg = sum(TOTALREV*1000)/sum(ENROLL),
                   FedAvg = sum(TFEDREV*1000)/sum(ENROLL),
                   StateAvg = sum(TSTREV*1000)/sum(ENROLL),
                   LocalAvg = sum(TLOCREV*1000)/sum(ENROLL),
                   Student = sum(ENROLL)/5 ) 
cor(cor_finance_90[,2:6])



#Correlation 2011-15
cor_finance_21 = finance2 %>% dplyr::group_by(STATE) %>%
  dplyr::filter(YRDATA==2011 | YRDATA==2012 | YRDATA==2013 | YRDATA==2014 | YRDATA==2015) %>%
  dplyr::summarize(TOtalRevAvg = sum(TOTALREV*1000)/sum(ENROLL),
                   FedAvg = sum(TFEDREV*1000)/sum(ENROLL),
                   StateAvg = sum(TSTREV*1000)/sum(ENROLL),
                   LocalAvg = sum(TLOCREV*1000)/sum(ENROLL),
                   Student = sum(ENROLL)/5 ) 
cor(cor_finance_21[,2:6])



****************************************Expenditure


#Mean Total Spend per Student 1993-97
finance2 %>% 
  dplyr::filter(YRDATA==1993 | YRDATA==1994 | YRDATA==1995 | YRDATA==1996 | YRDATA==1997) %>%
  dplyr::summarize(Avg = sum(TOTALEXP*1000)/sum(ENROLL)) 


#Top and Bottom States - Total Spend Per Student 1993-1997  
TotalExpPer_90s = finance2 %>% dplyr::group_by(STATE) %>% 
  dplyr::filter(YRDATA==1993 | YRDATA==1994 | YRDATA==1995 | YRDATA==1996 | YRDATA==1997) %>%
  dplyr::summarize(Avg = sum(TOTALEXP*1000)/sum(ENROLL)) %>% dplyr::arrange(-Avg)  
head(TotalExpPer_90s,10)
tail(TotalExpPer_90s,10)
  
  
#Mean Total Spend per Student 2011-15
finance2 %>% 
  dplyr::filter(YRDATA==2011 | YRDATA==2012 | YRDATA==2013 | YRDATA==2014 | YRDATA==2015) %>%
  dplyr::summarize(Avg = sum(TOTALEXP*1000)/sum(ENROLL)) 


#Top and Bottom States - Total Spend Per Student 2011-15 
TotalExpPer_21s = finance2 %>% dplyr::group_by(STATE) %>% 
  dplyr::filter(YRDATA==2011 | YRDATA==2012 | YRDATA==2013 | YRDATA==2014 | YRDATA==2015) %>%
  dplyr::summarize(Avg = sum(TOTALEXP*1000)/sum(ENROLL)) %>% dplyr::arrange(-Avg)  
head(TotalExpPer_21s,10)
tail(TotalExpPer_21s,10)
  


#Greatest Jump and Drop in Ranking
count=1
State1=character(51)
Rank90=numeric(51)
Rank21=numeric(51)
Diff1=numeric(51)

for (i in 1:51)
{
  for (j in 1:51)
  { if (TotalExpPer_90s$STATE[i]==TotalExpPer_21s$STATE[j])
  {
    State1[count]=TotalExpPer_90s$STATE[i]
    Rank90[count]=i
    Rank21[count]=j
    Diff1[count]=i-j
    count=count+1
  }
  }
}
rank_diff=data.frame(State1, Rank90, Rank21, Diff1, stringsAsFactors=FALSE)
rank_diff %>% dplyr::arrange(-Diff1) 



#% Expenditure 1993-97
finance2 %>% dplyr::group_by(STATE) %>%
  dplyr::filter(STATE=="North Dakota" | STATE=="Louisiana" | STATE=="Michigan" | STATE=="Florida")  %>%
  dplyr::filter(YRDATA==1993 | YRDATA==1994 | YRDATA==1995 | YRDATA==1996 | YRDATA==1997) %>%
  dplyr::summarize(InstPercent = sum(TCURINST)/sum(TOTALEXP)*100, 
                   SServPercent = sum(TCURSSVC)/sum(TOTALEXP)*100,
                   OtherPercent = sum(TCURONON)/sum(TOTALEXP)*100,
                   CapOutPercent = sum(TCAPOUT)/sum(TOTALEXP)*100,
                   Student = sum(ENROLL)/5 )

#% Expenditure 2011-15
finance2 %>% dplyr::group_by(STATE) %>%
  dplyr::filter(STATE=="North Dakota" | STATE=="Louisiana" | STATE=="Michigan" | STATE=="Florida")  %>%
  dplyr::filter(YRDATA==2011 | YRDATA==2012 | YRDATA==2013 | YRDATA==2014 | YRDATA==2015) %>%
  dplyr::summarize(InstPercent = sum(TCURINST)/sum(TOTALEXP)*100, 
                   SServPercent = sum(TCURSSVC)/sum(TOTALEXP)*100,
                   OtherPercent = sum(TCURONON)/sum(TOTALEXP)*100,
                   CapOutPercent = sum(TCAPOUT)/sum(TOTALEXP)*100,
                   Student = sum(ENROLL)/5 )


#Correlation 1993-97
cor_finance_90 = finance2 %>% dplyr::group_by(STATE) %>%
  dplyr::filter(YRDATA==1993 | YRDATA==1994 | YRDATA==1995 | YRDATA==1996 | YRDATA==1997) %>%
  dplyr::summarize(TOtalExpAvg = sum(TOTALEXP*1000)/sum(ENROLL),
                   InstAvg = sum(TCURINST*1000)/sum(ENROLL),
                   SServAvg = sum(TCURSSVC*1000)/sum(ENROLL),
                   OtherAvg = sum(TCURONON*1000)/sum(ENROLL),
                   CapOutAvg = sum(TCAPOUT*1000)/sum(ENROLL),
                   Student = sum(ENROLL)/5 ) 
cor(cor_finance_90[,2:7])



#Correlation 2011-15
cor_finance_21 = finance2 %>% dplyr::group_by(STATE) %>%
  dplyr::filter(YRDATA==2011 | YRDATA==2012 | YRDATA==2013 | YRDATA==2014 | YRDATA==2015) %>%
  dplyr::summarize(TOtalExpAvg = sum(TOTALEXP*1000)/sum(ENROLL),
                   InstAvg = sum(TCURINST*1000)/sum(ENROLL),
                   SServAvg = sum(TCURSSVC*1000)/sum(ENROLL),
                   OtherAvg = sum(TCURONON*1000)/sum(ENROLL),
                   CapOutAvg = sum(TCAPOUT*1000)/sum(ENROLL),
                   Student = sum(ENROLL)/5 ) 
cor(cor_finance_21[,2:7])



#Mean Total Instruction Spend per Student 1993-97
finance2 %>% 
  dplyr::filter(YRDATA==1993 | YRDATA==1994 | YRDATA==1995 | YRDATA==1996 | YRDATA==1997) %>%
  dplyr::summarize(Avg = sum(TCURINST*1000)/sum(ENROLL)) 


#Top and Bottom States - Total Spend Per Student 1993-1997  
InstExpPer_90s = finance2 %>% dplyr::group_by(STATE) %>% 
  dplyr::filter(YRDATA==1993 | YRDATA==1994 | YRDATA==1995 | YRDATA==1996 | YRDATA==1997) %>%
  dplyr::summarize(Avg = sum(TCURINST*1000)/sum(ENROLL)) %>% dplyr::arrange(-Avg)  
head(InstExpPer_90s,10)
tail(InstExpPer_90s,10)


****************** 
#Correlate all

colnames(finance2)
cor_finance2 = finance2 %>% dplyr::select(ENROLL,TotalRevPer,TotalFedRevPer,TotalStateRevPer,
  TotalLocalRevPer,TotalExpendRevPer,TotalInstruExpPer,TotalSupServExpPer, TotalOtherExpPer,TotalCapOutExpPer)
cor(cor_finance2[,1:10])
  
  
  
  
  
  
