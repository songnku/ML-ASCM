library(readxl)
library(dplyr)
library(augsynth)

#setwd("D:/Personal File/??????ů/code")
startdate<-c("2014-05-01","2015-05-01","2016-05-01","2017-05-01","2018-05-01","2019-05-01","2020-05-01","2021-05-01")
enddate<-c("2015-04-30","2016-04-30","2017-04-30","2018-04-30","2019-04-30","2020-04-30","2021-04-30","2021-12-31")
intervention<-c("10-15")
yearlist<-c("2014","2015","2016","2017","2018","2019","2020","2021")
treatmentlist<-c("2+26","Northern")
pollut<-c("SO2wn","NO2wn","PM2.5wn","PM10wn","O3_8hwn","COwn","SO2","NO2","PM2.5","PM10","O3_8h","CO")

test1 <- read_excel("RegionAverageAll.xlsx", 
                    col_types = c("date", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "text", "text"))
data_exp <- NULL
for (n in 1:length(startdate)){
  s_date<-startdate[n]
  e_date<-enddate[n]
  #it_date_<-intervention[n]
  test2 <- test1 %>% filter(date>=s_date & date <= e_date)
  
  year_s<-yearlist[n]
  for (m in 1:length(treatmentlist)){
    treat<-treatmentlist[m]
    test3 <- test2 %>% 
      filter(test2$ID == treat |
               test2$ID == "Dongying"| test2$ID == "Linfen" |test2$ID=="Linyi" | test2$ID == "Dandong"| test2$ID == "Urumqi"|
               test2$ID == "Lanzhou"|  test2$ID=="Baotou" | test2$ID == "Hohhot"| test2$ID == "Xianyang"| test2$ID == "Harbin"|
               test2$ID == "Datong"|  test2$ID=="Dalian" | test2$ID == "Weihai"| test2$ID == "Baoji"| test2$ID == "Yanan" |
               test2$ID == "Zhangjiakou"| test2$ID == "Chengde"| test2$ID == "Rizhao"| test2$ID == "Zaozhuang"| test2$ID=="Shenyang" |
               test2$ID == "Taian"|  test2$ID=="Weinan" | test2$ID == "Weifang"| test2$ID == "Yantai"| test2$ID == "Panjin"|
               test2$ID == "Qinhuangdao"| test2$ID == "Yingkou"| test2$ID == "Huludao"| test2$ID == "Xining"|  test2$ID=="Xian" | 
               test2$ID == "Ordos"| test2$ID == "Tongchuan"| test2$ID == "Yinchuan"|test2$ID == "Changchun" | test2$ID == "Qingdao"|
               test2$ID == "Sanmenxia" |test2$ID == "Rushan"| test2$ID == "Karamay" |test2$ID == "Jimo"| test2$ID == "Jilin" |
               test2$ID == "Jiayuguan" |test2$ID == "Shouguang"| test2$ID == "Pingdu" |test2$ID == "Pingdingshan" |test2$ID == "Kuerle"|
               test2$ID == "Fushun" |test2$ID == "Zhaoyuan"| test2$ID == "Wendeng" |test2$ID == "Benxi" |test2$ID == "Luoyang"|
               test2$ID == "Mudanjiang" |test2$ID == "Wafangdian"| test2$ID == "Shizuishan" |test2$ID == "Zhangqiu" |test2$ID == "Jiaonan"|
               test2$ID == "Jiaozhou"| test2$ID == "Rongcheng" |test2$ID == "Laizhou"| test2$ID == "Laixi" |test2$ID == "Penglai" |
               test2$ID == "Chifeng"| test2$ID == "Jinchang" |test2$ID == "Jinzhou"| test2$ID == "Anshan" |test2$ID == "Qiqihaer" |
               test2$ID == "Daqing"
      )
    it_day<-intervention[1]
    it_date<-paste(year_s,"-",it_day,sep='')
    test3$treatment <- "0"
    test3$treatment[test3$date>=it_date & test3$ID == treat]<-1
    for (p in 1:length(pollut)){
      y<-pollut[p]
      #pol_name<-paste(y)
      HBobs<- augsynth(y ~ treatment, ID, date, test3, progfunc="Ridge", scm=T)
      print(paste("end of augsyth",treat,y))
      summ=summary(HBobs,inf_type = "jackknife+")
      Pol<-summ$att %>% rename(date= Time)
      Pol$city<-treat
      Pol$pollutant<-y
      Pol$year <- year_s
      Pol$average_att <- summ$average_att$Estimate
      Pol$average_att_lower <- summ$average_att$lower_bound
      Pol$average_att_upper <- summ$average_att$upper_bound
      Pol$L2 <- summ$l2_imbalance
      Pol$Scaled_L2 <- summ$scaled_l2_imbalance
      Pol$est_bias<- format(round(mean(summ$bias_est), 3),nsmall=3)
      Pol$improvement<- format(round(1 - summ$scaled_l2_imbalance,3)*100)
      
      
      data_exp<-rbind(data_exp,Pol)
      
      }

  }
  
}

write.csv(data_exp,"north_control_result.csv",row.names = FALSE)