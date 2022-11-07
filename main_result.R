library(readxl)
library(dplyr)
library(augsynth)

#setwd("D:/Personal File/??????ů/code")
startdate<-c("2014-05-01","2015-05-01","2016-05-01","2017-05-01","2018-05-01","2019-05-01","2020-05-01","2021-05-01")
enddate<-c("2015-04-30","2016-04-30","2017-04-30","2018-04-30","2019-04-30","2020-04-30","2021-04-30","2021-12-31")
intervention<-c("10-23")
yearlist<-c("2014","2015","2016","2017","2018","2019","2020","2021")
treatmentlist<-c("2+26 cities","Other northern cities","Alternative","Northern","South mixing","Southern","Southern control","China")
pollut<-c("SO2wn","NO2wn","PM2.5wn","PM10wn","O3_8hwn","COwn","SO2","NO2","PM2.5","PM10","O3_8h","CO","O3","O3wn","Ox","Oxwn")

test1 <- read_excel("RegionAverageAll.xlsx", 
                    col_types = c("date", "text", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", "numeric", "numeric"))
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
               test2$ID == "Dongguan"| test2$ID == "Zhongshan" |test2$ID == "Foshan"| test2$ID == "Beihai"|
               test2$ID == "Nanning"|  test2$ID=="Nanchang" | test2$ID == "Xiamen"| test2$ID == "Taizhou"| 
               test2$ID == "Ningbo"|  test2$ID=="Guangzhou" | test2$ID == "Huizhou"| test2$ID == "Hangzhou" |
               test2$ID == "Liuzhou"| test2$ID == "Shantou"| test2$ID == "Jiangmen"| test2$ID == "Heyuan"| test2$ID == "Quanzhou"|  test2$ID=="Haikou" | test2$ID == "Shenzhen"| test2$ID == "Wenzhou"| test2$ID == "Huzhou"|
               test2$ID == "Zhuhai"| test2$ID == "Fuzhou"| test2$ID == "Shaoxing"| test2$ID == "Zhaoqing"|  test2$ID=="Zhoushan" | 
               test2$ID == "Quzhou"| test2$ID == "Jinhua"|test2$ID == "Shaoguan" | test2$ID == "Sanya"|
               test2$ID == "Jieyang" |test2$ID == "Meizhou"| test2$ID == "Shanwei" |
               test2$ID == "Zhanjiang" |test2$ID == "Chaozhou"| test2$ID == "Maoming" |test2$ID == "Yangjiang")
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

write.csv(data_exp,"main_result.csv",row.names = FALSE)