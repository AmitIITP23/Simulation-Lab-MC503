data_medals<-read.csv("C:/Users/notso/Downloads/medals_total.csv")

medals_india<-subset(data_medals, data_medals$Country.Code == 'IND')
medals_usa<- subset(data_medals, data_medals$Country.Code == 'USA')
medals_china<-subset(data_medals, data_medals$Country.Code == 'CHN')

#Total gold, silver and bronze medals won by the three countries
total_gold<-medals_india$Gold.Medal + medals_usa$Gold.Medal + medals_china$Gold.Medal
total_silver<-medals_india$Silver.Medal + medals_usa$Silver.Medal + medals_china$Silver.Medal
total_bronze<-medals_india$Bronze.Medal + medals_usa$Bronze.Medal + medals_china$Bronze.Medal

cat("Total gold medals by India, USA, China: ", total_gold)
cat("Total silver medals by India, USA, China: ", total_silver)
cat("Total bronze medals by India, USA, China: ", total_bronze)

#Total medals country-wise
total_india<-medals_india$Total
total_usa<-medals_usa$Total
total_china<-medals_china$Total

cat("Total medals by India: ", total_india)
cat("Total medals by USA: ", total_usa)
cat("Total medals by China: ", total_china)

#histograms
china_gold=rep(1, times=medals_china$Gold.Medal)
china_silver=rep(2, times=medals_china$Silver.Medal)
china_bronze=rep(3, times=medals_china$Bronze.Medal)
china_list_hist<-c(china_gold, china_silver, china_bronze)
hist(china_list_hist, plot=TRUE)
hist(china_list_hist, ylim=c(0,40), plot=TRUE)

usa_gold=rep(1, times=medals_usa$Gold.Medal)
usa_silver=rep(2, times=medals_usa$Silver.Medal)
usa_bronze=rep(3, times=medals_usa$Bronze.Medal)
usa_list_hist<-c(usa_gold, usa_silver, usa_bronze)
hist(usa_list_hist, ylim=c(0,50), plot=TRUE)

#filtering dataset
medals_japan<-subset(data_medals, data_medals$Country.Code == 'JPN')
medals_brazil<-subset(data_medals, data_medals$Country.Code == 'BRA')
country_codes=c('USA', 'CHN', 'JPN', 'BRA', 'IND')
country_names=c('USA', 'China', 'Japan', 'Brazil', 'India')
fd<-subset(data_medals, data_medals$Country.Code %in% country_codes)

#pie chart
pie_data<-c(fd$Total)
piepercent<- round(100 * pie_data / sum(pie_data), 1)
pie(piepercent, labels = piepercent,
    main = "Total number of medals won", col = rainbow(length(pie_data)), radius=1)
legend("topright", country_names, 
       cex = 0.7, fill = rainbow(length(pie_data)))
