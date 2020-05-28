apr14<-read.csv("C:/Users/lenovo/Documents/DATA/uber-raw-data-apr14.csv", header=TRUE)
may14<-read.csv("C:/Users/lenovo/Documents/DATA/uber-raw-data-may14.csv", header=TRUE)
jun14<-read.csv("C:/Users/lenovo/Documents/DATA/uber-raw-data-jun14.csv", header=TRUE)
jul14<-read.csv("C:/Users/lenovo/Documents/DATA/uber-raw-data-jul14.csv", header=TRUE)
aug14<-read.csv("C:/Users/lenovo/Documents/DATA/uber-raw-data-aug14.csv", header=TRUE)
sep14<-read.csv("C:/Users/lenovo/Documents/DATA/uber-raw-data-sep14.csv", header=TRUE)
data14<-bind_rows(apr14,may14,jun14,jul14,aug14,sep14)
summary(data14)
install.packages("VIM")
aggr(data14)
data14$Date.Time <- mdy_hms(data14$Date.Time)
data14$Year <- factor(year(data14$Date.Time))
data14$Month <- factor(month(data14$Date.Time))
data14$Day <- factor(day(data14$Date.Time))
data14$Weekday <- factor(wday(data14$Date.Time))
data14$Hour <- factor(hour(data14$Date.Time))
data14$Minute <- factor(minute(data14$Date.Time))
data14$Second <- factor(second(data14$Date.Time))
data5000<-data14[1:5000,]
data5000<-data5000[,-12]
set.seed(0)
fviz_nbclust(data5000[,2:3], kmeans, method="wss", print.summary = TRUE)
clust<-kmeans(data5000[,2:3],5,nstart=20)
str(clust)
data5000$Borough<-as.factor(clust$cluster)
fviz_cluster(clust,data5000,choose.vars=c("Lat","Lon"),ellipse=TRUE)
fviz_nbclust(data5000[,2:3],cluster::pam)






clusters<-kmeans(data14[,2:3],5)
str(clusters)
data14$Borough <- as.factor(clusters$cluster)
#install.packages("ggmap")
#data14$Month <- as.double(data14$Month)
#month_borough_14 <- count_(data14, vars = c('Month', 'Borough'), sort = TRUE) %>%
#arrange(Month, Borough)
#datatable(month_borough_14)

#monthly_growth <- month_borough_14 %>%
  #mutate(Date = paste("04", Month)) %>%
  #ggplot(aes(Month, n, colour = Borough)) + geom_line() +
 # ggtitle("Uber Monthly Growth - 2014")
#monthly_growth

pamclust<-pam(data5000[,2:3],4)
fviz_cluster(pamclust,data5000,choose.vars = c("Lat","Lon"),ellipse=TRUE)
