apr14<-read.csv("C:/Users/lenovo/Documents/DATA/uber-raw-data-apr14.csv", header=TRUE)
may14<-read.csv("C:/Users/lenovo/Documents/DATA/uber-raw-data-may14.csv", header=TRUE)
jun14<-read.csv("C:/Users/lenovo/Documents/DATA/uber-raw-data-jun14.csv", header=TRUE)
jul14<-read.csv("C:/Users/lenovo/Documents/DATA/uber-raw-data-jul14.csv", header=TRUE)
aug14<-read.csv("C:/Users/lenovo/Documents/DATA/uber-raw-data-aug14.csv", header=TRUE)
sep14<-read.csv("C:/Users/lenovo/Documents/DATA/uber-raw-data-sep14.csv", header=TRUE)
data14<-bind_rows(apr14,may14,jun14,jul14,aug14,sep14)
summary(data14)
install.packages("VIM")