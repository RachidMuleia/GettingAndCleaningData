

#QUESTION 1 WEEK3
destfile<-file.path(getwd(),"housing.csv")
url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(url, destfile=destfile)

housing<-read.csv(destfile)
head(housing)
housing$agricultureLogical<-ifelse(housing$ACR==3 & housing$AGS==6,TRUE, FALSE)
head(which(housing$agricultureLogical==TRUE ), n=3)

#QUESTION 2 WEEK3

install.packages("jpeg")
library(jpeg)
fileURL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
path<-file.path(getwd(),"jeff.jpg")
download.file(fileURL, destfile =path)
jeff<-readJPEG(path,native=TRUE)
quantile(jeff, probs=c(0.3,0.8))



#QUESTION 3 WEEK3
library(data.table)
library(dplyr)
fileURL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
path=file.path(getwd(),"gdp.csv")
download.file(fileURL, destfile =path)
gdp<-data.table(read.csv(path, skip = 4, nrows =231))

fileurl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
path<-file.path(getwd(),"country.csv")
download.file(fileurl, destfile =path)
country<-data.table(read.csv(path))

gdp<-gdp[!gdp$X=="",]

setnames(gdp, c("X", "X.1", "X.3", "X.4"), c("CountryCode", "rankingGDP","Long.Name", "gdp"))
gdp_country<-merge(gdp,country, by=c("CountryCode"), all=TRUE)

sum(!is.na(unique(gdp_country$rankingGDP)))
#gdp_country<-arrange(gdp_country, desc(Gross.domestic.product.2012))
gdp_country<-gdp_country[order(gdp_country$rankingGDP, decreasing =TRUE),]
gdp_country$Long.Name.x[13]


#QUESTION4 WEEK3
mean_group<-gdp_country %>%
            group_by(Income.Group) %>%
            summarize(mean(rankingGDP, na.rm=TRUE))

mean_group


#QUESTION5 WEEK5
quant<-quantile(gdp_country$rankingGDP, probs=seq(0, 1, 0.2), na.rm = TRUE)
gdp_country$quantileGDP <- cut(gdp_country$rankingGDP, breaks = quant)

table(gdp_country$quantileGDP,gdp_country$Income.Group)
