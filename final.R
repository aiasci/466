


# Libraries Required-------------

## Penn World Table 10
if (!require(pwt10)) install.packages('pwt10')
## Data Table for manipulating the data
if (!require(data.table)) install.packages('data.table')


## Loading the packages
library(pwt10)
library(data.table)

# Data manipulation ------------

## saving the pwt data as data.table
a <- data.table(pwt10.0)

## manipulating data for required countries
a<-a[isocode== "FRA"|
       isocode== "DEU"|
       isocode== "NLD"|
       isocode== "GBR"|
       isocode== "JPN"|
       isocode== "USA"]

## manipulating data for required countries
a<-a[,c(1:3,8:10,19,22,24)]
## there exist a NA data in rtfpna btw. 1950-1953
a[year<1954]
## manipulating data for eliminating the NA variables existed btw. 1950-1953
a<-a[year>1953]
#periodization
p1<-a[year<1974]
p2<-a[year>1973&year<1983]
p3<-a[year>1982&year<2001]
p4<-a[year>2000&year<2009]
p5<-a[year>2008&year<2016]
## Calculations of growth
### p1 
p1[,log(emp[nrow(p1)/6]*avh[nrow(p1)/6])-log(emp[1]*avh[1]), by=country] ## growth rate of total working hour
p1[,log(rgdpna[nrow(p1)/6])-log(rgdpna[1]), by=country] ## growth rate of output
p1[,log(rnna[nrow(p1)/6])-log(rnna[1]), by=country] ## growth rate of capital
p1[,log(rtfpna[nrow(p1)/6])-log(rtfpna[1]), by=country] ## growth rate of TFP
p1[,log(hc[nrow(p1)/6])-log(hc[1]), by=country] ## growth rate of capital
## Creating data table
p11<-data.table(country= unique(p1$country),
                gY=p1[,log(rgdpna[nrow(p1)/6])-log(rgdpna[1]), by=country]$V1,
                gA=p1[,log(rtfpna[nrow(p1)/6])-log(rtfpna[1]), by=country]$V1,
                gK=p1[,log(rnna[nrow(p1)/6])-log(rnna[1]), by=country]$V1,
                gL=p1[,log(emp[nrow(p1)/6]*avh[nrow(p1)/6])-log(emp[1]*avh[1]), by=country]$V1,
                gH=p1[,log(hc[nrow(p1)/6])-log(hc[1]), by=country]$V1)
## unexplained part of growth by model
p11[, error := (gY-gA-0.3*gK-0.7*gL-0.7*gH)/gY]
### p2 
p21<-data.table(country= unique(p1$country),
                gY=p2[,log(rgdpna[nrow(p2)/6])-log(rgdpna[1]), by=country]$V1,
                gA=p2[,log(rtfpna[nrow(p2)/6])-log(rtfpna[1]), by=country]$V1,
                gK=p2[,log(rnna[nrow(p2)/6])-log(rnna[1]), by=country]$V1,
                gL=p2[,log(emp[nrow(p2)/6]*avh[nrow(p2)/6])-log(emp[1]*avh[1]), by=country]$V1,
                gH=p2[,log(hc[nrow(p2)/6])-log(hc[1]), by=country]$V1)
## unexplained part of growth by model
p21[, error := (gY-gA-0.3*gK-0.7*gL-0.7*gH)/gY]
### p3
p31<-data.table(country= unique(p1$country),
                gY=p3[,log(rgdpna[nrow(p3)/6])-log(rgdpna[1]), by=country]$V1,
                gA=p3[,log(rtfpna[nrow(p3)/6])-log(rtfpna[1]), by=country]$V1,
                gK=p3[,log(rnna[nrow(p3)/6])-log(rnna[1]), by=country]$V1,
                gL=p3[,log(emp[nrow(p3)/6]*avh[nrow(p3)/6])-log(emp[1]*avh[1]), by=country]$V1,
                gH=p3[,log(hc[nrow(p3)/6])-log(hc[1]), by=country]$V1)
## unexplained part of growth by model
p31[, error := (gY-gA-0.3*gK-0.7*gL-0.7*gH)/gY]
### p4 
p41<-data.table(country= unique(p1$country),
                gY=p4[,log(rgdpna[nrow(p4)/6])-log(rgdpna[1]), by=country]$V1,
                gA=p4[,log(rtfpna[nrow(p4)/6])-log(rtfpna[1]), by=country]$V1,
                gK=p4[,log(rnna[nrow(p4)/6])-log(rnna[1]), by=country]$V1,
                gL=p4[,log(emp[nrow(p4)/6]*avh[nrow(p4)/6])-log(emp[1]*avh[1]), by=country]$V1,
                gH=p4[,log(hc[nrow(p4)/6])-log(hc[1]), by=country]$V1)
## unexplained part of growth by model
p41[, error := (gY-gA-0.3*gK-0.7*gL-0.7*gH)/gY]
### p5 
p51<-data.table(country= unique(p1$country),
                gY=p5[,log(rgdpna[nrow(p5)/6])-log(rgdpna[1]), by=country]$V1,
                gA=p5[,log(rtfpna[nrow(p5)/6])-log(rtfpna[1]), by=country]$V1,
                gK=p5[,log(rnna[nrow(p5)/6])-log(rnna[1]), by=country]$V1,
                gL=p5[,log(emp[nrow(p5)/6]*avh[nrow(p5)/6])-log(emp[1]*avh[1]), by=country]$V1,
                gH=p5[,log(hc[nrow(p5)/6])-log(hc[1]), by=country]$V1)
## unexplained part of growth by model
p51[, error := (gY-gA-0.3*gK-0.7*gL-0.7*gH)/gY]
## Countrywise grouping
## germany 
ger<-rbind(p11[1,],p21[1,],p31[1,],p41[1,],p51[1,])
ger[,year:= c("1954-1973","1974-1982","1983-2000","2001-2008","2009-2015")]
ger<-cbind(ger[,c(1,8)],ger[,c(2:7)])
## france
fra<-rbind(p11[2,],p21[2,],p31[2,],p41[2,],p51[2,])
fra[,year:= c("1954-1973","1974-1982","1983-2000","2001-2008","2009-2015")]
fra<-cbind(fra[,c(1,8)],fra[,c(2:7)])
## britain 
uk<-rbind(p11[3,],p21[3,],p31[3,],p41[3,],p51[3,])
uk[,year:= c("1954-1973","1974-1982","1983-2000","2001-2008","2009-2015")]
uk<-cbind(uk[,c(1,8)],uk[,c(2:7)])
## japan 
jap<-rbind(p11[4,],p21[4,],p31[4,],p41[4,],p51[4,])
jap[,year:= c("1954-1973","1974-1982","1983-2000","2001-2008","2009-2015")]
jap<-cbind(jap[,c(1,8)],jap[,c(2:7)])
## netherlands
nld<-rbind(p11[5,],p21[5,],p31[5,],p41[5,],p51[5,])
nld[,year:= c("1954-1973","1974-1982","1983-2000","2001-2008","2009-2015")]
nld<-cbind(nld[,c(1,8)],nld[,c(2:7)])
## us 
usa<-rbind(p11[6,],p21[6,],p31[6,],p41[6,],p51[6,])
usa[,year:= c("1954-1970","1971-1982","1983-2000","2001-2008","2009-2015")]
usa<-cbind(usa[,c(1,8)],usa[,c(2:7)])
## final table ----------
## Collectig results for all countries in one data.table
datum<- rbind(ger,fra,uk,jap,nld,usa)
## Rearraning the data.table
datum<- cbind(datum[,1:2],round(datum[,3],3),round(datum[,4],3),round(datum[,5],3),round(datum[,6],3),round(datum[,7],3),round(datum[,8],3))
## Contribution of the TFP
datum[, tfp_contribution:= gA/gY]
## Contribution of the Capital
datum[, capital_contribution := gK*0.3/gY]
## Contribution of the Total Hours Worked
datum[, labour_contribution := gL*0.7/gY]
## Contribution of the Human Capital
datum[, human_capital_contribution := gH*0.7/gY]
## Deleting unneccesary variables
rm(a,fra,ger,jap,nld,uk,usa,p1,p11,p2,p21,p3,p31,p4,p41,p5,p51)
datum
