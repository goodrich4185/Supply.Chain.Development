#Importing the data file
data.file<-"/Users/goodrich/Desktop/R Projects/Supply.Chain.Development/2012_Iowa_Census.csv"
county_area<-read.csv(data.file,header=TRUE,sep=",")
county_intervals<-as.matrix(subset(county_area,Domain.Category!="NOT SPECIFIED"))

#Renaming the area harvested variables
for(i in 1:nrow(county_intervals)){
  if(county_intervals[i,"Domain.Category"]=="AREA HARVESTED: (1,000 OR MORE ACRES)"){
    county_intervals[i,"Domain.Category"]<-"1000 OR MORE ACRES"
  }
  if(county_intervals[i,"Domain.Category"]=="AREA HARVESTED: (500 TO 999 ACRES)"){
    county_intervals[i,"Domain.Category"]<-"500 TO 999 ACRES"
  }
  if(county_intervals[i,"Domain.Category"]=="AREA HARVESTED: (100 TO 249 ACRES)"){
    county_intervals[i,"Domain.Category"]<-"100 TO 249 ACRES"
  }
  if(county_intervals[i,"Domain.Category"]=="AREA HARVESTED: (250 TO 499 ACRES)"){
    county_intervals[i,"Domain.Category"]<-"250 TO 499 ACRES"
  }
  if(county_intervals[i,"Domain.Category"]=="AREA HARVESTED: (25.0 TO 99.9 ACRES)"){
    county_intervals[i,"Domain.Category"]<-"25 TO 99 ACRES"
  }
  if(county_intervals[i,"Domain.Category"]=="AREA HARVESTED: (1.0 TO 24.9 ACRES)"){
    county_intervals[i,"Domain.Category"]<-"1 TO 24 ACRES"
  }
}
county_intervals<-as.data.frame(county_intervals)

#Sorting by area harvested variables
county_intervals<-county_intervals[with(county_intervals,order(County,factor(Domain.Category,levels=c("1 TO 24 ACRES","25 TO 99 ACRES","100 TO 249 ACRES","250 TO 499 ACRES","500 TO 999 ACRES","1000 OR MORE ACRES")))),]

#Removing unnecessary columns
county_intervals<-subset(county_intervals,select=c("Year","County","Domain.Category","Value"))
