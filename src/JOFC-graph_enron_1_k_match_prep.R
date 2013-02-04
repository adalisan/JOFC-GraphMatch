employees <- read.delim("~/projects/DataFusion-graphmatch/data/employees.csv", header=F,stringsAsFactors=F)
employees$V2[employees$V2=="xxx"] <- NA
na.name<-is.na(employees$V2)
employees$V2<-paste(employees$V2,employees$V3)
employees$V2[na.name] <- NA
emp.names<- unique(employees$V2)

unique.count<- length(emp.names)
emp.index <- sapply(employees$V2,function(x){t<- which(emp.names==x)
                                             if (length(t)>0) {return(t)
                                             } else {return(NA)}
                                             })
emp.index[is.na(emp.index)] <- (unique.count+1):length(emp.index)


corr.list<-list()
for (i in 1:unique.count){
  new.list.el <- list(a=which(emp.index==i),b=i)
  corr.list<-c(corr.list,new.list.el)
}
AAA.clean