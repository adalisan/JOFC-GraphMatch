
load("~/projects/DataFusion-graphmatch/data/AAA-187As-184x184.Rbin")

employees <- read.delim("~/projects/DataFusion-graphmatch/data/employees.csv", header=F,stringsAsFactors=F)
#Set employee names "xxx" to NA 
employees$V2[employees$V2=="xxx"] <- NA
na.name<-is.na(employees$V2)
employees$V2<-paste(employees$V2,employees$V3)
employees$V2[na.name] <- NA

#Find unique employee names (should be 124 of them +NA)
emp.names<- unique(employees$V2)
#Remove NA from unique names
emp.names <- emp.names[!is.na(emp.names)]
unique.count<- length(emp.names)

# Create a numeric vector which contains the index of the employee name for each of 184 verts
#In the original employee dataset, for each row, find the index of unique emp. name 
# the row corresponds to, if the name is not among the unique names, put NA temporarirliy
emp.index <- sapply(employees$V2,function(x){t<- which(emp.names==x)
                                             if (length(t)>0) {return(t)
                                             } else {return(NA)}
                                             })
num.unannotated.emp<-sum(is.na(emp.index))
clean.gr.size <- (unique.count+num.unannotated.emp)
# add extra indices for verts whose names are "xxx" now indices should run up to 150
emp.index[is.na(emp.index)] <- (unique.count+1):clean.gr.size
unique.count<- clean.gr.size

#Create the correspondence list , 
#  corr.list[[k]] = list(a=j,b=i)
# ith vertex(row/column) in second graph(adj. matrix) corresponds to  vertices in j (which is a list)
# It might be that k=i 
corr.list<-list()
for (i in 1:unique.count){
  new.list.el <- list(a=which(emp.index==i),b=i)
  corr.list <- c(corr.list,list(new.list.el))
}


#Create a pruned version of AAA where we have vertices corresponding to 150  unique  employees
gr.size <- dim(AAA[[1]])
AAA.clean<-array(NA,dim=c(length(AAA),rep(unique.count,2)))

find.pred<-function(list.el,searched.index) {list.el[[2]]==searched.index}


for (i in 1:length(AAA))
  for (s in 1:unique.count){
  list.el.s<-  Find(function(list.el) {list.el$b==s},corr.list) #corr.list[[which(lapply(corr.list,find.pred,s))]]
  for (t in 1:unique.count){
    
    list.el.t<- Find(function(list.el) {list.el$b==t},corr.list)
    AAA.clean[i,s,t]<- sum(AAA[[i]][list.el.s$a,list.el.t$a])
   }
}












