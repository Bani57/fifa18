dataset <- read.csv("./dataset_cleaned.csv", encoding="UTF-8", row.names=1, stringsAsFactors=FALSE)
specialities_start=grep("X1_on_1_rush_trait",colnames(dataset))
specialities_end=grep("clinical_finisher_speciality",colnames(dataset))
dataset_specialities=dataset[,c(specialities_start:specialities_end)]

transactions=list()
n=length(dataset_specialities[,1])
j=1
for(i in c(1:n))
{
  traits=names(dataset_specialities[i,which(dataset_specialities[i,]=="True")])
  if(length(traits)!=0)
  {
    transactions[[j]]=as.list(traits)
    j=j+1
  }
}

invisible(lapply(transactions, function(x) write.table(unname(data.frame(x)),"./transactions_specialities.csv",append=TRUE,sep=',')))