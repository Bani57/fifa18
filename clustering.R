normalize<-function(x,new_min=0,new_max=1)
{
  return ((new_max-new_min)*(x-min(x))/(max(x)-min(x))+new_min)
}

library(ggplot2)
dataset <- read.csv("./dataset_cleaned.csv", encoding="UTF-8", row.names=1, stringsAsFactors=FALSE)
skills_start=grep("^crossing$",colnames(dataset))
skills_end=grep("^gk_reflexes$",colnames(dataset))
dataset_skills=dataset[,c(skills_start:skills_end)]

result=kmeans(dataset_skills,4,nstart=25)
print(result$tot.withinss/result$betweenss)

pca_dataset_skills=prcomp(dataset_skills,center = TRUE,scale. = TRUE,rank. = 2)
print(pca_dataset_skills$sdev)
pca_dataset_skills=pca_dataset_skills$x

ggplot(data.frame(pca_dataset_skills), aes(PC1, PC2, color = result$cluster)) + geom_point()

clusters_dataset=matrix(0,nrow = length(dataset[,1]),ncol = 2)
clusters_dataset[,1]=names(result$cluster)
clusters_dataset[,2]=as.numeric(unname(unlist(result$cluster)))
colnames(clusters_dataset)=c("ID","Cluster")
clusters_dataset=data.frame(clusters_dataset)
write.csv(clusters_dataset,"./clustered_dataset.csv",sep=',',row.names = FALSE)

clusters <- hclust(dist(dataset_skills))
#plot(clusters)
clusterCut <- cutree(clusters, 4)

ggplot(data.frame(pca_dataset_skills), aes(PC1, PC2, color = clusterCut)) + geom_point()

clusters_dataset=matrix(0,nrow = length(dataset[,1]),ncol = 2)
clusters_dataset[,1]=names(clusterCut)
clusters_dataset[,2]=as.numeric(unname(unlist(clusterCut)))
colnames(clusters_dataset)=c("ID","Cluster")
clusters_dataset=data.frame(clusters_dataset)
write.csv(clusters_dataset,"./hierarchically_clustered_dataset.csv",sep=',',row.names = FALSE)

positions_start=grep("^rs$",colnames(dataset))
positions_end=grep("^gk$",colnames(dataset))
dataset_positions=dataset[,c(positions_start:positions_end)]
for(i in c(1:length(colnames(dataset_positions))))
{
  dataset_positions[which(is.na(dataset_positions[,i])),i]=0
  dataset_positions[,i]=normalize(dataset_positions[,i])
}

result=kmeans(dataset_positions,4,nstart=25)
print(result$tot.withinss/result$betweenss)

pca_dataset_positions=prcomp(dataset_positions,center = TRUE,scale. = TRUE,rank. = 2)
print(pca_dataset_positions$sdev)
pca_dataset_positions=pca_dataset_positions$x

ggplot(data.frame(pca_dataset_positions), aes(PC1, PC2, color = result$cluster)) + geom_point()

clusters_dataset=matrix(0,nrow = length(dataset[,1]),ncol = 2)
clusters_dataset[,1]=names(result$cluster)
clusters_dataset[,2]=as.numeric(unname(unlist(result$cluster)))
colnames(clusters_dataset)=c("ID","Cluster")
clusters_dataset=data.frame(clusters_dataset)
write.csv(clusters_dataset,"./clustered_dataset_positions.csv",sep=',',row.names = FALSE)

clusters <- hclust(dist(dataset_positions))
#plot(clusters)
clusterCut <- cutree(clusters, 4)

ggplot(data.frame(pca_dataset_positions), aes(PC1, PC2, color = clusterCut)) + geom_point()

clusters_dataset=matrix(0,nrow = length(dataset[,1]),ncol = 2)
clusters_dataset[,1]=names(clusterCut)
clusters_dataset[,2]=as.numeric(unname(unlist(clusterCut)))
colnames(clusters_dataset)=c("ID","Cluster")
clusters_dataset=data.frame(clusters_dataset)
write.csv(clusters_dataset,"./hierarchically_clustered_dataset_positions.csv",sep=',',row.names = FALSE)