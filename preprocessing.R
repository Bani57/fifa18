z.interval<-function(X,doverba)
{
  n=length(X)
  if(n>=30)
  {
    alfa=1-doverba
    zalfa=qnorm(1-alfa/2)
    S=sd(X)
    paste('Интервал на доверба за математичкото очекување: (',mean(X)-zalfa*S/sqrt(n),',',mean(X)+zalfa*S/sqrt(n),')')
  }
  else paste('Дисперзијата не е позната и примерокот е многу мал, не може да се користи z-тест!')
}

t.interval<-function(X,doverba)
{
  n=length(X)
  if(n>=30)
    print('Дисперзијата не е позната но примерокот е поголем, не мора да се користи t-тест.')
  alfa=1-doverba
  talfa=qt(1-alfa/2,n-1)
  S=sd(X)
  paste('Интервал на доверба за математичкото очекување: (',mean(X)-talfa*S/sqrt(n),',',mean(X)+talfa*S/sqrt(n),')')
}

normalize<-function(x,new_min=0,new_max=1)
{
  return ((new_max-new_min)*(x-min(x))/(max(x)-min(x))+new_min)
}

find_outliers<-function(x)
{
  q1=quantile(x,0.25)
  q3=quantile(x,0.75)
  iqr=q3-q1
  return(c(which(x<q1-1.5*iqr),which(x>q3+1.5*iqr)))
}

analyze_discrete<-function(x,name)
{
  x_stats=as.data.frame(table(x))
  x_freq=as.numeric(unname(unlist(x_stats$Freq)))
  m=mean(x_freq)
  S=sd(x_freq)
  par(mfrow=c(1,1))
  barplot(x_freq,main=paste('Barplot за ',name),xlab=name,names.arg=x_stats[,1],ylim=c(0,1.5*max(x_freq)),col=3)
  abline(h=sum(x_freq)/length(x_freq),col="blue",lwd=3,lty=2)
  p=chisq.test(x_freq)$p.value
  print(paste('Величината има рамномерна распределба: ',p>0.01))
  print(paste('p - вредноста: ', p))
  print(paste('Просек на примерокот: ',m))
  print(paste('Стандардна девијација на примерокот: ',S))
  t.interval(x_freq,doverba=0.99)
}

analyze_continous<-function(x,name)
{
  m=mean(x)
  S=sd(x)
  par(mfrow=c(1,2))
  hist(x,freq=FALSE,main=paste('Хистограм за ',name),xlab=name,ylim=c(0,2*max(density(x)$y)),col=3)
  lines(density(x),lwd=3)
  curve(dnorm(x,m,S),add=TRUE,col="blue",lwd=3,lty=2)
  legend("topright", c("Expected distribution", "Actual distribution"), lty=c(2, 1) , lwd=c(3, 3), col=c("blue","black"),box.lwd=3)
  boxplot(x,main=paste('Boxplot за ',name),varwidth=TRUE,col=3)
  par(mfrow=c(1,1))
  p=ks.test(x+runif(length(x),0,0.001),"pnorm",mean=m,sd=S)$p.value
  print(paste('Величината има нормална распределба: ',p>0.01))
  print(paste('p - вредноста: ', p))
  print(paste('Просек на примерокот: ',m))
  print(paste('Стандардна девијација на примерокот: ',S))
  z.interval(x,doverba=0.99)
}

check_correlation<-function(X,name1,name2,lg=FALSE)
{
  plot(X,pch=4,lwd=3,main=paste("Регресионен модел за зависноста меѓу ",name1," и ",name2," на играчите"),xlab=name1,ylab=name2)
  if(lg)
  {
    model<-lm(log(X[,2])~X[,1])
    coef=as.numeric(model$coef)
    x=X[,1]
    curve(exp(coef[2]*x+coef[1]),add=TRUE,col=3,lwd=3,lty=2)
    print(paste("Коефициент на корелација: ",cor(X[,1],log(X[,2]))))
  }
  else
  {
    model<-lm(X[,2]~X[,1])
    coef=as.numeric(model$coef)
    x=X[,1]
    curve(coef[2]*x+coef[1],add=TRUE,col=3,lwd=3,lty=2)
    print(paste("Коефициент на корелација: ",cor(X[,1],X[,2])))
  }
}


dataset <- read.csv("./complete.csv", row.names=1, stringsAsFactors=FALSE, encoding = 'UTF-8')

clubs=dataset$club
clubs=clubs[which(clubs!="")]
jpeg(file = "./plots/clubs.jpeg",width = 1920,height=1080, quality=100)
analyze_discrete(clubs,"играчи по тим")
dev.off()

leagues=dataset$league
leagues=leagues[which(leagues!="")]
jpeg(file = "./plots/leagues.jpeg",width = 1920,height=1080, quality=100)
analyze_discrete(leagues,"играчи по лига")
dev.off()

special=as.numeric(unname(unlist(dataset$special)))
jpeg(file = "./plots/special.jpeg",width = 1920,height=1080, quality=100)
analyze_continous(special,"special")
dev.off()


age=as.numeric(unname(unlist(dataset$age)))
birth_date=as.Date(unname(unlist(dataset$birth_date)),format = "%Y-%m-%d")
year=as.numeric(format(birth_date,"%Y"))
age_cleaned=age[which(abs(year+age-2016)<=1)]
jpeg(file = "./plots/age.jpeg",width = 1920,height=1080, quality=100)
analyze_continous(age_cleaned,"возраст")
dev.off()

height=as.numeric(unname(unlist(dataset$height_cm)))
jpeg(file = "./plots/height.jpeg",width = 1920,height=1080, quality=100)
analyze_continous(height,"висина")
dev.off()

weight=as.numeric(unname(unlist(dataset$weight_kg)))
jpeg(file = "./plots/weight.jpeg",width = 1920,height=1080, quality=100)
analyze_continous(weight,"тежина")
dev.off()

body_types=dataset$body_type
jpeg(file = "./plots/body_type.jpeg",width = 1920,height=1080, quality=100)
analyze_discrete(body_types,"играчи по тип на тело")
dev.off()

all_body_types=unique(body_types)
N=length(all_body_types)
colors=c(1:(1+N))
colors=colors[match(body_types,all_body_types)]
shapes=c(1:(1+N))
shapes=shapes[match(body_types,all_body_types)]
jpeg(file = "./plots/body_type_weight_height.jpeg",width = 1920,height=1080, quality=100)
plot(height,weight,main="Зависност на тип на тело од висина и тежина",xlab="висина",ylab="тежина",pch=shapes,col=colors,lwd=3)
legend("topleft", all_body_types, title="Типови на тело", pch=c(1:(1+N)), col=c(1:(1+N)), box.lwd=3, inset=+0.025, xpd=TRUE, ncol = 2)
dev.off()

nationalities=dataset$nationality
jpeg(file = "./plots/nationality.jpeg",width = 1920,height=1080, quality=100)
analyze_discrete(nationalities,"играчи по националност")
dev.off()

overall=as.numeric(unname(unlist(dataset$overall)))
jpeg(file = "./plots/overall.jpeg",width = 1920,height=1080, quality=100)
analyze_continous(overall,"вкупна вештина")
dev.off()

potential=as.numeric(unname(unlist(dataset$potential)))
jpeg(file = "./plots/potential.jpeg",width = 1920,height=1080, quality=100)
analyze_continous(potential,"потенцијал на играч")
dev.off()


preferred_feet=dataset$preferred_foot
jpeg(file = "./plots/preferred_foot.jpeg",width = 1920,height=1080, quality=100)
analyze_discrete(preferred_feet,"играчи по омилена нога")
dev.off()

work_rate_att=dataset$work_rate_att
jpeg(file = "./plots/work_rate_att.jpeg",width = 1920,height=1080, quality=100)
analyze_discrete(work_rate_att,"играчи по рата на вежбање напад")
dev.off()

work_rate_def=dataset$work_rate_def
jpeg(file = "./plots/work_rate_def.jpeg",width = 1920,height=1080, quality=100)
analyze_discrete(work_rate_def,"играчи по рата на вежбање одбрана")
dev.off()

wage=as.numeric(unname(unlist(dataset$eur_wage)))
wage=wage[which(wage!=0)]
jpeg(file = "./plots/wage.jpeg",width = 1920,height=1080, quality=100)
analyze_continous(log10(wage),"log(плата на играч)")
dev.off()

value=as.numeric(unname(unlist(dataset$eur_value)))
value=value[which(value!=0)]
jpeg(file = "./plots/value.jpeg",width = 1920,height=1080, quality=100)
analyze_continous(log10(value),"log(вредност на играч)")
dev.off()

release_clause=as.numeric(unname(unlist(dataset$eur_release_clause)))
release_clause=release_clause[which(release_clause!=0)]
jpeg(file = "./plots/release_clause.jpeg",width = 1920,height=1080, quality=100)
analyze_continous(log10(release_clause),"log(откупна клаузула на играч)")
dev.off()

value_overall=dataset[,c("overall","eur_value")]
value_overall=value_overall[which(value_overall[,2]!=0),]
jpeg(file = "./plots/value_overall.jpeg",width = 1920,height=1080, quality=100)
check_correlation(value_overall,"вкупна вештина","вредност",lg = TRUE)
dev.off()

wage_overall=dataset[,c("overall","eur_wage")]
wage_overall=wage_overall[which(wage_overall[,2]!=0),]
jpeg(file = "./plots/wage_overall.jpeg",width = 1920,height=1080, quality=100)
check_correlation(wage_overall,"вкупна вештина","плата",lg = TRUE)
dev.off()

release_clause_overall=dataset[,c("overall","eur_release_clause")]
release_clause_overall=release_clause_overall[which(release_clause_overall[,2]!=0),]
jpeg(file = "./plots/release_clause_overall.jpeg",width = 1920,height=1080, quality=100)
check_correlation(release_clause_overall,"вкупна вештина","откупна клаузула",lg = TRUE)
dev.off()

age_overall=dataset[,c("overall","birth_date")]
age_overall[,2]=as.Date(age_overall[,2],format = "%Y-%m-%d")
today=as.Date(Sys.Date(),format="%Y-%m-%d")
age_overall[,2]=as.numeric(today-age_overall[,2])
jpeg(file = "./plots/age_overall.jpeg",width = 1920,height=1080, quality=100)
check_correlation(age_overall,"вкупна вештина","возраст")
dev.off()

weight_overall=dataset[,c("overall","weight_kg")]
jpeg(file = "./plots/weight_overall.jpeg",width = 1920,height=1080, quality=100)
check_correlation(weight_overall,"вкупна вештина","тежина")
dev.off()

height_overall=dataset[,c("overall","height_cm")]
jpeg(file = "./plots/height_overall.jpeg",width = 1920,height=1080, quality=100)
check_correlation(height_overall,"вкупна вештина","висина")
dev.off()


dataset_outliers=dataset[,c("age","height_cm","overall","special","eur_value","eur_wage","eur_release_clause","weight_kg")]
dataset_outliers=dataset_outliers[which(dataset_outliers[,"eur_wage"]!=0),]
dataset_outliers=dataset_outliers[which(dataset_outliers[,"eur_value"]!=0),]
dataset_outliers=dataset_outliers[which(dataset_outliers[,"eur_release_clause"]!=0),]
dataset_outliers[,c("eur_value","eur_wage","eur_release_clause")]=log10(dataset_outliers[,c("eur_value","eur_wage","eur_release_clause")])

age_outliers=find_outliers(dataset_outliers$age)
special_outliers=find_outliers(dataset_outliers$special)
overall_outliers=find_outliers(dataset_outliers$overall)
height_outliers=find_outliers(dataset_outliers$height_cm)
weight_outliers=find_outliers(dataset_outliers$weight_kg)
value_outliers=find_outliers(dataset_outliers$eur_value)
wage_outliers=find_outliers(dataset_outliers$eur_wage)
release_clause_outliers=find_outliers(dataset_outliers$eur_release_clause)
attributes_outliers=list(age_outliers,special_outliers,overall_outliers,height_outliers,weight_outliers,value_outliers,wage_outliers,release_clause_outliers)
possible_outliers=Reduce(union,attributes_outliers)
outliers=character()
outlier_threshold=4
for (outlier in possible_outliers) {
  count=0
  for (at in attributes_outliers)
  {
    if (!is.na(match(outlier,at)))
      count=count+1
  }
  if(count>=outlier_threshold)
    outliers=c(outliers,dataset[outlier,"name"])
}
print(outliers)
#dataset=dataset[which(is.na(match(dataset[,"name"],outliers))),]

print("Атрибути со вредности кои недостасуваат: ")
attributes_na=colnames(dataset)
for (i in c(1:length(attributes_na))) {
  if(length(which(is.na(dataset[,i])))>0)
  {
    print(attributes_na[i])
  }
  if(is.numeric(dataset[,i]))
    dataset[which(!is.na(dataset[,i])),i]=normalize(dataset[which(!is.na(dataset[,i])),i])
}

write.csv(dataset,"dataset_cleaned.csv")
dataset_cleaned <- read.csv("./dataset_cleaned.csv", row.names=1, stringsAsFactors=FALSE, encoding = 'UTF-8')