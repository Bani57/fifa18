dataset <- read.csv("./complete.csv", row.names=1, stringsAsFactors=FALSE, encoding = 'UTF-8')
dataset_gk=dataset[which(dataset$prefers_gk=="True"),]
dataset=dataset[which(dataset$prefers_gk!="True"),]

pace=dataset[,c("acceleration","sprint_speed","pac")]
fit <- lm(pac ~ acceleration + sprint_speed, data=pace)
coef=coefficients(fit)
print(coef)
output=as.numeric(fitted.values(fit))
error=sum(abs(round(output)-pace$pac))
print(error)

shooting=dataset[,c("finishing","long_shots","penalties","positioning","shot_power","volleys","sho")]
fit <- lm(sho ~ finishing + long_shots + penalties + positioning + shot_power + volleys, data=shooting)
coef=coefficients(fit)
print(coef)
output=as.numeric(fitted.values(fit))
error=sum(abs(round(output)-shooting$sho))
print(error)

passing=dataset[,c("crossing","curve","free_kick_accuracy","long_passing","short_passing","vision","pas")]
fit <- lm(pas ~ crossing + curve + free_kick_accuracy + long_passing + short_passing + vision, data=passing)
coef=coefficients(fit)
print(coef)
output=as.numeric(fitted.values(fit))
error=sum(abs(round(output)-passing$pas))
print(error)

dribbling=dataset[,c("agility","balance","ball_control","composure","dribbling","reactions","dri")]
fit <- lm(dri ~ agility + balance + ball_control + composure + dribbling + reactions, data=dribbling)
coef=coefficients(fit)
print(coef)
output=as.numeric(fitted.values(fit))
error=sum(abs(round(output)-dribbling$dri))
print(error)

defending=dataset[,c("heading_accuracy","interceptions","marking","sliding_tackle","standing_tackle","def")]
fit <- lm(def ~ heading_accuracy + interceptions + marking + sliding_tackle + standing_tackle, data=defending)
coef=coefficients(fit)
print(coef)
output=as.numeric(fitted.values(fit))
error=sum(abs(round(output)-defending$def))
print(error)

physical=dataset[,c("aggression","jumping","stamina","strength","phy")]
fit <- lm(phy ~ aggression + jumping + stamina + strength, data=physical)
coef=coefficients(fit)
print(coef)
output=as.numeric(fitted.values(fit))
error=sum(abs(round(output)-physical$phy))
print(error)

goalkeeping=dataset_gk[,c("gk_diving","gk_handling","gk_kicking","gk_positioning","gk_reflexes","overall")]
fit <- lm(overall ~ gk_diving + gk_handling + gk_kicking + gk_positioning + gk_reflexes, data=goalkeeping)
coef=coefficients(fit)
print(coef)
output=as.numeric(fitted.values(fit))
error=sum(abs(round(output)-goalkeeping$overall))
print(error)