###outlier detection


speedData <-read.csv("SP.csv")
speedData <- speedData[,-c(47,48,8,44)]

mod <- lm(match ~ ., data=speedData)
cooksd <- cooks.distance(mod)

plot(cooksd, pch="*", cex=2, main="")  # plot cook's distance
abline(h = 10*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, 
     labels=ifelse(cooksd>10*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

