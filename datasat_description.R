require("dplyr")        # Data manipulation
require("reshape2")     # Data reshaping for ggplot
require("ggplot2")      # Data visualization
require("plotly")       # Dynamic data visualization
require("RColorBrewer") # Colors on plots
require("readr")        # CSV file I/O, e.g. the read_csv function
require("dataQualityR") # DQR generation
require("randomForest") # Random Forest for variable importance
require("scales")       # Colour palette
require("fmsb")         # Radar plots

missing.types <- c("NA", "")
data <- read.csv("speeddating.csv", na.strings = missing.types, stringsAsFactors = F)
attach(data)

##Attendance by waves
wave.distr <- as.data.frame(table(wave))
p <- ggplot(data=wave.distr, aes(x=wave, y=Freq)) +
    geom_bar(stat="identity",fill="#f9a65a", colour="black") + 
    xlab("Wave") + ylab("Attendance") + 
    geom_hline(yintercept = mean(wave.distr$Freq), color="blue",size=1)
p
##Attendance by waves

##Race distribution
raceTypes <- c("Asian","Black","Latino","?","European",
               "Other","Asian","Black","Latino","?","European","Other")
raceDistribution <- data[!is.na(data$race),] %>% group_by(gender, race) %>% summarise(my.n = n())
raceDistribution$race <- raceTypes
q <- ggplot(raceDistribution, aes(x = race, y = my.n, fill = factor(gender))) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_discrete(name = "") +
    xlab("") + ylab("")
   
q
##Race distribution

##Top Studies distribution
fields <- tolower(data$field)
fields.t <- as.data.frame(table(fields))
meanStudies <- round(mean(fields.t$Freq))
fields.t <- fields.t[order(fields.t$Freq),]
fields.t <- fields.t[211:220,]
finalfields <- data.frame(
    name=c("Education","Engineering","Sociology","Psychology",
           "Elect.Eng.","Int.Affairs","Social Work","MBA","Law","Business"),
    val=fields.t$Freq
)

finalfields %>%
    ggplot( aes(x=name, y=val)) +
    geom_bar(stat="identity",fill="#f9a65a") +
    coord_flip()+xlab("") + ylab("") +
    geom_hline(yintercept = meanStudies, color="blue",size=1) 
##Top Studies distribution

##Number of matches
nmatches <- as.data.frame(table(data$match))
names(nmatches) <- c("match","val")
nmatches$match<-as.character(nmatches)
nmatches[1,1]<-"No"
nmatches[2,1]<-"Yes"
nmatches<-as.data.frame(nmatches)
nmatches
nmatches %>%
    ggplot( aes(x=match, y=val)) +
    geom_bar(stat="identity",fill=c("tomato2","olivedrab"))+
    xlab("") + ylab("") + geom_text(aes(label = c("6698 (83%)","1380 (17%)")),size=6)
##Number of matches

#Barplot of ages per gender
ageDistribution<- data[!is.na(data$age),] %>%
    group_by(gender,age) %>% summarise(count = n())

ageDistribution2<- data[!is.na(data$age),]
ageDistribution2<- ageDistribution2[,c(3,4)]
ageDistribution2<-ageDistribution2 %>% group_by(gender)
ageDistribution2$gender<-as.factor(ageDistribution2$gender)
ageDistribution2$age<-as.numeric(ageDistribution2$age)

age_dif<-as.numeric(data$d_age)
mean(age_dif,na.rm=TRUE)
mean(ageDistribution2$age,na.rm=TRUE)

ageDistribution3<-ageDistribution2[ageDistribution2$gender=="male",]
ageDistribution4<-ageDistribution2[ageDistribution2$gender=="female",]

femalesAge<-ageDistribution4$age
malesAge<-ageDistribution3$age
meanAgeMales<-mean(malesAge,na.rm=TRUE)
meanAgeFemales<-mean(femalesAge,na.rm=TRUE)

r <- ggplot(ageDistribution, aes(x = age, y = count, fill = factor(gender))) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_discrete(name = "") +
    xlab("") + ylab("") 
r

s<- ggplot(ageDistribution2, aes(x = gender, y = age)) +
    geom_boxplot(fill=c("#F8766D","#00BFC4"),outlier.colour="red", 
                 outlier.shape=10,outlier.size=5, notch=FALSE)+
    coord_cartesian(ylim = c(10, 54)) +
    xlab("") + ylab("")
    
s+geom_jitter(position=position_jitter(0.2),alpha=0.18)
#Barplot of ages per gender

#Importance race and religion
imp_race<-as.numeric(data$importance_same_race)
mean(imp_race,na.rm=TRUE)

imp_religion<-as.numeric(data$importance_same_religion)
mean(imp_religion,na.rm=TRUE)

allsec<-cbind(imp_race,imp_religion)
allsec<-as.matrix(allsec)
allsec<-as.data.frame(allsec)
allsec
t<- ggplot(stack(allsec), aes(x = ind, y = values)) +
    geom_boxplot(fill="#f9a65a")+
    xlab("") + ylab("")
    
t + geom_jitter(position=position_jitter(0.2),alpha=0.18)
#Importance race and religion

#Barplot of the distribution of what you value in the others
gender<-data[,3]

PValue<-cbind(gender,data[,40:45])#45 shared

PValue<-cbind(gender,data[,40:45])#45 shared
pv <- split(PValue, PValue$gender)
pv.females <- as.data.frame(pv[1])
pv.males <- as.data.frame(pv[2])
pv.females2 <- pv.females[,2:7]
pv.males2 <- pv.males[,2:7]

am<-as.numeric(pv.males2$male.attractive_important)
bm<-as.numeric(pv.males2$male.sincere_important)
cm<-as.numeric(pv.males2$male.intellicence_important)
dm<-as.numeric(pv.males$male.funny_important)
em<-as.numeric(pv.males$male.sincere_important)
fm<-as.numeric(pv.males$male.shared_interests_important)

af<-as.numeric(pv.females2$female.attractive_important)
bf<-as.numeric(pv.females2$female.sincere_important)
cf<-as.numeric(pv.females2$female.intellicence_important)
df<-as.numeric(pv.females$female.funny_important)
ef<-as.numeric(pv.females$female.sincere_important)
ff<-as.numeric(pv.females$female.shared_interests_important)

Male <- (c(mean(am,na.rm=TRUE),mean(bm,na.rm=TRUE),mean(cm,na.rm=TRUE),
           mean(dm,na.rm=TRUE),mean(em,na.rm=TRUE),mean(fm,na.rm=TRUE)))

Female <- (c(mean(af,na.rm=TRUE),mean(bf,na.rm=TRUE),mean(cf,na.rm=TRUE),
             mean(df,na.rm=TRUE),mean(ef,na.rm=TRUE),mean(ff,na.rm=TRUE)))
names2<-c("Male","Female")
PValueMatrix <- as.data.frame(rbind(Male,Female))
PValueMatrix <- as.data.frame(cbind(names2,PValueMatrix))
colnames(PValueMatrix)<-c("Sex","Atractiveness","Sincerity","Intelligence","Funny","Ambition",
                          "Shared Interests")
PValueMatrix.long<-melt(PValueMatrix,id.vars="Sex")
PValueMatrix.long

ggplot(PValueMatrix.long,aes(x=variable,y=value,fill=factor(Sex)))+
    geom_bar(stat="identity",position="dodge")+
    scale_fill_discrete(name="Gender",
                        breaks=c(1, 2),
                        labels=c("Male", "Female"))+
    xlab("")+ylab("Points distribution")+geom_hline(yintercept=100/6, color = "blue",size=1)
#Barplot of the distribution of what you value in the others


#Radarplot of what you really rate to the others

gender<-data[,3]

PRates<-cbind(gender,data[,62:66])#45 shared

PRates<-cbind(gender,data[,62:66])#45 shared
pr <- split(PRates, PRates$gender)
pr.females1 <- as.data.frame(pr[1])
pr.males1 <- as.data.frame(pr[2])
pr.females1 <- pr.females1[,2:6]
pr.males1 <- pr.males1[,2:6]

am1<-as.numeric(pr.males1$male.attractive_partner)
bm1<-as.numeric(pr.males1$male.sincere_partner)
cm1<-as.numeric(pr.males1$male.intelligence_partner)
dm1<-as.numeric(pr.males1$male.funny_partner)
em1<-as.numeric(pr.males1$male.sincere_partner)

af1<-as.numeric(pr.females1$female.attractive_partner)
bf1<-as.numeric(pr.females1$female.sincere_partner)
cf1<-as.numeric(pr.females1$female.intelligence_partner)
df1<-as.numeric(pr.females1$female.funny_partner)
ef1<-as.numeric(pr.females1$female.sincere_partner)

Male1 <- (c(mean(am1,na.rm=TRUE),mean(bm1,na.rm=TRUE),mean(cm1,na.rm=TRUE),
           mean(dm1,na.rm=TRUE),mean(em1,na.rm=TRUE)))

Female1 <- (c(mean(af1,na.rm=TRUE),mean(bf1,na.rm=TRUE),mean(cf1,na.rm=TRUE),
             mean(df1,na.rm=TRUE),mean(ef1,na.rm=TRUE)))

cents1<-c(9,9,9,9,9)
zeros1<-c(6,6,6,6,6)


PValueMatrix1 <- as.data.frame(rbind(cents1,zeros1,Male1,Female1))
colnames(PValueMatrix1)<-c("Atractive","Sin.","Intelligence","Funny","Amb.")
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4))

radarchart( PValueMatrix1 , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="black", cglty=1, axislabcol="black", caxislabels=seq(0,10,2.5), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)

legend(x=0.7, y=-0.05, legend = c("Male","Female"), bty = "n", pch=20 , col=colors_in ,
       text.col = "black", cex=1.2, pt.cex=3)

#Radarplot of what you really rate to the others

#Radarplot of what you rate yourself
RYou<-cbind(gender,data[,52:56])

gender<-data[,3]

PYou<-cbind(gender,data[,52:56])

PYou<-cbind(gender,data[,52:56])
py <- split(RYou, RYou$gender)
py.females2 <- as.data.frame(py[1])
py.males2 <- as.data.frame(py[2])

am2<-as.numeric(py.males2$male.attractive)
bm2<-as.numeric(py.males2$male.sincere)
cm2<-as.numeric(py.males2$male.intelligence)
dm2<-as.numeric(py.males2$male.funny)
em2<-as.numeric(py.males2$male.sincere)

af2<-as.numeric(py.females2$female.attractive)
bf2<-as.numeric(py.females2$female.sincere)
cf2<-as.numeric(py.females2$female.intelligence)
df2<-as.numeric(py.females2$female.funny)
ef2<-as.numeric(py.females2$female.sincere)

Male2 <- (c(mean(am2,na.rm=TRUE),mean(bm2,na.rm=TRUE),mean(cm2,na.rm=TRUE),
            mean(dm2,na.rm=TRUE),mean(em2,na.rm=TRUE)))

Female2 <- (c(mean(af2,na.rm=TRUE),mean(bf2,na.rm=TRUE),mean(cf2,na.rm=TRUE),
              mean(df2,na.rm=TRUE),mean(ef2,na.rm=TRUE)))

cents1<-c(9,9,9,9,9)
zeros1<-c(6,6,6,6,6)


PValueMatrix2 <- as.data.frame(rbind(cents1,zeros1,Male2,Female2))
colnames(PValueMatrix2)<-c("Atractive","Sin.","Intelligence","Funny","Amb.")
PValueMatrix2
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4))

radarchart( PValueMatrix2 , axistype=1 , title="Rate yourself",
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="black", cglty=1, axislabcol="black", caxislabels=seq(0,10,2.5), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)

legend(x=0.7, y=-0.05, legend = c("Male","Female"), bty = "n", pch=20 , col=colors_in ,
       text.col = "black", cex=1.2, pt.cex=3)
#Radarplot of what you rate yourself

#Merge last 2 spider graphs

par(mfrow=c(1,2))

cents1<-c(9,9,9,9,9)
zeros1<-c(6,6,6,6,6)

radarchart( PValueMatrix1 , axistype=1 ,
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="black", cglty=1, axislabcol="black", caxislabels=seq(6,9,0.75), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)

legend(x=0.7, y=-0.05, legend = c("M","F"), bty = "n", pch=20 , col=colors_in ,
       text.col = "black", cex=1.2, pt.cex=3)
text(0,-1.2,c("(a)"))
radarchart( PValueMatrix2 , axistype=1 ,
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="black", cglty=1, axislabcol="black", caxislabels=seq(6,9,0.75), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)

legend(x=0.7, y=-0.05, legend = c("M","F"), bty = "n", pch=20 , col=colors_in ,
       text.col = "black", cex=1.2, pt.cex=3)

text(0,-1.2,c("(b)"))
#Merge last 2 spider graphs


