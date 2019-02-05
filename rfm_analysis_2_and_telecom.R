library(dplyr)
library(ggplot2)

rfmtxndata=read.csv("rfmtxn.csv",header=TRUE,sep=",",na.strings = c("","NA"),strip.white = TRUE)
rfmtxndata <- rfmtxndata %>% mutate(txndate = as.Date(Date,"%m/%d/%Y"))

rfmdata <- rfmtxndata %>% group_by(ID) %>% summarise(txncount = n(), recentdate = max(txndate),amount=sum(Amount))

#convert date filled so that it can be binned into quantiles
rfmdata <- rfmdata %>% mutate(recent_wt=as.numeric(as.Date(rfmdata$recentdate)-as.Date(min(rfmdata$recentdate))))

rfm.Pct20.Breaks.recency=as.vector(quantile(rfmdata$recent_wt, probs=seq(0,1,0.2), na.rm=TRUE))
rfm.Pct20.Breaks.recency
rfm.Pct20.Breaks.frequency=as.vector(quantile(rfmdata$txncount, probs=seq(0,1,0.2), na.rm=TRUE))
rfm.Pct20.Breaks.frequency
rfm.Pct20.Breaks.monetary=as.vector(quantile(rfmdata$amount, probs=seq(0,1,0.2), na.rm=TRUE))
rfm.Pct20.Breaks.monetary

rfmdata <- rfmdata %>% mutate(recency = ifelse(recent_wt >= 862,5,
ifelse(recent_wt >= 796 & recent_wt < 862 ,4,
ifelse(recent_wt >= 679.6 & recent_wt < 796,3,
ifelse(recent_wt >= 495 & recent_wt < 679.6,2,
ifelse(recent_wt < 495,1,NA
))))))
barplot(table(rfmdata$recency), main="Recency")

rfmdata <- rfmdata %>% mutate(frequency = ifelse(txncount >= 7,5,
ifelse(txncount >= 5.5 & txncount < 7,4,
ifelse(txncount >= 5 & txncount < 5.5,3,
ifelse(txncount >= 3.1 & txncount < 5,2,
ifelse(txncount < 3.1,1,NA
))))))
barplot(table(rfmdata$frequency), main="Frequency")

rfmdata <- rfmdata %>% mutate(monetary = ifelse(amount >= 665,5,
ifelse(amount >= 505.4 & amount < 665,4,
ifelse(amount >= 381 & amount < 505.4,3,
ifelse(amount >= 254.8 & amount < 381,2,
ifelse(amount < 254.8,1,NA
))))))
barplot(table(rfmdata$monetary), main="Monetary")

ggplot(rfmdata,mapping=aes(x=as.factor(recency))) +geom_bar() + facet_grid(as.factor(monetary) ~ as.factor(frequency), labeller = label_both)

#frequency and monetary go together in direction and in case of high F,M the value of R is skewed to left (more number high value R) and vice-versa which means most high valued, frequent customers are more #recent ones and #the older customers are low value, low frequency

#cluster analysis on the RFM scores>>>>>>>>>>>

rfmdata.for.cluster=rfmdata[,c(6,7,8)]

k.max <- 15
wss <- sapply(1:k.max,function(k){kmeans(rfmdata.for.cluster, k, nstart=50,iter.max = 15)$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

library(reshape)
km.rfm <- kmeans(rfmdata.for.cluster,5, nstart =50, iter.max = 500, algorithm = "Lloyd")
km.rfm.output <- km.rfm$centers
km.rfm.output<-as.data.frame(km.rfm.output)
km.rfm.output<-cbind(km.rfm.output,c(1,2,3,4,5))
km.rfm.output<- rename(km.rfm.output,c(`c(1, 2, 3, 4, 5)`="cluster"))

km.rfm.output.melt<- melt(km.rfm.output, id=(c("cluster")))
ggplot(km.rfm.output.melt)+geom_line(aes(x=cluster,y=value, colour=variable))


#experiment with varrying number of clusters ---- a clear picture emerges at 5 or 6 cluster solution; experiment with various numbers - 4, 5, 6, 7,8,9,10

#Tree based analysis >>>>>>>>>>>>>>>>>

rfmdata <- rfmdata %>% mutate(cluster=km.rfm$cluster)
library(rpart)
rfm.tree <- rpart(as.factor(cluster)~as.factor(recency)+as.factor(frequency)+as.factor(monetary), data=rfmdata)
rfm.tree

library(partykit)
rfm.tree <- as.party(rfm.tree)
plot.modelparty(rfm.tree)

#Rules based analysis >>>>>>>>>>>>>>>>>>

library(arules)
library(arulesViz)

rfm.rules.data<- rfmdata[,c(6:9)]

rfm.rules.data<- rfm.rules.data %>% mutate(r=ifelse(recency==1,"r1",ifelse(recency==2,"r2",ifelse(recency==3,"r3",ifelse(recency==4,"r4",ifelse(recency==5,"r5",NA))))))
rfm.rules.data<- rfm.rules.data %>% mutate(f=ifelse(frequency==1,"f1",ifelse(frequency==2,"f2",ifelse(frequency==3,"f3",ifelse(frequency==4,"f4",ifelse(frequency==5,"f5",NA))))))
rfm.rules.data<- rfm.rules.data %>% mutate(m=ifelse(monetary==1,"m1",ifelse(monetary==2,"m2",ifelse(monetary==3,"m3",ifelse(monetary==4,"m4",ifelse(monetary==5,"m5",NA))))))
rfm.rules.data<- rfm.rules.data %>% mutate(c=ifelse(cluster==1,"c1",ifelse(cluster==2,"c2",ifelse(cluster==3,"c3",ifelse(cluster==4,"c4",ifelse(cluster==5,"c5",ifelse(cluster==6,"c6",NA)))))))
rfm.rules.data<-rfm.rules.data[,c(5:8)]

rfm.rules.data$r<-as.factor(rfm.rules.data$r)
rfm.rules.data$f<-as.factor(rfm.rules.data$f)
rfm.rules.data$m<-as.factor(rfm.rules.data$m)
rfm.rules.data$c<-as.factor(rfm.rules.data$c)

rules <- apriori(rfm.rules.data, parameter = list(supp = 0.001, conf = 0.8,maxlen=3))
summary(rules)
rules<-sort(rules, by="confidence", decreasing=TRUE)
inspect(rules[1:20])

rules<-apriori(data=rfm.rules.data, parameter=list(supp=0.01,conf = 0.15), appearance = list(default="rhs",lhs="c=c1"), control = list(verbose=F))
plot(rules,method="graph",interactive=TRUE,shading=NA)

#-------------------------------->>>>>>>>>>>>>>>>>===============<<<<<<<<<<<<<<<<<<<<<------------------------
#-------------------------------->>>>>>>>>>>>>>>>>===============<<<<<<<<<<<<<<<<<<<<<------------------------

#running ANN with TELECOM data for classification

telecomdata=read.csv("telecom_classify.csv",header=TRUE,sep=",",na.strings = c("","NA"),strip.white = TRUE)

library(RSNNS)
telecomdata$marital.f <- factor(telecomdata$marital)
telecomdata$retire.f <- factor(telecomdata$retire)
telecomdata$gender.f <- factor(telecomdata$gender)
telecomdata$ed.f <- factor(telecomdata$ed, ordered=TRUE)
telecomdata$age.s <- normalizeData(telecomdata$age, type = "0_1")
telecomdata$address.s <- normalizeData(telecomdata$address, type = "0_1")
telecomdata$income.s <- normalizeData(telecomdata$income, type = "0_1")
telecomdata$employ.s <- normalizeData(telecomdata$employ, type = "0_1")
telecomdata$reside.s <- normalizeData(telecomdata$reside, type = "0_1")
telecomdata$custcat.f <- factor(telecomdata$custcat)

library(nnet)

set.seed(1234)
train=sample(1:nrow(telecomdata),700)
telecomdata.test=telecomdata[-train,]
telecomdata.train=telecomdata[train,]

telecom_X_train <- telecomdata.train %>% select(marital.f,retire.f,gender.f,ed.f,age.s,address.s,income.s,employ.s,reside.s)
telecom_Y_train <- telecomdata.train[,c(20)]
telecom_train <- cbind(telecom_X_train,telecom_Y_train)
telecom_X_test <- telecomdata.test %>% select(marital.f,retire.f,gender.f,ed.f,age.s,address.s,income.s,employ.s,reside.s)
telecom_Y_test <- telecomdata.test[,c(20)]

set.seed(1234)
telecom.ann <- nnet(telecom_Y_train ~ marital.f + retire.f +gender.f +ed.f+age.s+address.s+income.s+employ.s+reside.s, data=telecom_train, size = 9, decay =0.1, linout = FALSE, maxit =1000)
telecom.ann
telecom.test.pred <- predict(telecom.ann,newdata=telecom_X_test, type = "class")
telecom.test.probs <-predict(telecom.ann,newdata=telecom_X_test)

table(telecom.test.pred,telecom_Y_test)

library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
 
#plot each model
plot.nnet(telecom.ann)

telecom.test.probs <- as.data.frame(telecom.test.probs)
telecom.test.probs <- cbind(telecom.test.probs, telecom_Y_test)

head(telecom.test.probs)
md <- melt(telecom.test.probs, id=(c("telecom_Y_test")))
ggplot(data = md, mapping = aes(x= telecom_Y_test, y = value, fill=variable)) +  geom_boxplot(mapping = aes(group = interaction(variable,telecom_Y_test)))





