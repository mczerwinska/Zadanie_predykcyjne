# Marta Czerwiñska
# Zadanie predykcyjne

##### START #######

library("zoo")
library("MASS")
library("rpart")
library("klaR")
library("ROCR")
library("randomForest") 
library("e1071")
library("glmnet")
library("class")
library("adabag")
library("sqldf")


setwd("C:\\Users\\Marta\\Desktop\\zadanie predykcyjne")

Train_churn <- read.csv("churn.csv", header=TRUE, sep=";")
head(Train_churn)
dim(Train_churn) # 1666 21
attach(Train_churn)

Test_churn <- read.csv("churn_wdrozenie.csv", header=TRUE, sep=";")
head(Test_churn)
dim(Test_churn)

# Zmienne objaœniane:
# Churn. -> kto odszed³

barplot(table(Churn.), col=c("red","blue"))
table(Train_churn$Churn.)/length(Churn.) # 0.1470588 => odeszlo 14,7% klientóW

# 0.8529412/0.1470588 = 5.800001
# Zosta³o blisko 6 razy wiêcej osób ni¿ odesz³o
# Nadamy wagi

my_hist <- function(zmienna, ...){
  zmienna <- as.numeric(zmienna)
  h <- hist(zmienna, col="red", main=NA, xlab=NA, ...)
  hist(zmienna[which(Churn.=='False.')], breaks=length(h$breaks), col="blue", add=TRUE)
  title(main="0 - red, 1 - blue")
}


cor_NA <- function(x,y){
  stopifnot(length(x)==length(y))
  x <- as.numeric(x)
  y <- as.numeric(y)
  
  wektory <- data.frame(numeric(0), numeric(0))
  
  for(i in 1:length(x)){
    if(!is.na(x[i]) && !is.na(y[i])){
      wektory <- rbind(wektory, data.frame(x[i], y[i]))
    }
  }
  
  return(as.numeric(cor(wektory[,1],wektory[2])))
}


# Korelacje ze zmienn¹ binarn¹ Churn.:

cor_Churn <- numeric(ncol(Train_churn))
for(j in 1:ncol(Train_churn)){
  cor_Churn[j] <- cor_NA(Train_churn[,j], Churn.)
}
# Korelacje zmiennych ze zmienn¹ Churn 
cor_Churn 

# Liczba braków:

braki <- numeric(ncol(Train_churn))
for(j in 1:ncol(Train_churn)){
  if(is.factor(Train_churn[,j])){
    braki[j] <- length(which(Train_churn[,j]==" "))
  }else if(is.numeric(Train_churn[,j])){
    braki[j] <- sum(is.na(Train_churn[,j]))
  }
}


# Nie ma pustych pól

high_cor <- which(cor_Churn>0.01)

braki[high_cor]


##### ZMIENNE #########

Train_org <- Train_churn
Train_churn <-Train_org[,c(2, 4, 5, 8, 9, 10, 11, 12, 13, 14, 16, 17, 19, 20, 21)]


pre_proc <- function(zbior){
  stopifnot(is.data.frame(zbior))
  n <- nrow(zbior) # l.wierszy
  m <- ncol(zbior) # l.kolumn
  
  which_factor <- numeric(m)
  
  for(j in 1:m){
    if(is.factor(zbior[,j])){
      which_factor[j] <- 1
      uzupelnienie <- names(sort(table(zbior[,j][which(zbior[,j]!=" ")]), decreasing=TRUE)[1])
      
      for(i in 1:n){
        if(zbior[i,j]==" ") zbior[i,j] <- uzupelnienie
      }
      
      zbior[,j] <- as.numeric(zbior[,j])
      
    }else if(is.numeric(zbior[,j])){
      med <- median(zbior[,j][which(!is.na(zbior[,j]))])
      zbior[,j][which(is.na(zbior[,j]))] <- med
    }
  }
  
  wyniki <- vector("list", 2)
  wyniki[[1]] <- zbior
  wyniki[[2]] <- which_factor
  return(wyniki)
}

T_churn <-Train_churn

l_faktorow <- length(which(which_factor==1))
l_poziomow <- numeric(l_faktorow)
i <- 0

for(j in 1:ncol(Train_churn)){
  if(which_factor[j]==1){
    i <- i+1
    l_poziomow[i] <- length(levels(as.factor(Train_churn[,j])))
  }
}


# Ponownie badamy korelacjê:

cor_churn_2 <- numeric(ncol(Train_churn))
for(j in 1:ncol(Train_churn)){
  cor_churn_2[j] <- cor_NA(Train_churn[,j], Churn.)
}

ktore <- which(cor_churn_2>0.02)
T2_churn <- Train_churn
Train_churn <- Train_churn[,ktore]

#### PREDYKCJA ####

# Dzielimy próbkê Train na treningowa oraz testowa w sposob losowy

n <- sample(1:1666,800)
T1 <- Train_churn[n,]
T2 <- Train_churn[-n,]

colnames(T1)

###

### LDA ###

bank.lda <- lda(Churn.~., data=T1) 
bank.lda.pred <- predict(bank.lda,newdata=T2)
Tab.lda <- table(T2$Churn.,bank.lda.pred$class)
dokladnosc.lda <- sum(diag(Tab.lda))/sum(Tab.lda) 
czulosc.lda<-Tab.lda[1,1]/(Tab.lda[1,1]+Tab.lda[1,2])  
precyzja.lda <- Tab.lda[1,1]/(Tab.lda[1,1]+Tab.lda[2,1])


# dokladnosc.lda 0.8498845
# czulosc.lda 0.9647696
# precyzja.lda  0.872549

### GLM ###

# pred <-predict(model, Test_churn, type="response")
# pred_1 <- pred[which(pred>=0.5)] 
# Tab.glm <- table(Test_churn$y,pred)
# pred <- data.frame(as.numeric(names(pred)))
# 
# 1-sum(diag(table(Churn., pred)))/sum(table(Churn., pred)) # 0.1482593

bank.glm <- glm(Churn.~.,data=T1,family="binomial")
bank.pred.glm <- predict(bank.glm,type="response",newdata=T2)
Predi <- ifelse(bank.pred.glm>0.5,1,0)
Pop <- ifelse(Predi==T2$Churn.[i],1,0)
Tab.glm <- table(T2$Churn.,Predi)

dokladnosc.glm <- sum(diag(Tab.glm))/sum(Tab.glm) 
czulosc.glm <-Tab.glm[1,1]/(Tab.glm[1,1]+Tab.glm[1,2])  
precyzja.glm <- Tab.glm[1,1]/(Tab.glm[1,1]+Tab.glm[2,1])

# dokladnosc.glm 0.852194
# czulosc.glm 0.9769648
# precyzja.glm  0.8665865


### NAIWNY BAYES ###

naiwny_bayes <- naiveBayes(Churn.~.,data=T1)
naiwny_bayes.pred <- predict(naiwny_bayes,newdata=T2, type="raw")
bayes.pred <- naiwny_bayes.pred[,2]>0.5

Tab.bayes <- table(T2$Churn.,bayes.pred)
dokladnosc.bayes <- sum(diag(Tab.bayes))/sum(Tab.bayes) 
czulosc.bayes<-Tab.bayes[1,1]/(Tab.bayes[1,1]+Tab.bayes[1,2])  
precyzja.bayes <- Tab.bayes[1,1]/(Tab.bayes[1,1]+Tab.bayes[2,1])

# dokladnosc.bayes 0.8741339
# czulosc.bayes 0.9539295
# precyzja.bayes  0.9037227

# Uzupe³nienie luk
# naiwny_bayes <- naiveBayes(Churn.~.,data=Train_churn)
# naiwny_bayes.pred <- predict(naiwny_bayes,newdata=Test_churn, type="raw")
# bayes.pred <- naiwny_bayes.pred[,2]>0.5
# Test_bay <- Test_churn
# Test_bay$Churn.<- bayes.pred
# 
# table(bayes.pred)

### DRZEWA DECYZYJNE ###

bank.tree <- rpart(Churn.~., data=T1) 
bank.tree.pred <- predict(bank.tree,newdata=T2, type="class")

Tab.tree <- table(T2$Churn.,bank.tree.pred)
dokladnosc.tree <- sum(diag(Tab.tree))/sum(Tab.tree) 
czulosc.tree<-Tab.tree[1,1]/(Tab.tree[1,1]+Tab.tree[1,2])  
precyzja.tree <- Tab.tree[1,1]/(Tab.tree[1,1]+Tab.tree[2,1])

# dokladnosc.tree 0.8718245
# czulosc.tree 0.9241192
# precyzja.tree  0.9253731

table(bank.tree.pred)


### BAGGING ###

bank.bagg <- bagging(Churn.~.,data=T1,mfinal=25)
bank.bagg.pred <- predict.bagging(bank.bagg, newdata=T2)

Tab.bagg <- table(T2$Churn.,bank.bagg.pred$class)
dokladnosc.bagg <- sum(diag(Tab.bagg))/sum(Tab.bagg) 
czulosc.bagg <-Tab.bagg[1,1]/(Tab.bagg[1,1]+Tab.bagg[1,2])  
precyzja.bagg <- Tab.bagg[1,1]/(Tab.bagg[1,1]+Tab.bagg[2,1])

# dokladnosc.bagg 0.8972286
# czulosc.bagg 0.9607046
# precyzja.bagg  0.9219766

# Uzupe³nienie luk
bank.bagg <- bagging(Churn.~.,data=Train_churn,mfinal=25)
bank.bagg.pred <- predict.bagging(bank.bagg, newdata=Test_churn)

Test_bagg <- Test_churn
Test_bagg$Churn.<- bank.bagg.pred$class

### LASY LOSOWE ###

bank.forest <- randomForest(Churn.~., data=T1) 
bank.forest.pred <- predict(bank.forest,newdata=T2)
Tab.forest <- table(T2$Churn.,bank.forest.pred)

dokladnosc.forest <- sum(diag(Tab.forest))/sum(Tab.forest) 
czulosc.forest <-Tab.forest[1,1]/(Tab.forest[1,1]+Tab.forest[1,2])  
precyzja.forest <- Tab.forest[1,1]/(Tab.forest[1,1]+Tab.forest[2,1])

# dokladnosc.forest 0.9099307
# czulosc.forest 0.9647696
# precyzja.forest  0.9319372

# Lasy losowe daj¹ najwiêksz¹ dok³adnoœæ, maj¹ najwiêksz¹ czu³oœæ oraz precyzjê. Z tego te¿ powodu
# wybieram ten model do predykcji danych 


# Uzupe³nienie luk
bank.forest <- randomForest(Churn.~., data=Train_churn) 
bank.forest.pred <- predict(bank.forest,newdata=Test_churn)

Test_forest <- Test_churn
Test_forest$Churn.<- bank.forest.pred

table(bank.forest.pred)

churn_wdrozenie <- read.csv("churn_wdrozenie.csv", header=TRUE, sep=";")
churn_wdrozenie$Churn. <- bank.forest.pred

write.table(churn_wdrozenie,"churn_wdrozenie_Marta_Czerwinska.csv",row.names=FALSE)

#### END ####




