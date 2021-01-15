rm(list = ls())
library(readxl) # подключение библиотеки работы с MS Excel
Radius <- read_excel(file.choose()) # чтение набора данных из файла формата Excel

library(DMwR)# подключение библиотеки обработки пропущенных значений
library(zoo) # подключение библиотеки для векторных операций
# проверка отсутствующих значений
rrr<-rowSums(is.na(Radius))
if (manyNAs(Radius, 0.2))
    {
  # удаление   записей, в которых число пропущенных измерений превышает 20%
        Radius <- Radius[-manyNAs(Radius, 0.2), ] 
}
if (any(is.na(Radius))==TRUE)
{ 

# Замена отсутствующих одиночных измерений матожиданием прогона
Radius <- na.aggregate(Radius)
}
coef <- cor(Radius) # оценка коэффициентов корреляции

kl <- (nrow(Radius)-1)*sum(apply(Radius,2,var))
for (i in 2:15) kl[i] <- sum(kmeans(Radius, centers=i)$withinss)
plot(1:15, kl, type="b", xlab="Число кластеров", ylab="Сумма квадратов расстояний внутри кластеров") 

kc<-kmeans(Radius,2) # кластеризация 
meanAgg<-aggregate(Radius,by=list(kc$cluster),FUN=mean) # покластерные средние значениях радиусов 
library(cluster)
clusplot(Radius, kc$cluster, color=TRUE, shade=TRUE, labels=2, lines=0, main = "Clastering",
                 xlab="Principal component 1", ylab="Principal Component 2")
#library(xlsx)
#write.xlsx(Radius, fpath+1)
