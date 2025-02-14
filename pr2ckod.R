piwo <- read.csv("piwo.csv", row.names=1, sep=";")
piwo

#U�ywane biblioteki
install.packages("clusterSim")
library(clusterSim)
install.packages("ggpubr")
install.packages("factoextra")
library(factoextra)
library(ggpubr)

summary(piwo)

#1. Wyb�r obiekt�w i zmiennych
wspzm<- c() #Obliczenie wsp�czynnik�w zmienno�ci
for (i in 1:5){
  wspzm[i]<- sd(piwo[,i]/mean(piwo[,i]))
}
wspzm
round(cor(piwo), 3) #Korelacje

#2. Wyb�r formu�y normalizacji zmiennych
piwo_st<- data.Normalization(piwo, type="n1") #n1 -standardization ((x-mean)/sd) 
round(piwo_st, 3)

#3. Miara odleg�o�ci
odl1 <- dist(piwo_st, method = "euclidean")
odl2 <- dist(piwo_st, method = "manhattan")
odl3 <- dist(piwo_st, method= "minkowski")

#4. Metoda klasyfikacji
#Grupowanie podzia�owe
#kmeans
ksrednich4<- kmeans(piwo_st, 4, nstart=10) #4grupy
ksrednich4
str(ksrednich4)
ksrednich4$centers #zapisane wyniki
cbind(ksrednich4$cluster)
#pam
pam4<- pam(piwo_st, 4)#4grupy
pam4
str(pam4)
pam4$medoids #zapisane wyniki
cbind(pam4$clustering)

#Wykresy
fviz_cluster(ksrednich4, data = piwo,
             palette = c("#6F70FC", "#B57DDA", "#F075C1","#FCAC65"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
fviz_cluster(pam4, data = piwo,
             palette = c("#6F70FC", "#B57DDA", "#F075C1","#FCAC65"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
fviz_cluster(clara4, data = piwo,
             palette = c("#6F70FC", "#B57DDA", "#F075C1","#FCAC65"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

#Metody hierarchiczne
gr_ward1<-hclust(odl1, method="ward.D2")
gr_ward2<-hclust(odl2, method="ward.D2")
gr_ward3<-hclust(odl3, method="ward.D2")
plot(gr_ward1, main="Metoda Warda, metryka euk")
plot(gr_ward2, main="Metoda Warda, metryka manhattan")
plot(gr_ward3, main="Metoda Warda, metryka minko")
#hclust average - metoda �redniej
#hclust complete - najdalszego s�siada
#hclust single - najbli�szego s�siada

#5. Ustalenie liczby klas
ward4<-cutree(gr_ward1, 4) 
ward4
ward5<-cutree(gr_ward1, 5) 
ward5
ward6<-cutree(gr_ward1, 6) 
ward6
s<-c()
for (i in 1:8){
  podz = cutree(gr_ward1, k=i)
  s[i]<-index.S(odl1, podz)
}
s
#wybieram podzia� na 4 klasy
piwo <- read.csv("piwo.csv", row.names=1, sep=";")
piwo
klasy <- cutree(gr_ward1, k=4)
klasy
cbind(klasy)
opis <- cluster.Description(piwo, klasy)
print(opis)
