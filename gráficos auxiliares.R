#######
#haciendo una linea de tendencia de los cursos
#librerias necesearias.
library("ggplot2")
library(readxl)
library("FactoMineR")
library("factoextra")
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales) 

ae611 <- read_excel("A6-11_v2.xlsx")
#View(Autoevaluacion_6_11)
#ncol(ae611)
pre <- names(ae611)
#names(ae611) <-c("mt" ,"A",	"N"	,"S","p1",	"p2",	"p3",	"p4",	"p5",	"p6",	"ap7",	"p8",	"p9", "p10",	"p11",	"p12",	"p13",	"ap14",	"p15",	"p16",	"p17",	"p18",	"p19",	"ap20",	"p21",	"p22",	"p23",	"p24",	"p25"	,"ap26"	,"p27"	,"p28",	"p29"	,"p30"	,"p31"	,"ap32")

#length(c("mt" ,"A",	"N"	,"S","eu",	"p1","p2",	"p3",	"p4",	"p5",	"p6",	"ap7",	"p8",	"p9", "p10",	"p11",	"p12",	"p13",	"ap14",	"p15",	"p16",	"p17",	"p18",	"p19",	"ap20",	"p21",	"p22",	"p23",	"p24",	"p25"	,"ap26"	,"p27"	,"p28",	"p29"	,"p30"	,"p31"	,"ap32"))

base=ae611
theme_set(theme_classic())


####
base2=base
names(base2) <-c("mt" ,"A",	"N"	,"S","p1",	"p2",	"p3",	"p4",	"p5",	"p6",	"ap7",	"p8",	"p9", "p10",	"p11",	"p12",	"p13",	"ap14",	"p15",	"p16",	"p17",	"p18",	"p19",	"ap20",	"p21",	"p22",	"p23",	"p24",	"p25"	,"ap26"	,"p27"	,"p28",	"p29"	,"p30"	,"p31"	,"ap32")

#abiertas
cerradas=c("p2",	"p3",	"p4",	"p5",	"p6",	"p8",	"p9", "p10",	"p11",	"p12",	"p13","p15",	"p16",	"p17",	"p18",	"p19","p21",	"p22",	"p23",	"p24",	"p25","p27"	,"p28",	"p29"	,"p30"	,"p31")

base2=base2[cerradas]
#cm <- MCA(data.frame(base2[,6:10],base2[,12:16]))
cm <- MCA(data.frame(base2))

#fviz_famd_var(cm, elipses=TRUE)

  plot(cm$eig[,2])

#length(unique(base$`Marca temporal`))
#por esta razón se puede usar la marca temporarl cómo indice

a=as.factor(base$`Identifico y reconozco mis fortalezas y dificultades.`)
b=as.factor(base$`Establezco relaciones cordiales con mis compaÃ±eros.`)
#c=
cor(cbind(a,b),method = "kendall", use="pairwise")
library(scatterplot3d)

scatterplot3d(cm$ind$coord[,1],cm$ind$coord[,2], cm$ind$coord[,3])

#install.packages(c("rgl", "car"))
library(car)
library(rgl)
scatter3d(cm$ind$coord[,1],cm$ind$coord[,2], cm$ind$coord[,3], point.col = "blue",  ellipsoid = TRUE,
          axis.col = c("black", "black", "black"), surface = FALSE)

scatter3d(cm$var$coord[,1],cm$var$coord[,2], cm$var$coord[,3], point.col = "blue",  ellipsoid = TRUE,
          axis.col = c("black", "black", "black"))
          #, group= as.factor(base$Seccion))


#gráfuico de los indivduos 
d=as.data.frame(cbind(cm$var$coord))
gg=ggplot(d,aes(x=d$`Dim 2`,y=d$`Dim 3`))+geom_text(aes(label=rownames(d)),hjust=0, vjust=0)
gg=gg+geom_abline(slope=0,intercept = 0)+geom_vline(xintercept=0)
gg
library(ggrepel)

gg1=ggplot(d,aes(x=d$`Dim 1`,y=d$`Dim 2`))

  gg1+geom_text_repel(aes(label = rownames(d)),
                     box.padding   = 0.35, 
                     point.padding = 0.5,
                     segment.color = 'grey50', cex= 2.2)
data(tips, package = "reshape2")
tips %>% 
  count(day) %>% 
  mutate(perc = n / nrow(tips)) -> tips2

library(plot3D)

#viendolo por curso
#
d2= as.data.frame(cbind(cm$ind$coord,base$curso))
ggplot(d2, aes(x=d2$`Dim 1`,y=d2$`Dim 2`,group=as.factor(d2$V6)))+geom_point()



  ggplot(base, aes(x=base$`Identifico y reconozco mis fortalezas y dificultades.` ,  group=base$curso)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill = "Escala") +
  facet_grid(~base$curso) +
  scale_y_continuous(labels = scales::percent)
  
  
  ggplot(base2, aes(p2, group = base$Seccion)) + 
    geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
    scale_y_continuous(labels=scales::percent) +
    ylab("relative frequencies") +
    facet_grid(~base$Seccion)
  
  ###una forma de hacerlo
  
  ggplot(base, aes(`Identifico y reconozco mis fortalezas y dificultades.`)) + 
    geom_bar(aes(y = (..count..)/sum(..count..), fill =as.factor(..x.. ))) + 
    scale_y_continuous(labels=scales::percent) +
    ylab("Frecuencias relativas")+ theme(legend.position='none')+
    geom_text(aes( label =paste( round(100*(..count..)/sum(..count..),2),"%"),size=10,y= (..count..)/sum(..count..) ), stat= "count", vjust = -.5)  
  
  
#### Que hacer
  
  
    ggplot(base, aes(`Identifico y reconozco mis fortalezas y dificultades.`, color = `Identifico y reconozco mis fortalezas y dificultades.`
                     , fill = `Identifico y reconozco mis fortalezas y dificultades.`
                  ,group=factor(curso)  )) + 
      geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
      scale_y_continuous(labels=scales::percent) +
      ylab("Frecuencias relativas")+ facet_grid(~curso)+
      geom_text(aes( label = round((..count..)/sum(..count..),2),y= (..count..)/sum(..count..) ), stat= "count", vjust = -.5)  
        
    
    
    #scale_y_continuous(labels = scales::percent)
  
    clusters <- hclust(dist(cm$ind$coord))
    plot(clusters)
    clusterCut <- cutree(clusters,5)
  
    d3=as.data.frame(cbind(cm$ind$coord, clusterCut))
    ggplot(d3, aes(x = d3$`Dim 1`, y=d3$`Dim 2`, color = factor(clusterCut)))+geom_point()
   
    scatter3d( d3$`Dim 1`,d3$`Dim 2`,d3$`Dim 3`, point.col = factor(clusterCut),  ellipsoid = TRUE,sphere.size=2,
              axis.col = c("black", "black", "black"), surface = FALSE)
    