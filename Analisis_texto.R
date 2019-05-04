#####analisis de texto

#primero unas gráficas sencillas con lo que se codifico

library(readxl)
library(tidyverse)
library(tidytext)
library("ggplot2")
library(readxl)
library("FactoMineR")
library("factoextra")
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales) 

BF_CON_ETQ <- read_excel("BF_CON_ETQ.xlsx")

c2=c(BF_CON_ETQ$P7_COD2[!is.na(BF_CON_ETQ$P7_COD2)])

c3=c(BF_CON_ETQ$P7_COD3[!is.na(BF_CON_ETQ$P7_COD3)])

c1=c(BF_CON_ETQ$P7_COD1[!is.na(BF_CON_ETQ$P7_COD1)])


d =as.data.frame(c( c1,c2,c3))

dc<- d %>% count(`c(c1, c2, c3)`,sort=TRUE)

t=table(d)

#table(d)/length(BF_CON_ETQ$P7_COD1)
#barplot(t)
#barplot(head(sort(table(d)/length(BF_CON_ETQ$P7_COD1)
#,decreasing = T),10))

BD=as.data.frame(head(sort(table(d)/length(BF_CON_ETQ$P7_COD1)
          ,decreasing = T),10))

p<-ggplot(data=BD, aes(x=d, y=Freq,fill=d)) +
  geom_bar(stat="identity")+ theme(
    axis.text.x = element_blank())+
  guides(fill=guide_legend(title="Etiquetas de la pregunta"))+
  xlab("")+ylab("Frecuencias relativas")
p
#####################
##########################
#BF_CON_ETQ$
c1=NULL
c2=NULL
c3 =NULL
c2=c(BF_CON_ETQ$P14_COD2[!is.na(BF_CON_ETQ$P14_COD2)])

c3=c(BF_CON_ETQ$P14_COD3[!is.na(BF_CON_ETQ$P14_COD3)])

c1=c(BF_CON_ETQ$P14_COD1[!is.na(BF_CON_ETQ$P14_COD1)])


d =as.data.frame(c( c1,c2,c3))

BD=as.data.frame(head(sort(table(d)/length(BF_CON_ETQ$P7_COD1)
                           ,decreasing = T),10))

ggplot(data=BD, aes(x=d, y=Freq,fill=d)) +
  geom_bar(stat="identity")+ theme(
    axis.text.x = element_blank())+
  guides(fill=guide_legend(title="Etiquetas de la pregunta"))+
  xlab("")+ylab("Frecuencias relativas")


##########################
##########################
#BF_CON_ETQ$
c1=NULL
c2=NULL
c3 =NULL

c2=c(BF_CON_ETQ$P20_COD2[!is.na(BF_CON_ETQ$P20_COD2)])

c3=c(BF_CON_ETQ$P20_COD3[!is.na(BF_CON_ETQ$P20_COD3)])

c1=c(BF_CON_ETQ$P20_COD1[!is.na(BF_CON_ETQ$P20_COD1)])


d =as.data.frame(c( c1,c2,c3))

BD=as.data.frame(head(sort(table(d)/length(BF_CON_ETQ$P7_COD1)
                           ,decreasing = T),10))

ggplot(data=BD, aes(x=d, y=Freq,fill=d)) +
  geom_bar(stat="identity")+ theme(
    axis.text.x = element_blank())+
  guides(fill=guide_legend(title="Etiquetas de la pregunta"))+
  xlab("")+ylab("Frecuencias relativas")


#########

c1=NULL
c2=NULL
c3 =NULL

c2=c(BF_CON_ETQ$P26_COD2[!is.na(BF_CON_ETQ$P26_COD2)])

c3=c(BF_CON_ETQ$P26_COD3[!is.na(BF_CON_ETQ$P26_COD3)])

c1=c(BF_CON_ETQ$P26_COD1[!is.na(BF_CON_ETQ$P26_COD1)])


d =as.data.frame(c( c1,c2,c3))

BD=as.data.frame(head(sort(table(d)/length(BF_CON_ETQ$P7_COD1)
                           ,decreasing = T),10))

ggplot(data=BD, aes(x=d, y=Freq,fill=d)) +
  geom_bar(stat="identity")+ theme(
    axis.text.x = element_blank())+
  guides(fill=guide_legend(title="Etiquetas de la pregunta"))+
  xlab("")+ylab("Frecuencias relativas")



#########

c1=NULL
c2=NULL
c3 =NULL

c2=c(BF_CON_ETQ$P32_COD2[!is.na(BF_CON_ETQ$P32_COD2)])

c3=c(BF_CON_ETQ$P32_COD3[!is.na(BF_CON_ETQ$P32_COD3)])

c1=c(BF_CON_ETQ$P32_COD1[!is.na(BF_CON_ETQ$P32_COD1)])


d =as.data.frame(c( c1,c2,c3))

BD=as.data.frame(head(sort(table(d)/length(BF_CON_ETQ$P7_COD1)
                           ,decreasing = T),10))

ggplot(data=BD, aes(x=d, y=Freq,fill=d)) +
  geom_bar(stat="identity")+ theme(
    axis.text.x = element_blank())+
  guides(fill=guide_legend(title="Etiquetas de la pregunta"))+
  xlab("")+ylab("Frecuencias relativas")


