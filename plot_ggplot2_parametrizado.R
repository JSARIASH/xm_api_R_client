# gráficas paper
# Number of publication
#install.packages("extrafont")
# remotes::install_github('rensa/ggflags')
#install.packages("ggflags")

# Flags code:
# https://github.com/jimjam-slam/ggflags

library(ggflags)
library(extrafont)
library(ggplot2)
a1<- as.character(c(2009:2010,2012:2022))
a1[1]<-"<=2009"
datosyear <- data.frame(year=a1, np=c(13,1,2,4,4,4,7,11,11,12,13,28,27))
g <- ggplot(data = datosyear, aes(x = factor(year), y=np))+ geom_bar(stat = "identity",  fill= "gray")+
  xlab(label = "Year of publication") + ylab(label = "Number of publication") + 
  theme_light()+
  scale_y_continuous(breaks = seq(5, 30, by =5), expand = c(0, 0.2)) +
  scale_x_discrete(labels = c("\u2264 2009", 2010, 2012:2022)) +
  theme(text = element_text(family = "Times New Roman", size = 8))
g
ggsave("/tmp/test.jpg", g, width=1380, height=900, units="px", dpi=350)




library(extrafont)
library(ggplot2)
a2<- c("Torres, Carlos A.","Lang, Daniel W.","Deem, Rosemary","Atalar, Abdullah",
       "Bellei, Cristián ","Wetherbe, James C","García Fanelli, Ana","Jongbloed, Ben",
       "Rama, Claudio","Espinoza, Óscar", "Ashwin, Paul", "Liefner, Ingo", "McCormick, Alexander C.","Harrison, Reema",
       "Tandberg, David A.","Didou Aupetit, Sylvie Andree", "Rahman Ahmad, Abd", "García Guadilla, Carmen", "Jacob, W. James",
       "Melo-Becerra, Ligia Alba","Frølich, Nicoline","Cantwell, Brendan","Kelchen, Robert",
       "Guzmán-Valenzuela, Carolina","Garritzmann, Julian L.","Miller, Kristel",
       "Alshubiri, Faris Nasif","Chambers, David","Zatonatska","Smolentseva, Anna","Bolli, Thomas", "Iregui-Bohórquez, Ana María",
       "Britton, Jack","Li, Amy Y.","Almagtomea, Akeel","Delaney, Jennifer A.","Medina, Pablo") 
pais <-c("us","de","gb","tr","cl","us","ar","nl","uy","cl","gb","de","us","au","us","mx","my","ve","us","co","no","us","us","cl","de","ie","om","gb","ua","gb","ch","co","gb","us","iq","us","es")
citas <- data.frame(
  author=a2, cite= c(68,50,50,45,41,39,37,35,33,33,31,29,29,29,28,26,25,23,23,22,22,22,22,20,19,19,18,18,18,18,17,17,16,15,15,15,15), 
  country=pais)
b <- ggplot(data = citas, aes(x = reorder(author,cite), y= cite))+ geom_bar(stat= "identity", fill= "gray")+
  ylab(label = "H-Index by author") + xlab(label = "Full name first author") + theme_light()+
  scale_y_continuous(expand=c(0,0.1)) +
  theme(text = element_text(family = "Times New Roman", size = 8),
        axis.text.x = element_text(angle = 90)
        )
g2 <-  b + geom_flag(aes(x=author,y= cite - cite - 1 ,country=pais))


ggsave("/tmp/hindex.jpg", g2, width = 3500, height = 2300, units = "px", dpi = 400)














