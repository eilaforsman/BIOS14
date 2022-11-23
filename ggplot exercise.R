setwd("~/Documents/Documents/R/Statcourse/BIOS14")

rm(list=ls())
blossoms <- read.csv("blossoms.csv")
library(ggplot2)

#Lauras plot#####
ggplot(data=blossoms, aes(x=log(LBL), y=log(UBL))) +  #read in data
  geom_point(aes(color = pop, shape=pop),             #add points and specify grouping factor
             size = 5,                                #modify point size
             alpha = 0.5) +                           #make points transparent
  scale_shape_manual(values=c(23,22,21,20,19,18,17,16,15)) +   #manually specify which symbols to use for the groups
  scale_color_manual(values=c("red","blue","orange",           #manually specify colors for the groups   
                              "yellow","purple","black",
                              "#f1eef6","#d0d1e6","#a6bddb")) +
  labs(y="Upper brace length",                        #define axes labels
       x="Lower bract length (log mm)") +
  scale_x_continuous(limits = c(2.2 , 3.5))+          #specify length of x-axis
  scale_y_continuous(limits = c(2.2 , 3.5))+          #specify length of y-axis 
  theme_gray() +                                      #specify the overall aesthetic of the plot
  theme(aspect.ratio=0.5,                             #modify the ratio of x- and y-axis
        text = element_text(size=14),                 #modify size of axis label text
        legend.text = element_text(size=3))  +        #modify size of legend text
  labs(shape="Population", color="Population") +      #modify legend title
  geom_smooth(method='lm',                            #add a regression line
              se=TRUE,                                #add or remove the standard error
              color = "lightpink") +                  #modify the color of the regression line
  geom_segment(x=1,y=1,xend=30,yend=30,               #manually add a line to the plot 
               linetype="dashed",                     #modify line type
               size=1)                                #modify line width
#facet_grid(cols=vars(pop))                         #splits up graphs by a grouping variable


#My edit####
ggplot(data=blossoms, aes(x=log(LBL), y=log(UBL))) +  #read in data
  geom_point(aes(color = pop),             #add points and specify grouping factor
             size = 3,                                #modify point size
             alpha = 1) +                           #make points transparent
     #manually specify which symbols to use for the groups
  scale_color_manual(values=c("red","blue","orange",           #manually specify colors for the groups   
                              "yellow","purple","black",
                              "#f1eef6","#d0d1e6","#a6bddb")) +
  labs(title = "Bract size relationship between populations",
       y="Upper bract length (log mm)",                        #define axes labels
       x="Lower bract length (log mm)") +
  scale_x_continuous(limits = c(2.2 , 3.5))+          #specify length of x-axis
  scale_y_continuous(limits = c(2.2 , 3.5))+          #specify length of y-axis 
  theme_classic() +                                      #specify the overall aesthetic of the plot
  theme(aspect.ratio=1,                             #modify the ratio of x- and y-axis
        text = element_text(size=14), #modify size of axis label text
        plot.title = element_text(hjust=0.5),
        legend.text = element_text(size=12))  +        #modify size of legend text
  labs(color="Population")  +     #modify legend title
  geom_smooth(method='lm',                            #add a regression line
              se=TRUE,                                #add or remove the standard error
              color = "black")  #modify the color of the regression line
           
#facet_grid(cols=vars(pop))                         #splits up graphs by a grouping variable

