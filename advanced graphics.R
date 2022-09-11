# from the UScereal data from the MASS package 
#create a copy of data frame
#name it cereal 
#collaps the mfr column to be a factor with three levels namely
# General Mills, Kelloggs, Other
# factor the shelf column in the cereal data
library(MASS)
library(ggplot2)
data("UScereal")
cereal<- UScereal
levels(cereal$mfr)<- c("General Mills", "Kelloggs",rep("Other", 4) )
cereal$shelf<- factor(cereal$shelf)

## first plot 
# A scatter plot of calories variable on protein variable
# color the point according to shelf position
#shape the points according to manufacturer
# include a simple linear regression split according to shelf position
calories_plot<- ggplot(data= cereal, mapping= aes(x= calories, y= protein))+ 
  geom_point(aes(color= shelf, shape= mfr)) + 
  geom_smooth(method= "lm", aes(color= shelf))+
  ggtitle("A scatterplot of calories on proteins of cereal data")+
  labs(x= "Calories", y="Protein", shape=  "Manufucturer")

## second plot
# a set of kernel estimates of calories with filled color differentiating shelf position
kernel_plot<- ggplot(data= cereal, mapping = aes(x= calories, fill= shelf))+
  geom_density(alpha= 0.5) +
  ggtitle("a set of kernel estimates of calories probability densities")+
  labs(x= "Calories", y= "Kernel estimates", fill= "Shelf")

## display the two on one plot
library(gridExtra)
dev.new() # set a window to display the two plots
grid.arrange(calories_plot,kernel_plot) #display the two plots in one panel

#facet the graphics of calories on protein with each panel corresponding to a manufacturer
# add a LOESS object with a 90 percent span
#point should be colored according to sugar content,
# sized according to sodium content, and shaped according to shelf position

ggplot(data= cereal,mapping= aes(x= calories, y= protein))+
  geom_point(mapping= aes(color= sugars, size= sodium, shape= shelf))+
  geom_smooth(method= "loess",span = 90)+
  facet_wrap(~mfr)
### first plot- scatterplot
## consider the salaries object from the car package
## produce a ggplot object named gg1 of a scatterplot of salary against years of service
## distinguish different gender with different colors
## add a sex-specific LOESS trend

library(car)
library(ggplot2)
data("Salaries")
dev.new()
gg1 <- ggplot(data= Salaries, mapping = aes(x= yrs.service, y= salary, color= sex))+
  geom_point(aes(color= sex)) + 
  geom_smooth(method= "loess") + 
  ggtitle("Scatterplot of the salary against the years of service for Salaries dataset")+
  labs(x= "Years of Service", y= "Salary")
gg1

### second plot = side-by-side boxplot
## salary boxplot split by rank
## each boxplot should be further split up according to sex

gg2 <- ggplot(data = Salaries, mapping = aes(x= salary, color= sex, fill= sex))+
  geom_boxplot()+
  facet_wrap(~rank)

### third plot= sidebyside boxplot 
## salary boxplot split by discipline
#with each displine split with the different sex using color or fill

gg3 <- ggplot(data= Salaries,mapping= aes(x= salary, color= sex, fill= sex))+
  geom_boxplot()+
  facet_wrap(~discipline)

### fourth plot = kernel estimate density plot
## density plot of salary using 30 percent opaque fill to distinguish rank

gg4 <- ggplot(data= Salaries, mapping= aes(x= salary, fill= rank))+
  geom_density(alpha= .30)

# display the four plot on a single device
library(gridExtra)
dev.new()
grid.arrange(gg1,gg2,gg3,gg4)

### from Salaries dataset, plot a series of kernel density estimate
### using 70 percent opaque fills to distinguish between male and female
### faceted by academic rank

library(dplyr)
Salaries %>% 
  ggplot(mapping= aes(x= salary, fill= sex))+
  geom_density(alpha= 0.7)+
  facet_wrap(~rank)

### scatterplot of salary on years of service using color to differentiate male and female
### faceted by discipline as rows and by rank as column
### each plot sholud have a sex-specific linear regression line with confidence band and free horizontal scales

dev.new()
sal_on_yos_plot <- Salaries %>% 
  ggplot(mapping= aes(x= salary, y= yrs.service, color= sex))+
  geom_point()+
  geom_smooth(method= "lm")

sal_on_yos_plot+
  facet_grid(discipline~ rank, scales = "free")
