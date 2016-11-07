#### INTRODUCTION TO REGRESSION PRACTICE WORKSHEET
### Conducing regression on sample from population, then comparing to population

### This exercise: how does IQ predict academic performance 

library(tidyverse)
## load data 
population_data <- read_csv("population_data.csv")

##glimpse to see number of variables and numner of people
glimpse(population_data)

##SAMPLE1
##obtain sample of n people (in this case, 200)
set.seed(1) #not needed unless matching data 
sample1_data <- sample_n(population_data, size=200)
glimpse(sample1_data)

##conduct the sample regression
sample1_lm_results <- lm(performance~IQ, data=sample1_data)
##get detailed output with apatables
library(apaTables)
apa.reg.table(sample1_lm_results)

##conduct the population regression
pop_results <- lm(performance~IQ, data=population_data)
summary(pop_results)

##CONFIDENCE INTERVALS

##confidence interval: find a predicted value for Y for a given SINGLE value on X 
#look at IQ of 120
x_axis_range <- data.frame(IQ = c(120))
CI_data <- predict(sample1_lm_results,
                   newdata = x_axis_range, interval = "confidence", level = 0.95)
CI_data <- as.data.frame(cbind(x_axis_range, CI_data))
print(CI_data)

##confidence interval: predicted value for Y for a given RANGE of values on X
# Calculate the minimum and maximum values of X (i.e., min and max IQ values)
min_predictor <- min(sample1_data$IQ)
max_predictor <- max(sample1_data$IQ)

# We use .5 to generate a large number of steps between the min and max IQ values.
# You will need to adjust the step size (i.e., by size) for each data set
x_axis_range <- data.frame(IQ = seq(min_predictor, max_predictor, by=0.5))
# Calculate the confidence interval
CI_data <- predict(sample1_lm_results,
                   newdata = x_axis_range, interval = "confidence", level = 0.95)
CI_data <- as.data.frame(cbind(x_axis_range, CI_data))

# We will use CI_data later when we graph
print(CI_data)

##PREDICTION INTERVALS

##prediction interval: predicted value for Y for a given value of X
x_axis_range <- data.frame(IQ = c(120))
PI_data <- predict(sample1_lm_results,
                   newdata = x_axis_range, interval = "prediction", level = 0.95)
PI_data <- as.data.frame(cbind(x_axis_range, PI_data))
print(PI_data)



##prediction interval: predicted value for Y for a given RANGE of values on X
# Calculate the minimum and maximum values of X (i.e., min and max IQ values)
min_predictor <- min(sample1_data$IQ)
max_predictor <- max(sample1_data$IQ)

# We use .5 to generate a large number of steps between the min and max IQ values.
# You will need to adjust the step size (i.e., by size) for each data set
x_axis_range <- data.frame(IQ = seq(min_predictor, max_predictor, by=0.5))

# Calculate the confidence interval
PI_data <- predict(sample1_lm_results,
                   newdata = x_axis_range, interval = "prediction", level = 0.95)
PI_data <- as.data.frame(cbind(x_axis_range, PI_data))

# We will use CI_data later when we graph
print(PI_data)

##GRAPHING
#regression line only
reg_plot <- ggplot(sample1_data, aes(x = IQ, y = performance))
reg_plot <- reg_plot + geom_smooth(method = "lm", se = FALSE, color="black")
reg_plot <- reg_plot + geom_point()
reg_plot <- reg_plot + theme_classic()
reg_plot <- reg_plot + coord_cartesian(xlim=c(50,150),ylim=c(0,100))
reg_plot <- reg_plot + scale_x_continuous(breaks=seq(50,150,by=10))
print(reg_plot)

#with CIs - shortcut
reg_plot <- ggplot(sample1_data, aes(x=IQ, y=performance))
reg_plot <- reg_plot + geom_point()
reg_plot <- reg_plot + theme_classic()
reg_plot <- reg_plot + geom_smooth(method="lm", se=TRUE)
print(reg_plot)

#with prediction interval

reg_plot <- ggplot(sample1_data, aes(x = IQ, y = performance))
reg_plot <- reg_plot + geom_smooth(data = PI_data, aes(x=IQ, y=fit, ymin = lwr, ymax = upr), stat = "identity")

reg_plot <- reg_plot + geom_point()
reg_plot <- reg_plot + theme_classic()
reg_plot <- reg_plot + coord_cartesian(xlim=c(50,150),ylim=c(0,100))
reg_plot <- reg_plot + scale_x_continuous(breaks=seq(50,150,by=10))
print(reg_plot)

                                   
                                   

