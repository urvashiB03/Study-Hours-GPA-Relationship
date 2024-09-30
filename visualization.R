# Parts of this code contains snippets from lecture slides 
# (Week 5) by Dr. John Noll, and from the Lab Practicals
# and resources provided during the lab, (Practical 4, Week 5)
# of the course 7COM1079-0901-2023 - Team Research
# and Development Project taught at the University of Hertfordshire
# as part of the MSc Computer Science program.
# The ideas and concepts have been formed by attending the lectures 
# and practicals of the same module. 

pdf(file = "visualization.pdf"
    , width = 7
    , height = 5)
par(mar = c(5.1, 4.8, 4.1, 2.1)) # setting page margins
df = read.csv("gpa.csv")

x <- df$studyweek # independent variable (studyweek / Study Hours per week)
y <- df$gpa       # dependent variable (gpa / Grade Point Average)

#Scatter Plot
plot(  x         # independent variable (studyweek)
     , y         # dependent variable (gpa)
     , main = "Grade Point Average Vs. Study Hours \nof Duke University Students" # Plot title
     , xlim = c(0,50) # setting the x-axis range from 0 - 50 hrs
     , ylim = c(2.5,5.0) # setting the y-axis range from 2.5 - 5.0 GPA
     , xlab = "Study Hours (hours per week)" # x label
     , ylab = "Grade Point Average (on a scale of 5.0)" # y label
     , pch = 19  # data point shape, 19 = filled circle
     , frame = TRUE # Frame around chart
)
model <- lm(y ~ x, data = df) # compute the linear model
abline(model, col = "blue") # draw the model as a blue line

# Histogram for visualisation of distribution of Grade Point Average
h <- hist( y
           , breaks = 5
           , main = "Frequency Distribution of Grade Point Average \nAmong Duke University Students"
           , xlim = c(2.5,5.0) #setting the x-axis limits slightly beyond the data range
           , ylim = c(0,35) #setting the y-axis limits slightly beyond the data range
           , xlab = "Grade Point Average (on a scale of 5.0)"
           , ylab = "Frequency"
           , col  = "azure")

# Calculate sequence of values for x-axis
seqX <- seq( min(y) # Minimum Value in Grade Point Average data points
             , max(y) # Maximum Value in Grade Point Average data points
             , length.out = length(y)) # Total no. of values in y

# Calculate normal density values for the x-axis
dnY <- dnorm(seqX,mean=mean(y),sd=sd(y))

# Calibrate the curve against actual frequency
dnY_scaled <- dnY * diff(h$mids[1:2]) * length(y)

# Add a line plot of the normal distribution over the histogram
lines(seqX, dnY_scaled, col="red")

#Putting a legend on the top right of the graph
legend("topright", c("Normal Curve Overlay"), col = c("Red"), inset=0.01, lty = 1, cex=0.75)

dev.off()