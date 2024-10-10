# exercise 1:
# Produce a line graph of the probability density function of a normal 
# distribution with mean 5 and standard deviation 2. Note that 
# Try to make the line smooth by evaluating the function at lots 
# you can use the dnorm function to calculate the pdf values. 
# of points - you may find the seq function helpful.

mean <- 5
sd <- 2

# creates a sequence of 100 points from 4 standard deviations 
# above and below the mean
x <- seq(mean -4*sd, mean + 4*sd, length.out = 100)
# calculate pdf values for each point x
y <- dnorm(x, mean = mean, sd = sd)

plot(x,y, type = 'l', lwd = '2', col = 'red',
     main = "PDF of Gaussian distribution with mean = 5 and sd = 2",
     xlab = 'x', ylab = 'Density')

# exercise 2
# Simulate 50 random observations from a normal distribution 
# with mean 5 and standard deviation 2. Plot two histograms of 
# your simulated values - one with frequency on the vertical 
# axis and one with density.

set.seed(1) # to reproduce results
sim <- rnorm(50, mean = mean, sd = sd) # simulates 50 random observations

hist(sim, freq = TRUE,
     main = 'Frequency Histogram', xlab = "values")

hist(sim, freq = FALSE,
     main = 'Density Histogram', xlab = 'values')

# exercise 3
# Use the lines function to combine your answers from the first 
# two questions on the same plot. The histogram and the pdf 
# should have approximately the same shape. What happens if you 
# change the number of random observations in the histogram?

lines(x,y,
      col = 'red', lwd = 2)

# as the number of observations in the normal simulation increases,
# the histogram tends towards the normal distribution curve, by
# the law of large numbers. As the sample size increases, the 
# sample mean converges to the population mean. 

standard_normal <- rnorm(500, mean = 10, sd = 10)

qqnorm(standard_normal)
abline(0,1)

# the q-q normal plot is used to test the difference between 
# two distributions. if the scatter points fall on the 45 degree
# line, then the data points are normally distributed. Increasing
# the number of random observations means the scatter points are
# more closely standard normally distributed. 

# when changing the mean and standard deviation, the shape of the
# scatter points remains the same (in a straight line), but 
# they move away from the 45 degree line, indicating they are 
# normally distributed but with different mean and SD.

# p2
data <- read.csv('MATH4007/cardata.csv')
str(data) # see column headings

# exercise 1
# Calculate the mean and standard deviation of city MPG for vehicles 
# which cost more than $30,000.

# create data subset only including cars above 30 grand
data1 <- subset(data, Price > 30)
print(data1)

print(mean(data1$MPG.city))
print(sd(data1$MPG.city))

# exercise 2 
# Produce a scatter plot of price against city MPG. What 
# relationship do you notice?

plot(data$Price, data$MPG.city)
# negative relationship; higher mpg, lower price

# exercise 3
# Which vehicle has the largest price difference between
# its premium and base versions?

data$pricedif <- data$Max.Price - data$Min.Price
print(data[which.max(data$pricedif),])
# 300E has the largest price difference

# exercise 4
# Produce a histogram of highway MPG with an 
# appropriate number of columns.

hist(data$MPG.highway,
     main = "Highway MPG Histogram", breaks = 10)

# exercise 5
# Produce a boxplot of highway MPG for each type of vehicle on the 
# same graph. Make sure you give your plot an appropriate 
# title and label each axis.

boxplot(data$MPG.highway ~ data$Model,
        main = 'Boxplots of Highway MPG by vehicle model',
        xlab = 'Vehicle type',
        ylab = 'Highway MPG')

# Solving simul. equations in R:

# Coefficient matrix
A <- matrix(c(2,1,0,
            1,3,1,
            0,1,2),
            nrow = 3, byrow = TRUE)

# Constant matrix
B <- c(1,-3,1)

print(solve(A,B)) # x1 = 1.5, x2 = -2, x3 = 1.5

# Eigenvalues and Eigenvectors
i <- eigen(A)

print(i$values)
print(i$vectors)