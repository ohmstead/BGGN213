# The beginning of this script is the work we did in-class; the end is for the optional "Lab 4" document

# IN-CLASS WORK
x <- 1:50
plot(x, sin(x), type='b', col='blue', lwd=2)

# LAB 4 OPTIONAL EXERCISE
# load BRFSS data
source("http://thegrantlab.org/misc/cdc.R")

# Q2
plot(cdc$weight, cdc$height)

# Q3
hist(cdc$height)
hist(cdc$weight)

# Q4
cor(cdc$height, cdc$weight)

# Q5
POUND_TO_KG <- 0.454
weight_kg <- cdc$weight * POUND_TO_KG

# Q6
INCH_TO_METER <- 0.0254
height_m = cdc$height * INCH_TO_METER
bmi = weight_kg / height_m^2

plot(cdc$height, bmi)

# Q7
cor(bmi, cdc$height)

# Q8
numObese <- sum(bmi > 30)
numObese

# Q9
plot(cdc$height[1:100], cdc$weight[1:100])

# Q10
obeseGenders = cdc$gender[bmi >= 30]
table(obeseGenders)

# convoluted, non-elegant solution to Q10
# bmiIndex <- bmi >= 30
# allObese <- cdc$gender[bmiIndex]
# numObeseMales <- sum(allObese == 'm')
# numObeseMales














