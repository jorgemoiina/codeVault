#import the data and display it
quality <- read.csv("Quality.csv")
View(quality)

#view the sample size of this data set
sample_size <- nrow(quality)
sample_size

#population standard dev.
sigma <- 0.21
sigma 

#significance level
alpha <- 0.01
alpha

#hypothesized mean
miu0 <- 12
miu0

#calculate the standard error of the sample mean
std_error <- sigma/sqrt(sample_size)
std_error

#display the first few observations with variables
head(quality)

#pick the first sample
s1 <- quality$Sample.1

#calculate the z test statistic for sample 1
Myz <- (mean(s1)-miu0)/std_error
Myz

#find p-value for sample 1
p_value <- 2*pnorm(abs(Myz), lower.tail = FALSE)
p_value

#compare p_value data with alpha 
#if p_value is less than alpha, print "reject---"; otherwise, print will "fail---"
if(p_value < alpha) {
  print.default("reject the null hypothesis")
} else {
  print.default("fail to reject the null hypothesis")
} 

#calculate the standard deviation for Sample 1 for question 2
sample_stdv1 <- sd(s1)
sample_stdv1

#critical value
qnorm(0.005)

#calculate the lower limit for the confidence interval for question 3
low_limit1 <- miu0 + qnorm(0.005)*sigma/sqrt(sample_size)
low_limit1

# **added for calcuate upper limit for question 3**
upper_limit1 <- miu0 - qnorm(0.005)*sigma/sqrt(sample_size)
upper_limit1
