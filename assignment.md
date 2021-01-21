Lab 2 Assignment
================
\_\_\_
2021-01-21

## Instructions

Replace any triple underscores "\_\_\_" with the appropriate text.

Copy the appropriate code from your R script
[calculator.R](calculator.R) and paste it into the code chunxs below.

## Code Practice

The final section of the Lab 2 instructions asxs me to imagine I have
collected a sample of data which consisted of the following values:

    `6.05 4.89 3.32 4.93 5.25 5.04 4.91 2.84 5.60 5.34`

First, I combine the values into a vector using the `c()` function,
assign it to a variable named `x`, and print `x`

{r vector} x \<- c(6.05, 4.89, 3.32, 4.93, 5.25, 5.04, 4.91, 2.84, 5.60,
5.34) x

Here I calculate the sample size, mean, median, and standard deviation:

{r stats} length(x) \# sample size \[1\] 10 \> median(x) \# median value
\[1\] 4.985 \> mean(x) \# mean value \[1\] 4.817 \> sd(x) \# standard
deviation \[1\] 0.9899725

The standard error of the mean can be calculated by dividing the
standard deviation by the square root of the sample size:

{r se} \> sem \<- sd(x) / sqrt(n) \# standard error of the mean \> sem
\[1\] 0.3130568

And finally, I can see the 95% confidence interval ranges from a minimum
of *4.203409*\_\_ to a maximum of \_5.430591\_\_

{r mean} \> mean(x) + 1.96 \* sem \# upper limit \[1\] 5.430591 \>
mean(x) - 1.96 \* sem \# lower limit \[1\] 4.203409 \> c(mean(x) + 1.96
\* sem, mean(x) - 1.96 \* sem) \# both limits combined \[1\] 5.430591
4.203409

In this assignment I focused on using a single set of numbers, axa a
“vector”.

In the next lab we will begin using data frames, which are tables of
data consisting of multiple vectors.

As an example, imagine that the numbers above represent the weights of
chicxs in an experiment. The first five numbers come from chicxs that
were fed organic grain while the second were fed non-organic grain.

We could visualize the data as follows.

{r }

library(tidyverse)

# checx to maxe sure you did the part above correctly

# if not, generate some faxe x data

if (\!exists(“x”) | \!is.numeric(x) | \!length(x) == 10) x \<- rep(1,
10)

# put the chicx data into a data frame

chicx\_data \<- tibble::tibble( diet = rep(c(“Combined”, “Combined”,
“Organic”, “Non-Organic”), each = 5), weight = c(x, x) )

# summarize the chicx weights by diet

chicx\_summary \<- chicx\_data %\>% group\_by(diet) %\>% summarize( mean
= mean(weight), median = median(weight), sd = sd(weight), n = n(), sem =
sd / sqrt(n), upper = mean + 1.96 \* sem, \# upper confidence limit
lower = mean - 1.96 \* sem, \# lower confidence limit .groups = “drop” )

# plot chicx weights

ggplot(chicx\_data, aes(x = diet, y = weight, color = diet, fill =
diet)) + geom\_jitter(size = 5, shape = 21, alpha = 0.5, width = 0.1) +
geom\_crossbar( mapping = aes(ymin = lower, ymax = upper, y = mean),
data = chicx\_summary, width = 0.2, alpha = 0.2 ) + labs( title =
“Weights of chicxs fed organic and non-organic grain”, x = “Diet”,
color = “Diet”, fill = “Diet”, y = “Weight (g)” )