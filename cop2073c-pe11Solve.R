# cop2073c-pe11Solve.R
# D. Singletary
# 10/26/24
# Use the tidyverse, modelr, and base R functions to analyze a dataset

# install.packages('tidyverse') # uncomment if required
# install.packages('modelr') # uncomment if required
library(tidyverse)
library(modelr)

# 1. create tibble from mtcars hp and mpg columns
t <- tibble(mtcars$hp,mtcars$mpg)
colnames(t) <- c("hp","mpg")

# 2. use lm to determine coefficients of linear relationship
tm <- lm(mpg ~ hp, t)

# 3. print coefficients
cat('coef:', coef(tm), '\n')

# 4. plot points and fitted line using coefficients
ggplot(t, aes(x=hp,y=mpg)) +
  geom_abline(
    aes(intercept = 30.09886, slope = -0.06823)) +
  geom_point()

# 5. create a data grid
t_grid <- t %>% data_grid(hp)

# 6. add predictions and plot
t_gridp <- t_grid %>% add_predictions(tm)
ggplot(t, aes(hp)) +
  geom_point(aes(y = mpg)) +
  geom_line(aes(y = pred), data = t_gridp, colour = "red", size = 1)

# 7. residuals
t_r <- t %>% add_residuals(tm)
print('residuals:')
print(t_r)

# 8. frequency polygon
ggplot(t_r, aes(resid)) + geom_freqpoly(binwidth = 0.5)

# Anaylsis:
# This program analyzes the relationship between horsepower (hp) and miles per 
# gallon (mpg) in the mtcars dataset using linear regression. The linear model 
# fitted with lm reveals a negative relationship between horsepower and fuel 
# efficiency, as the coefficients indicate that mpg decreases as hp increases. 
# The fitted line, plotted alongside the data points, visually confirms this 
# trend. A second plot, showing predicted mpg values based on the data grid, 
# aligns closely with the regression line, illustrating that the trend continues 
# in the predicted values. The residuals plot (showing the differences between 
# observed and predicted mpg values) suggests that most predictions are close 
# to the actual values, though some variation exists, indicating that while 
# the model captures the general trend, it may not explain all the variability
# in mpg.
