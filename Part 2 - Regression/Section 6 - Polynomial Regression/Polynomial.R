

# Importing the dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3]
# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
# library(caTools)
# set.seed(123)
# split = sample.split(dataset$DependentVariable, SplitRatio = 0.8)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# fitting linear regression to the dataset
lin_reg = lm(formula = Salary ~ .,
             data = dataset)
dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3
dataset$Level4 = dataset$Level^4
poly_reg = lm(formula = Salary ~ .,
              data = dataset)

#visualizing the linear regression results
#install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(lin_reg, newdata = dataset)),
            colour = 'blue') +
    ggtitle('Truth or Bluff (Linear Regression)') +
    xlab('Level') +
    ylab('Salary')

#visualizing the polynimial regression results
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(poly_reg, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Polynomial Regression)') +
  xlab('Level') +
  ylab('Salary')

# Predicting a new result with Linear regression
 y_pred = predict(lin_reg, data.frame(Level = 3.5))
# Predicting a anew result with Polynomial Regression
 y_pred = predict(poly_reg, data.frame(Level = 4.5,
                                       Level2 = 4.5^2,
                                       Level3 = 4.5^3,
                                       Level4 = 4.5^4))
