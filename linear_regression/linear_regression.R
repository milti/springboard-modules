#  Introduction
## ══════════════

#   • Learning objectives:
##     • Learn the R formula interface
##     • Specify factor contrasts to test specific hypotheses
##     • Perform model comparisons
##     • Run and interpret variety of regression models in R

## Set working directory
## ─────────────────────────

##   It is often helpful to start your R session by setting your working
##   directory so you don't have to type the full path names to your data
##   and other files

# set the working directory
# setwd("~/Desktop/Rstatistics")
# setwd("C:/Users/dataclass/Desktop/Rstatistics")

##   You might also start by listing the files in your working directory

setwd(getwd()) # where am I?
list.files("linear_regression/dataSets") # files in the dataSets folder

## Load the states data
## ────────────────────────

# read the states data
states.data <- readRDS("linear_regression/dataSets/states.rds") 
#get labels
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
#look at last few labels
tail(states.info, 8)

## Linear regression
## ═══════════════════

## Examine the data before fitting models
## ──────────────────────────────────────────

##   Start by examining the data to check for problems.

# summary of expense and csat columns, all rows
sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
summary(sts.ex.sat)
# correlation between expense and csat
cor(sts.ex.sat)

## Plot the data before fitting models
## ───────────────────────────────────────

##   Plot the data to look for multivariate outliers, non-linear
##   relationships etc.

# scatter plot of expense vs csat
plot(sts.ex.sat)

## Linear regression example
## ─────────────────────────────

##   • Linear regression models can be fit with the `lm()' function
##   • For example, we can use `lm' to predict SAT scores based on
##     per-pupal expenditures:

# Fit our regression model
sat.mod <- lm(csat ~ expense, # regression formula
              data=states.data) # data set
# Summarize and print the results
summary(sat.mod) # show regression coefficients table

## Why is the association between expense and SAT scores /negative/?
## ─────────────────────────────────────────────────────────────────────

##   Many people find it surprising that the per-capita expenditure on
##   students is negatively related to SAT scores. The beauty of multiple
##   regression is that we can try to pull these apart. What would the
##   association between expense and SAT scores be if there were no
##   difference among the states in the percentage of students taking the
##   SAT?

summary(lm(csat ~ expense + percent, data = states.data))

## The lm class and methods
## ────────────────────────────

##   OK, we fit our model. Now what?
##   • Examine the model object:

class(sat.mod)
names(sat.mod)
methods(class = class(sat.mod))[1:9]

##   • Use function methods to get more information about the fit

confint(sat.mod)
hist(residuals(sat.mod))

## Linear Regression Assumptions
## ─────────────────────────────────

##   • Ordinary least squares regression relies on several assumptions,
##     including that the residuals are normally distributed and
##     homoscedastic, the errors are independent and the relationships are
##     linear.

##   • Investigate these assumptions visually by plotting your model:

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(sat.mod, which = c(1, 2)) # "which" argument optional

## Comparing models
## ────────────────────

##   Do congressional voting patterns predict SAT scores over and above
##   expense? Fit two models and compare them:

# fit another model, adding house and senate as predictors
sat.voting.mod <-  lm(csat ~ expense + house + senate,
                      data = na.omit(states.data))
sat.mod <- update(sat.mod, data=na.omit(states.data))
# compare using the anova() function
anova(sat.mod, sat.voting.mod)
coef(summary(sat.voting.mod))

## Exercise: least squares regression
## ────────────────────────────────────────

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model
states <- readRDS("linear_regression/dataSets/states.rds")
states.info <- subset(states, select = c("energy", "metro"))
str(states)
str(states.info)
cor(states.info)
plot(states.info)

##   2. Print and interpret the model `summary'
states.mod <- lm(energy ~ metro, data=states.info)
summary(states.mod)

##   3. `plot' the model to look for deviations from modeling assumptions
par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(states.mod, which = c(1, 2))

##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. 
##   1. Examine/plot the data before fitting the model
states.info2 <- subset(states, select = c("energy", "metro", "density", "green" , "waste"))
str(states.info2)
cor(states.info2)
plot(states.info2)

##   2. Print and interpret the model `summary'
states.mod2 <- lm(energy ~ metro + density + green + waste + density*green, data=states.info2)
summary(states.mod2)

##   3. `plot' the model to look for deviations from modeling assumptions
par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(states.mod2, which = c(1, 2))

#Is this model significantly better than the model
##   with /metro/ as the only predictor?
## Multiple R-squared:  0.1154,	Adjusted R-squared:  0.097   # energy ~ metro
## Multiple R-squared:  0.1397,	Adjusted R-squared:  0.1031  # energy ~ metro + density
## Multiple R-squared:  0.5962,	Adjusted R-squared:  0.5687  # energy ~ metro + green + waste
## Multiple R-squared:  0.5939,	Adjusted R-squared:  0.5758  # energy ~ metro + green
## Multiple R-squared:  0.6157,	Adjusted R-squared:   0.58   # energy ~ metro + density + green + waste

## Interactions and factors
## ══════════════════════════

## Modeling interactions
## ─────────────────────────

##   Interactions allow us assess the extent to which the association
##   between one predictor and the outcome depends on a second predictor.
##   For example: Does the association between expense and SAT scores
##   depend on the median income in the state?

  #Add the interaction to the model
sat.expense.by.percent <- lm(csat ~ expense*income,
                             data=states.data) 
#Show the results
  coef(summary(sat.expense.by.percent)) # show regression coefficients table

## Regression with categorical predictors
## ──────────────────────────────────────────

##   Let's try to predict SAT scores from region, a categorical variable.
##   Note that you must make sure R does not think your categorical
##   variable is numeric.

# make sure R knows region is categorical
str(states.data$region)
states.data$region <- factor(states.data$region)
#Add region to the model
sat.region <- lm(csat ~ region,
                 data=states.data) 
#Show the results
coef(summary(sat.region)) # show regression coefficients table
anova(sat.region) # show ANOVA table

##   Again, *make sure to tell R which variables are categorical by
##   converting them to factors!*

## Setting factor reference groups and contrasts
## ─────────────────────────────────────────────────

##   In the previous example we use the default contrasts for region. The
##   default in R is treatment contrasts, with the first level as the
##   reference. We can change the reference group or use another coding
##   scheme using the `C' function.

# print default contrasts
contrasts(states.data$region)
# change the reference group
coef(summary(lm(csat ~ C(region, base=4),
                data=states.data)))
# change the coding scheme
coef(summary(lm(csat ~ C(region, contr.helmert),
                data=states.data)))

##   See also `?contrasts', `?contr.treatment', and `?relevel'.
##  > relevel(states.data$region, ref = 3)
##  [1] South   West    West    South   West    West    N. East South   <NA>    South  
##  [11] South   West    West    Midwest Midwest Midwest Midwest South   South   N. East
##  [21] South   N. East Midwest Midwest South   Midwest West    Midwest West    N. East
##  [31] N. East West    N. East South   Midwest Midwest South   West    N. East N. East
##  [41] South   Midwest South   South   West    N. East South   West    South   Midwest
##  [51] West   
##  Levels: South West N. East Midwest
##
##  > contr.treatment(states.data$region)
##  West West South West West N. East South <NA> South South West West Midwest
##  South      0    0     0    0    0       0     0    0     0     0    0    0       0
##  West       1    0     0    0    0       0     0    0     0     0    0    0       0
##  West       0    1     0    0    0       0     0    0     0     0    0    0       0
##  South      0    0     1    0    0       0     0    0     0     0    0    0       0
##  West       0    0     0    1    0       0     0    0     0     0    0    0       0
##  West       0    0     0    0    1       0     0    0     0     0    0    0       0
##  N. East    0    0     0    0    0       1     0    0     0     0    0    0       0
##  South      0    0     0    0    0       0     1    0     0     0    0    0       0
##  <NA>       0    0     0    0    0       0     0    1     0     0    0    0       0
##  South      0    0     0    0    0       0     0    0     1     0    0    0       0
##  South      0    0     0    0    0       0     0    0     0     1    0    0       0
##  West       0    0     0    0    0       0     0    0     0     0    1    0       0

## Exercise: interactions and factors
## ────────────────────────────────────────

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

summary(lm(energy ~ metro + density, data = states.data))
## Multiple R-squared:  0.1397,	Adjusted R-squared:  0.1031  # energy ~ metro + density
## Multiple R-squared:  0.5962,	Adjusted R-squared:  0.5687  # energy ~ metro + green + waste
## Multiple R-squared:  0.5939,	Adjusted R-squared:  0.5758  # energy ~ metro + green
## Multiple R-squared:  0.6157,	Adjusted R-squared:   0.58   # energy ~ metro + density + green + waste

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?

summary(lm(energy ~ metro + density + region, data = states.data))
## Call:
##  lm(formula = energy ~ metro + density + region, data = states.data)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -139.29  -72.28  -32.57   14.73  542.63 
##
## Coefficients:
##  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    536.4747    78.9979   6.791 2.32e-08 ***
##  metro           -2.1447     1.1774  -1.822   0.0753 .  
## density          0.0449     0.1409   0.319   0.7516    
## regionN. East -150.0513    80.1997  -1.871   0.0680 .  
## regionSouth    -28.1886    53.0806  -0.531   0.5981    
## regionMidwest  -66.6310    55.7548  -1.195   0.2385    
## ---
##  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## Residual standard error: 137.9 on 44 degrees of freedom
## (1 observation deleted due to missingness)
## Multiple R-squared:  0.2153,	Adjusted R-squared:  0.1261 
## F-statistic: 2.414 on 5 and 44 DF,  p-value: 0.05103
