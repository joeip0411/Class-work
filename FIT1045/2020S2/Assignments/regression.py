"""
In this assignment we create a Python module
to perform some basic data science tasks. While the
instructions contain some mathematics, the main focus is on 
implementing the corresponding algorithms and finding 
a good decomposition into subproblems and functions 
that solve these subproblems. 

To help you to visually check and understand your
implementation, a module for plotting data and linear
prediction functions is provided.

The main idea of linear regression is to use data to
infer a prediction function that 'explains' a target variable 
of interest through linear effects of one 
or more explanatory variables. 

Part I - Univariate Regression

Task A: Optimal Slope

-> example: price of an apartment

Let's start out simple by writing a function that finds
an "optimal" slope (a) of a linear prediction function 
y = ax, i.e., a line through the origin. A central concept
to solve this problem is the residual vector defined as

(y[1]-a*x[1], ..., y[m]-a*x[m]),

i.e., the m-component vector that contains for each data point
the difference of the target variable and the corresponding
predicted value.

With some math (that is outside the scope of this unit) we can show
that for the slope that minimises the sum of squared the residual

x[1]*(y[1]-a*x[1]) + ... + x[m]*(y[m]-a*x[m]) = 0

Equivalently, this means that

a = (x[1]*y[1]+ ... + x[m]*y[m])/(x[1]*x[1]+ ... + x[m]*x[m])

Write a function slope(x, y) that, given as input
two lists of numbers (x and y) of equal length, computes
as output the lest squares slope (a).

Task B: Optimal Slope and Intercept

To get a better fit, we have to consider the intercept b as well, 
i.e., consider the model f(x) = ax +b. 
To find the slope of that new linear model, we \centre the explanatory variable 
by subtracting the mean from each data point. 
The correct slope of the linear regression f(x)=ax + b is the same 
slope as the linear model without intercept, f(x)=ax, calculated on the 
centred explanatory variables instead of the original ones. 
If we have calculated the correct slope a, we can calculate the intercept as
b = mean(y) - a*mean(x).

Write a function line(x,y) that, given as input
two lists of numbers (x and y) of equal length, computes
as output the lest squares slope a and intercept b and
returns them as a tuple a,b.


Task C: Choosing the Best Single Predictor

We are now able to determine a regression model that represents 
the linear relationship between a target variable and a single explanatory variable.
However, in usual settings like the one given in the introduction, 
we observe not one but many explanatory variables (e.g., in the example `GDP', `Schooling', etc.). 
As an abstract description of such a setting we consider n variables 
such that for each j with 0 < j < n we have measured m observations 

$x[1][j], ... , x[m][j]$. 

These conceptually correspond to the columns of a given data table. 
The individual rows of our data table then become n-dimensional 
data points represented not a single number but a vector.

A general, i.e., multi-dimensional, linear predictor is then given by an n-dimensional 
weight vector a and an intercept b that together describe the target variable as

y = dot(a, x) + b 

i.e., we generalise y = ax + b by turning the slope a into an n-component linear weight vector
and replace simple multiplication by the dot product (the intercept b is still a single number).
Part 2 of the assignment will be about finding such general linear predictors. 
In this task, however, we will start out simply by finding the best univariate predictor 
and then represent it using a multivariate weight-vector $a$. %smooth out with the text that follows.

Thus, we need to answer two questions: (i) how do we find the best univariate predictor, 
and (ii) how to we represent it as a multivariate weight-vector. 

Let us start with finding the best univariate predictor. For that, we test all possible
predictors and use the one with the lowest sum of squared residuals.
Assume we have found the slope a^j and intercept b^j of the best univariate predictor---and assume it 
uses the explanatory variable x^j---then we want to represent this as a multivariate 
slope a and intercept b. That is, we need to find a multivariate slop a such that dot(a, x) + b 
is equivalent to a^jx^j + b^j. Hint: The intercept remains the same, i.e., $b = b^j$.

Task D: Regression Analysis

You have now developed the tools to carry out a regression analysis. 
In this task, you will perform a regression analysis on the life-expectancy 
dataset an excerpt of which was used as an example in the overview. 
The dataset provided in the file /data/life_expectancy.csv.


Part 2 - Multivariate Regression

In part 1 we have developed a method to find a univariate linear regression model 
(i.e., one that models the relationship between a single explanatory variable and the target variable), 
as well as a method that picks the best univariate regression model when multiple 
explanatory variables are available. In this part, we develop a multivariate regression method 
that models the joint linear relationship between all explanatory variables and the target variable. 


Task A: Greedy Residual Fitting

We start using a greedy approach to multivariate regression. Assume a dataset with m data points 
x[1], ... , x[m] 
where each data point x[i] has n explanatory variables x[i][1], ... , x[i][m], 
and corresponding target variables y[1], ... ,y[m]. The goal is to find the slopes for 
all explanatory variables that help predicting the target variable. The strategy we 
use greedily picks the best predictor and adds its slope to the list of used predictors. 
When all slopes are computed, it finds the best intercept. 
For that, recall that a greedy algorithm iteratively extends a partial solution by a 
small augmentation that optimises some selection criterion. In our setting, those augmentation 
options are the inclusion of a currently unused explanatory variable (i.e., one that currently 
still has a zero coefficient). As selection criterion, it makes sense to look at how much a 
previously unused explanatory variable can improve the data fit of the current predictor. 
For that, it should be useful to look at the current residual vector r,
because it specifies the part of the target variable that is still not well explained. 
Note that a the slope of a predictor that predicts this residual well is a good option for 
augmenting the current solution. Also, recall that an augmentation is used only if it 
improves the selection criterion. In this case, a reasonable selection criterion is 
again the sum of squared residuals.

What is left to do is compute the intercept for the multivariate predictor. 
This can be done  as


b = ((y[1]-dot(a, x[1])) + ... + (y[m]-dot(a, x[m]))) / m

The resulting multivariate predictor can then be written as 

y = dot(a,x) + b .



Task B: Optimal Least Squares Regression

Recall that the central idea for finding the slope of the optimal univariate regression line (with intercept) 
that the residual vector has to be orthogonal to the values of the centred explanatory variable. 
For multivariate regression we have many variables, and it is not surprising that for an optimal 
linear predictor dot(a, x) + b, it holds that the residual vector is orthogonal to each of the 
centred explanatory variables (otherwise we could change the predictor vector a bit to increase the fit). 
That is, instead of a single linear equation, we now end up with n equations, one for each data column.
For the weight vector a that satisfies these equations for all i=1, ... ,n, you can again simply find the 
matching intercept b as the mean residual when using just the weights a for fitting:

b = ((y[1] - dot(a, x[1])) + ... + (y[m] - dot(a, x[m])))/m .

In summary, we know that we can simply transform the problem of finding the least squares predictor to solving a system of linear equation, which we can solve by Gaussian Elimination as covered in the lecture. An illustration of such a least squares predictor is given in Figure~\ref{fig:ex3dPlotWithGreedyAndLSR}.
"""

from math import inf, sqrt




def slope(x, y):
    """
    Computes the slope of the least squares regression line
    (without intercept) for explaining y through x.

    For example:
    >>> slope([0, 1, 2], [0, 2, 4])
    2.0
    >>> slope([0, 2, 4], [0, 1, 2])
    0.5
    >>> slope([0, 1, 2], [1, 1, 2])
    1.0
    >>> slope([0, 1, 2], [1, 1.2, 2])
    1.04
    """
    pass




def line(x, y):
    """
    Computes the least squares regression line (slope and intercept)
    for explaining y through x.

    For example:
    >>> a, b = line([0, 1, 2], [1, 1, 2])
    >>> round(a,1)
    0.5
    >>> round(b,2)
    0.83
    """
    pass


def best_single_predictor(data, y):
    """
    >>> data = [[1, 0],
    ...         [2, 3],
    ...         [4, 2]]
    >>> y = [2, 1, 2]
    >>> weights, b = best_single_predictor(data, y)
    >>> weights[0]
    0.0
    >>> round(weights[1],2)
    -0.29
    >>> round(b,2)
    2.14
    """
    pass


def greedy_predictor(data, y):
    """
    This implements a greedy correlation pursuit algorithm.

    >>> data = [[1, 0],
    ...         [2, 3],
    ...         [4, 2]]
    >>> y = [2, 1, 2]
    >>> weights, intercept = greedy_predictor(data, y)
    >>> round(weights[0],2)
    0.21
    >>> round(weights[1],2)
    -0.29
    >>> round(intercept, 2)
    1.64
    
    >>> data = [[0, 0],
    ...         [1, 0],
    ...         [0, -1]]
    >>> y = [1, 0, 0]
    >>> weights, intercept = greedy_predictor(data, y)
    >>> round(weights[0],2)
    -0.5
    >>> round(weights[1],2)
    0.75
    >>> round(intercept, 2)
    0.75
    """
    pass




def equation(i, data, y):
    """
    Finds the row representation of the i-th least squares condition,
    i.e., the equation representing the orthogonality of
    the residual on data column i:

    (x_i)'(y-Xb) = 0
    (x_i)'Xb = (x_i)'y

    x_(1,i)*[x_11, x_12,..., x_1n] + ... + x_(m,i)*[x_m1, x_m2,..., x_mn] = <x_i , y>

    For example:
    >>> data = [[1, 0],
    ...         [2, 3],
    ...         [4, 2]]
    >>> y = [2, 1, 2]
    >>> coeffs, rhs = equation(0, data, y)
    >>> round(coeffs[0],2)
    4.67
    >>> round(coeffs[1],2)
    2.33
    >>> round(rhs,2)
    0.33
    >>> coeffs, rhs = equation(1, data, y)
    >>> round(coeffs[0],2)
    2.33
    >>> round(coeffs[1],2)
    4.67
    >>> round(rhs,2)
    -1.33
    """
    pass

def least_squares_predictor(data, y):
    """
    Finding the least squares solution by:
    - centering the variables (still missing)
    - setting up a system of linear equations
    - solving with elimination from the lecture

    For example:
    >>> data = [[0, 0],
    ...         [1, 0],
    ...         [0, -1]]
    >>> y = [1, 0, 0]
    >>> weights, intercept = least_squares_predictor(data, y)
    >>> round(weights[0],2)
    -1.0
    >>> round(weights[1],2)
    1.0
    >>> round(intercept, 2)
    1.0
    
    >>> data = [[1, 0],
    ...         [2, 3],
    ...         [4, 2]]
    >>> y = [2, 1, 2]
    >>> weights, intercept = least_squares_predictor(data, y)
    >>> round(weights[0],2)
    0.29
    >>> round(weights[1],2)
    -0.43
    >>> round(intercept, 2)
    1.71
    """
    pass


def regression_analysis():        
    """
    The regression analysis can be performed in this function or in any other form you see
    fit. The results of the analysis can be provided in this documentation. If you choose
    to perform the analysis within this funciton, the function could be implemented 
    in the following way.
    
    The function reads a data provided in "life_expectancy.csv" and finds the 
    best single predictor on this dataset.
    It than computes the predicted life expectancy of Rwanda using this predictor, 
    and the life expectancy of Liberia, if Liberia would improve its schooling 
    to the level of Austria.
    The function returns these two predicted life expectancies.
    
    For example:
    >>> predRwanda, predLiberia = regression_analysis()
    >>> round(predRwanda)
    65
    >>> round(predLiberia)
    79
    """
    pass
    
if __name__=='__main__':
    import doctest
    doctest.testmod()
