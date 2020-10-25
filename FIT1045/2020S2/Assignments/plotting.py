from matplotlib import pyplot as plt
from matplotlib.pyplot import plot

from regression import line, slope

def label(a, b):
    if a!=0 and b!=0:
        return r'$y={a}x + {b}$'
    elif a!=0:
        return r'$y={a}x$'
    else:
        return r'$y={b}$'

def linear(a, b, x_min=-1, x_max=1, points=1000, **kwargs):
    xx = [x_min + i/points*(x_max-x_min) for i in range(points+1)]
    yy = [a*x + b for x in xx]
    plot(xx, yy, label=label(a,b), **kwargs)

def plot2DData(expVariable, target):
    plt.plot(expVariable, target, marker='x', linestyle=' ', color='black')

def plot_1d_predictors(lines, x=[], y=[], outfile=None):
    rng =  max(x) - min(x)
    x_min = min(x) - 0.1*rng
    x_max = max(x) + 0.1*rng
    plt.xlim(x_min, x_max)
    for a, b in lines:
        linear(a, b, x_min, x_max)
    plt.xlabel('$x$')
    plt.ylabel('$y$')
    plt.legend()
    plt.plot(x, y, marker='x', linestyle=' ', color='black')

def example1():
    linear(1/3, 0.5, 0, 4, color='black')
    linear(0.0, 0.5, 0, 4, color='black', linestyle='--')
    plt.plot([1, 2, 3], [1, 1, 2], marker='o', linestyle=' ', color='black')
    plt.plot([-1, 0, 1], [-1/3, -1/3, 2/3], marker='x', linestyle=' ', color='black')
    plt.xlim(-4, 4)
    plt.ylim(-3, 3)
    plt.tight_layout()
    plt.show()

def example2():
    x = [0, 1, 2]
    y = [1, 1, 2]
    plot_1d_predictors([line(x, y), (slope(x, y), 0.0)], x, y)
    plt.show()

example2()
    