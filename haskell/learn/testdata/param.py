#! /usr/bin/env python
# coding:utf-8

import numpy as np
import matplotlib.pyplot as plt


def plot_points(filename, color="red", left=-10, right=10, step=1):
    hdl = open(filename, "r")
    a, b, c = [float(x) for x in hdl.readline().split()]

    d = - a / b
    e = - c / b
    xlst = np.arange(left, right, step)
    ylst = [d * x + e for x in xlst]
    print("{} ({}): y = {} x + {}".format(filename, color, d, e))
    plt.plot(xlst, ylst, color=color)


if __name__ == '__main__':
    plot_points("param/perceptronParam", color="red")
    plot_points("param/l1svmParam", color="blue")
    plot_points("param/l1svmHingeParam", color="green")

    filename = "testdata/testdata"
    pos = []
    neg = []
    for x1, x2, y in [[float(_) for _ in xs.split()]
                      for xs in open(filename, "r")]:
        if y == 1.0:
            pos.append([x1, x2])
        else:
            neg.append([x1, x2])
    plt.scatter(*zip(*pos), color="r", marker="x")
    plt.scatter(*zip(*neg), color="b")
    plt.show()
