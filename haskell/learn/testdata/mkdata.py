#! /usr/bin/env python
# coding:utf-8

import numpy as np
#import matplotlib.pyplot as plt


if __name__ == '__main__':
    filename = "testdata/testdata"
    hdl = open(filename, "w")

    mu1 = [10, 10]
    cov1 = [[3, -1/2],
            [-1/2, 3]]
    mu2 = [0, 0]
    cov2 = [[3, -1/2],
            [-1/2, 3]]

    pos = np.random.multivariate_normal(mu1, cov1, 5000)
    neg = np.random.multivariate_normal(mu2, cov2, 5000)

    for _x, _y in pos:
        print("{} {} {}".format(_x, _y, 1), file=hdl)
    for _x, _y in neg:
        print("{} {} {}".format(_x, _y, -1), file=hdl)
