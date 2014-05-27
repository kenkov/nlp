#! /usr/bin/env python
# coding:utf-8

import itertools


class NgramException(Exception):
    pass


def ngram(
        seq: "sequence",
        n: int,
        start_symbol=True,
        end_symbol=True) -> "iterator":
    seq = list(seq)
    s_len = len(seq)
    if s_len < n:
        raise NgramException("the sentences length is not enough:\
                             len(sentences)={} < n={}".format(s_len, n))
    if start_symbol:
        ss = []
        for i in range(n-1):
            ss.append("<s_{}>".format(i))
        seq = ss + seq
    if end_symbol:
        es = []
        for i in range(n-2, -1, -1):
            es.append("</s_{}>".format(i))
        seq = seq + es

    xs = itertools.tee(seq, n)
    for i, t in enumerate(xs[1:]):
        for _ in range(i+1):
            next(t)
    return zip(*xs)


if __name__ == '__main__':
    print(list(ngram("今日はもう疲れたので寝たい", 2)))
