#! /usr/bin/env python
# coding:utf-8

import itertools


class NgramException(Exception):
    pass


def ngram(
        seq: "sequence",
        n: int,
        start_symbol: bool=True,
        end_symbol: bool=True) -> "iterator":

    seq = list(seq)
    s_len = len(seq)

    if (not start_symbol) and (not end_symbol) and s_len < n:
        raise NgramException("the sentences length is not enough:\
                             len(sentences)={} < n={}".format(s_len, n))
    if start_symbol:
        ss = []
        for i in range(n-1):
            ss.append("<s_{}>".format(i))
        seq = ss + seq
    if end_symbol:
        seq.append("</s>")

    xs = itertools.tee(seq, n)
    for i, t in enumerate(xs[1:]):
        for _ in range(i+1):
            next(t)
    return zip(*xs)


def bothngram(
        seq: "sequence",
        n: int,
        keyword_len: int=1) -> "iterator":
    """Return n-gram for both sides.

    >>> both_ngram(["a", "b", "c", "d"], 1)
    [('<s_0>', '<s_1>', 'a'),
     ('<s_1>', 'a', 'b'),
     ('a', 'b', 'c'),
     ('b', 'c', 'd'),
     ('c', 'd', '</s_1>'),
     ('d', '</s_1>', '</s_0>')]
    """
    seq = list(seq)
    s_len = len(seq)
    if s_len < keyword_len:
        raise NgramException("the sentences length is not enough:\
                             len(sentences)={} < n={}".format(s_len, n))
    # start_symbol:
    ss = []
    for i in range(n):
        ss.append("<s_{}>".format(i))
    seq = ss + seq
    # end_symbol:
    es = []
    for i in range(n-1, -1, -1):
        es.append("</s_{}>".format(i))
    seq = seq + es

    xs = itertools.tee(seq, 2*n+keyword_len)
    for i, t in enumerate(xs[1:]):
        for _ in range(i+1):
            next(t)
    return zip(*xs)


if __name__ == '__main__':
    print(list(ngram("今日は熱い", 4)))
    print(list(bothngram("今日は疲れた", 2, 2)))
