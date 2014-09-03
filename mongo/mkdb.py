#! /usr/bin/env python
# coding:utf-8

import traceback
import pymongo as pm
from ngram import bothngram, NgramException


def keydic(
        seq: list,
        n: int=1,
        keyword_len: int=1):
    dic = dict()
    for i in range(1, n+1):
        dic["w{}".format(i)] = seq[n+keyword_len+i-1]
        dic["w-{}".format(i)] = seq[n-i]
    for j in range(keyword_len):
        dic["w0_{}".format(j)] = seq[n+j]
    return dic


def mkbothngram_db(
        n: int,
        seq: "sequences",
        db: str,
        keyword_len: int=1,
        coll: str=None):

    # define the name of both n gram with keyword_len key database
    if not coll:
        coll = "both{}key{}gram".format(n, keyword_len)

    client = pm.MongoClient("localhost", 27017)
    search_coll = client[db][coll]
    output_coll = client[db][coll]

    print("start mktweetsdb: to {}.{}".format(
        db, coll))

    for i, tweet in enumerate(seq):
        if (i % 100 == 0):
            print("{} words are processed".format(i))

        try:
            n_seq = list(bothngram(tweet, n, keyword_len=keyword_len))
        except NgramException:
            print("  NgramException occured: {}".format(tweet))
            print("  continue loop")
            continue
        for seq in n_seq:
            dic = keydic(seq, n, keyword_len)

            try:
                rem = search_coll.find_one(dic)

                if rem:
                    count = rem["count"]
                    output_coll.update(
                        dic,
                        {"$set": {"count": count + 1}})
                else:
                    dic.update({"count": 1})
                    output_coll.insert(dic)

            except KeyboardInterrupt:
                print("Bye")
                exit(0)
            except:
                traceback.print_exc()


#def mkcountdb(
#        n: int,
#        keyword_len: int,
#        fromdb: str="fromdbname",
#        todb: str="todbname"):
#
#    fromcoll = "both{}key{}gram".format(n, keyword_len)
#
#    client = pm.MongoClient("localhost", 27017)
#    w_coll_name = "C_w"
#
#    # input form ngram collection
#    input_coll = client[fromdb][fromcoll]
#
#    search_w_coll = client[todb][w_coll_name]
#    output_w_coll = client[todb][w_coll_name]
#
#    print("start mkcountdb: from {}.{}".format(
#        fromdb, fromcoll,
#        todb, w_coll_name))
#
#    for i, item in enumerate(input_coll.find(timeout=False)):
#        if (i % 100 == 0):
#            print("{} words are processed".format(i))
#
#        # aiming at n = 5
#        word = {"w0_0": item["w0_0"]}
#
#        try:
#            # for w_coll
#            rem_word = search_w_coll.find_one(word)
#
#            if rem_word:
#                count = rem_word["count"]
#                output_w_coll.update(
#                    word,
#                    {"$set": {"count": count + item["count"]}})
#            else:
#                word.update({"count": item["count"]})
#                output_w_coll.insert(word)
#
#        except KeyboardInterrupt:
#            print("Bye")
#            exit(0)
#        except:
#            traceback.print_exc()


if __name__ == '__main__':

    filepath = "path/to/file"
    mkbothngram_db(
        n=1,
        seq=(line.split() for line in open(filepath)),
        db="dbname"
    )
