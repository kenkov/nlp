#! /usr/bin/env python
# coding:utf-8

from preprocessing import Preprocess
import re
from chartype import Chartype
import traceback


class JaWikiPreprocess(Preprocess):

    def __init__(self):
        Preprocess.__init__(self)

        self.equal_regex = re.compile(r'=+[^=]+=+')
        self.pars_regex = re.compile(r'（[^（）]+）|\([^\(\)]+\)')

        self.chartype = Chartype()

    def _is_nihongo(self, text):
        return all(
            self.chartype.is_nihongo(s) or self.chartype.is_ascii(s)
            for s in text)

    def _subs(self, regex: "re obj", repl: str, text: str):
        return regex.sub(repl, text)

    def remove_equal(self, text: str) -> str:
        return self._subs(self.equal_regex, "", text)

    def remove_pars(self, text: str) -> str:
        return self._subs(self.pars_regex, "", text)

    def ignore(self, text: str) -> str:
        if self._is_nihongo(text):
            return text
        else:
            return ""

    def execute(self, text: str) -> str:
        funcs = [
            self.ignore,
            self.remove_equal,
            self.remove_pars,
            self.remove_newline,
            self.remove_link,
            self.convert_cont_spaces,
            self.strip
        ]
        _text = text
        for func in funcs:
            _text = func(_text)
        return _text


if __name__ == '__main__':
    import sys
    from normalize import Normalizer

    fd = open(sys.argv[1]) if len(sys.argv) >= 2 else sys.stdin
    ps = JaWikiPreprocess()
    norm = Normalizer()

    for _line in (_.strip() for _ in fd):
        for line in _line.split("。"):
            try:
                conv = ps.execute(line)
                if conv:
                    print(norm.normalize(conv + "。"))
            except KeyboardInterrupt:
                exit()
            except:
                traceback.print_exc()

    # for file in jawiki-latest*.txt; do python
    #       ~/Projects/cabocha/jawiki_preprocess.py $file >preprocess/$file.pre.txt;
    #   done
    # for file in ~/jawiki/preprocess/jawiki-latest*.txt;
    #    do python ~/Projects/cabocha/case_mongo.py $file || exit; done
