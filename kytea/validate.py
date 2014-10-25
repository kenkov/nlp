#! /usr/bin/env python
# coding:utf-8


import re


#_word_exp = r"([^\-\\|!&\s]|(\\[-!|\s\w]))"
_word_exp = r"([^\-\\|!&\s/?]|(\\[-!|&\s\\/?]))"
_block_exp = r"(({word}(-{word})*)(/{word}+)?)".format(word=_word_exp)
# separator には ! は使えない
_separator_exp = r"([-|\s])"
_sent_exp = r"^({block}{separator})*{block}$".format(
    block=_block_exp,
    word=_word_exp,
    separator=_separator_exp)


def validate(text: str) -> bool:
    regex = re.compile(_sent_exp)
    return regex.search(text)


def test_word_exp():

    def func(text: str) -> bool:
        regex = re.compile(r"^{word}$".format(word=_word_exp))
        return True if regex.search(text) else False

    # True pattern
    assert func("\-")
    assert func("\!")
    assert func("\|")
    assert func("\ ")
    assert func("a")
    assert func("\\/")
    assert func("\\?")
    # False pattern
    assert not func(" ")
    assert not func("-")
    assert not func("!")
    assert not func("|")
    assert not func("\\")
    assert not func("/")
    assert not func("?")


def test_block_exp():

    def func(text: str) -> bool:
        regex = re.compile(r"^{block}$".format(block=_block_exp))
        return True if regex.search(text) else False

    # True case
    assert func("ほ")
    assert func("ほ/名詞")
    assert func("\-")
    assert func("\-/ハイフン")
    assert func("\!")
    assert func("\|")
    assert func("\ ")
    assert func("\ /スペース")

    assert func("ほ-げ")
    assert func("ほ-げ/スペース")

    # False case
    assert not func("\\")
    assert not func("-")
    assert not func("!")
    assert not func("|")
    assert not func(" ")
    assert not func("ほ /名詞")
    assert not func("ほ-げ-/スペース")


def test_validate():
    true_texts = [
        "げ-こ-げ-こ|っ/こふ語",
        "ｱ-ﾝ-ｯ-！-ｱ-ﾝ-ｯ-！-ｱ-ﾝ-ｯ-！-ｱ-ﾝ-ｯ-！/こふ語",
        "初-心-者|は|突-っ-込|ん|で|ﾊ-ﾞ-ｲ-ﾌ-ﾞ/こふ語|っ/こふ語|！ ？",
        "お-ま-ん-ま-ん|ｿ-ｰ-ｽ/こふ語|お-い-し-い|っ/こふ語",
        "お-そ-く|て|ﾌ-ﾞ-ﾁ-ｷ-ﾞ-ﾚ/こふ語|て|血|が|た-ら-た-ら|っ/こふ語",
        "ぱ-い-て-ょ-ん|か い て た ら|っ/こふ語|完 全 に|ね-お-ち/名詞|っ/こふ語",
        "な-ん-だ-か-ん-だ|で|っ/こふ語|下-鴨 神-社 ま で|ﾗ-ﾝ-ﾆ-ﾝ-ｸ-ﾞ/こふ語|し て き た|っ/こふ語",
        "あ|っ/こふ語|急|に|体-調-不-良|が|っ/こふ語|…|ﾗ-ﾝ-ﾆ-ﾝ-ｸ-ﾞ/こふ語|は-や-め-と-く-か|っ/こふ語|…",
        "も-ち-っ-と|明-る|く|な|っ/語尾|た-ら/助動詞|ﾗ-ﾝ-ﾆ-ﾝ-ｸ-ﾞ/こふ語|い|こ|っ/こふ語",
        "ほ-へ-〜|っ/こふ語",
        "謎|ﾃ-ﾞ-ｨ-ﾚ-ｸ-ﾖ-ﾘ/こふ語|多 く て 頭 い た く な っ て き た|っ/こふ語",
        "ｓ-ａ-ｍ-ｕ-ｉ/こふ語",
        "こ-れ|ね-む-み/名詞|だ-な-ー|っ/こふ語|…|緑-茶|で/助詞|も/助詞|い-れ/動詞|る|か|っ/こふ語|…",
        "…",
        "し-ょ-う-け-ん-こ-ふ/っ",
        "ｱ ﾝ ｯ ！ ｱ ﾝ ｯ ！ ｱ ﾝ ｯ ！ ｱ ﾝ ｯ ！",
        "は-ず-か-し|い|っ/こふ語|…",
        "や-め-て|〜-〜-〜|っ/こふ語|ｗ",
        "ｱ-ｯ/こふ語|…",
        "あ れ っ な ん か 誤 爆 し た っ",
        "お|ち-ん-ぽ|っ/こふ語",
        "と|い う わ け で|っ/こふ語|ぽ-ん-ぺ/こふ語|で|し に そ う|な-う/名詞|っ/こふ語",
        "お-な-か|す い た な|ー|っ/こふ語|…",
        "あ-ー/感動詞|っ/こふ語|こ-れ|は|い-つ|ｲ/こふ語|っ|て も|お-か-し|く|な い な|っ/こふ語",
        "え っ ち|な|か お り|っ/こふ語|！",
        "お|ち-ん-ち-ん|ﾉ-ｲ-ｽ-ﾞ/こふ語|っ/こふ語|！",
        "し|に|そ-う|な-ん-じ-ゃ|〜|っ/こふ語",
        "や ば い|し/動詞|に|そ う|っ/こふ語",
        "遠 く で 発 見 し て|っ/こふ語|ｽ-ﾞ-ｯ-ﾑ/こふ語|し た の で|っ/こふ語|あ れ で し た ね|っ/こふ語",
        "ｙ-ａ-ｂ-ａ-ｉ",
        "ぱ-い-て-ょ-ん … \\?",
    ]
    false_texts = [
        "げ-こ-げ-こ|っ/こふ語|",
        "ｱ-ﾝ-ｯ-！  ｱ-ﾝ-ｯ-！-ｱ-ﾝ-ｯ-！-ｱ-ﾝ-ｯ-！/こふ語",
        "初- 心 者|は|突-っ-込|ん|で|ﾊ-ﾞ-ｲ-ﾌ-ﾞ/こふ語|っ/こふ語|！ ？",
        "お!ま-ん-ま-ん|ｿ-ｰ-ｽ/こふ語|お-い-し-い|っ/こふ語",
        "お-そ-く |て|ﾌ-ﾞ-ﾁ-ｷ-ﾞ-ﾚ/こふ語|て|血|が|た-ら-た-ら|っ/こふ語",
        "ぱ-い-て-ょ-ん |か い て た ら|っ/こふ語|完 全 に|ね-お-ち/名詞|っ/こふ語 ",
        "ぱ-い-て-ょ-ん … ?",
    ]

    for text in true_texts:
        assert validate(text)
    for text in false_texts:
        assert not validate(text)


if __name__ == '__main__':
    import sys

    fd = sys.stdin if len(sys.argv) == 1 else open(sys.argv[1], "r")
    #print("Input: {}".format(fd.name))
    invalid = []
    for num, text in enumerate(fd):
        if not validate(text):
            invalid.append((num, text))
    if invalid:
        for num, text in invalid:
            print("line {num} is invalid: {text}".format(
                num=num+1,
                text=text.strip()))
        sys.exit(1)
