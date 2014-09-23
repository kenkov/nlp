.PHONY: main perceptron

main: perceptron l1svm

perceptron: perceptron.hs
	ghc -O2 -o perceptron perceptron.hs

l1svm: l1svm.hs
	ghc -O2 -o l1svm l1svm.hs


run: main
	python3 testdata/mkdata.py
	cat testdata/testdata | ./l1svm >testdata/l1svmParam
	cat testdata/testdata | ./perceptron >testdata/perceptronParam
	python3 testdata/param.py
