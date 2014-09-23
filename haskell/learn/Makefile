.PHONY: main perceptron clean

main: perceptron l1svm

perceptron: perceptron.hs
	ghc -O2 -o perceptron perceptron.hs

l1svm: l1svm.hs
	ghc -O2 -o l1svm l1svm.hs

run: main
	python3 testdata/mkdata.py
	cat testdata/testdata | ./l1svm 1.0e-5 >param/l1svmParam
	cat testdata/testdata | ./l1svm 0 >param/l1svmHingeParam
	cat testdata/testdata | ./perceptron >param/perceptronParam
	python3 testdata/param.py

clean:
	rm testdata/l1svmParam
	rm testdata/perceptronParam
