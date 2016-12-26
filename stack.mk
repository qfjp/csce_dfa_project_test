stack-all :
	stack build --coverage

stack-test : isDFA
	echo "" | stack --no-terminal ${ARGS} test ":spec" --bench --no-run-benchmarks --haddock --no-haddock-deps --coverage

stack-clean :
	stack clean

.PHONY : stack-all stack-clean stack-test
