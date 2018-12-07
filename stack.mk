stack-all :
	stack ${ARGS} build ${MORE}

stack-test :
	stack --no-terminal --install-ghc ${ARGS} test ${MORE} --bench --only-dependencies
	echo "" | stack --no-terminal ${ARGS} test ${MORE} ":spec" --bench --no-run-benchmarks --haddock --no-haddock-deps --coverage

stack-clean :
	stack clean

.PHONY : stack-all stack-clean stack-test
