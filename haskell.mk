Main :
	stack build --coverage

hs-test :
	echo "" | stack --no-terminal test ":spec" --bench --no-run-benchmarks --haddock --no-haddock-deps --coverage

hs-clean :
	stack clean

.PHONY : hs-test hs-clean
