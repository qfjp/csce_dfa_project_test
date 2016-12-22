all:
	stack build --coverage
	cd c_files; $(MAKE)

test:
	# The ugly echo "" hack is to avoid complaints about 0 being an
	# invalid file descriptor on windows
	echo "" | stack --no-terminal test ":spec" --bench --no-run-benchmarks --haddock --no-haddock-deps --coverage

clean:
	stack clean
	cd c_files; $(MAKE) clean

.PHONY: clean test
