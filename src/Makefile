build:
	ghc Freak.hs

clean:
	rm -f Tests
	find . -name "*.hi" -type f -delete
	find . -name "*.o" -type f -delete

lint:
	hlint .

test:
	runhaskell Tests.hs

check: build lint test
