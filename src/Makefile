build:
	ghc Main.hs -o freak

link:
	ln -fs $(shell pwd)/freak /usr/local/bin/freak

clean:
	rm -f freak
	rm -f Tests
	find . -name "*.hi" -type f -delete
	find . -name "*.o" -type f -delete

lint:
	hlint .

test:
	runhaskell Tests.hs

check: build lint test
