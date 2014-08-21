default: exes

help:
	echo targets are:
	echo exes, install, clean, push.bit, push.git

exes: stquery.exe

build:
	mkdir build

install: stquery.exe
	cp stquery.exe c:/files/rbin

stquery.exe: build StatQuery.hs Main.hs $(wildcard ../../hs/Tim/*.hs)
	ghc -hidir build -odir build -O -i../../hs --make Main.hs -o stquery.exe
	
clean:	
	rm -rf build/ stquery.exe

push.bit:
	@echo hg push https://trbauer:[password]@bitbucket.org/trbauer/stquery
	@echo hg push ssh://hg@bitbucket.org/trbauer/stquery

push.git:
	@echo git push origin
	

