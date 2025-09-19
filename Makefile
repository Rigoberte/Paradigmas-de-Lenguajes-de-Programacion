# Makefile para compilar y ejecutar Practica01.hs

all: practica01

practica01: Practica01.hs
	ghc Practica01.hs -o practica01

run_practica01:
	$(MAKE) practica01
	./practica01
	$(MAKE) clean

clean:
	rm -f practica01 *.hi *.o