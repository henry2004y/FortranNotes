all: exe

exe: kronrod.o
	gfortran -o run.exe kronrod_prb.f90 kronrod.o

kronrod.o:
	gcc -c kronrod.c

clean:
	rm run.exe *.o
