all: exe

exe: cfunc.o
	gfortran -o run.exe main.f90 cfunc.o

func.o:
	gcc -c cfunc.c

clean:
	rm run.exe *.o *.mod *~
