all: exe

exe: func.o
	gfortran -o run.exe main.f90 func.o

func.o:
	gcc -c func.c

clean:
	rm run.exe *.o *.mod *~
