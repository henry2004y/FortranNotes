all: exe

exe: cfun.o
	gfortran -o run.exe f-call-c.f90 cfun.o

kronrod.o:
	gcc -c cfun.c

clean:
	rm run.exe *.o
