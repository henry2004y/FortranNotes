# makefile for EECS587 Assignment 4: OpenMP
# Hongyang Zhou, 10/25/2017

default: parallel3

.PHONY: help
help:
	@echo ' '
	@echo '  You can "make" the following:'
	@echo ' '
	@echo '    help         (makefile option list)'
	@echo '    serial       (compile the serial version)'
	@echo '    parallel     (compile the parallel version1)'
	@echo '    parallel2    (compile the parallel version2)'
	@echo '    parallel3    (compile the parallel version3)'
	@echo '    debug        (compile the serial code in debugging mode)'
	@echo '    compile_fast (compile the code -O4)'
	@echo '    clean        (clean *.o *.mod *.exe)'

# Variables
FLAGS = -r8 #-C=all 
DFLAG = -C=all -gline -nan
OPT = -O4
COMPILER = nagfor $(FLAGS) $(OPT)
DEPEND = \
	Modqueue_circle.o	\
	g.o	\
	Estimate.o	\
	queue.o		
DEPENDMP = \
	Modqueue_circle.o	\
	g.o	\
	Estimate.o	\
	queue_openmp.o


serial: $(DEPEND)	
	$(COMPILER) $^ -o $ BFS.exe

debug:
	nagfor -O0 $(DFLAG) -c Modqueue_circle.f90
	nagfor -O0 $(DFLAG)  -c g.f90
	nagfor -O0 $(DFLAG)  -c Estimate.f90
	nagfor -O0 $(DFLAG)  -c queue.f90
	nagfor -O0 $(DFLAG)  -o BFS.exe Modqueue_circle.o g.o Estimate.o queue.o

%.o: %.f90
	$(COMPILER) -c $<

parallel:
	$(COMPILER) -c Modqueue_circle.f90
	$(COMPILER) -c g.f90
	$(COMPILER) -c Estimate.f90
	$(COMPILER) -c -openmp queue_openmp.f90
	$(COMPILER) -o BFS.exe -openmp Modqueue_circle.o g.o Estimate.o queue_openmp.o

parallel2:
	$(COMPILER) -c Modqueue_circle.f90
	$(COMPILER) -c g.f90
	$(COMPILER) -c Estimate.f90
	$(COMPILER) -c -openmp queue_openmp_version2.f90
	$(COMPILER) -o BFS.exe -openmp Modqueue_circle.o g.o Estimate.o queue_openmp_version2.o

parallel3:
	$(COMPILER) -c Modqueue_circle.f90
	$(COMPILER) -c g.f90
	$(COMPILER) -c Estimate.f90
	$(COMPILER) -c -openmp queue_openmp_version3.f90
	$(COMPILER) -o BFS.exe -openmp Modqueue_circle.o g.o Estimate.o queue_openmp_version3.o

parallel4:
	$(COMPILER) -c Modqueue_circle.f90
	$(COMPILER) -c g.f90
	$(COMPILER) -c Estimate.f90
	$(COMPILER) -c -openmp queue_openmp_version4.f90
	$(COMPILER) -o BFS.exe -openmp Modqueue_circle.o g.o Estimate.o queue_openmp_version4.o

parallel5:
	$(COMPILER) -c Modqueue_circle.f90
	$(COMPILER) -c g.f90
	$(COMPILER) -c Estimate.f90
	$(COMPILER) -c -openmp queue_openmp_version5.f90
	$(COMPILER) -o BFS.exe -openmp Modqueue_circle.o g.o Estimate.o queue_openmp_version5.o

compile_fast: $(DEPEND)
	$(COMPILER) $(OPT) $^ -o $ BFS.exe

.PHONY: clean
clean:
	rm -f *~ *.o *.mod BFS.exe
