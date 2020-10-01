Test: FortranCallsCuda.f95 vecAdd.o
	gfortran FortranCallsCuda.f95 vecAdd.o
#gfortran -L /usr/local/cuda/lib -I /usr/local/cuda/include -lcudart -lcuda FortranCallsCuda.f95 vecAdd.o
vecAdd.o: vecAdd.cu
	nvcc -c -O3 vecAdd.cu
clean:
	rm a.out FortranCallsCuda.o FortranCallsCuda.linkinfo
