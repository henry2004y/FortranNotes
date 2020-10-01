GFORTRAN    = gfortran
CC          = gcc
MPI_FORTRAN = mpif90
MPI_CC      = mpicc
LD = -lm

DIR_SEQ = objSeqDir
DIR_PAR = objParDir
SRC_SEQ = explicitSeq.f90 explUtilSeq.f90
OBJ_SEQ = $(addprefix $(DIR_SEQ)/,$(SRC_SEQ:.f90=.o))
SRC_PAR = explicitPar.f90 explUtilPar.f90 updateBound.f90 readParam.f90
OBJ_PAR = $(addprefix $(DIR_PAR)/,$(SRC_PAR:.f90=.o))

.PHONY: all

all: explicitSeq explicitPar

explicitSeq: $(OBJ_SEQ)
	$(GFORTRAN) -o $@ $^

$(DIR_SEQ)/%.o: %.f90
	$(GFORTRAN) -c $< -o $@

explicitPar: $(OBJ_PAR)
	$(MPI_FORTRAN) -o $@ $^

$(DIR_PAR)/%.o: %.f90
	$(MPI_FORTRAN) -c $< -o $@

$(OBJ_SEQ): | $(DIR_SEQ)
$(OBJ_PAR): | $(DIR_PAR)

$(DIR_SEQ):
	mkdir $(DIR_SEQ)

$(DIR_PAR):
	mkdir $(DIR_PAR)

clean:
	rm -f explicitSeq explicitPar
	rm -rf $(DIR_SEQ)
	rm -rf $(DIR_PAR)
