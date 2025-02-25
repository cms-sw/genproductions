# DD standalone or for use in Collier
#PREPRFLAGS = -Dcollierdd

# Makro-Definitions:

#FC and FFLAGS for running on a alpha/dec workstation:
#FC = f77
#FFLAGS = -O2 -g 
#FFLAGS = -E1 -O3 -warn unused -check bounds -check overflow -check underflow -V -cross_reference -g3

#FC and FFLAGS for running on a LINUX PC:
#FC = g77
#FFLAGS = -O2 -fno-emulate-complex

#FC = pgf77
#FFLAGS = -O2 -g77libs

FC = gfortran
FFLAGS = -O2 -Wall -ffixed-line-length-132
#FFLAGS = -O2 -ff2c

#FFLAGS = -g -pc 64 -r8 -O2 -g77libs -Mdollar,x -Minform,inform -Mbounds -Mdalign -Mdepchk -Msecond_underscore
#FFLAGS += -pc 64 -r8
# compatibility with g77
#FFLAGS += -g77libs -Msecond_underscore
# optimization level(default = 2)
# FFLAGS += -O0
# debugging information
# FFLAGS += -g
# FFLAGS += -Mbounds
# compiler checks
# FFLAGS += -Minform,inform
# declaration of variables
# FFLAGS += -Mdclchk
# substitution of dollar
#FFLAGS += -Mdollar,x
# trap run-time errors
# FFLAGS += -Ktrap=fp -Mfptrap

OBJS = \
	DD_global.o \
	DD_aux.o \
	DD_2pt.o \
	DD_3pt.o \
	DD_4pt.o \
	DD_5pt.o \
	DD_6pt.o 

# Commands:

#all: DD.a

%.o: %.F90 
	$(FC) $(FFLAGS) -c -o $@ $<

%.o: %.F 
	$(FC) $(FFLAGS) $(PREPRFLAGS) -c -o $@ $<

.f.o:
	$(FC) -c $(FFLAGS) $*.f 

DD.a: $(OBJS)
	$(AR) r libDD.a $(OBJS)

clean:
	rm -f *.o DD.out libDD.a 

