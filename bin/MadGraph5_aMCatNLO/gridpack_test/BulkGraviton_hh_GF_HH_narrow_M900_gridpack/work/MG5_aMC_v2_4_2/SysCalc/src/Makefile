COPTS = -O -DDROP_CGAL
INCLUDES = -I../include -I/cvmfs/cms.cern.ch/slc6_amd64_gcc481/external/boost/1.51.0-cms2/include -I$(shell lhapdf-config --incdir)
OBJECTS = SysCalc.o tinyxml2.o alfas_functions.o
LIBS = $(shell lhapdf-config --ldflags)

.cc.o:
	$(CXX) $(COPTS) $(INCLUDES) -c $<

.cpp.o:
	$(CXX) $(COPTS) $(INCLUDES) -c $<

all: ../sys_calc

SysCalc.o: ../include/SysCalc.h ../include/tinyxml2.h alfas_functions.o
sys_calc.o: ../include/SysCalc.h ../include/tinyxml2.h

../sys_calc: $(OBJECTS) sys_calc.o 
	$(CXX) -o $@ $^ $(LIBS) -lgfortran

alfas_functions.o: alfas_functions.f
	gfortran -c alfas_functions.f

clean:
	rm -f ../sys_calc *.o