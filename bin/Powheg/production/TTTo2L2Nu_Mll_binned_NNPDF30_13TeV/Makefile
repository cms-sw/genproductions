 INCDIR = $(shell  root-config --incdir)
 
 #CMSSW_RELEASE_BASE_NOPATCH:=$(shell echo $(CMSSW_RELEASE_BASE) | sed -e 's/-patch//' -e 's/_patch.//')
 #CMSSW_INC:=-I$(CMSSW_RELEASE_BASE)/src -I$(CMSSW_BASE)/src
 #CMSSW_LIBS:=-L$(CMSSW_BASE)/lib/$(SCRAM_ARCH) -L$(CMSSW_RELEASE_BASE)/lib/$(SCRAM_ARCH) -L$(CMSSW_RELEASE_BASE)/external/$(SCRAM_ARCH)/lib -L$(CMSSW_RELEASE_BASE_NOPATCH)/lib/$(SCRAM_ARCH)
 ROOTGLIBS    := $(shell root-config --glibs)
 ROOTGLIBS   += -lX3d
 
 LIBS = $(ROOTGLIBS)
 #LIBS += $(CMSSW_LIBS) -lCondFormatsJetMETObjects -lPhysicsToolsUtilities



CXXFLAGS = -g  --exceptions  -I$(ROOTSYS)/include $(CMSSW_INC) -std=gnu++11

CPP    = g++

SRC= LHE_filter.cpp

#=====================================================================

%.o: %.cpp
		$(CPP) $(CXXFLAGS) -c -Wno-deprecated $<

all: test
test: Run
#==================

Run.o: Run.cpp 
#		$(CPP) $(CXXFLAGS) -c Run.cpp -Wno-deprecated
Run: $(SRC:.cpp=.o)
		$(CPP) $(CXXFLAGS) $(SRC:.cpp=.o) $(LIBS) -o LHE_filter -O3 -Wall -fPIC
		@echo
		@echo ' --> LHE_filter created'
		@echo

#====================================================================

clean:
		@echo "rm -f LHE_filter  *~  *.o"
		@rm -f *~ *.o
		@rm -f LHE_filter
		@echo ' --> clean'
