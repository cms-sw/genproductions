#-*- Makefile -*-
## Choose compiler: gfortran,ifort (g77 not supported, F90 constructs in use!)
COMPILER=gfortran
FC=$(COMPILER)
## Choose PDF: native,lhapdf
## LHAPDF package has to be installed separately
PDF=lhapdf
#PDF=hoppet
## ANALYSIS: none, BnJ, HnJ, alt, NNLOPS
##           the first 4 analyses require the FASTJET package, that has to be 
##           installed separately (see below)
ANALYSIS=MiNNLO

## For static linking uncomment the following
#STATIC= -static
#

V2DIR=$(shell (cd ../.. ; pwd))
STDCLIB=-lstdc++
#STDCLIB=-lc++

stdclib=$(STDCLIB)

ifeq ("$(COMPILER)","gfortran")	
F77=gfortran -fno-automatic -ffixed-line-length-none -ffree-line-length-none -fbounds-check -fno-align-commons
# -finit-real=nan -ffpe-trap=invalid,zero,overflow
## -fbounds-check sometimes causes a weird error due to non-lazy evaluation
## of boolean in gfortran.
#FFLAGS= -Wall -Wimplicit-interface -fbounds-check
## For floating point exception trapping  uncomment the following 
#FPE=-ffpe-trap=invalid,zero,overflow
#,underflow 
## gfortran 4.4.1 optimized with -O3 yields erroneous results
## Use -O2 to be on the safe side
OPT=-O3
## For debugging uncomment the following
#DEBUG= -ggdb -pg
endif

ifeq ("$(COMPILER)","g77")
F77= g77 -fno-automatic 
#FFLAGS= -Wall -ffortran-bounds-check
## For floating point exception trapping  uncomment the following 
#FPEOBJ=trapfpe.o
OPT=-O3
## For debugging uncomment the following
#DEBUG= -ggdb -pg
endif

CXX = gcc

ifeq ("$(COMPILER)","ifort")
F77 = ifort
#CXX = icpc
#LIBS = -limf
#FFLAGS =  -check
## For floating point exception trapping  uncomment the following 
#FPE = -fpe0
#OPT = -O3 #-fast
## For debugging uncomment the following
#DEBUG= -debug -g
endif

ifdef DEBUG
#FPE=-ffpe-trap=invalid,zero,overflow
#,underflow
OPT=-O0
endif

PWD=$(shell pwd)
OBJ=$(PWD)/obj-$(COMPILER)
WDNAME=$(shell basename $(PWD))

Wj=$(V2DIR)/Wj
PLUGINSDIR=$(V2DIR)/MiNNLOStuff
PLUGINSDIRINC=$(PLUGINSDIR)/include
LOCALHOPPET=$(PLUGINSDIR)/hoppet
VPATH=.:$(PLUGINSDIR):$(LOCALHOPPET):$(Wj):$(Wj)/Madlib:$(Wj)/MODEL:$(Wj)/DHELAS:$(V2DIR):$(OBJ)/



INCLUDE0=$(Wj)
INCLUDE1=$(V2DIR)/include
FF=$(F77) $(FFLAGS) $(FPE) $(OPT) $(DEBUG) -I$(PWD) -I$(PLUGINSDIRINC) -I$(INCLUDE0) -I$(INCLUDE1) -J$(OBJ)

#LIBFILES=$(shell  for dir in $(Wj)/Madlib $(Wj)/MODEL $(Wj)/DHELAS ; do cd $$dir ; echo *.[fF] ' ' | sed 's/[fF] /o /g' ; cd .. ; done  )

INCLUDE =$(wildcard ../include/*.h *.h include/*.h)

ifeq ("$(PDF)","lhapdf")
#LHAPDF_CONFIG=~/Pheno/PDFpacks/lhapdf-5.8.4-$(FC)/bin/lhapdf-config
LHAPDF_CONFIG=lhapdf-config
PDFPACK=lhapdf6if.o lhapdf6ifcc.o
FJCXXFLAGS+= $(shell $(LHAPDF_CONFIG) --cxxflags)
LIBSLHAPDF= -Wl,-rpath,$(shell $(LHAPDF_CONFIG) --libdir)  -L$(shell $(LHAPDF_CONFIG) --libdir) -lLHAPDF $(stdclib) -std=c++11
ifeq  ("$(STATIC)","-static") 
## If LHAPDF has been compiled with gfortran and you want to link it statically, you have to include
## libgfortran as well. The same holds for libc++. 
## One possible solution is to use fastjet, since $(shell $(FASTJET_CONFIG) --libs --plugins ) $(stdclib)
## does perform this inclusion. The path has to be set by the user. 
# LIBGFORTRANPATH= #/usr/lib/gcc/x86_64-redhat-linux/4.1.2
# LIBSTDCPP=/lib64
# LIBSLHAPDF+=  -L$(LIBGFORTRANPATH)  -lgfortranbegin -lgfortran -L$(LIBSTDCPP) $(stdclib)
endif
LIBS+=$(LIBSLHAPDF)
else
PDFPACK=mlmpdfif.o hvqpdfpho.o
endif

ifeq ("$(PDF)","hoppet")
LHAPDF_CONFIG=lhapdf-config
PDFPACK=hoppetif.o lhapdf6ifcc.o
FJCXXFLAGS+= $(shell $(LHAPDF_CONFIG) --cxxflags)
LIBSLHAPDF= -Wl,-rpath,$(shell $(LHAPDF_CONFIG) --libdir)  -L$(shell $(LHAPDF_CONFIG) --libdir) -lLHAPDF $(stdclib) -std=c++11
LIBS+=$(LIBSLHAPDF)
endif

ifeq ("$(ANALYSIS)","HnJ")
##To include Fastjet configuration uncomment the following lines. 
FASTJET_CONFIG=$(shell which fastjet-config)
#FASTJET_CONFIG=~/lib/fastjet242/bin/fastjet-config
LIBSFASTJET += $(shell $(FASTJET_CONFIG) --libs --plugins ) $(stdclib)
FJCXXFLAGS+= $(shell $(FASTJET_CONFIG) --cxxflags)
PWHGANAL=pwhg_bookhist-multi.o pwhg_analysis-HnJ.o fastjetfortran.o
## Also add required Fastjet drivers to PWHGANAL (examples are reported)#PWHGANAL+= fastjetsisconewrap.o fastjetktwrap.o fastjetCDFMidPointwrap.o fastjetD0RunIIConewrap.o fastjetfortran.o
endif

ifeq ("$(ANALYSIS)","dummy")
##To include Fastjet configuration uncomment the following lines. 
#FASTJET_CONFIG=$(shell which fastjet-config)
#FASTJET_CONFIG=~/lib/fastjet242/bin/fastjet-config
#LIBSFASTJET += $(shell $(FASTJET_CONFIG) --libs --plugins ) $(stdclib)
#FJCXXFLAGS+= $(shell $(FASTJET_CONFIG) --cxxflags)
#PWHGANAL=pwhg_bookhist-multi.o pwhg_analysis-NZ.o #fastjetfortran.o
PWHGANAL=pwhg_bookhist-multi.o pwhg_analysis-dummy.o  #fastjetfortran.o
LIBSFASTJET = $(stdclib) 
## Also add required Fastjet drivers to PWHGANAL (examples are reported)#PWHGANAL+= fastjetsisconewrap.o fastjetktwrap.o fastjetCDFMidPointwrap.o fastjetD0RunIIConewrap.o fastjetfortran.o
endif

ifeq ("$(ANALYSIS)","NZ")
##To include Fastjet configuration uncomment the following lines. 
FASTJET_CONFIG=$(shell which fastjet-config)
#FASTJET_CONFIG=~/lib/fastjet242/bin/fastjet-config
LIBSFASTJET += $(shell $(FASTJET_CONFIG) --libs --plugins ) $(stdclib)
FJCXXFLAGS+= $(shell $(FASTJET_CONFIG) --cxxflags)
PWHGANAL=pwhg_bookhist-multi.o pwhg_analysis-NZ.o fastjetfortran.o
## Also add required Fastjet drivers to PWHGANAL (examples are reported)#PWHGANAL+= fastjetsisconewrap.o fastjetktwrap.o fastjetCDFMidPointwrap.o fastjetD0RunIIConewrap.o fastjetfortran.o
endif

ifeq ("$(ANALYSIS)","tmp")
##To include Fastjet configuration uncomment the following lines. 
FASTJET_CONFIG=$(shell which fastjet-config)
#FASTJET_CONFIG=~/lib/fastjet242/bin/fastjet-config
LIBSFASTJET += $(shell $(FASTJET_CONFIG) --libs --plugins ) $(stdclib)
FJCXXFLAGS+= $(shell $(FASTJET_CONFIG) --cxxflags)
PWHGANAL=pwhg_bookhist-multi.o pwhg_analysis-tmp.o fastjetfortran.o
## Also add required Fastjet drivers to PWHGANAL (examples are reported)#PWHGANAL+= fastjetsisconewrap.o fastjetktwrap.o fastjetCDFMidPointwrap.o fastjetD0RunIIConewrap.o fastjetfortran.o
endif

ifeq ("$(ANALYSIS)","fintest")
##To include Fastjet configuration uncomment the following lines. 
FASTJET_CONFIG=$(shell which fastjet-config)
#FASTJET_CONFIG=~/lib/fastjet242/bin/fastjet-config
LIBSFASTJET += $(shell $(FASTJET_CONFIG) --libs --plugins ) $(stdclib)
FJCXXFLAGS+= $(shell $(FASTJET_CONFIG) --cxxflags)
PWHGANAL=pwhg_bookhist-multi.o pwhg_analysis-fintest.o fastjetfortran.o
## Also add required Fastjet drivers to PWHGANAL (examples are reported)#PWHGANAL+= fastjetsisconewrap.o fastjetktwrap.o fastjetCDFMidPointwrap.o fastjetD0RunIIConewrap.o fastjetfortran.o
endif



ifeq ("$(ANALYSIS)","BnJ")
##To include Fastjet configuration uncomment the following lines. 
FASTJET_CONFIG=$(shell which fastjet-config)
#FASTJET_CONFIG=~/lib/fastjet242/bin/fastjet-config
LIBSFASTJET += $(shell $(FASTJET_CONFIG) --libs --plugins ) $(stdclib)
FJCXXFLAGS+= $(shell $(FASTJET_CONFIG) --cxxflags)
PWHGANAL=pwhg_bookhist-multi.o pwhg_analysis-BnJ.o fastjetfortran.o
## Also add required Fastjet drivers to PWHGANAL (examples are reported)#PWHGANAL+= fastjetsisconewrap.o fastjetktwrap.o fastjetCDFMidPointwrap.o fastjetD0RunIIConewrap.o fastjetfortran.o
endif

ifeq ("$(ANALYSIS)","alt")
##To include Fastjet configuration uncomment the following lines. 
FASTJET_CONFIG=$(shell which fastjet-config)
#FASTJET_CONFIG=~/lib/fastjet242/bin/fastjet-config
LIBSFASTJET += $(shell $(FASTJET_CONFIG) --libs --plugins ) $(stdclib)
FJCXXFLAGS+= $(shell $(FASTJET_CONFIG) --cxxflags)
PWHGANAL=pwhg_bookhist-multi.o pwhg_analysis-alt-H2J.o fastjetfortran.o
## Also add required Fastjet drivers to PWHGANAL (examples are reported)#PWHGANAL+= fastjetsisconewrap.o fastjetktwrap.o fastjetCDFMidPointwrap.o fastjetD0RunIIConewrap.o fastjetfortran.o
endif

ifeq ("$(ANALYSIS)","NNLOPS")
##To include Fastjet configuration uncomment the following lines. 
FASTJET_CONFIG=$(shell which fastjet-config)
#FASTJET_CONFIG=~/lib/fastjet242/bin/fastjet-config
LIBSFASTJET += $(shell $(FASTJET_CONFIG) --libs --plugins ) $(stdclib)
FJCXXFLAGS+= $(shell $(FASTJET_CONFIG) --cxxflags)
PWHGANAL=pwhg_bookhist-multi.o pwhg_analysis-pheno_2.o fastjetfortran.o \
         genclust_kt.o miscclust.o ptyrap.o r.o swapjet.o jet_finder.o  \
         auxiliary.o get_hdamp.o
endif

ifeq ("$(ANALYSIS)","MiNNLO")
##To include Fastjet configuration uncomment the following lines. 
FASTJET_CONFIG=$(shell which fastjet-config)
#FASTJET_CONFIG=~/lib/fastjet242/bin/fastjet-config
LIBSFASTJET += $(shell $(FASTJET_CONFIG) --libs --plugins ) $(stdclib)
FJCXXFLAGS+= $(shell $(FASTJET_CONFIG) --cxxflags)
PWHGANAL=pwhg_bookhist-multi.o pwhg_analysis-minnlo.o #fastjetfortran.o
endif


# PYTHIA 8
PYTHIA_CONFIG=$(shell which pythia8-config)

FJCXXFLAGS+=-I$(shell $(PYTHIA_CONFIG) --includedir)
FJCXXFLAGS+=-I$(shell $(PYTHIA_CONFIG) --includedir)/Pythia8
FJCXXFLAGS+=-I$(shell $(PYTHIA_CONFIG) --includedir)/Pythia8Plugins

LIBPYTHIA8= -L$(shell $(PYTHIA_CONFIG) --libdir) -lpythia8 -ldl -lstdc++ # -lHepMC #-llhapdfdummy

# RIVET
##RIVET_CONFIG=$(shell which rivet-config)
##FJCXXFLAGS+=-I$(shell $(RIVET_CONFIG) --includedir)
##LIBPYTHIA8= -L$(shell $(PYTHIA_CONFIG) --libdir) -lpythia8 -L$(shell $(RIVET_CONFIG) --libdir) -lRivet -lYODA -lHepMC -ldl -lstdc++

LIBPYTHIA8= -L$(shell $(PYTHIA_CONFIG) --libdir) -lpythia8 -L$(shell $(RIVET_CONFIG) --libdir) -ldl -lstdc++

%.o: %.f90 $(INCLUDE)
	$(FF) -c -o $(OBJ)/$@ $<

%.o: %.f $(INCLUDE)
	$(FF) -c -o $(OBJ)/$@ $<


# %.o: %.F $(INCLUDE)
# 	$(FF) -c -o $(OBJ)/$@ $<

%.o: %.c
	$(CC) $(DEBUG) -c -o $(OBJ)/$@ $^ 

%.o: %.cc
	$(CXX) $(DEBUG) -c -o $(OBJ)/$@ $^ $(FJCXXFLAGS)
LIBS+=-lz

NNLOPS_PLUGINS =pdfs_tools.o rad_tools.o coefficient_functions_nnlops.o internal_parameters.o\
	sudakov_radiators.o frcgauss.o

HOPPET= hplog.o xpij2e.o xpns2e.o assertions.o coefficient_functions.o \
	convolution.o dglap_choices.o dglap_holders.o dglap_objects.o \
	evolution.o hoppet_v1.o integrator_hoppet.o interpolation.o new_as.o \
	pdf_general.o pdf_representation.o pdf_tabulate.o qcd.o \
	qcd_coupling.o random_hoppet.o runge_kutta.o sort.o special_functions.o \
	splitting_functions.o splitting_functions_nnlo.o types.o \
	warnings_and_errors.o welcome_message.o xa2hgp.o xpij2n.o xpij2p.o \
	xpns2n.o xpns2p.o

USER=init_couplings.o init_processes.o Born_phsp.o Born.o virtual.o \
        real.o InverseISRMapping.o ProjKinematics.o $(PWHGANAL) \
        brphasespace.o sigd3term.o $(NNLOPS_PLUGINS) $(HOPPET)
# notice that last file is needed for Wj_distribute option


PWHG=pwhg_main.o pwhg_init.o bbinit.o btilde.o lhefwrite.o  		\
	LesHouches.o LesHouchesreg.o gen_Born_phsp.o find_regions.o	\
	test_Sudakov.o pt2maxreg.o sigborn.o gen_real_phsp.o maxrat.o	\
	gen_index.o gen_radiation.o Bornzerodamp.o sigremnants.o	\
	random.o boostrot.o bra_ket_subroutines.o cernroutines.o	\
	init_phys.o powheginput.o pdfcalls.o sigreal.o sigcollremn.o	\
	pwhg_analysis_driver.o checkmomzero.o		\
	setstrongcoupl.o integrator.o newunit.o mwarn.o sigsoftvirt.o	\
	reshufflemoms.o    \
	sigcollsoft.o sigvirtual.o validflav.o mint_upb.o  \
	pwhgreweight.o opencount.o ubprojections.o minlo_checks.o \
        setlocalscales.o  process_dependent_minnlo.o $(PDFPACK) $(USER) $(FPEOBJ) \
        lhefread.o pwhg_io_interface.o rwl_weightlists.o rwl_setup_param_weights.o


#LIBDIRMG=.
#LINKMGLIBS =  -L$(LIBDIRMG)  -lmadgraph -lmodel -ldhelas3 

#MADLIBS=libdhelas3.a libmadgraph.a libmodel.a

# Get SVN info for SVN version stamping code
$(shell $(V2DIR)/svnversion/svnversion.sh $(PWD) $(Wj) $(V2DIR) >/dev/null)



# target to generate LHEF output
pwhg_main:$(PWHG)
	$(FF) $(patsubst %,$(OBJ)/%,$(PWHG))  $(LIBSFASTJET) $(LIBS) $(STATIC) -o $@



# libfiles.a: $(LIBFILES)
# 	cd $(OBJ) ; \rm libfiles.a ; ar cru libfiles.a $(LIBFILES)

LHEF=lhef_analysis.o boostrot.o random.o cernroutines.o		\
     opencount.o powheginput.o $(PWHGANAL)	\
     lhefread.o pwhg_io_interface.o rwl_weightlists.o newunit.o pwhg_analysis_driver.o bra_ket_subroutines.o $(FPEOBJ)

# target to analyze LHEF output
lhef_analysis:$(LHEF)
	$(FF) $(patsubst %,$(OBJ)/%,$(LHEF)) $(LIBS) $(LIBSFASTJET) $(STATIC)  -o $@ 



# target to read event file, shower events with HERWIG + analysis
HERWIG=main-HERWIG.o setup-HERWIG-lhef.o herwig.o boostrot.o	\
	powheginput.o $(PWHGANAL) lhefread.o pwhg_io_interface.o rwl_weightlists.o	\
	pdfdummies.o opencount.o $(FPEOBJ) 

main-HERWIG-lhef: $(HERWIG)
	$(FF) $(patsubst %,$(OBJ)/%,$(HERWIG))  $(LIBSFASTJET)  $(STATIC) -o $@

# target to read event file, shower events with PYTHIA + analysis
PYTHIA=main-PYTHIA.o setup-PYTHIA-lhef.o pythia.o boostrot.o powheginput.o		\
	$(PWHGANAL) lhefread.o pwhg_io_interface.o rwl_weightlists.o newunit.o pdfdummies.o  bra_ket_subroutines.o \
	pwhg_analysis_driver.o random.o cernroutines.o opencount.o	\
	$(FPEOBJ)

main-PYTHIA-lhef: $(PYTHIA)
	$(FF) $(patsubst %,$(OBJ)/%,$(PYTHIA)) $(LIBSFASTJET)  $(STATIC) -o $@


DELTASUD=integral-deltasud.o pdfcalls.o cernroutines.o newunit.o \
         pwhg_bookhist-multi.o random.o  $(PDFPACK)

deltasud: $(DELTASUD)
	$(FF)  $(patsubst %,$(OBJ)/%,$(DELTASUD)) $(LIBS) -o $@



# target to read event file, shower events with PYTHIA8 + analysis
PYTHIA8=main-PYTHIA8.o pythia8F77.o boostrot.o powheginput.o \
	$(PWHGANAL) opencount.o lhefread.o pwhg_io_interface.o newunit.o pdfdummies.o \
	random.o cernroutines.o bra_ket_subroutines.o utils.o \
	$(FPEOBJ) \
	rwl_weightlists.o $(LIBZDUMMY) 

#  

main-PYTHIA8-lhef: $(PYTHIA8)
	@echo "compiling pythia8"
	@echo $(PYTHIA_CONFIG)
	$(FF) $(patsubst %,$(OBJ)/%,$(PYTHIA8)) $(LIBSFASTJET) $(LIBPYTHIA8) $(STATIC) $(LIBS) -o $@








# target to cleanup
.PHONY: clean veryclean $(OBJ)

$(OBJ):
	if ! [ -d $(OBJ) ] ; then mkdir $(OBJ) ; fi

clean:
	rm -f pwhg_main lhef_analysis main-HERWIG-lhef main-PYTHIA-lhef ; \
        cd $(OBJ) ; rm -f $(PWHG) $(HERWIG) $(PYTHIA) $(LHEF) $(NNLOPSREWEIGHTER)  $(NNLOPSREWEIGHTERNRW) 


veryclean:
	cd $(OBJ) ; \rm *

# Dependencies of SVN version stamp code
#pwhg_main.o: svn.version
#lhefwrite.o: svn.version

ifeq ("$(COMPILER)","gfortran")
XFFLAGS +=-ffixed-line-length-132
else
XFFLAGS +=-extend-source
endif


rad_tools.o: hoppet_v1.o internal_parameters.o
setlocalscales.o: rad_tools.o pdfs_tools.o sudakov_radiators.o
process_dependent_minnlo.o: internal_parameters.o
internal_parameters.o: hoppet_v1.o
coefficient_functions_nnlops.o: rad_tools.o internal_parameters.o
pdfs_tools.o: rad_tools.o internal_parameters.o coefficient_functions_nnlops.o
driver.o: rad_tools.o pdfs_tools.o internal_parameters.o sudakov_radiators.o
sudakov_radiators.o: rad_tools.o frcgauss.o setstrongcoupl.o
frcgauss.o:


xpij2e.o: qcd.o
xpns2e.o: qcd.o
assertions.o: types.o
coefficient_functions.o: convolution.o qcd.o types.o
convolution.o: assertions.o integrator_hoppet.o interpolation.o sort.o types.o \
	warnings_and_errors.o
dglap_holders.o: assertions.o coefficient_functions.o convolution.o \
	dglap_choices.o dglap_objects.o pdf_representation.o qcd.o types.o \
	warnings_and_errors.o
dglap_objects.o: assertions.o convolution.o dglap_choices.o \
	pdf_representation.o qcd.o splitting_functions.o types.o \
	warnings_and_errors.o
evolution.o: assertions.o convolution.o dglap_choices.o dglap_holders.o \
	dglap_objects.o pdf_representation.o qcd.o qcd_coupling.o \
	runge_kutta.o types.o warnings_and_errors.o
hoppet_v1.o: convolution.o dglap_choices.o dglap_holders.o dglap_objects.o \
	evolution.o pdf_general.o pdf_representation.o pdf_tabulate.o qcd.o \
	qcd_coupling.o types.o warnings_and_errors.o
integrator_hoppet.o: types.o
interpolation.o: types.o warnings_and_errors.o
new_as.o: assertions.o qcd.o runge_kutta.o types.o warnings_and_errors.o
pdf_general.o: convolution.o pdf_representation.o types.o
pdf_representation.o: assertions.o random_hoppet.o types.o warnings_and_errors.o
pdf_tabulate.o: convolution.o dglap_holders.o dglap_objects.o evolution.o \
	interpolation.o pdf_general.o pdf_representation.o qcd_coupling.o \
	types.o warnings_and_errors.o
qcd.o: types.o
qcd_coupling.o: assertions.o new_as.o types.o warnings_and_errors.o
random_hoppet.o: types.o
runge_kutta.o: types.o
sort.o: assertions.o types.o warnings_and_errors.o
special_functions.o: types.o
splitting_functions.o: coefficient_functions.o convolution.o qcd.o \
	special_functions.o splitting_functions_nnlo.o types.o \
	warnings_and_errors.o
splitting_functions_nnlo.o: convolution.o dglap_choices.o qcd.o types.o \
	warnings_and_errors.o xpij2e.o xpij2n.o xpij2p.o xpns2e.o xpns2n.o \
	xpns2p.o
types.o: 
warnings_and_errors.o: types.o
