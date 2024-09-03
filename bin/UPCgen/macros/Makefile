# compiler flags:
CFLAGS = $(shell root-config --cflags)
LIBS   = -L$(shell root-config --libdir) -lCore -lRIO -lTreePlayer

# The build target
TARGET = convert_UGHEPMC2LHE
 
all: $(TARGET)

convert_UGHEPMC2LHE: %: %.cpp
	$(CXX) $(CFLAGS) -o $@ $<

convert_UGROOT2LHE: %: %.cpp
	$(CXX) $(CFLAGS) $(LIBS) -o $@ $<
 
clean:
	rm -f $(TARGET)
