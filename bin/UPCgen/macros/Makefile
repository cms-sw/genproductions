# compiler flags:
CFLAGS  = $(shell root-config --cflags)
LIBS    = -L$(shell root-config --libdir) -lCore -lRIO -lTreePlayer -lGenVector

# The build target
TARGET = convert_UGHEPMC2LHE
 
all: $(TARGET)
 
$(TARGET): $(TARGET).cpp
	$(CXX) $(CFLAGS) $(LIBS) -o $(TARGET) $(TARGET).cpp
 
clean:
	rm -f $(TARGET)
