# compiler flags:
CFLAGS  = $(shell root-config --cflags)
LIBS    = $(shell root-config --evelibs) -lGenVector
 
# The build target
TARGET = convert_SL2LHE
 
all: $(TARGET)
 
$(TARGET): $(TARGET).cpp
	$(CXX) $(CFLAGS) $(LIBS) -o $(TARGET) $(TARGET).cpp
 
clean:
	rm -f $(TARGET)
