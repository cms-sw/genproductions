# compiler flags:
CFLAGS  = $(shell root-config --cflags)

# The build target
TARGET = convert_UG2LHE
 
all: $(TARGET)
 
$(TARGET): $(TARGET).cpp
	$(CXX) $(CFLAGS) -o $(TARGET) $(TARGET).cpp
 
clean:
	rm -f $(TARGET)
