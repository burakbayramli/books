# Compiler and Flags
CXX      := g++
CXXFLAGS := -std=c++20 -Wall -Wextra
LDFLAGS  := 

# Build Configurations
# Use 'make' for Release (default) or 'make DEBUG=1' for Debug
ifeq ($(DEBUG), 1)
    CXXFLAGS += -g -O0 -D_DEBUG
    TARGET   := DSMC2D_debug.exe
else
    CXXFLAGS += -O3 -DNDEBUG
    TARGET   := DSMC2D.exe
endif

# Definitions from vcxproj
DEFS     := -D_CONSOLE -D_LIB

# Source Files
# Only DSMC2D_Template08.cpp is set to build in your vcxproj.
# Add other .cpp or .cxx files here as needed.
SRCS     := DSMC2D_Template08.cpp
OBJS     := $(SRCS:.cpp=.o)
OBJS     := $(OBJS:.cxx=.o)

# Build Rules
all: $(TARGET)

$(TARGET): $(OBJS)
	$(CXX) $(CXXFLAGS) $(OBJS) -o $(TARGET) $(LDFLAGS)

%.o: %.cpp
	$(CXX) $(CXXFLAGS) $(DEFS) -c $< -o $@

%.o: %.cxx
	$(CXX) $(CXXFLAGS) $(DEFS) -c $< -o $@

clean:
	rm -f $(OBJS) $(TARGET)

.PHONY: all clean
