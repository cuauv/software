.PHONY: clean

TARGET=test
TESTS=vision
OBJS=$(TESTS:=_test.o)
CFLAGS=-std=c++11
LDFLAGS=-L../../link-stage -lsimulator2 -lshm

CFLAGS += `pkg-config --cflags eigen3`
LDFLAGS += `pkg-config --libs eigen3`

.cpp.o:
	g++ $(CFLAGS) -c $<

$(TARGET): $(OBJS)
	g++ $(LDFLAGS) $(OBJS) -o $@

clean:
	rm -rf *.o
	rm -rf $(TARGET)
