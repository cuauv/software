g++ -c -std=c++11 -fpic aslam.cpp -D__CTYPES_
g++ -shared -o libaslam.so aslam.o
g++ -c -std=c++11 -g aslam.cpp # -fpermissive
g++ -std=c++11 aslamt.cpp -o aslamt.o aslam.o
g++ -std=c++11 aslamv.cpp -o aslamv aslam.o -lGL -lglfw
g++ -std=c++11 aslamd.cpp -o aslamd aslam.o
