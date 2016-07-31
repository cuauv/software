FROM pritunl/archlinux:latest
MAINTAINER Daryl Sew "darylsew@gmail.com"

# usage: 
# docker build -t darylsew/auv .
# docker run -i -t darylsew/auv /bin/bash

# Add repo source
RUN mkdir /root/software
ADD . /root/software

# Set up environment variables
# From the old days.
ENV software_path="/root/software"
## Used by a lot of software. Need the trailing slash! Use it in your programs too.
#export CUAUV_SOFTWARE=$software_path/
ENV CUAUV_SOFTWARE=$software_path/
## So you can use the binaries we build easily.                                              
#export PATH=$software_path/link-stage:$PATH
ENV PATH=$software_path/link-stage:$PATH
## Warning! If $LD_LIBRARY_PATH is previously empty, you should not have the :$LD_LIBRARY_PATH at the end.
## Some software doesn't like that.
## Also some say this is not recommended, but the OCaml bindings & Peacock (Guile) need it for now.
#export LD_LIBRARY_PATH=$software_path/link-stage:$LD_LIBRARY_PATH
ENV LD_LIBRARY_PATH=$software_path/link-stage:$LD_LIBRARY_PATH
## Ditto.
#export LDFLAGS=-L$software_path/link-stage
ENV LDFLAGS=-L$software_path/link-stage
## Tell GCC where includes might (so you can use 'absolute' paths).
#export CPLUS_INCLUDE_PATH=$software_path
ENV CPLUS_INCLUDE_PATH=$software_path
#export C_INCLUDE_PATH=$software_path
ENV C_INCLUDE_PATH=$software_path
## So Python will let you do import shm, etc.
#export PYTHONPATH=$software_path
ENV PYTHONPATH=$software_path:$PYTHONPATH
## So you can use Go packages in gocode.
#export GOPATH=$software_path/gocode:$GOPATH
ENV GOPATH=$software_path/gocode:$GOPATH
## So Go will be able to use libshm.
#export CGO_LDFLAGS=-L$software_path/link-stage
ENV CGO_LDFLAGS=-L$software_path/link-stage
#export CSC_OPTIONS="-L$CUAUV_SOFTWARE/link-stage -C -I$CUAUV_SOFTWARE -I$CUAUV_SOFTWARE"
ENV CSC_OPTIONS="-L$CUAUV_SOFTWARE/link-stage -C -I$CUAUV_SOFTWARE -I$CUAUV_SOFTWARE"
#export PROMPT_COLOR=red
ENV PROMPT_COLOR=red
## Like PYTHONPATH, but for Guile.
#export GUILE_LOAD_PATH="$CUAUV_SOFTWARE/guile"
ENV GUILE_LOAD_PATH="$CUAUV_SOFTWARE/guile"

#RUN alias build="ninja -C /root/software"

RUN pacman -Syy
# XXX don't comment this one
RUN pacman -Syu --noconfirm

# TODO alphabetize and multiline
RUN pacman -S --noconfirm base-devel git openssh cmake ninja eigen libdc1394 ffmpeg python python-numpy python-scipy python-scikit-learn python-pip opencv python2 stack chicken glm ghc gtk3

RUN stack setup

#RUN cd /root/software; ./configure.py
#RUN mkdir root/sources
#RUN git clone https://github.com/Itseez/opencv.git root/sources/opencv
#RUN mkdir root/sources/opencv/build
#RUN cd root/sources/opencv/build; cmake -D CMAKE_BUILD_TYPE=RELEASE -D CMAKE_INSTALL_PREFIX=/usr/local ..; make -j8; make install

#RUN cd sources/opencv/build; cmake -D WITH_IPP=ON -D CMAKE_INSTALL_PREFIX=/usr/local ..; make -j8 && make install
#RUN ln -s /usr/share/OpenCV/3rdparty/lib/libippicv.a /usr/lib/libippicv.a
