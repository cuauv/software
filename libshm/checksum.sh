#!/usr/bin/env bash

sum=`cat $* | sha1sum | cut -d' ' -f1`

echo \#ifndef SHM_CHECKSUM_H > libshm/c/checksum.h.new
echo \#define SHM_CHECKSUM_H >> libshm/c/checksum.h.new
echo \#define SHM_CHECKSUM \"$sum\" >> libshm/c/checksum.h.new
echo \#endif  // SHM_CHECKSUM_H >> libshm/c/checksum.h.new

if cmp -s libshm/c/checksum.h.new libshm/c/checksum.h; then
    rm libshm/c/checksum.h.new
else
    mv libshm/c/checksum.h.new libshm/c/checksum.h
fi
