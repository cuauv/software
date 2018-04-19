#!/usr/bin/env python2

def test(stmnt, r, n):
    print "\n", stmnt

    #old = Timer(stmnt,'from auval.shmem import SharedVar;var=SharedVar("/desires/heading")')
    new = Timer(stmnt,'from shm import desires;var=desires.heading')

    #ores=old.repeat(r,n)
    #oavg=sum(ores)/r
    #print "Old: ", oavg

    nres=new.repeat(r,n)
    navg=sum(nres)/r
    print "New: ",navg




from timeit import Timer

r=3
n=10000

vget='var.get()'
vset='var.set(5.0)'
both='var.set(5.0);var.get()'

test(vget, r, n)
test(vset, r, n)
test(both, r, n)
