CXX := clang++
CXXFLAGS := --std=c++1z -g



all:


clean:
	-rm *.o *.exe

%.exe : %.o
	${CXX} -o $@ ${CXXFLAGS} $<

gmp_test1: gmp_test1.cpp
	${CXX} ${CXXFLAGS} -o $@ $^ -lgmpxx -lgmp
