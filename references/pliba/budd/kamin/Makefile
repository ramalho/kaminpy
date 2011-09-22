####################################################################### 
#      Makefile for use with C++ kamin interpreters
# #######################################################################
CC = gcc
CXX = g++
CFLAGS=
LIBS = -lg++
#     The following two rules add automaticity to c++ suffixes
.SUFFIXES: .cc .C
.cc.o:
	$(CXX) $(CFLAGS) -c  $<
.C.o:
	$(CXX) $(CFLAGS) -c $<
#######################################################################
ifiles = main.o reader.o expression.o list.o function.o environment.o lisp.o

chap1: $(ifiles) chap1.o
	g++ -o chap1 $(ifiles) chap1.o

chap2: $(ifiles) chap2.o
	g++ -o chap2 $(ifiles) chap2.o

chap3: $(ifiles) chap3.o
	g++ -o chap3 $(ifiles) chap3.o

chap4: $(ifiles) chap4.o
	g++ -o chap4 $(ifiles) chap4.o

chap5: $(ifiles) chap5.o
	g++ -o chap5 $(ifiles) chap5.o

chap6: $(ifiles) chap6.o
	g++ -o chap6 $(ifiles) chap6.o

chap7: $(ifiles) chap7.o
	g++ -o chap7 $(ifiles) chap7.o

chap8: $(ifiles) chap8.o
	g++ -o chap8 $(ifiles) chap8.o

package:
	packmail -o'kamin' Makefile *.tex *.sty *.cc *.h 
