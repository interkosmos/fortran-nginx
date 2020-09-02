.POSIX:
.SUFFIXES:

FC       = gfortran
FFLAGS   = -fmax-errors=1 -Wall -Wno-maybe-uninitialized -shared -fPIC
PREFIX   = /usr/local
LDFLAGS  = -I$(PREFIX)/include/ -L$(PREFIX)/lib/
LDLIBS   = -ldl
DISLIN   = -lX11 -lXt -lXm -lGL
LAPACK95 = -llapack95 -llapack -lblas -ltmglib
TARGET   = libngx_link_func.a

HELLO    = hello
LAAS     = laas
PLOT     = plot
POST     = post

.PHONY: all clean $(HELLO) $(LAAS) $(PLOT) $(POST) $(TARGET)

all: $(TARGET)

$(TARGET):
	$(FC) $(FFLAGS) -c src/ngx_link_func.f90
	ar rcs $(TARGET) ngx_link_func.o

$(HELLO): $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(HELLO).so examples/hello/hello.f90 $(TARGET) $(LDLIBS)

$(LAAS): $(TARGET)
	$(FC) $(FFLAGS) -I$(PREFIX)/lib/lapack95_modules/ $(LDFLAGS) -o $(LAAS).so \
		examples/laas/laas.f90 $(TARGET) $(LDLIBS) $(LAPACK95)

$(PLOT): $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -I./examples/plot/dislin/ -o $(PLOT).so examples/plot/plot.f90 \
		examples/plot/dislin/dislin.o \
		examples/plot/dislin/dislin-11.3.a \
		$(TARGET) $(LDLIBS) $(DISLIN)

$(POST): $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(POST).so examples/post/post.f90 $(TARGET) $(LDLIBS)

clean:
	rm *.mod *.o *.so *.a
