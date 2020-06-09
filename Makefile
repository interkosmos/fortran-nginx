.POSIX:
.SUFFIXES:

FC       = gfortran
FFLAGS   = -fmax-errors=1 -Wall -Wno-maybe-uninitialized \
           -shared -fPIC
PREFIX   = /usr/local
LDFLAGS  = -I$(PREFIX)/include/ -L$(PREFIX)/lib/
LDLIBS   = -ldl
DISLIN   = -lX11 -lXt -lXm -lGL
LAPACK95 = -llapack95 -llapack -lblas -ltmglib

NGX_LINK_FUNC = ngx_link_func.o

HELLO    = hello
LAAS     = laas
PLOT     = plot
POST     = post

.PHONY: all clean $(HELLO) $(LAAS) $(PLOT) $(POST)

all: $(NGX_LINK_FUNC)

$(NGX_LINK_FUNC):
	$(FC) $(FFLAGS) -c src/ngx_link_func.f90

$(HELLO): $(NGX_LINK_FUNC)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(HELLO).so examples/hello/hello.f90 $(NGX_LINK_FUNC) $(LDLIBS)

$(LAAS): $(NGX_LINK_FUNC)
	$(FC) $(FFLAGS) -I$(PREFIX)/lib/lapack95_modules/ $(LDFLAGS) -o $(LAAS).so \
		examples/laas/laas.f90 $(NGX_LINK_FUNC) $(LDLIBS) $(LAPACK95)

$(PLOT): $(NGX_LINK_FUNC)
	$(FC) $(FFLAGS) $(LDFLAGS) -I./examples/plot/dislin/ -o $(PLOT).so examples/plot/plot.f90 \
		examples/plot/dislin/dislin.o \
		examples/plot/dislin/dislin-11.3.a \
		$(NGX_LINK_FUNC) $(LDLIBS) $(DISLIN)

$(POST): $(NGX_LINK_FUNC)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(POST).so examples/post/post.f90 $(NGX_LINK_FUNC) $(LDLIBS)

clean:
	rm *.mod *.o *.so
