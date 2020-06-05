.POSIX:
.SUFFIXES:

FC       = gfortran
FFLAGS   = -fmax-errors=1 -Wall -Wno-maybe-uninitialized \
           -shared -fPIC
PREFIX   = /usr/local
LDFLAGS  = -I$(PREFIX)/include/ -L$(PREFIX)/lib/
LDLIBS   = -ldl
LAPACK95 = -llapack95 -llapack -lblas -ltmglib

NGX_LINK_FUNC = ngx_link_func.o

EXAMPLES = examples
HELLO    = hello
LAAS     = laas
POST     = post

.PHONY: all clean $(HELLO) $(LAAS) $(POST)

all: $(NGX_LINK_FUNC) $(HELLO) $(LAAS) $(POST)

$(NGX_LINK_FUNC):
	$(FC) $(FFLAGS) -c src/ngx_link_func.f90

$(HELLO): $(NGX_LINK_FUNC)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(HELLO).so $(EXAMPLES)/$(HELLO)/$(HELLO).f90 $(NGX_LINK_FUNC) $(LDLIBS)

$(LAAS): $(NGX_LINK_FUNC)
	$(FC) $(FFLAGS) -I$(PREFIX)/lib/lapack95_modules/  $(LDFLAGS) -o $(LAAS).so $(EXAMPLES)/$(LAAS)/$(LAAS).f90 $(NGX_LINK_FUNC) $(LDLIBS) $(LAPACK95)

$(POST): $(NGX_LINK_FUNC)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(POST).so $(EXAMPLES)/$(POST)/$(POST).f90 $(NGX_LINK_FUNC) $(LDLIBS)

clean:
	rm *.mod *.o *.so
