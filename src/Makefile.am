## Process this file with automake to produce Makefile.in

bin_PROGRAMS = proas
proas_SOURCES = cambcoor.f90 chlower.f90 decimal.f90 dibuhoja.f90 dibuobje.f90 \
endprogram.f90 fj_ts0.f90 lrunx.f90 observat.f90 orcaso.f90 possol.f90 \
proas.f90 \
ranred.f90 readc.f90 readf.f90 readi.f90 readilim.f90 rmblank.f90 sexag.f90 \
tiempou.f90 traspasa.f90 truebeg.f90 truelen.f90 version.inc \
extras.f90

proas_LDADD=  $(PLOT_LIBS)
proas_FCFLAGS=  $(PLOT_CFLAGS)

EXTRA_DIST = proas.inc
