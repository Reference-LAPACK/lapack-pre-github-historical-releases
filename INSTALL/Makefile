include ../make.inc

all:  testlsame testslamch testdlamch testsecond testdsecnd testieee testversion 

testlsame:  lsame.o lsametst.o
	$(LOADER) $(LOADOPTS) -o testlsame lsame.o lsametst.o

testslamch: slamch.o lsame.o slamchtst.o
	$(LOADER) $(LOADOPTS) -o testslamch slamch.o lsame.o slamchtst.o

testdlamch: dlamch.o lsame.o dlamchtst.o
	$(LOADER) $(LOADOPTS) -o testdlamch dlamch.o lsame.o dlamchtst.o

testsecond: second.o secondtst.o
	$(LOADER) $(LOADOPTS) -o testsecond second.o secondtst.o

testdsecnd: dsecnd.o dsecndtst.o
	$(LOADER) $(LOADOPTS) -o testdsecnd dsecnd.o dsecndtst.o

testieee: tstiee.o
	$(LOADER) $(LOADOPTS) -o testieee tstiee.o

testversion: ilaver.o LAPACK_version.o
	    $(LOADER) $(LOADOPTS) -o testversion ilaver.o LAPACK_version.o

clean:
	rm -f *.o

slamch.o: slamch.f ; $(FORTRAN) $(NOOPT) -c $<
dlamch.o: dlamch.f ; $(FORTRAN) $(NOOPT) -c $<

.f.o: ; $(FORTRAN) $(OPTS) -c $<
