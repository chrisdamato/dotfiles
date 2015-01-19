

VERSION=1.7.2
SRCS = args.c listen.c rfbproto.c sockets.c x2vnc.c x.c vncauth.c d3des.c
HFILES=d3des.h rfbproto.h vncauth.h x2vnc.h
OBJS = args.o listen.o rfbproto.o sockets.o x2vnc.o x.o vncauth.o d3des.o
INCLUDES = -I. -DVERSION=\"$(VERSION)\"
XFILES=README COPYING ChangeLog \
       $(SRCS) $(HFILES) \
       x2vnc.man x2vnc.spec.in \
       Imakefile Makefile.in Makefile.top Makefile.bottom \
       configure configure.in \
       install-sh \
       contrib/tkx2vnc
RPMDIR=/usr/src/RPM
LDLIBS=-lX11 -lm

all:: x2vnc

install:: install.man
CFLAGS=$(INCLUDES) -g -O2    -DPACKAGE_NAME=\"\" -DPACKAGE_TARNAME=\"\" -DPACKAGE_VERSION=\"\" -DPACKAGE_STRING=\"\" -DPACKAGE_BUGREPORT=\"\" -DX_DISPLAY_MISSING=1 -DHAVE_LIBXEXT=1 -DHAVE_X11_EXTENSIONS_XINERAMA_H=1 -DHAVE_XINERAMA=1 -DHAVE_X11_EXTENSIONS_XRANDR_H=1 -DHAVE_XRANDR=1 
LDFLAGS= 
LIBS=-lXrandr -lXinerama -lXext     $(LDLIBS)
CC=gcc

prefix=/usr/share
exec_prefix=${prefix}
BINDIR=/usr/bin
MANDIR=${prefix}/man
INSTALL=/usr/bin/install -c
MKDIRHIER=mkdir -p 
DESTDIR=

x2vnc: $(OBJS)
	$(CC) -o x2vnc $(LDFLAGS) $(OBJS) $(LIBS)

install:: install.bin install.man

install.bin:
#	test -d "$(DESTDIR)$(BINDIR)" || mkdir "$(DESTDIR)$(BINDIR)"
	${INSTALL} x2vnc "$(DESTDIR)$(BINDIR)/x2vnc"

install.man:
#	test -d "$(DESTDIR)$(MANDIR)" || mkdir "$(DESTDIR)$(MANDIR)"
#	test -d "$(DESTDIR)$(MANDIR)/man1" || mkdir "$(DESTDIR)$(MANDIR)/man1"
	${INSTALL} -m 644 x2vnc.man "$(DESTDIR)$(MANDIR)/man1/x2vnc.1"

clean:
	rm -f x2vnc *.o core



export: x2vnc-$(VERSION).tar.gz


x2vnc-$(VERSION).tar.gz: $(XFILES)
	( dir=`pwd`; cd .. ; \
	rm x2vnc-$(VERSION) ||: ; ln -s x2vnc x2vnc-$(VERSION) ; \
	 tar cvf - `for a in $(XFILES) ; do echo x2vnc-$(VERSION)/$$a ; done` | gzip -c9 >$$dir/x2vnc-$(VERSION).tar.gz ;\
	rm x2vnc-$(VERSION) ; \
	echo Done )

x2vnc.spec: x2vnc.spec.in
	sed <x2vnc.spec.in >x2vnc.spec -e 's/@VERSION@/$(VERSION)/g'

rpm: $(RPMDIR)/SOURCES/x2vnc-$(VERSION).tar.gz ./x2vnc.spec
	rm -rf rpmtmp ||:
	$(MKDIRHIER) rpmtmp/$(MANDIR)
	$(MKDIRHIER) rpmtmp/$(BINDIR)
	rpm -ba --buildroot `pwd`/rpmtmp x2vnc.spec
	cp $(RPMDIR)/SRPMS/x2vnc-$(VERSION)-1.src.rpm .
	cp $(RPMDIR)/RPMS/?*/x2vnc-$(VERSION)-1.*.rpm .
	rm -rf rpmtmp

$(RPMDIR)/SOURCES/x2vnc-$(VERSION).tar.gz: x2vnc-$(VERSION).tar.gz
	cp x2vnc-$(VERSION).tar.gz $(RPMDIR)/SOURCES/x2vnc-$(VERSION).tar.gz



