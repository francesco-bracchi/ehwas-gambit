GSC		=	gsc
GSI		=	gsi
COPY		=	cp -r
INSTALL		= 	cp
CATENATE	= 	cat
REMOVE		= 	rm -r
MAKEDIR		= 	mkdir
CC		=	gcc -shared 

LIBNAME		= 	ehwas
SRCDIR		= 	src
LIBDIR		= 	lib
EXAMPLEDIR	=	examples

INSTALLDIR	= 	$(shell ${GSI} -e "(display (path-expand \"~~${LIBNAME}\"))")
SOURCES		=	$(shell ls ${SRCDIR}/*[a-zA-Z0-9].scm) $(shell ls ${SRCDIR}/repr/*[a-zA-Z0-9].scm)
CFILES		= 	$(SOURCES:.scm=.c)
OBJECT_FILES	=	$(SOURCES:.scm=.o)
INCLUDES	= 	$(shell ls ${SRCDIR}/*\#.scm) $(shell ls ${SRCDIR}/repr/*\#.scm)

LINKFILE	=	$(SRCDIR)/$(LIBNAME).o1
CLINKFILE	=	$(LINKFILE:.o1=.o1.c)
OBJECT_LINKFILE =	$(LINKFILE:.o1=.o1.o)
INC_LINKFILE	=       $(SRCDIR)/$(LIBNAME)\#.scm

TESTDIR		= 	test
TESTFILES	= 	$(shell ls ${TESTDIR}/*[a-zA-Z0-9].scm)

all: libdir

clean: 
	-$(REMOVE) $(SRCDIR)/*~
	-$(REMOVE) $(CFILES)
	-$(REMOVE) $(OBJECT_FILES)
	-$(REMOVE) $(CLINKFILE)
	-$(REMOVE) $(OBJECT_LINKFILE)
	-$(REMOVE) $(LINKFILE)
	-$(REMOVE) $(INC_LINKFILE)
	-$(REMOVE) $(LIBDIR)

clean-linkfile:
	-$(REMOVE) $(CLINKFILE)
	-$(REMOVE) $(LINKFILE)

libdir: $(LINKFILE) $(LIBDIR) $(INC_LINKFILE)
	$(COPY) $(LINKFILE) $(LIBDIR)
	$(COPY) $(INC_LINKFILE) $(LIBDIR)
	$(COPY) $(INCLUDES) $(LIBDIR)

$(LINKFILE): $(OBJECT_LINKFILE) $(OBJECT_FILES)
	$(CC) $(OBJECT_FILES) $(OBJECT_LINKFILE) -o $(LINKFILE)

$(CLINKFILE):
	$(GSC) -link -flat -o $(CLINKFILE) $(SOURCES)

$(OBJECT_LINKFILE): $(CLINKFILE)
	$(GSC) -cc-options "-D___DYNAMIC" -obj -o $(OBJECT_LINKFILE) $(CLINKFILE)

$(INC_LINKFILE): 
	$(CATENATE) $(INCLUDES) > $(INC_LINKFILE)

%.o: %.c 
	$(GSC) -cc-options "-D___DYNAMIC" -obj -o $@ $<

%.c : %.scm
	$(GSC) -c -o $@ $<

$(LIBDIR):
	-$(MAKEDIR) $(LIBDIR)

$(INSTALLDIR): 
	-$(MAKEDIR) $(INSTALLDIR)

install: libdir $(INSTALLDIR) 
	@echo "installing in:"
	@echo $(INSTALLDIR)
	$(INSTALL) -r $(LIBDIR)/* $(INSTALLDIR)

composable: libdir
	@echo "testing continuation"
	@echo "connect to http://localhost:6080"
	@echo
	$(GSI) -:~~$(LIBNAME)=$(LIBDIR) $(LIBDIR)/$(LIBNAME) $(EXAMPLEDIR)/shift-reset $(EXAMPLEDIR)/composable

test: libdir
	@echo "tests"
	$(GSI) -:~~$(LIBNAME)=$(LIBDIR) -e "(load \"~~$(LIBNAME)/$(LIBNAME)\")" $(TESTFILES) -e "(newline)"
