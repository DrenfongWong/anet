PREFIX   ?= $(HOME)/libraries
TESTDIR   = tests
OBJDIR    = obj
COVDIR    = $(OBJDIR)/cov
LIBDIR    = lib
SRCDIR    = src
GPR_FILES = gnat/*.gpr

MAJOR    = 0
MINOR    = 2
REVISION = 3
VERSION  = $(MAJOR).$(MINOR).$(REVISION)
ANET     = libanet-$(VERSION)
TARBALL  = $(ANET).tar.bz2

SO_LIBRARY   = libanet.so.$(VERSION)
LIBRARY_KIND = dynamic

# Command variables
INSTALL         = install
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA    = $(INSTALL) --mode=644 --preserve-timestamps
INSTALL_ALI     = $(INSTALL) --mode=444

NUM_CPUS ?= 1

# GNAT_BUILDER_FLAGS, ADAFLAGS and GNATFLAGS may be overridden in the
# environment or on the command line.
GNAT_BUILDER_FLAGS ?= -R -j$(NUM_CPUS)
GNATFLAGS          ?= ${GNAT_BUILDER_FLAGS} -cargs ${ADAFLAGS}
# GMAKE_OPTS should not be overridden because -p is essential.
GMAKE_OPTS = -p ${GNATFLAGS} -margs

# GNU-style directory variables
prefix      = ${PREFIX}
exec_prefix = ${prefix}
includedir  = ${prefix}/include
libdir      = ${exec_prefix}/lib
gprdir      = ${prefix}/lib/gnat

all: build_lib

build_lib:
	@gnatmake $(GMAKE_OPTS) -Panet_lib -XVERSION="$(VERSION)" \
		-XLIBRARY_KIND="$(LIBRARY_KIND)"

build_tests:
	@gnatmake $(GMAKE_OPTS) -Panet_tests

tests: build_tests
	@$(OBJDIR)/$(TESTDIR)/test_runner

build_all: build_tests build_lib

cov:
	@rm -f $(COVDIR)/*.gcda
	@gnatmake $(GMAKE_OPTS) -Panet_tests.gpr -XBUILD="coverage"
	@$(COVDIR)/test_runner || true
	@lcov -c -d $(COVDIR) -o $(COVDIR)/cov.info
	@lcov -e $(COVDIR)/cov.info "$(PWD)/src/*.adb" -o $(COVDIR)/cov.info
	@genhtml --no-branch-coverage $(COVDIR)/cov.info -o $(COVDIR)

examples:
	@gnatmake $(GMAKE_OPTS) -Panet_examples

install: install_lib install_$(LIBRARY_KIND)

install_lib: build_lib
	$(INSTALL) -d $(DESTDIR)$(gprdir)
	$(INSTALL) -d $(DESTDIR)$(libdir)/anet
	$(INSTALL) -d $(DESTDIR)$(includedir)/anet
	$(INSTALL_DATA) $(SRCDIR)/*.ad[bs] $(DESTDIR)$(includedir)/anet
	$(INSTALL_ALI) $(LIBDIR)/$(LIBRARY_KIND)/*.ali $(DESTDIR)$(libdir)/anet
	$(INSTALL_DATA) $(GPR_FILES) $(DESTDIR)$(gprdir)

install_static:
	$(INSTALL_DATA) $(LIBDIR)/$(LIBRARY_KIND)/libanet.a $(DESTDIR)$(libdir)

install_dynamic:
	$(INSTALL_PROGRAM) $(LIBDIR)/$(LIBRARY_KIND)/$(SO_LIBRARY) $(DESTDIR)$(libdir)
	cd $(DESTDIR)$(libdir) && ln -sf $(SO_LIBRARY) libanet.so

install_tests: build_tests
	$(INSTALL) -v -d $(DESTDIR)$(prefix)/$(TESTDIR)
	$(INSTALL_PROGRAM) $(OBJDIR)/$(TESTDIR)/test_runner $(DESTDIR)$(prefix)/$(TESTDIR)
	cp -r data $(DESTDIR)$(prefix)/$(TESTDIR)

doc:
	@$(MAKE) -C doc

clean:
	@rm -rf $(OBJDIR)
	@rm -rf $(LIBDIR)
	@$(MAKE) -C doc clean

dist:
	@echo "Creating release tarball $(TARBALL) ... "
	@git archive --format=tar HEAD --prefix $(ANET)/ | bzip2 > $(TARBALL)

.PHONY: doc examples tests
