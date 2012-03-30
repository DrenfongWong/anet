PREFIX   ?= $(HOME)/libraries
TESTDIR   = tests
OBJDIR    = obj
COVDIR    = $(OBJDIR)/cov
LIBDIR    = lib
SRCDIR    = src
DOCDIR    = doc/html
GPR_FILES = gnat/*.gpr

MAJOR   = 0
MINOR   = 1
VERSION = $(MAJOR).$(MINOR)
ANET    = libanet-$(VERSION)
TARBALL = $(ANET).tar.bz2

SO_LIBRARY   = libanet.so.$(VERSION)
LIBRARY_KIND = dynamic

NUM_CPUS := $(shell getconf _NPROCESSORS_ONLN)

GMAKE_OPTS = -p -R -j$(NUM_CPUS)

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

install: install_lib install_$(LIBRARY_KIND)

install_lib: build_lib
	install -d $(PREFIX)/lib/gnat
	install -d $(PREFIX)/lib/anet
	install -d $(PREFIX)/include/anet
	install -m 644 $(SRCDIR)/*.ad[bs] $(PREFIX)/include/anet
	install -m 444 $(LIBDIR)/$(LIBRARY_KIND)/*.ali $(PREFIX)/lib/anet
	install -m 644 $(GPR_FILES) $(PREFIX)/lib/gnat

install_static:
	install -m 444 $(LIBDIR)/$(LIBRARY_KIND)/libanet.a $(PREFIX)/lib

install_dynamic:
	install -m 444 $(LIBDIR)/$(LIBRARY_KIND)/$(SO_LIBRARY) $(PREFIX)/lib
	cd $(PREFIX)/lib && ln -sf $(SO_LIBRARY) libanet.so

install_tests: build_tests
	install -v -d $(PREFIX)/$(TESTDIR)
	install -m 755 $(OBJDIR)/$(TESTDIR)/test_runner $(PREFIX)/$(TESTDIR)
	@cp -vr data $(PREFIX)/$(TESTDIR)

doc:
	@$(MAKE) -C doc

clean:
	@rm -rf $(OBJDIR)
	@rm -rf $(LIBDIR)
	@$(MAKE) -C doc clean

dist:
	@echo "Creating release tarball $(TARBALL) ... "
	@git archive --format=tar HEAD --prefix $(ANET)/ | bzip2 > $(TARBALL)

.PHONY: build_all build_lib build_tests clean dist doc install \
        install_dynamic install_lib install_static install_tests tests
