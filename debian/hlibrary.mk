# -*- mode: makefile -*-
# Copyright 2008 Kari Pahula <kaol@debian.org>
# Description: A class for Haskell library packages
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2, or (at
# your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
# 02111-1307 USA.

DEB_CABAL_PACKAGE ?= $(shell cat *.cabal |\
 perl -ne \
 'if (/^name\s*:\s*(.*)$$/i) {$$_ = $$1; tr/A-Z/a-z/; print; exit 0;}')
CABAL_PACKAGE=$(DEB_CABAL_PACKAGE)
CABAL_VERSION=$(shell cat *.cabal | egrep -i '^\s*version:' | head -n1 | sed -r 's,^\s*version:\s*,,i')

ENABLE_PROFILING = $(shell egrep -qe '^Package: libghc6-.*-prof$$' debian/control && echo --enable-library-profiling; exit 0)

DEB_COMPRESS_EXCLUDE += .haddock

# TODO:
# - some of this would probably be useful for generic Haskell programs,
#   not just libraries
# - provide more hooks
# - get this included in the cdbs package once this gets mature enough (maybe?)

DEB_SETUP_BIN_NAME ?= /usr/bin/cabal
#debian/hlibrary.setup
DEB_HADDOCK_HTML_DIR ?= /usr/share/doc/libghc6-$(CABAL_PACKAGE)-doc/html/

# most likely you don't need to touch this one
GHC6_VERSION = $(shell ghc --numeric-version)
DEB_HADDOCK_DIR ?= /usr/lib/ghc-$(GHC6_VERSION)/haddock/$(CABAL_PACKAGE)-$(CABAL_VERSION)/

ifndef DEB_NO_IMPLICIT_HADDOCK_HYPERLINK
DEB_HADDOCK_OPTS += --hyperlink-source
endif

BUILD_GHC6 := $(DEB_SETUP_BIN_NAME) build
MAKEFILE := debian/hlibrary.Makefile

#ifneq (,$(filter parallel=%,$(DEB_BUILD_OPTIONS)))
#    NUMJOBS = $(patsubst parallel=%,%,$(filter parallel=%,$(DEB_BUILD_OPTIONS)))
#    MAKEFLAGS := -j$(NUMJOBS)
#    BUILD_GHC6 := $(DEB_SETUP_BIN_NAME) makefile -f $(MAKEFILE) && $(MAKE) $(MAKEFLAGS) -f $(MAKEFILE) && $(BUILD_GHC6)
#endif

ifneq (,$(findstring noopt,$(DEB_BUILD_OPTIONS)))
   OPTIMIZATION = --disable-optimization
endif

clean::
	rm -rf dist dist-ghc6 dist-hugs Setup.hi Setup.ho Setup.o .*config*
	rm -f build-ghc6-stamp build-hugs-stamp build-haddock-stamp
	rm -rf debian/tmp-inst-ghc6
	rm -f $(MAKEFILE)
	rm -rf debian/dh_haskell_shlibdeps

$(DEB_SETUP_BIN_NAME):
	if test ! -e Setup.lhs -a ! -e Setup.hs; then echo "No setup script found!"; exit 1; fi
	for setup in Setup.lhs Setup.hs; do if test -e $$setup; then ghc6 --make $$setup -o $(DEB_SETUP_BIN_NAME); exit 0; fi; done

dist-ghc6: $(DEB_SETUP_BIN_NAME)
	$(DEB_SETUP_BIN_NAME) configure --ghc -v2 \
		--prefix=/usr --libdir=/usr/lib/haskell-packages/ghc6/lib \
		--builddir=dist-ghc6 \
		--haddockdir=$(DEB_HADDOCK_DIR) \
		--htmldir=$(DEB_HADDOCK_HTML_DIR) $(ENABLE_PROFILING) \
		$(DEB_SETUP_GHC6_CONFIGURE_ARGS) $(OPTIMIZATION)


build-ghc6-stamp: dist-ghc6
	$(BUILD_GHC6) --builddir=dist-ghc6
	touch build-ghc6-stamp

build/libghc6-$(CABAL_PACKAGE)-prof build/libghc6-$(CABAL_PACKAGE)-dev:: build-ghc6-stamp build-haddock-stamp

build-haddock-stamp:
	[ ! -x /usr/bin/haddock ] || $(DEB_SETUP_BIN_NAME) haddock --builddir=dist-ghc6 $(DEB_HADDOCK_OPTS)
	touch build-haddock-stamp

build/haskell-$(CABAL_PACKAGE)-doc build/libghc6-$(CABAL_PACKAGE)-doc:: dist-ghc6 build-haddock-stamp

dist-hugs: $(DEB_SETUP_BIN_NAME)
	$(DEB_SETUP_BIN_NAME) configure --hugs --prefix=/usr -v2 --builddir=dist-hugs $(DEB_SETUP_HUGS_CONFIGURE_ARGS)

build/libhugs-$(CABAL_PACKAGE):: dist-hugs
	$(DEB_SETUP_BIN_NAME) build --builddir=dist-hugs

debian/tmp-inst-ghc6: $(DEB_SETUP_BIN_NAME) dist-ghc6 build-ghc6-stamp
	$(DEB_SETUP_BIN_NAME) copy --builddir=dist-ghc6 --destdir=debian/tmp-inst-ghc6

install/libghc6-$(CABAL_PACKAGE)-dev:: debian/tmp-inst-ghc6
	cd debian/tmp-inst-ghc6 ; find usr/lib/haskell-packages/ghc6/lib/ \
		\( ! -name "*_p.a" ! -name "*.p_hi" \) \
		-exec install -Dm 644 '{}' ../$(notdir $@)/'{}' ';'
	pkg_config=`$(DEB_SETUP_BIN_NAME) register --builddir=dist-ghc6 --gen-pkg-config | sed -r 's,.*: ,,'`; \
		$(if $(HASKELL_HIDE_PACKAGES),sed -i 's/^exposed: True$$/exposed: False/' $$pkg_config;) \
		install -Dm 644 $$pkg_config debian/$(notdir $@)/var/lib/ghc-$(GHC6_VERSION)/package.conf.d/$$pkg_config; \
		rm -f $$pkg_config
	mkdir -p debian/$(notdir $@)/$(DEB_HADDOCK_DIR)
	[ 0 = `ls debian/tmp-inst-ghc6/$(DEB_HADDOCK_DIR)/ 2>/dev/null | wc -l` ] || \
		cp -r debian/tmp-inst-ghc6/$(DEB_HADDOCK_DIR)/*.haddock \
		debian/$(notdir $@)/$(DEB_HADDOCK_DIR)
	dh_haskell_provides -p$(notdir $@)
	dh_haskell_depends -p$(notdir $@)
	dh_haskell_shlibdeps -p$(notdir $@)

install/libghc6-$(CABAL_PACKAGE)-prof:: debian/tmp-inst-ghc6 install/libghc6-$(CABAL_PACKAGE)-dev
	cd debian/tmp-inst-ghc6 ; find usr/lib/haskell-packages/ghc6/lib/ \
		! \( ! -name "*_p.a" ! -name "*.p_hi" \) \
		-exec install -Dm 644 '{}' ../$(notdir $@)/'{}' ';'
	dh_haskell_provides -p$(notdir $@)
	dh_haskell_depends -p$(notdir $@)

install/haskell-$(CABAL_PACKAGE)-doc install/libghc6-$(CABAL_PACKAGE)-doc:: debian/tmp-inst-ghc6
	mkdir -p debian/$(notdir $@)/$(DEB_HADDOCK_HTML_DIR)
	cd debian/tmp-inst-ghc6/ ; find ./$(DEB_HADDOCK_HTML_DIR)/ \
		! -name "*.haddock" -exec install -Dm 644 '{}' \
		../$(notdir $@)/'{}' ';'
	dh_haskell_depends -p$(notdir $@)

install/libhugs-$(CABAL_PACKAGE):: $(DEB_SETUP_BIN_NAME) dist-hugs
	$(DEB_SETUP_BIN_NAME) copy --destdir=debian/libhugs-$(CABAL_PACKAGE) --builddir=dist-hugs
	rm -rf debian/libhugs-$(CABAL_PACKAGE)/usr/share/doc/*
	dh_haskell_depends -p$(notdir $@)

install/yahc:: debian/tmp-inst-ghc6
	mkdir -p debian/yahc/usr/bin
	mkdir -p debian/yahc/usr/man/man1
	mkdir -p debian/yahc/usr/share/yahc
	mkdir -p debian/yahc/usr/share/doc/yahc
	cp debian/tmp-inst-ghc6/usr/bin/yahc debian/yahc/usr/bin
	cp doc/yahc.1 debian/yahc/usr/man/man1
	cp doc/yahc.inputrc debian/yahc/usr/share/yahc
	cp -r examples debian/yahc/usr/share/doc/yahc
