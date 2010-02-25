###########################################################################
#                                  GnadeLite
#
#                          Copyright (C) 2006-2010
#                       Pascal Obry - Olivier Ramonat
#
#   This library is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or (at
#   your option) any later version.
#
#   This library is distributed in the hope that it will be useful, but
#   WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#   General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this library; if not, write to the Free Software Foundation,
#   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
#
###########################################################################

all::

# Options
MODE              = Release
BUILD_DIR         = .build/$(shell echo $(MODE) | tr [[:upper:]] [[:lower:]])
GNATMAKE_OPTIONS  = -XPRJ_BUILD=$(MODE)
VALGRIND          =

CP      = cp -p
MKDIR   = mkdir -p
RM      = rm -f
TAR_DIR = tar czf
MV      = mv
DIFF    = diff

GNATMAKE  = gnat make $(GNATMAKE_OPTIONS) -p
GNATCLEAN = gnat clean $(GNATMAKE_OPTIONS)
GNATCHECK = gnat check
GNATCHOP  = gnat chop

APP := $(ADA_PROJECT_PATH)
OPATH := $(PATH)
OPB := $(PRJ_BUILD)

export PRJ_BUILD=$(OPB)

ifeq ($(OS),Windows_NT)
export ADA_PROJECT_PATH=$(PWD)/external-libs/morzhol\;${APP}
export PATH=$(PWD)/lib:$(PWD)/external-libs/morzhol/$(B_DIR)/lib:$(OPATH)
else
export ADA_PROJECT_PATH=$(PWD)/external-libs/morzhol:${APP}
endif

GNAT_ROOT         = $(dir $(shell which gnatls))..
prefix            =
ifeq (,$(prefix))
	prefix = $(shell cat $(BUILD_DIR)/gnat.root 2>/dev/null)
endif

GPR =
GPR += gnadelite
GPR += tests/regtests

BLD_GPR := $(addprefix bld-, $(GPR))
CLN_GPR := $(addprefix cln-, $(GPR))
CHK_GPR := $(addprefix chk-, $(GPR))

$(BLD_GPR): bld-% :
	$(GNATMAKE) -P$*

$(CLN_GPR): cln-% :
	$(GNATCLEAN) -P$*

$(CHK_GPR): chk-% :
	$(GNATCHECK) -P$*

all:: prepare_install bld-gnadelite


# Aliases or dummy target
setup:
runtests: regtests
test: regtests
build: all

regtests: bld-tests/regtests
	@(cd tests; printf 't1... '; \
		if test `$(RUNTEST) $(BUILD_DIR)/obj/t1 | wc -l` = "10"; then \
			printf "ok\n"; \
		else \
			printf "nok\n"; \
		fi;)
	@(cd tests; printf 't2... '; \
		case `$(RUNTEST) $(BUILD_DIR)/obj/t2` in \
			1234*) \
				printf "ok\n"; \
				;; \
			*) \
				printf "nok\n"; \
				;; \
		esac)
	@(cd tests; printf 't3... '; \
		if test `$(RUNTEST) $(BUILD_DIR)/obj/t3` = "OK"; then \
			printf "ok\n"; \
		else \
			printf "nok\n"; \
		fi;)

clean:: $(CLN_GPR)

I_BIN	   = $(prefix)/bin
I_INC	   = $(prefix)/include/gnadelite
I_INC_G	   = $(prefix)/include/gnadelite/gnade
I_LIB	   = $(prefix)/lib/gnadelite
I_GPR	   = $(prefix)/lib/gnat

install_clean:
ifeq ("$(prefix)", "")
	$(error "Wrong install path : empty prefix var")
endif
	$(RM) -fr $(I_INC)
	$(RM) -fr $(I_LIB)
	$(RM) -f $(I_GPR)/gnadelite.gpr

install_dirs: install_clean
	$(MKDIR) $(I_BIN)
	$(MKDIR) $(I_INC)
	$(MKDIR) $(I_INC_G)
	$(MKDIR) $(I_LIB)
	$(MKDIR) $(I_GPR)

ifeq ("$(INSTALL)", "..")
# IF GNAT_ROOT is empty and INSTALL var is not set by the user,
# the INSTALL var is equal to ".."
# In this case, read INSTALL from mk.install. This file is created
# before building
install: INSTALL = $(shell cat mk.install)
endif

install: install_dirs
	$(CP) src/*.ad[sb] $(I_INC)
	$(CP) $(BUILD_DIR)/obj/*.ali $(I_LIB)
	$(CP) $(BUILD_DIR)/lib/*$(SOEXT) $(I_LIB)
	$(CP) gpr/*.gpr $(I_GPR)
ifeq ($(OS), Windows_NT)
	$(CP) $(I_LIB)/*$(SOEXT) $(I_LIB)/..
endif

prepare_install::
	$(MKDIR) $(BUILD_DIR)
	$(shell echo $(GNAT_ROOT) > $(BUILD_DIR)/gnat.root)
