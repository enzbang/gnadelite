###########################################################################
#                                  GnadeLite
#
#                          Copyright (C) 2006-2008
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

include mk.config

B_DIR=".build/$(shell echo $(MODE) | tr [[:upper:]] [[:lower:]])"

APP := $(ADA_PROJECT_PATH)

ifeq ($(OS),Windows_NT)
export ADA_PROJECT_PATH=$PWD/external-libs/morzhol\;${APP}
else
export ADA_PROJECT_PATH=$PWD/external-libs/morzhol:${APP}
endif

all: build

setup:
runtests:
	(cd tests; $(GNATMAKE) -Pregtests;)
	@(cd tests; printf 't1... '; \
		if test `$(RUNTEST) ./t1 | wc -l` = "10"; then \
			printf "ok\n"; \
		else \
			printf "nok\n"; \
		fi;)
	@(cd tests; printf 't2... '; \
		if test `$(runtest) ./t2` = 1234; then \
			printf "ok\n"; \
		else \
			printf "nok\n"; \
		fi;)

build:
ifneq ($(INSTALL), "")
# Write INSTALL target into mk.install (see install target)
	$(shell echo $(INSTALL) > mk.install)
endif
	$(GNATMAKE) -Pgnadelite

clean:
	$(GNATCLEAN) -Pgnadelite

I_BIN	   = $(INSTALL)/bin
I_INC	   = $(INSTALL)/include/gnadelite
I_INC_G	   = $(INSTALL)/include/gnadelite/gnade
I_LIB	   = $(INSTALL)/lib/gnadelite
I_GPR	   = $(INSTALL)/lib/gnat

install_clean:
ifeq ("$(INSTALL)", "")
	$(error "Wrong install path : empty INSTALL var")
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
	$(CP) gnade_src/*.ad[sb] $(I_INC_G)
	$(CP) lib/*.ali $(I_LIB)
	$(CP) lib/*$(SOEXT) $(I_LIB)
	$(CP) gpr/*.gpr $(I_GPR)
ifeq ($(OS), Windows_NT)
	$(CP) $(I_LIB)/*$(SOEXT) $(I_LIB)/..
endif
