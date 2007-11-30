###########################################################################
#                                  GnadeLite
#
#                          Copyright (C) 2006-2007
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
#  Simple makefile to build Vision2Pixels
###########################################################################

include mk.config

B_DIR=".build/$(shell echo $(MODE) | tr [[:upper:]] [[:lower:]])"

ifeq ($(OS),Windows_NT)
export ADA_PROJECT_PATH=$PWD/external-libs/morzhol\;$ADA_PROJECT_PATH
else
export ADA_PROJECT_PATH=$PWD/external-libs/morzhol:$ADA_PROJECT_PATH
endif

all: build

build:
	$(GNATMAKE) -Pgnadelite

clean:
	$(GNATCLEAN) -Pgnadelite

I_BIN	   = $(INSTALL)/bin
I_INC	   = $(INSTALL)/include/gnadelite
I_INC_G	   = $(INSTALL)/include/gnadelite/gnade
I_LIB	   = $(INSTALL)/lib/gnadelite
I_GPR	   = $(INSTALL)/lib/gnat

install_clean:
ifeq ("$(INSTALL)", "..")
	$(error "Wrong install path : INSTALL='$(INSTALL)'")
else
ifeq ("$(INSTALL)", "")
	$(error "Wrong install path : empty INSTALL var")
endif
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

install: install_dirs
	$(CP) src/*.ad[sb] $(I_INC)
	$(CP) gnade_src/*.ad[sb] $(I_INC_G)
	$(CP) lib/*.ali $(I_LIB)
	$(CP) lib/*$(SOEXT) $(I_LIB)
	$(CP) gpr/*.gpr $(I_GPR)
ifeq ($(OS), Windows_NT)
	$(CP) $(I_LIB)/*$(SOEXT) $(I_LIB)/..
endif
