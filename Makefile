# Copyright 2007-2008, Matthew Welland.
# 
#  This program is made available under the GNU GPL version 2.0 or
#  greater. See the accompanying file COPYING for details.
# 
#  This program is distributed WITHOUT ANY WARRANTY; without even the
#  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#  PURPOSE.
#
# Following needed on bluehost
#
# CSC_OPTIONS='-C "-fPIC"' make
#
include install.cfg

SRCFILES    = stml.scm misc-stml.scm session.scm sqltbl.scm formdat.scm setup.scm keystore.scm html-filter.scm cookie.scm 
MODULEFILES = $(wildcard modules/*/*-mod.scm)
SOFILES     = $(MODULEFILES:%.scm=%.so)
CFILES      = $(MODULEFILES:%.scm=%.c)
OFILES      = $(SRCFILES:%.scm=%.o)
TARGFILES   = $(notdir $(SOFILES))
MODULES     = $(addprefix $(TARGDIR)/modules/,$(TARGFILES))

install : $(TARGDIR)/stmlrun $(LOGDIR) $(MODULES)
	chicken-install

all : $(SOFILES)

# stmlrun : stmlrun.scm formdat.scm  misc-stml.scm  session.scm stml.scm \
#           setup.scm html-filter.scm requirements.scm keystore.scm \
#           cookie.scm sqltbl.scm
# 	csc stmlrun.scm

$(TARGDIR)/stmlrun : stmlrun stml.so
	install stmlrun $(TARGDIR)
	chmod a+rx $(TARGDIR)/stmlrun

$(TARGDIR)/modules :
	mkdir -p $(TARGDIR)/modules

$(MODULES) : $(SOFILES) $(TARGDIR)/modules
	cp $< $@

stmlrun : $(OFILES) stmlrun.scm requirements.scm stmlcommon.scm
	csc $(OFILES) stmlrun.scm -o stmlrun

stml.so : stmlmodule.so
	cp stmlmodule.so stml.so

stmlmodule.so : $(OFILES) stmlmodule.scm requirements.scm stmlcommon.scm
	csc $(OFILES) -s stmlmodule.scm

# logging currently relies on this
#
$(LOGDIR) :
	mkdir -p $(LOGDIR)
	chmod a+rwx $(LOGDIR)

test: kiatoa.db cookie.so
	echo '(exit)'| csi -q  ./tests/test.scm 

# modules
#
%.so : %.scm
	csc -I modules/* -s $<

%.o : %.scm
	csc -c $<

# Cookie is a special case for now. Make a loadable so for test
# Complile it in by include (see dependencies above).
cookie.so : cookie.scm
	csc -s cookie.scm


# $(CFILES): build/%.c: ../scm/%.scm ../scm/macros.scm
# 	chicken $< -output-file $@
# 
# 
# $(OFILES): src/%.o: src/%.c
# 	gcc -c $< `chicken-config -cflags` -o $@
# 
# $(src_code): %: src/%.o src/laedlib.o src/layobj.o
# 	gcc src/$*.o src/laedlib.o src/layobj.o -o $* `chicken-config -libs`
# 
