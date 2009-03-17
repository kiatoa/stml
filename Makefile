# Copyright 2007-2008, Matthew Welland.
# 
#  This program is made available under the GNU GPL version 2.0 or
#  greater. See the accompanying file COPYING for details.
# 
#  This program is distributed WITHOUT ANY WARRANTY; without even the
#  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#  PURPOSE.

include install.cfg

MODULEFILES = $(wildcard modules/*/*-mod.scm)
SOFILES     = $(MODULEFILES:%.scm=%.so)
CFILES      = $(MODULEFILES:%.scm=%.c)
OFILES      = $(MODULEFILES:%.scm=%.o)
TARGFILES   = $(notdir $(SOFILES))
MODULES     = $(addprefix $(TARGDIR)/modules/,$(TARGFILES))

install : $(TARGDIR)/stmlrun $(LOGDIR) $(MODULES)

stmlrun : stmlrun.scm formdat.scm  misc-stml.scm  session.scm stml.scm \
          setup.scm html-filter.scm requirements.scm dbi.scm keystore.scm \
          sugar.scm
	csc stmlrun.scm


$(TARGDIR)/stmlrun : stmlrun 
	cp stmlrun $(TARGDIR)
	chmod a+rx $(TARGDIR)/stmlrun

$(TARGDIR)/modules :
	mkdir -p $(TARGDIR)/modules

$(MODULES) : $(SOFILES) $(TARGDIR)/modules
	cp $< $@

# logging currently relies on this
#
$(LOGDIR) :
	mkdir -p $(LOGDIR)
	chmod a+rwx $(LOGDIR)

test: kiatoa.db
	echo '(exit)'| csi -q  ./tests/test.scm 

# modules
#
%.so : %.scm
	csc -I modules/* -s $<

all : $(SOFILES)

# 
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
