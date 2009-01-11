# Copyright 2007-2008, Matthew Welland.
# 
#  This program is made available under the GNU GPL version 2.0 or
#  greater. See the accompanying file COPYING for details.
# 
#  This program is distributed WITHOUT ANY WARRANTY; without even the
#  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#  PURPOSE.

# Uncomment and fix path if you want your models to be compiled
#
MODELS := $(wildcard models/*scm)
SOFILES := $(patsubst %.scm,%.so,$(MODELS))


# all : $(SOFILES)

# If you want compiled models uncomment the following
#
# $(SOFILES) : %.so: %.scm
#	csc -s $<

test: # $(SOFILES)
	echo '(exit)'| csi -q  ./tests/test.scm 

# cgi-util proplist cgi-util cookie
