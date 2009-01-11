# Copyright 2007-2008, Matthew Welland.
# 
#  This program is made available under the GNU GPL version 2.0 or
#  greater. See the accompanying file COPYING for details.
# 
#  This program is distributed WITHOUT ANY WARRANTY; without even the
#  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#  PURPOSE.

include install.cfg

install : $(TARGDIR)/stmlrun $(LOGDIR) $(SOFILES)

stmlrun : stmlrun.scm formdat.scm  misc-stml.scm  session.scm stml.scm \
          setup.scm html-filter.scm requirements.scm dbi.scm keystore.scm \
          sugar.scm
	csc stmlrun.scm


$(TARGDIR)/stmlrun : stmlrun 
	cp stmlrun $(TARGDIR)
	chmod a+rx $(TARGDIR)/stmlrun

# logging currently relies on this
#
$(LOGDIR) :
	mkdir -p $(LOGDIR)
	chmod a+rwx $(LOGDIR)

test: kiatoa.db
	echo '(exit)'| csi -q  ./tests/test.scm 

# cgi-util proplist cgi-util cookie

kiatoa.db: /tmp/sqlite3 sessions.sql
	rm -f kiatoa.db
	$(SQLITE3) kiatoa.db < sessions.sql

/tmp/sqlite3: sqlite3.scm
	csc sqlite3.scm -o /tmp/sqlite3

