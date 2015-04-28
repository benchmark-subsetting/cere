RONN_FLAGS=\
	--manual="CERE manual" \
	--organization="Université de Versailles St-Quentin-en-Yvelines"

man_MANS = cere-configure.1 cere-profile.1 cere-dump.1
doc_DATA = cere-configure.1.html cere-profile.1.html cere-dump.1.html

%.1: %.1.ronn
	$(RONN) $(RONN_FLAGS) -r $<

%.1.html: %.1.ronn
	$(RONN) $(RONN_FLAGS) -5 $<


EXTRA_DIST=\
	cere-configure.1.ronn \
	cere-profile.1.ronn
