RONN_FLAGS=\
	--manual="CERE manual" \
	--organization="Université de Versailles St-Quentin-en-Yvelines"

man_MANS = man1/cere-configure.1 man1/cere-profile.1 man1/cere-capture.1 man1/cere-tutorial.1 man1/cere-replay.1 man1/cere-regions.1 man1/cere-trace.1 man1/cere-selectinv.1
doc_DATA = html/cere-configure.1.html html/cere-profile.1.html html/cere-capture.1.html html/cere-tutorial.1.html html/cere-replay.1.html html/cere-regions.1.html html/cere-trace.1.html html/cere-selectinv.1.html

man1/%.1: %.1.ronn
	mkdir -p man1
	$(RONN) $(RONN_FLAGS) --pipe -r $< > $@

html/%.1.html: %.1.ronn
	mkdir -p html
	$(RONN) $(RONN_FLAGS) --pipe -5 $< > $@


EXTRA_DIST=\
	cere-configure.1.ronn \
	cere-profile.1.ronn \
	cere-capture.1.ronn \
	cere-tutorial.1.ronn \
	cere-replay.1.ronn \
	cere-regions.1.ronn \
	cere-trace.1.ronn \
	cere-selectinv.1.ronn
