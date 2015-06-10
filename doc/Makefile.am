RONN_FLAGS=\
	--manual="CERE manual" \
	--organization="Université de Versailles St-Quentin-en-Yvelines"

man_MANS = man1/cere-configure.1 man1/cere-profile.1 man1/cere-capture.1 man1/cere-tutorial.1 man1/cere-replay.1 man1/cere-regions.1 man1/cere-trace.1 man1/cere-selectinv.1 man1/cere-instrument.1 man1/cere-check-io.1 \
           man1/cere-check-matching.1 man1/cere-report.1
doc_DATA = html/cere-configure.1.html html/cere-profile.1.html html/cere-capture.1.html html/cere-tutorial.1.html html/cere-replay.1.html html/cere-regions.1.html html/cere-trace.1.html html/cere-selectinv.1.html \
           html/cere-instrument.1.html html/cere-check-io.1.html html/cere-check-matching.1.html html/cere-report.1.html

man1/%.1: %.1.md
	mkdir -p man1
	$(RONN) $(RONN_FLAGS) --pipe -r $< > $@

html/%.1.html: %.1.md
	mkdir -p html
	$(RONN) $(RONN_FLAGS) --pipe -5 $< > $@


EXTRA_DIST=\
	cere-configure.1.md \
	cere-profile.1.md \
	cere-capture.1.md \
	cere-tutorial.1.md \
	cere-replay.1.md \
	cere-regions.1.md \
	cere-trace.1.md \
	cere-selectinv.1.md \
	cere-instrument.1.md \
	cere-check-io.1.md \
	cere-check-matching.1.md \
	cere-report.1.md
