SUBDIRS=ccan rdtsc memory_dump RegionOutliner RegionInstrumentation RegionReplay RegionDump GlobalRename

dist_bin_SCRIPTS=cerec
pkgpython_PYTHON= \
	cere/lel.py \
	cere/cere_sanity_check.py \
	cere/ilp_update_graph.py \
	cere/cere_trace.py \
	cere/granularity.py \
	cere/cere_regions.py \
	cere/__main__.py \
	cere/cere_select_max_cov.py \
	cere/cere_selectinv.py \
	cere/graph_utils.py \
	cere/vars.py \
	cere/compress.py \
	cere/__init__.py \
	cere/cere_select_ilp.py \
	cere/cere_profile.py \
	cere/utils.py \
	cere/cere_hybrid.py \
	cere/errors.py \
	cere/max_cov_update_graph.py \
	cere/create_graph.py \
	cere/graph_error.py \
	cere/cere_check_matching.py \
	cere/regions_selector.py \
	cere/lec.py \
	cere/cere_instrument.py \
	cere/cere_replay.py \
	cere/cere_capture.py \
	cere/cere_report.py \
	cere/cere_flag.py \
	cere/cere_configure.py \
	cere/template.html \
	cere/template_realmain.c \
	cere/Report.js

edit_script = $(SED) -e 's,%LIBDIR%,$(libdir),'g $(NULL)

cere/vars.py: cere/vars.py.in Makefile
	$(AM_V_GEN)rm -f $@ $@.tmp && \
	$(edit_script) $< >$@.tmp && \
	chmod a-w $@.tmp && \
	mv $@.tmp $@

CLEANFILES = cere/vars.py
