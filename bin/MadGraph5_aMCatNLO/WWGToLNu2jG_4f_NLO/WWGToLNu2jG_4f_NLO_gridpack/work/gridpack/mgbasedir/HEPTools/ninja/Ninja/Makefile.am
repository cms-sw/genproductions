SUBDIRS = src examples
ACLOCAL_AMFLAGS = -I m4

EXTRA_DIST = README.md \
	PACKAGE_DESCRIPTION \
	VERSION \
	utils/setup.py \
	utils/ninjanumgen/ninjanumgen \
	utils/ninjanumgen/__init__.py \
	utils/ninjanumgen/ninjanumgen.py \
	utils/ninjanumgen/templates/ninja_laurent.frm \
	utils/ninjanumgen/templates/ninjanumgen_template.cc \
	utils/ninjanumgen/templates/ninjanumgen_template.hh \
	utils/ninjanumgen/templates/ninja_opt.frm

.PHONY: examples
examples:
	make examples -C examples

.PHONY: thread-examples
thread-examples:
	make thread-examples -C examples
