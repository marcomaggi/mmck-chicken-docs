## Process this file with automake to produce Makefile.in

include meta/automake/mmck-rules.make


#### documentation

EXTRA_DIST		+= src/mmck-chicken-docs.style.css
CLEANFILES		+= src/mmck-chicken-docs.css

AM_MAKEINFOHTMLFLAGS	+= --css-ref=mmck-chicken-docs.css \
	-c PRE_BODY_CLOSE=''

info_TEXINFOS	= \
	src/chicken-internals.texi

src_chicken_internals_TEXINFOS = \
	src/macros.texi

src/chicken-internals.html/$(am__dirstamp): src/$(am__dirstamp)
	@$(MKDIR_P) src/chicken-internals.html/
	@: > src/chicken-internals.html/$(am__dirstamp)

src/chicken-internals.html/mmck-chicken-docs.css: src/mmck-chicken-docs.style.css src/chicken-internals.html/$(am__dirstamp)
	$(INSTALL) -m 0444 "$(top_srcdir)/src/mmck-chicken-docs.style.css" "$(builddir)/src/chicken-internals.html/mmck-chicken-docs.css"

html-local: src/chicken-internals.html/mmck-chicken-docs.css

install-html-local:
	$(MKDIR_P) "$(DESTDIR)$(htmldir)"
	$(INSTALL) -m 0444 $(top_srcdir)/src/*.css "$(DESTDIR)$(htmldir)"


#### interface to "make check"
#
# Read "Parallel Test Harness" in the documentation of GNU Automake to
# understand how to use this interface for "make check".
#

EXTRA_DIST		+= \
	tests/test-version.scm				\
	tests/test-chicken-internals-slots.scm		\
	tests/test-chicken-internals-structs.scm

MMCK_CHECK_OBJECTS	= \
	tests/test-version.$(OBJEXT)			\
	tests/test-chicken-internals-slots.$(OBJEXT)	\
	tests/test-chicken-internals-structs.$(OBJEXT)

MMCK_CHECK_PROGS	= \
	tests/test-version.exe				\
	tests/test-chicken-internals-slots.exe		\
	tests/test-chicken-internals-structs.exe

CLEANFILES	+= $(MMCK_CHECK_PROGS) $(MMCK_CHECK_OBJECTS)

if MMCK_TESTING_ENABLED
TESTS		= $(MMCK_CHECK_PROGS)
endif

#MMCK_CHECK_DEPS	+=

## --------------------------------------------------------------------

tests/test-version.$(OBJEXT): tests/test-version.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-version.exe: tests/test-version.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

## --------------------------------------------------------------------

tests/test-chicken-internals-slots.$(OBJEXT): tests/test-chicken-internals-slots.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-chicken-internals-slots.exe: tests/test-chicken-internals-slots.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

## --------------------------------------------------------------------

tests/test-chicken-internals-structs.$(OBJEXT): tests/test-chicken-internals-structs.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-chicken-internals-structs.exe: tests/test-chicken-internals-structs.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

### end of file
