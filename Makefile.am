## Process this file with automake to produce Makefile.in

include meta/automake/mmck-rules.make


#### documentation

EXTRA_DIST		+= src/mmck-chicken-docs.style.css
CLEANFILES		+= src/mmck-chicken-docs.css

AM_MAKEINFOFLAGS	+= -I$(srcdir)

AM_MAKEINFOHTMLFLAGS	+= --css-ref=mmck-chicken-docs.css \
	-c PRE_BODY_CLOSE=''

info_TEXINFOS	= \
	src/chicken-core.texi		\
	src/chicken-internals.texi

src_chicken_core_TEXINFOS = \
	src/r5rs.texi				\
	src/macros.texi				\
	src/srfi-45-lazy-evaluation.texi

src_chicken_internals_TEXINFOS = \
	src/macros.texi

## --------------------------------------------------------------------

src/chicken-core.html/$(am__dirstamp): src/$(am__dirstamp)
	@$(MKDIR_P) src/chicken-core.html/
	@: > src/chicken-core.html/$(am__dirstamp)

src/chicken-core.html/mmck-chicken-docs.css: src/mmck-chicken-docs.style.css src/chicken-core.html/$(am__dirstamp)
	$(INSTALL) -m 0444 "$(top_srcdir)/src/mmck-chicken-docs.style.css" "$(builddir)/src/chicken-core.html/mmck-chicken-docs.css"

## --------------------------------------------------------------------

src/chicken-internals.html/$(am__dirstamp): src/$(am__dirstamp)
	@$(MKDIR_P) src/chicken-internals.html/
	@: > src/chicken-internals.html/$(am__dirstamp)

src/chicken-internals.html/mmck-chicken-docs.css: src/mmck-chicken-docs.style.css src/chicken-internals.html/$(am__dirstamp)
	$(INSTALL) -m 0444 "$(top_srcdir)/src/mmck-chicken-docs.style.css" "$(builddir)/src/chicken-internals.html/mmck-chicken-docs.css"

## --------------------------------------------------------------------

html-local: \
	src/chicken-core.html/mmck-chicken-docs.css		\
	src/chicken-internals.html/mmck-chicken-docs.css

install-html-local:
	$(MKDIR_P) "$(DESTDIR)$(htmldir)"
	$(INSTALL) -m 0444 $(top_srcdir)/src/*.css "$(DESTDIR)$(htmldir)"


#### interface to "make check"
#
# Read "Parallel Test Harness" in the documentation of GNU Automake to
# understand how to use this interface for "make check".
#

# The list of object files.  We will store it in CLEANFILES.
#
MMCK_CHECK_OBJECTS	=

# The list of test programs.  We will set TESTS to it.
#
MMCK_CHECK_PROGS	=

# Dependencies common to all the test programs.
#
MMCK_CHECK_DEPS		+= $(MMCK_ALL_RUNTIME_SHARED_LIBS) $(MMCK_ALL_IMPORT_LIBS)

## --------------------------------------------------------------------

EXTRA_DIST		+= tests/test-version.scm
MMCK_CHECK_OBJECTS	+= tests/test-version.$(OBJEXT)
MMCK_CHECK_PROGS	+= tests/test-version.exe

tests/test-version.$(OBJEXT): tests/test-version.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-version.exe: tests/test-version.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)


#### tests associated to the document "chicken-internals"

EXTRA_DIST		+= tests/test-chicken-internals-slots.scm
MMCK_CHECK_OBJECTS	+= tests/test-chicken-internals-slots.$(OBJEXT)
MMCK_CHECK_PROGS	+= tests/test-chicken-internals-slots.exe

tests/test-chicken-internals-slots.$(OBJEXT): tests/test-chicken-internals-slots.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-chicken-internals-slots.exe: tests/test-chicken-internals-slots.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

## --------------------------------------------------------------------

EXTRA_DIST		+= tests/test-chicken-internals-structs.scm
MMCK_CHECK_OBJECTS	+= tests/test-chicken-internals-structs.$(OBJEXT)
MMCK_CHECK_PROGS	+= tests/test-chicken-internals-structs.exe

tests/test-chicken-internals-structs.$(OBJEXT): tests/test-chicken-internals-structs.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-chicken-internals-structs.exe: tests/test-chicken-internals-structs.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

## --------------------------------------------------------------------

EXTRA_DIST		+= tests/test-chicken-internals-checks.scm
MMCK_CHECK_OBJECTS	+= tests/test-chicken-internals-checks.$(OBJEXT)
MMCK_CHECK_PROGS	+= tests/test-chicken-internals-checks.exe

tests/test-chicken-internals-checks.$(OBJEXT): tests/test-chicken-internals-checks.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-chicken-internals-checks.exe: tests/test-chicken-internals-checks.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

## --------------------------------------------------------------------

EXTRA_DIST		+= tests/test-chicken-internals-procs.scm
MMCK_CHECK_OBJECTS	+= tests/test-chicken-internals-procs.$(OBJEXT)
MMCK_CHECK_PROGS	+= tests/test-chicken-internals-procs.exe

tests/test-chicken-internals-procs.$(OBJEXT): tests/test-chicken-internals-procs.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-chicken-internals-procs.exe: tests/test-chicken-internals-procs.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)


#### tests associated to the document "chicken-core"

EXTRA_DIST		+= tests/test-chicken-core-condition.scm
MMCK_CHECK_OBJECTS	+= tests/test-chicken-core-condition.$(OBJEXT)
MMCK_CHECK_PROGS	+= tests/test-chicken-core-condition.exe

tests/test-chicken-core-condition.$(OBJEXT): tests/test-chicken-core-condition.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-chicken-core-condition.exe: tests/test-chicken-core-condition.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

## --------------------------------------------------------------------

EXTRA_DIST		+= tests/test-chicken-core-base.scm
MMCK_CHECK_OBJECTS	+= tests/test-chicken-core-base.$(OBJEXT)
MMCK_CHECK_PROGS	+= tests/test-chicken-core-base.exe

tests/test-chicken-core-base.$(OBJEXT): tests/test-chicken-core-base.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-chicken-core-base.exe: tests/test-chicken-core-base.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)


#### wrapping up tests

CLEANFILES	+= $(MMCK_CHECK_PROGS) $(MMCK_CHECK_OBJECTS)

if MMCK_TESTING_ENABLED
TESTS		= $(MMCK_CHECK_PROGS)
endif

### end of file
