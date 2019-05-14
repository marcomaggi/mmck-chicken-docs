## Process this file with automake to produce Makefile.in

include meta/automake/mmck-rules.make


#### documentation

info_TEXINFOS	= \
	src/chicken-internals.texi

src_chicken_internals_TEXINFOS = \
	src/macros.texi


#### interface to "make check"
#
# Read "Parallel Test Harness" in the documentation of GNU Automake to
# understand how to use this interface for "make check".
#

EXTRA_DIST		+= \
	tests/test-version.scm		\
	tests/test-slots.scm

MMCK_CHECK_OBJECTS	= \
	tests/test-version.$(OBJEXT)	\
	tests/test-slots.$(OBJEXT)

MMCK_CHECK_PROGS	= \
	tests/test-version.exe		\
	tests/test-slots.exe

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

tests/test-slots.$(OBJEXT): tests/test-slots.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-slots.exe: tests/test-slots.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

### end of file
