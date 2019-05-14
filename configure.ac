# @configure_input@
#

AC_PREREQ([2.69])
MMUX_PKG_VERSIONS([0],[1],[0],[devel.0])
AC_INIT([MMCK CHICKEN Docs],[MMUX_PACKAGE_VERSION],
  [marco.maggi-ipsu@poste.it],
  [mmck-chicken-docs],[http://github.com/marcomaggi/mmck-chicken-docs])
MMUX_INIT
AC_REVISION([0.1])

AC_CONFIG_SRCDIR([src/])
AC_CONFIG_MACRO_DIR([meta/autotools])
AC_CONFIG_AUX_DIR([meta/autotools])
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET
AM_INIT_AUTOMAKE([1.16 foreign subdir-objects])
AM_MAINTAINER_MODE

AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_MKDIR_P


#### basic system inspection

MMUX_CHECK_TARGET_OS
AX_IS_RELEASE([git-directory])

AC_CACHE_SAVE


#### external libraries

MMUX_CHICKEN_SCHEME
MMUX_CHICKEN_CHECK_LIBRARY([MMCK_CHECKS],[mmck.checks])

AM_CONDITIONAL([MMCK_TESTING_ENABLED],[test "x$chicken_cv_schemelib_MMCK_CHECKS" = xyes])

AC_CACHE_SAVE


#### finish

AC_CONFIG_FILES([Makefile])
MMUX_OUTPUT
AC_OUTPUT

### end of file
