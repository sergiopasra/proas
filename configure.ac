# Process this file with autoconf to produce a configure script.
#

AC_INIT(proas,2.2.1,[cardiel@ucm.es])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([config.h.in])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])

# Checks for programs
AC_REVISION($Revision$)
AC_PROG_FC

# Safety check
AC_CONFIG_SRCDIR([src/proas.f90])

PKG_CHECK_MODULES(PLOT, plplot-fortran, ,AC_MSG_ERROR(plplot library not found))

# Output files
AC_CONFIG_FILES([Makefile
                src/version.inc
                src/Makefile])
AC_OUTPUT
