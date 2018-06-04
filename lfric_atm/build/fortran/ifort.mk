##############################################################################
# (c) Crown copyright 2017 Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
##############################################################################
# Various things specific to the Intel Fortran compiler.
##############################################################################

$(info Project specials for Intel compiler)

export FFLAGS_UM_PHYSICS = -r8

# Options to apply to LFRic code but not other code such as UM code.
# -qoverride-limits mandatory due to Intel compiler bug ref #1486
# This will be removed by #1490
export LFRIC_INTEL_FIX_ARG         = -qoverride-limits
