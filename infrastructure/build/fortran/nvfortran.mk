##############################################################################
# (C) Crown copyright 2025 Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
##############################################################################
# Various things specific to the Nvidia Fortran compiler.
##############################################################################
#
# This macro is evaluated now (:= syntax) so it may be used as many times as
# desired without wasting time rerunning it.
#
NVFORT_VERSION := $(shell nvfortran -V | awk '/^nvfortran +[0-9]+\.[0-9]+/ { split($$2, a, "[.-]"); printf "%03i%02i%02i\n", a[1],a[2],a[3] }')
$(info ** Chosen Nvidia Fortran compiler version $(NVFORT_VERSION))
ifeq ($(shell test $(NVFORT_VERSION) -lt 0241100; echo $$?), 0)
  $(error nvFort is too old to build LFRic. Must be at least 24.11)
endif

F_MOD_DESTINATION_ARG = -module$(SPACE)
OPENMP_ARG            = -mp

FFLAGS_COMPILER           =
FFLAGS_NO_OPTIMISATION    = -O0
FFLAGS_SAFE_OPTIMISATION  = -O2
FFLAGS_RISKY_OPTIMISATION = -O4
FFLAGS_DEBUG              = -g -traceback
FFLAGS_RUNTIME            =
# Option for checking code meets Fortran standard (not available for PGI)
FFLAGS_FORTRAN_STANDARD   =

LDFLAGS_COMPILER = -g
FPPFLAGS = -P -D__NVCOMPILER

# Flags for OpenMP threading / OpenMP offloading / OpenACC Offloading
# The LFRIC_OFFLOAD_DIRECTIVES env_variable is also queried in the PSyclone
# script to generate matching directives
ifeq ("$(LFRIC_OFFLOAD_DIRECTIVES)", "omp")
	OPENMP_ARG = -mp=gpu -gpu=managed
	LDFLAGS_COMPILER = -mp=gpu -gpu=managed -cuda
else ifeq ("$(LFRIC_OFFLOAD_DIRECTIVES)", "acc")
	OPENMP_ARG = -acc=gpu -gpu=managed -mp=multicore
	LDFLAGS_COMPILER = -acc=gpu -gpu=managed -mp=multicore -cuda
else
	OPENMP_ARG = -mp
	LDFLAGS_COMPILER = -mp
endif
