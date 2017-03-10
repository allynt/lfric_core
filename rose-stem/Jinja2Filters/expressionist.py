#!/usr/bin/env python
# -*- coding: utf-8 -*-
##############################################################################
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014. However, it has been created with the help of the
# GungHo Consortium, whose members are identified at
# https://puma.nerc.ac.uk/trac/GungHo/wiki
##############################################################################
'''
A filter for use by Jinja2 which filters a string for expressions surrounded by
square brackets.

A number of additional functions provide capabilities within the expression:

period( seconds ) : Formats a period in seconds in the form 'hh:mm:ss'.
'''
import math
import re

def expressionist( string, parameters={} ):
  '''
  Takes the filter text and identifies expressions by their square brackets.
  These expressions are evaluated making use of any values from the passed
  dictionary.

  e.g.

  '-nodes [(mpiProcesses+4)/8] -cores [mpiProcesses]' | expression( {mpiProcesses:3} )
  => '-nodes 1 -cores 3'

  As well as the functions provided by this module the following standard
  functions are supported:

  int()
  round()

  @param [in] string Filter text to process.
  @param [in] parameters Dictionary of string:numeric pairs for substitution.
  '''
  pattern = re.compile( r'.*\[(.+?)\].*' )

  processed = ''
  index = 0
  for match in pattern.finditer( string ):
    expression = match.group(1)
    functions = {'__builtins__':None,
                 'int':__builtins__['int'],
                 'period':period,
                 'round':__builtins__['round']}
    value = eval( expression, functions, parameters )
    processed += string[index:match.start(1)-1]
    processed += str(value)
    index = match.end(1) + 1

  if index < len(string):
    processed += string[index:]

  return processed

def period( seconds ):
  '''
  Converts a time period in seconds to the form 'hours:minutes:seconds'.

  @param[in] seconds Integer number of seconds in period.
  @return String containing converted period.
  '''
  hours = int( math.floor( seconds / (60*60) ) )
  seconds = seconds % (60*60)
  minutes = int( math.floor( seconds / 60 ) )
  seconds = int( seconds % 60 )
  return '{hours:02d}:{minutes:02d}:{seconds:02d}'.format( hours=hours,
                                                           minutes=minutes,
                                                           seconds=seconds )
