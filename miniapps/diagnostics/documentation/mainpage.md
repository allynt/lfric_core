@mainpage Overview

@section diagnostics_miniapp Diagnostics Miniapp

This miniapp forms part of the LFRic and Gungho project. It provides an example of how to write a fully working application that uses the LFRic infrastructure including timestepping, but contains no science to understand. The driver and infrastructure is a closer approximation of the gungho driver than the skeleton miniapp and includes timestepping. This is designed to test core infrastructure as-is and provides a fuller example to clone than skeleton for those wishing to carry out a more detailed (or time stepping) test.

It creates a set of prognostic fields (red, green, blue), seeds selected coordinates to 100% (255) and then spreads them to the neighbouring cells (horizontally and vertically) on each time step, optionally using the blend percentage variable to slow the spread.

This produces a simple yet progressive global model.

The basic test suite is set to use a c3 model with 3 atmosphere layers, ensuring all possible corner / edge / middle conditions are explored.

@section lfric_gungho_sec LFRic and GungHo

The documentation related to LFRic and GungHo projects is hosted at the <a href="https://code.metoffice.gov.uk/trac/home">Met Office Science Repository Service (MOSRS)</a>.

- <a href="https://code.metoffice.gov.uk/trac/lfric/wiki">LFRic Project Space</a>,
- <a href="https://code.metoffice.gov.uk/trac/lfric/wiki/GungHo">GungHo Project Space</a>.

More information about checking out and running the code can be found at <a href="https://code.metoffice.gov.uk/trac/lfric/wiki/LFRicTechnical/QuickStart">LFRic QuickStart page</a>.

@section psyclone_in_lfric_sec PSyclone in LFRic

The LFRic code uses <a href="https://code.metoffice.gov.uk/trac/lfric/wiki/PsycloneTool">PSyclone tool</a>, which is hosted at the 
<a href="https://github.com/stfc/PSyclone">PSyclone GitHub repository</a>.

One of the PSyclone features used very explicitly in the LFRic code are <a href="https://github.com/stfc/PSyclone/blob/master/doc/built_ins.rst">Built-ins</a>:
operations which can be specified within an invoke call in the algorithm layer but do not require an associated kernel to be implemented as they are provided 
directly by the infrastructure.

For more up-to-date information about the <b>LFRic-specific Built-ins</b> functionality (e.g. names, argument order) please refer to the 
<a href="https://github.com/stfc/PSyclone/blob/master/doc/dynamo0p3.rst#built-ins">dynamo 0.3 API Built-ins documentation</a>.


