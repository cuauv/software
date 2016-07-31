==========================
@ System Check Framework @
==========================

Overview
--------

The system check framework is intended to define a cross-language method of implementing simple assertions in code that can be run system-wide. Assertions are defined in statement form in software source files, then compiled into a list of tests by a Python script, which can then be run efficiently whenever.

Assertions are defined with the form "<NAME> = <EXPR>" (). Expressions are evaluated in Python with several useful helper functions in scope; see "Examples" below for more information. All lines beginning with "$COMMENT@" ($COMMENT = //, #, --, etc depending on the language) are parsed by the system check compilation script and concatenated into `system_check/gen.py`. Names can then be filtered at test-time to determine what tests to run. A hierarchical namespace ("supergroup.subgroup.test") is suggested but not enforced.

Running Tests
-------------

The testing script can be invoked with "auv-syscheck". "auv-syscheck" takes any number of prefixes (default: ['thor', 'loki']), which determine which tests will be run. This is just string matching, but it can be used to implement test groups: if all controller-related tests are prefaced with "thor.controller", then "auv-syscheck thor.controller" will run them. By default, only results of failed tests will be displayed, but "-v / --verbose" will cause successful results to be displayed as well. Tests are executed in parallel using gevent (so the script should be speedy).

Of additional interest may be "continuous mode": pass "-c / --continuous" to run system check in continuous mode. Tests will be re-run every half a second (plus test execution time); only results that change will be displayed.

Examples
--------

*Putting tests in another file*

The inline tests can also be written in another file with a preferable extension of `.test.py`. The format of tests written in a separate file is the same as any other inline test.

*SHM Access*

"shm" is in the namespace as usual.

> #@ thor.controller.running = shm.settings_control.enabled.get() == 1

*Shell Commands*

"shell" runs a command as if in a shell, capturing exit code, standard output, and standard error. These attributes are accessible as ".code", ".stdout", and ".stderr" from the returned object.

> #@ thor.can.running = shell('sudo systemctl status cand').code == 0

*Delays / Changes*

"delayed" runs a command with a delay (this is done through gevent; tests are executed in parallel). In particular, this can be used to check that some state is changing. Note that the expression must be passed as a string.

> #@ thor.threedmg.updating = shm.threedmg.roll.get() != delayed(1.0, 'shm.threedmg.roll.get()')

*Advanced*

Note that the provided statement is executed, verbatim, in Python - this can be used to effect arbitrary functionality as desired. In particular, it can be useful to import a particular Python module and check the value of a variable - 'real_import' can be used to import the module by name.

> #@ thor.controller.thrusters.all_enabled = len(real_import('control.thrusters').thrusters) == 8

*Warning/Error*

Use "@W" to generate a warning instead of an error when the assertion fails:

> #@W thor.controller.thrusters.all_enabled = len(real_import('control.thrusters').thrusters) == 8
