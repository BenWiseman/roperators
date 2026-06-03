## Submission summary

This is a feature release (1.4.0). It adds a handful of new operators and
helpers (`f()`, `%else%`, `%+-%`, `%/0%`, `%~%`, `as.percent()`), fixes a few
bugs (notably `is.bad_for_calcs()` not flagging infinities), exports three
functions that were documented in earlier versions but never actually exported
(`get_system_python()`, `is_rda_file()`, `is_rds_file()`), overhauls the
documentation, and adds a full `testthat` test suite. Existing behaviour is
otherwise unchanged.

## Maintainer email change

The maintainer's email address has changed from
`benjamin.wiseman@kornferry.com` to `benjamin.h.wiseman@gmail.com`.

I am the same maintainer (Ben Wiseman). I have since left Korn Ferry and no
longer have access to the previous (kornferry.com) address; the new gmail
address is reachable and monitored. The package copyright remains with Korn
Ferry International and the co-authors are unchanged. Please update your records
to the new maintainer address.

CRAN's incoming checks will flag this as:

    New maintainer:
      Ben Wiseman <benjamin.h.wiseman@gmail.com>
    Old maintainer(s):
      Ben Wiseman <benjamin.wiseman@kornferry.com>

This change is intentional and is explained above.

## Test environments

* local macOS, R 4.4.1
* <add win-builder (devel + release) and/or R-hub before submitting>

## R CMD check results

0 errors | 0 warnings | 1 note

The only substantive NOTE is the maintainer-email change described above
("New maintainer ... Old maintainer(s) ..."), which is intentional.

(Local checks also emitted two environment-specific NOTEs that do not occur on
CRAN: "unable to verify current time", and HTML-manual validation messages from
an outdated local 'tidy' that does not understand R's HTML5 output.)

## Reverse dependencies

There are no reverse dependencies on CRAN.
