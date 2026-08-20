# val.criterion Options

Internally used, package-specific options. All options will prioritize R
options() values, and fall back to environment variables if undefined.
If neither the option nor the environment variable is set, a default
value is used.

## Checking Option Values

Option values specific to `val.criterion` can be accessed by passing the
package name to `env`.

    options::opts(env = "val.criterion")

    options::opt(x, default, env = "val.criterion")

## Options

- repos:

  default:

  :   character(0L)

  option:

  :   val.criterion.repos

  envvar:

  :   R_VAL_CRITERION_REPOS (evaluated if possible, raw string
      otherwise)

- exceptions:

  default:

  :   character(0L)

  option:

  :   val.criterion.exceptions

  envvar:

  :   R_VAL_CRITERION_EXCEPTIONS (evaluated if possible, raw string
      otherwise)

- quiet:

  default:

  :   FALSE

  option:

  :   val.criterion.quiet

  envvar:

  :   R_VAL_CRITERION_QUIET (evaluated if possible, raw string
      otherwise)

- actions:

  default:

  :   default_actions()

  option:

  :   val.criterion.actions

  envvar:

  :   R_VAL_CRITERION_ACTIONS (evaluated if possible, raw string
      otherwise)

## See also

options getOption Sys.setenv Sys.getenv
