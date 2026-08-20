# Package index

## Package filtering

The central tools for filtering repositories based on expressed
criteria.

- [`package_filter()`](https://pharmar.github.io/val.criterion/reference/package_filter.md)
  : Create a Package Filter

## Available filters

A selection of ready-made filters

- [`cooldown()`](https://pharmar.github.io/val.criterion/reference/cooldown.md)
  : Filter by date

## Actions

Behaviors to apply when packages fail to meet the expressed criteria.

- [`default_actions()`](https://pharmar.github.io/val.criterion/reference/default_actions.md)
  : A default set of package tools to impose actions upon
- [`action_disallow()`](https://pharmar.github.io/val.criterion/reference/action_disallow.md)
  : Disallow an action
- [`last_rejected()`](https://pharmar.github.io/val.criterion/reference/last_rejected.md)
  : Retrieve the last package rejection, imposed by a criteria
- [`last_rejected_permit()`](https://pharmar.github.io/val.criterion/reference/last_rejected_permit.md)
  : Permit the last package rejection

## Execution controls

Options to configure how `val.criterion` behaves.

- [`options`](https://pharmar.github.io/val.criterion/reference/options.md)
  : val.criterion Options
