otdb: org-table Database
========================

This package uses Emacs `org-mode` tables to assist storing relatively
small amounts of user-collected data and perform any calculations
required. The two current applications manage detailed ingredient
lists for recipes (the cooking variety) and equipment lists for
backpacking trips.

The motivation for the recipe application was the frustration of it
not being straightforward to calculate useful cost and nutritional
information for home cooked food. With this application, up to several
dozen variations on a menu plan are easily compared and matched to
current needs.

The motivation for the backpacking gear application arose from the
frustration of too many trips with overweight and/or under-equipped
backpacking loads.  For the solo backing done by the developer
(akroshko), it has been especially important to get all factors
correct when trip planning.  Up to several dozen variations on a pack
load can be easily examined for iterative finding the best combination
of equipment for a specific trip.

Mobile applications, such as Paprika for storing recipes, are now
quite capable and mature.  However, I (akroshko) still prefer to edit
and store the data as a text file.  This also has advantages for both
portability and archival purposes.

<!-- TODO: view whole thing in single buffer, filter by tags/pattern,etc. -->

Requirements
============

This package requires a standard Emacs installation, of course. It has
mostly tested with the current Emacs 25 package on *Debian Linux
(Stretch)*.

The library from
[http://github.com/akroshko/cic-emacs-common](http://github.com/akroshko/cic-emacs-common)
is the only additional Emacs requirement. Particularly important is
the
[tblel.el](https://github.com/akroshko/cic-emacs-common/blob/master/tblel.el),
which supports functions that act directly on the Emacs Lisp
representation of Emacs `org-mode` tables.  The `git` version control
system is an optional requirement to using the installation
instructions below.

Installation
============

The library from
[http://github.com/akroshko/cic-emacs-common](http://github.com/akroshko/cic-emacs-common)
can be installed using the command:

    git clone https://github.com/akroshko/cic-emacs-common.git ../cic-emacs-common

meeting the assumption of the sample configuration file
`otdb-sample-config.el` that `cic-emacs-common` it is in a sibling
directory of this package.

Basic usage
===========

The
[backpacking-recipes](https://github.com/akroshko/emacs-otdb/tree/master/backpacking-recipes),
[gear](https://github.com/akroshko/emacs-otdb/tree/master/gear), and
[recipes](https://github.com/akroshko/emacs-otdb/tree/master/recipes)
directories contain some sample database files, recipes, and gear
collections.  Launch Emacs with the command `emacs -q --load
otdb-sample-init.el` to try the `otdb` package out.  Ensure the
aforementioned
[cic-emacs-common](http://github.com/akroshko/cic-emacs-common)
library is at `../cic-emacs-common/`, or modify the
[otdb-sample-init.el](http://github.com/akroshko/emacs-otdb/otdb-sample-init.el)
file accordingly.

Go to any of the directories mentioned in the previous paragraph, open
a `.org` file that does not contain `-database` in the name.  Use
`otdb-table-recalculate` (`s-d *`) to do a lookup in the database for
all items in the table at point and recalculate, where `s-` is the
super key.  If the super key is not defined for a particular system,
then remap the keys in `otdb-table.el` or call the appropriate
commands using `M-x`.  Use one, two, or three `C-u` prefixes with
`otdb-table-recalculate` (`s-d *`) to respectively recalculate for the
current file, current directory, or current directory three times.

New table rows, recipes, or gear collections are added manually for
now but the names of ingredients/items/recipes/gear collections can be
inserted with `otdb-table-goto-key-in-database` (`s-d d`).

The recipe at point can be exported with `M-x otdb-recipe-export` and
a set of recipes in a table can be exported with `M-x
otdb-recipe-export-multiple`.  There are no default key sequences to
perform export yet so the menu shuld be used.

Other functionality is also in the pull-down menu.

Complete Description
====================

### Recipes

The food database stores ingredients in an `org-mode` table, and
includes information on cost, calories, and macro-nutrients.  The
recipes themselves are also specified by `org-mode` tables with one
ingredient or a component recipe (a recipe used within the current
recipe) on each table row.  A single key sequence then queries both
the cost and nutritional values in the food database and performs the
necessary calculations.  Nested recipes allow, for example, the
cost/nutritional value of part of a tomato sauce recipe to be used in
a lasagna recipe.  This can easily be expanded into full menu plans
based solely on data about the individual ingredients. Even when
cooking completely from scratch with a partially complete database,
many different tradeoffs concerning cost/calories/etc. can be readily
examined.

##### Screenshots

<img src="screenshots/recipes.png" width=200 alt="A set of recipes and meal plans.">

<img src="screenshots/food-database.png" width=200 alt="Food database">

### Backpacking gear

The backpacking gear database stores individual items in an `org-mode`
table, and includes the weight and cost of purchase.  The gear
collections themselves (equivalent to the recipes above) are also
specified by `org-mode` tables with one item or a gear collection
(nested like the component recipes above) on each table row. A single
key sequence can then be used to query both the weight and cost values
in the database and performs the necessary calculations.  Nested gear
collections allow easy comparisons of different substitutions of gear
collections, e.g., different first-aid/repair/kitchen kits, in order
to explore and quantify any tradeoffs.  This can be easily extended
into large nested collections of gear based solely on data about the
individual items.

##### Screenshots

<img src="screenshots/backpacking.png" width=200 alt="Planning a backpacking trip">

<img src="screenshots/gear-database.png" width=200 alt="Backpacking gear database">


New development that needs to be documented
===========================================

- generating temporary buffers with list of items matching tags,
  regex, or that have been checked (uses the "s-d c" keybinding)

Planned development
===================

TODO: hardcopy for travelling where electronics is not doable

- templates/keys to create a recipe/collection and to insert a line

- export to nicely formatted .html

- export gear collections and checklists

- improve export to .pdf, collections of recipes with one recipe per page

- export to .pdf for mobile devices or hardcopy for convenience while
  working or travelling

- more complete agenda integration with better testing

- give a range of values when optional ingredients are included or
  excluded

- more convenient ways to query recipes/gear collections for
  tablet/laptop/mobile

- incorporate an SQL database for data manipulation, while still
  retaining the `org-mode` based editing and storage
