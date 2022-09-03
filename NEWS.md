NEWS for the forestplot package

Changes for 2.1.0
-----------------
* Fixed bug with how grouped data frames are processed and presented.
* Expressions are now allowed in data.frame tidyverse input
* Moved to native R-pipe operator (|> instead of %>%)

Changes for 2.0.2
-----------------
* Fixed case when all rows are summaries (Thanks Christian Röver)

Changes for 2.0.1
-----------------
* Fixed missing `lwd.zero` (issue #42)
* Fixed ignored `clip` argument when `zero` was outside the `clip` area.
* zero-line is now properly ignored when NA

Changes for 2.0
----------------
* Changed function so that it returns a `gforge_forestplot` object instead of directly plotting. The `print.gforge_forestplot` calls the draw function that converts the object to the actual forestplot. **Breaking** if you have used the function within loops this will cause a break in the old behavior.
* Implemented `dplyr` compatible API that allows using standard dplyr syntax
* Fixed shapes_gp legend bug

Changes for 1.10
-----------------
* Minor improvement for error message when plotting lines
* Bugfix for multicolumn expressions (issue #30, thanks André Gillibert)
* Added `shapes_gp` parameter (issue #32, thanks André Gillibert)

Changes for 1.9
-----------------
* Fixed bug for factor label argument

Changes for 1.8
-----------------
* Fixed issue #10 with non-log grid lines
* Suppressing the "zero" line #8
* Fixed bug when automatic lower/upper are to be identified and the data contains NA
* Fixed bug reported related to legends on SO: https://stackoverflow.com/questions/43217570/forestplot-want-to-give-legend-for-each-4-colums

Changes for 1.7
-----------------
* Fixed bug when merging `gpar()` with font & fontface options (thanks Katherine T. Mills)
* Fixed input bug with 3-dim array
* Improved auto detection of upper and lower intervals

Changes for 1.6
-----------------
* Changed forestplot function to S3 style (thanks Christian Röver)
* Fixed bug with alignment for summary elements
* When any of the inputs is missing for a line the line is skipped (issue #5)
* Added checkmate for argument validation
* Added option of just specifying the rows that the CI should be applied to (issue #7)
* Added `fpDrawBarCI` (thanks Christian Röver)
* Fixed `xlog` bug
* Added a `colgap` option
* Bug fix for plots wider than the div (Thanks Michael Obeda for reporting)

Changes for 1.5
-----------------
* Changed `new_page` to default to TRUE
* Removed warnings for compatibility
* Fixed so that squared multiline tables should now follow color behavior

Changes for 1.4
-----------------
* Added a bug fix for when number of colors isn't equal to the dimension of the input causing the box not to be drawn
* Fixed bug concerning vertical offset for clippers

Changes for 1.3
---------------
* Fixed bug when specifying `graphwidth`
* Added ability to tailor the tick text
* Added vertices example to vignette

Changes for 1.2
---------------
* Line-type can be specified
* Line endings can now be marked by a T vertical, this default for all line types other than 1
* The arrow height defaults to the same height as the vertices
* Added a grid option
* Improved documentation

Changes for 1.1
-----------------
* Multiple lines can now have NA values
* Added ability to position the graph
* Added ability to generate horizontal lines
* Minor documentation changes

Changes for 1.0
---------------
* Separation from the Gmisc-package
* Name change - forestplot2 is now just forestplot in order to allow a deprecated call
  within the Gmisc package without generating a conflict
* The fontfamily arguments are now embedded in the txt_gp that takes input from the
  `fpTxtGp` function. The fonts for the labels/summaries can be specified down to the
  cell level.
* If provided a vector of length two for the `zero` argument you get an area for the zero-effect
* There is a maximum `colgap` length for the box corresponding to the box height
* `confintNormalFn`, `confintSummaryFn`, `legendMarkerFn` are now called `fn.ci_norm`, `fn.ci_sum`, `fn.legend`
* Argument `main` in forestplot has changed to `title`
