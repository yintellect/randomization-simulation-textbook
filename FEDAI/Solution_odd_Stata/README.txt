Solution Sets for: Field Experiments: Design, Analysis, and Interpretation

This folder contains three things:

1. A complete solution set (in PDF form) for all 13 chapters
2. A set of solutions for each of the 13 chapters, in .rmd and .pdf format
3. The data and r programs for all of the exercises that require coding

The problem sets for chapters 1 and 2 do not include any coding, so they are written in pure LaTeX.

The problem sets for chapters 3-13 do include code, so they are written using knitr, a package for R that combines LaTeX with R code. (see http://yihui.name/knitr/). The source files for these chapters are .rmd files. These .rmd files produce a .tex file, which, when compiled, produces the .pdf file. Writing in knitr does not require, but is vastly aided by, the use of Rstudio as an editor for R (www.rstudio.com).

The code you see in the .rmd solution files is contained within “code chunks.” The code is contained in a special wrapper, e.g.:

<<>>=
2 + 2
@

When the .rmd is compiled, the line “2 + 2” will be included in the resulting .pdf, as will the evaluation of that line, “[1] 4”.

In order to run many of the examples, you will need to obtain the replication datasets posted at http://isps.yale.edu/FEDAI.  You will need to change the lines in the .rmd files to point to the correct locations on your own computer.

These solution sets have been compiled through the joint effort of TAs and RAs over a number of years of teaching from this text.  However, it is possible that some errors in the solutions remain.  If you think you have found an error, please email Donald P. Green or Alan S. Gerber.

