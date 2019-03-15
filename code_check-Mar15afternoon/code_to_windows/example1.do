ssc install texdoc

texdoc init example1.tex, replace

// copy http://www.stata-journal.com/production/sjlatex/stata.sty stata.sty


/***
\documentclass[a4paper]{article}
\usepackage{stata}
\begin{document}

\section*{Exercise 1}
Open the 1978 Automobile Data and summarize the variables.

***/

texdoc stlog
sysuse auto
summarize
texdoc stlog close

/***

\section*{Exercise 2}
Run a regression of price on milage and weight.

***/

texdoc stlog
regress price mpg weight
texdoc stlog close

/***

\end{document}
***/
