


use "/Users/donaldgreen/Dropbox/Field Experimentation Book/Final Code for Vignettes and Problems/Chapter 4/GerberGreenBook_Chapter4_Combined_Table_4_1_4_2.dta"

xi: reg y d2               [aw=wt]
xi: reg y d2 i.block       [aw=wt]
xi: reg y d2         xweak [aw=wt]
xi: reg y d2 i.block xweak [aw=wt]

xi: reg y d2 i.block 
xi: reg y d2 i.block xweak
