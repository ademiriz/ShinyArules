# ShinyArules
# I modified Andrew Brooks' code to handle transaction data. The sample transaction data file is given in Excel format. 
# I ran attached code without any issue in a R version of MS SQL - R Server. But, when I tried to test on a different machine I ran into some problems. 
# So, I advice that you install most recent version of R, Rstudio and RTools on your machine. properly.
# You need to compile the package rlang in order to run (install) the package devtools properly 
# install.packages("pak")
# pak::pkg_install("r-lib/rlang")
# install rlang package by executing above two lines. But make sure that you properly installed RTools for Windows first. 
# Then install package devtools by
# install.packages("devtools", dependencies = TRUE)
# after that you can install other necessary packages before running the content of file "Run_arulesInterfaceXL.R". After they are all installed, you can just highlight 
# (select all lines) the content of file "Run_arulesInterfaceXL.R" and run the selected files.
# comments are all welcome. Email me at ademiriz@gmail.com
