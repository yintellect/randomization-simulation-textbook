* syntax to check the coefficients in exercise 9.8

gen Race = 0
replace Race = 1 if race=="w"

gen City = 0
replace City = 1 if city == "c"

gen Resume = h

gen Call = call*100

gen Race_City = Race * City
gen Race_Resume = Race * Resume
gen City_Resume = City * Resume
gen Race_Resume_City = Race * Resume * City

reg Call Race Resume City Race_Resume Race_City  City_Resume Race_Resume_City

/*
RESULTS

----------------------------------------------------------------------------------
            Call |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-----------------+----------------------------------------------------------------
            Race |   3.136531   1.647629     1.90   0.057    -.0935665    6.366629
          Resume |   1.491703    1.64839     0.90   0.366    -1.739888    4.723293
            City |  -1.488682   1.566962    -0.95   0.342    -4.560635    1.583271
     Race_Resume |   1.484541   2.331176     0.64   0.524    -3.085618    6.054699
       Race_City |   -1.49474   2.216018    -0.67   0.500    -5.839138    2.849658
     City_Resume |  -1.735498   2.212222    -0.78   0.433    -6.072453    2.601457
Race_Resume_City |   .5393574   3.128554     0.17   0.863    -5.594024    6.672738
           _cons |    7.01107    1.16505     6.02   0.000     4.727046    9.295094
----------------------------------------------------------------------------------

. 


/*
