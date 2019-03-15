// start with an empty data sets of classes containing
        // only schoolid and classid
        use example_classes.dta
        forvalues i = 1/100 { //create and save 100 permutations of treatment
        tempvar random cutoff
        gen `random'=rnormal()
        bys schoolid: egen `cutoff' = median(`random')
        gen treatment`i' = `random'> `cutoff'
        }
        tempfile permutations
        save `permutations'
        use example_students.dta
        //using this file
        ritest treatment _b[treatment], r(500) ///
        samplingsourcefile("`permutations'") samplingmatchvar(schoolid classid): ///
        reg testscore treatment age, cluster(classid)
        //redoing the analysis testing the null of the treatment effect being 10(constant)
        ritest treatment _b[treatment], r(500) null(testscore 10) ///
        samplingsourcefile("`permutations'") samplingmatchvar(schoolid classid): ///
        reg testscore treatment age, cluster(classid)
        //redoing the same analysis using automatic resampling
        ritest treatment _b[treatment], r(500) ///
        strata(schoolid) cluster(classid) : ///
        reg testscore treatment age, cluster(classid)
