clear

/*----------------------------------------------
 part b
----------------------------------------------*/
	scalar pi_at = 2/398
	scalar pi_nt = (3+1)/(404+398)
	scalar pi_c = 1- pi_at - pi_nt
	scalar itt_d = pi_c
	disp %8.2f itt_d
	
	scalar itt_hotline = (296+301)/(404+398) - (261/398)
	disp %8.5f itt_hotline

	scalar cace_hotline = itt_hotline/itt_d
	disp %8.5f cace_hotline
	
	scalar itt_arrest = (303)/(802) - (151/398)
	disp %8.6f itt_arrest
	
	scalar cace_arrest = itt_arrest/itt_d
	disp %8.6f cace_arrest
