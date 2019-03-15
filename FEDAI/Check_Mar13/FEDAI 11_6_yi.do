clear


*part a
	scalar p_c_MI06 = 8755/26481
	scalar p_t_MI06 = 2123/5310
	scalar ATE_MI06 = p_t_MI06 - p_c_MI06
	disp ATE_MI06
	
	scalar p_c_MI07 = 88960/348227
	scalar p_t_MI07 = 3791/12391
	scalar ATE_MI07 = p_t_MI07 - p_c_MI07
	disp ATE_MI07
	
	scalar p_c_IL09 = 2600/15676
	scalar p_t_IL09 = 1936/9326
	scalar ATE_IL09 = p_t_IL09 - p_c_IL09
	disp ATE_IL09
	
*part b
	scalar SE_MI06 = sqrt((p_t_MI06 * (1-p_t_MI06))/5310 + (p_c_MI06 * (1-p_c_MI06))/26481)
	disp SE_MI06
	
	scalar SE_MI07 = sqrt((p_t_MI07 * (1-p_t_MI07))/12391 + (p_c_MI07 * (1-p_c_MI07))/348227)
	disp SE_MI07
	
	scalar SE_IL09 = sqrt((p_t_IL09 * (1-p_t_IL09))/9326 + (p_c_IL09 * (1-p_c_IL09))/15676)
	disp SE_IL09
	
	scalar prec_MI06 = 1/SE_MI06^2
	scalar prec_MI07 = 1/SE_MI07^2
	scalar prec_IL09 = 1/SE_IL09^2
	
	disp prec_MI06
	disp prec_MI07
	disp prec_IL09
	
*part c
	scalar weighted_mean = (ATE_MI06*prec_MI06 + ATE_MI07*prec_MI07 + ATE_IL09*prec_IL09)/(prec_MI06+prec_MI07+prec_IL09)
	disp weighted_mean
	
*part e

	input ate	
		.06919727
		.05048232
		.04173304
		end
		
	input ate_se	
		.00731764
		.00420513
		.00514433
		end

// ssc install metaan 
metaan ate ate_se, fe

//  inverse-variance weighted effect
disp  r(eff) 

//  inverse-variance weighted se
disp  sqrt(r(effvar))
