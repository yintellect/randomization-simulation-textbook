clear
input D 
	0 
	0 
	0 
	0 
	0 
	1 
	1 
	1 
	1 
	1
	end
	
input Y 
	1
	0
	0
	4
	3
	2
	11
	14
	0
	3
	end


gen Y_star= Y+D*(-7)
ritest D _b[D], reps(10000) left seed(1234567): regress Y_star D
