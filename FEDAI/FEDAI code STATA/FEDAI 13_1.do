clear
use "Middleton_Rogers_AI_2010"

*part a
	decode treatment, gen(treatment_string)
	mean relevant_measures_net if treatment_string=="yes"
	scalar avg_treat = _b[relevant_measures_net]
	mean relevant_measures_net if treatment_string=="no"
	scalar avg_control = _b[relevant_measures_net]
	disp avg_treat - avg_control
	scatter relevant_measures_net treatment

*part c
	ritest treatment _b[treatment], reps(10000) kdensityplot right: regress relevant_measures_net treatment

*part d
