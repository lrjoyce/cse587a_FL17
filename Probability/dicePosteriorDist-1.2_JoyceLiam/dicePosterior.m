
(*A draw is the series of rolls between the time a die is drawn from the bag and the time it is returned to the bag.
  dicePosterior calculates the posterior probability of a type1 versus type2 die, based the number of times each face
  appears in the draw and the relative numbers of Type 1 and Type 2 dice in the bag as well as the face probabilities
  for Type 1 and Type 2 dice. The single number returned is the posterior probability of Type 1.
  
  You will need to catch the case where the probability of a face showing is zero and it shows zero times. 0^0 is undefined,
  but there is a simple intuitive number that you should use in this case.
  *)

dicePosterior[binCounts_, type1Prior_, type2Prior_, faceProbs1_, faceProbs2_] :=
Module[{
	(* type1LikelihoodAndPrior is Pr[bin|die type 1]Pr[type1Prior] and type2LikelihoodAndPrior is same but for die 2 *)
	type1LikelihoodAndPrior = (type1Prior*(Apply[Times,
		
		(*delete all positions in binCounts[]=0, and their corresponding probabilities. produce new lists.
		take the faceProbs^binCounts to determine the probability that die type X outputs the given binCounts*)
		
		 ( (Delete[faceProbs1, Position[binCounts, 0]]) ^ (Delete[binCounts, Position[binCounts, 0]]) )])),
		 
	type2LikelihoodAndPrior = (type2Prior*(Apply[Times, 
		 ( (Delete[faceProbs2, Position[binCounts, 0]]) ^ (Delete[binCounts, Position[binCounts, 0]]) )]))	
	},
	(* outer if statement: return the value of 0 (or 0%) that the outcome of binCounts is produced by die type 1 if die type 1 never occurs*)
	If[ type1LikelihoodAndPrior==0, 
		(*if both type 1 and 2 priors are 0, or they have no probability of creating a value that was obtained, print out a message *)
		If[type2LikelihoodAndPrior==0, "not possible", 0],
		(*rearrange the binomial coefficent formula*)
	1 / (1 + (type2LikelihoodAndPrior / type1LikelihoodAndPrior) )
	]
]
