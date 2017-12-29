 diceEM[sample_, maxIterations_, accuracy_]:=
	Module[{numFaces = Max[sample],
			(*choose somewhat arbitrary type 1 and 2 probs. just can't be .5, 0, or *)
			oldType1Prob=.55, oldType2Prob=.45, 
			binCounts, oldFaceProbs1,oldFaceProbs2, 
		    newType1Prob, newType2Prob, 
		    newFaceProbs1, newFaceProbs2, loopNumber},
		
		(*choose somewhat random face probabilities and normalize them *)
		oldFaceProbs1= RandomReal[ {1/numFaces, 2/numFaces}, numFaces];
		oldFaceProbs1= Normalize[oldFaceProbs1, Total];
		oldFaceProbs2= RandomReal[ {1/numFaces, 2/numFaces}, numFaces];
		oldFaceProbs2= Normalize[oldFaceProbs2, Total];
		
		(*create empty binCounts array, append in appropriate values, get rid of the extra one*)
		binCounts={{}};
		Do[binCounts = AppendTo[binCounts, BinCounts[sample[[i]],{1, numFaces+1, 1}]], {i, Length[sample]}];
		binCounts = Delete[binCounts, 1];
		
		(* Loop here until either maxIterations has been reached or the sum of the absolute 
		   values of the changes from one iteration to the next in all estimated parameters is 
		   less than accuracy.
		   On each iteration, call updateProbs, passing in the old values, to set the new values. 
		   Then test whether the termination conditions have been met (Return[] breaks a loop 
		   returning its argument). Finally, if termination conditions have not been met, set 
		   old values to be the same as the new values.*)		
		
		(*loopNumber does the a number of loops up to and including maxIterations*)
		For[loopNumber=0, loopNumber<=maxIterations, loopNumber++,
			{newType1Prob, newType2Prob, newFaceProbs1, newFaceProbs2}=
					updateProbs[binCounts, oldType1Prob, oldType2Prob, oldFaceProbs1, oldFaceProbs2];
			(*if sum of absolute values of changes from iteration to next in all estimated
			parameters is >accuracy, call updateProbs. Otherwise, break and update Type and face Probs *)
		If[  (Total[Abs[oldFaceProbs1-newFaceProbs1]]+Total[Abs[oldFaceProbs2-newFaceProbs2]]
			+Abs[oldType1Prob-newType1Prob]+Abs[oldType2Prob-newType2Prob])> accuracy,
				(oldType1Prob  = newType1Prob;
				oldFaceProbs1 = newFaceProbs1;
				oldType2Prob  = newType2Prob;
				oldFaceProbs2 = newFaceProbs2;),
			Return[If[newType1Prob <= newType2Prob,
		   			{newType1Prob, newType2Prob, newFaceProbs1, newFaceProbs2},
		   			{newType2Prob, newType1Prob, newFaceProbs2, newFaceProbs1}]]
		];
	];
		(*At the end, return the estimated parameters with the less likely die first.*)
	If[newType1Prob <= newType2Prob,
		   {newType1Prob, newType2Prob, newFaceProbs1, newFaceProbs2},
		   {newType2Prob, newType1Prob, newFaceProbs2, newFaceProbs1}
		];
	]

updateProbs[binCounts_, oldType1Prob_, oldType2Prob_, oldFaceProbs1_, oldFaceProbs2_] :=
	Module[{posteriors,type1Count, type2Count, faceCounts1, faceCounts2, type1and2Count},
		(*Create list of posterior probabilities of a Type1 die having been rolled on each 
			draw by calling your dicePosteriors*)
		posteriors = Map[dicePosterior[#, oldType1Prob, oldType2Prob, oldFaceProbs1, oldFaceProbs2]&,
			 binCounts];
		(* Now use the posteriors to calculate EXPECTED counts for each die and each face in the sample.*)
		faceCounts1 = Total[(binCounts * posteriors)];
		faceCounts2 = Total[(binCounts * (1-posteriors))];
		type1Count  = Total[posteriors];
		type2Count  = Total[1-posteriors];
		(* Finally, use these counts to compute maximum likelihood estimates 
			for the parameters and return these estimates in a list.*)
		type1and2Count = (type1Count + type2Count);
		{type1Count/type1and2Count,type2Count/type1and2Count,
			faceCounts1/Total[faceCounts1],faceCounts2/Total[faceCounts2]}
	]

diceSample[numType1_, numType2_, type1_, type2_, draws_, rollsPerDraw_] :=
 Module[{ totalDie = (numType1 + numType2)},
 	
	  Table[
	  	(*choose which die, stays for this draw*)
 		If[RandomReal[]<= N[numType1 / totalDie], 
 			(*choose a random variable from the type face, with correct probabilities.*)
 			(*create a table of the face values, with rollsPerDraw as the length *)
		 		(*if true, use dice 1 type*)
		 		Table[RandomVariate[ EmpiricalDistribution[ type1 -> Range[Length[type1]]]], rollsPerDraw]
		 		(* else, use dice 2 type*)
		 		,Table[RandomVariate[ EmpiricalDistribution[ type2-> Range[Length[type2]]]], rollsPerDraw]
 		]
 	(*make a table of the face tables, draws number of times*)
 	,draws]	
 ]
 
 
dicePosterior[dicePosteriorBinCounts_, type1Prior_, type2Prior_, faceProbs1_, faceProbs2_] :=
	Module[{
	(* type1LikelihoodAndPrior is Pr[bin|die type 1]Pr[type1Prior] and type2LikelihoodAndPrior is same but for die 2 *)
		type1LikelihoodAndPrior = (type1Prior*(Apply[Times,
		
		(*delete all positions in binCounts[]=0, and their corresponding probabilities. produce new lists.
		take the faceProbs^binCounts to determine the probability that die type X outputs the given binCounts*)
		
			 ( (Delete[faceProbs1, Position[dicePosteriorBinCounts, 0]]) ^ (Delete[dicePosteriorBinCounts, Position[dicePosteriorBinCounts, 0]]) )])),
		 
		type2LikelihoodAndPrior = (type2Prior*(Apply[Times, 
			 ( (Delete[faceProbs2, Position[dicePosteriorBinCounts, 0]]) ^ (Delete[dicePosteriorBinCounts, Position[dicePosteriorBinCounts, 0]]) )]))	
		},
	(* outer if statement: return the value of 0 (or 0%) that the outcome of binCounts is produced by die type 1 if die type 1 never occurs*)
		If[ type1LikelihoodAndPrior==0, 
			(*if both type 1 and 2 priors are 0, or they have no probability of creating a value that was obtained, print out a message *)
			If[type2LikelihoodAndPrior==0, "not possible", 0],
			(*rearrange the binomial coefficent formula*)
		1 / (1 + (type2LikelihoodAndPrior / type1LikelihoodAndPrior) )
	]


]