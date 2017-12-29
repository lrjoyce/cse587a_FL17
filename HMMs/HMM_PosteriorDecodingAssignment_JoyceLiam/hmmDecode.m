(* decode
INPUT
 - observationSeq is a list of observations, e.g., {2,3,4,1,2}
 - states is a list of the state names, e.g., {m, h}
 - alphabet is a list of the HMM alphabet, e.g., {1, 2, 3, 4}
 - emissionMatrix is a matrix of dimensions {Length[states], Length[alphabet]}.  
     emissionMatrix[[i,j]] is the probability of emitting letter j from state i, 
     e.g., {{0.4, 0.1, 0.1, 0.4}, {0.05, 0.4, 0.5, 0.05}}
 - transitionMatrix is a matrix of dimensions {Length[states], Length[states]}.
     transitionMatrix[[i, j]] is the probability of transitioning to state j on one transition starting from state i.
     e.g., {{0.99, 0.01}, {0.01, 0.99}}
 - initialStateProbs is a list of dimensions {Length[states]}
     initialStateProbs[[i]] is the prior probability that the state from which the first observations was
     emitted is state i.  
*)

(***************************************************************)
(**************** WARNING - LONG RUN TIME **********************)
(***************************************************************)

(*outputs the sequence of states, calling upon the functions forwardAlpha and backwardBeta*)
posteriorDecode[observationSeq_, {states_, alphabet_, emissionMatrix_, transitionMatrix_, initialStateProbs_}] := 
	Module[{alpha, beta, posteriorMatrix, alphaBetaMatrix, k, stateNumberSequence={}, stateSeq={}},
	alpha = forwardAlpha[observationSeq, {states, alphabet, emissionMatrix, transitionMatrix, initialStateProbs}];
	beta = backwardBeta[observationSeq, {states, alphabet, emissionMatrix, transitionMatrix, initialStateProbs}];
	(*Multiply the forward and backward matrices and divide each column (time step) of the product matrix 
		by the sum of it's entries, making the new sum 1.*)
	alphaBetaMatrix = alpha * beta;
	(*returns the sublist position of the maximum element, in the final element of the list. i.e. 1 for m and 2 for h. puts them 
	into the list called "stateNumberSequence*)
	For[k = 1, k <= Length[alphaBetaMatrix], k++,
 		AppendTo[stateNumberSequence, Ordering[alphaBetaMatrix[[k]], -1]] 
 	];
 	(*normalize the alphaBetaMatrix to the actual posteriors. you might want to return this later*)
 	posteriorMatrix = MapThread[#1[[#2]] &, {alphaBetaMatrix,Flatten[stateNumberSequence]}];
	(*turn the list of 1's and 2's into h's and m's*)
	stateSeq = Flatten[Map[states[[#]] &, stateNumberSequence]];
	(*finally, return the list of h's and m's as stateSeq*)
	Return[stateSeq]	
];
(*outputs the alpha values for each state, from first to last*)
forwardAlpha[observationSeq_, {states_, alphabet_, emissionMatrix_, transitionMatrix_, initialStateProbs_}] := 
(*mBaseProbs and hBaseProbs are the probability of an observed base, given the state (malaria or human) *)
	Module[{ baseCases, alphaCases={}, i, mBaseProbs = Map[emissionMatrix[[1,#]] &, observationSeq],
		hBaseProbs = Map[emissionMatrix[[2,#]] &, observationSeq], forwardAlpha={}, delta},		
		(*define base cases for the viterbi. multiply inital state probs * the pr(obs|a certain state) to find basecases. 
			returns a list of length[states] *)
		(*take first observation, use that value to find the indicie of emissionMatrix sublist. apply to the 2nd level 
			(aka sublist) of emissionmatrix*)
		baseCases = MapThread[ #1* #2[[First[observationSeq] ]] &, {initialStateProbs, emissionMatrix}];
		alphaCases = Append[alphaCases, baseCases];
		(*start the counter at 2 because we already did the base case*)
		i=2;
		While[i<=Length[observationSeq], 
			(*take the last observation in viterbiCases and multiply that by the probability of changing/keeping states *)
			delta = Level[Last[alphaCases],3] * transitionMatrix;
			(*take the sum of flat delta position 1 and 3, as well as 2 and 4. this is the forward alpha*)	
			AppendTo[forwardAlpha, mBaseProbs[[i]] * Total[Part[Flatten[delta], 1;;3;;2]]];
			AppendTo[forwardAlpha, hBaseProbs[[i]] * Total[Part[Flatten[delta], 2;;4;;2]]];
			AppendTo[alphaCases, forwardAlpha];
			(*reset case so you don't get extra appends of case on case*)
			forwardAlpha={};
			i++;
		];	
		Return[alphaCases];
];
(*outputs the list of beta values, calcuting them from the final state to the first*)
backwardBeta[observationSeq_, {states_, alphabet_, emissionMatrix_, transitionMatrix_, initialStateProbs_}]:=
	Module[{betaCases={{1,1}}, i, mBaseProbs = Map[emissionMatrix[[1,#]] &, observationSeq],
		hBaseProbs = Map[emissionMatrix[[2,#]] &, observationSeq], backwardBeta={}, beta={}},		
(*			Print[observationSeq, " is obs seq"];
			Print[emissionMatrix, " is emission Matrix"];
			Print[transitionMatrix, " is transition Matrix"];
			Print["_________while loop_________"];*)
		(*go backwards through observationSeq, excluding the final observation. The final observation
		is the base case beta, which we know to be 1 for both states*)
		i=Length[observationSeq]-1;
		While[i>=1, 
			(*take the first (most recently calculated) observation in betaCases and multiply that by the probability of changing/keeping states *)
			(*find backwardBeta, which is the transition pr * the beta of the next state * the base pr|state*)
			AppendTo[backwardBeta, transitionMatrix[[1]] * Level[First[betaCases],3] *mBaseProbs[[i+1]]];
			AppendTo[backwardBeta, transitionMatrix[[2]] * Level[First[betaCases],3] *hBaseProbs[[i+1]]];
			(* ****This currently calculates Pr(base|state) * Pr(transition) * Next Beta OF SAME STATE (that you're calculating)
				i.e. if you're calculating Beta(human) it uses the beta(human) of next observation instead of both betas from the next observation **** *)
			(*find the beta of a state, by adding *)
			AppendTo[beta, Total[Part[Flatten[backwardBeta], 1;;3;;2]]];
			AppendTo[beta, Total[Part[Flatten[backwardBeta], 2;;4;;2]]];
			(*add values to the beginning of betaCases, since we are working backwards thru the list*)	
			PrependTo[betaCases, beta];
			(*reset case so you don't get extra appends of case on case*)
			beta={};
			backwardBeta={};
			i--;	
		];
		(*returns the betas in sequential order, as seen in observationSeq*)
		Return[betaCases]
	];
	
	
 (*OUTPUT
- stateSeq is a list of dimensions {Length[observationSeq]}.
  stateSeq[[i]] is the ith state in the the most likely sequence of states, given the observations. 
  e.g., {h,h,m,m,m}.
  *)
decode[observationSeq_, {states_, alphabet_, emissionMatrix_, transitionMatrix_, initialStateProbs_}] := 
(*mBaseProbs and hBaseProbs are the probability of an observed base, given the state (malaria or human*)
	Module[{stateSeq={}, stateSeqNumbers={}, baseCases, viterbiCases={}, i, mBaseProbs = Map[emissionMatrix[[1,#]] &, observationSeq],
		hBaseProbs = Map[emissionMatrix[[2,#]] &, observationSeq], case={}, k, stateTraceback, delta},		
		(*define base cases for the viterbi. multiply inital state probs * the pr(obs|a certain state) to find basecases. 
			returns a list of length[states] *)
		(*take first observation, use that value to find the indicie of emissionMatrix sublist. apply to the 2nd level 
			(aka sublist) of emissionmatrix*)
		baseCases = MapThread[ #1* #2[[First[observationSeq] ]] &, {initialStateProbs, emissionMatrix}];
		viterbiCases = Append[viterbiCases, baseCases];
		(*start the counter at 2 because we already did the base case*)
		i=2;
		While[i<=Length[observationSeq], 
			(*take the last observation in viterbiCases and multiply that by the probability of changing/keeping states *)
			delta = Level[Last[viterbiCases],3] * transitionMatrix;
			(*multiply the prob of state changes to their prob of previous states. output is list with 4 elements, called "case".
			first 2 values are for state M and last two are for state H*)		
			AppendTo[case, mBaseProbs[[i]] * Max[Part[Flatten[delta], 1;;3;;2]]];
			AppendTo[case, hBaseProbs[[i]] * Max[Part[Flatten[delta], 2;;4;;2]]];
			AppendTo[viterbiCases, case];
			(*reset case so you don't get extra appends of case on case*)
			case={};
			i++;
		];	
		(*returns the sublist position of the maximum element, in the final element of the list. i.e. 1 for m and 2 for h*)
		stateSeqNumbers = Append[stateSeqNumbers, Ordering[Last[viterbiCases],-1]]; 
		For[k=1, k<Length[viterbiCases], k++,
			stateTraceback = First[Flatten[Position[viterbiCases[[k]], Max[viterbiCases[[k]] ]] ]];
			If[TrueQ[stateTraceback == 1], AppendTo[stateSeqNumbers, 1], AppendTo[stateSeqNumbers, 2]];	
		];
		(*turn the list of 1's and 2's into h's and m's*)
		stateSeq = Flatten[Map[states[[#]] &, stateSeqNumbers]];
		(*finally, return the list of h's and m's as stateSeq*)
		Return[stateSeq]
]; 

(* calculateAccuracy takes a state sequence genereted from mixed2.fa and calculates 
the number of correctly labeled states.  Note: this function only computes the
accuracy for the mixed2.fa observations.
INPUT
 - stateSeq is a list of state sequences, e.g., {h,m,h,m,m}
 OUTPUT
 - numCorrectStates = [int] number of correcly labeled states.
*)
	
calculateAccuracy[stateSeq_] := 
	Module[{keyStateSequence, numCorrectStates},
	
	keyStateSequence = Flatten[Characters[ToLowerCase[Import["mixed2key.fa"]]]];
	numCorrectStates = Count[MapThread[Equal, {stateSeq, keyStateSequence}], True]
	];

(* readHMM takes a HMM text file and outputs the state names, the alphabet
the transition matrix, and emission matrix of the HMM

INPUT
 - file is a path to an HMM file (see notebook for the format of HMM files). 

  OUTPUT
 - states = [list] list of the state names, e.g., {m, h}
 - alphabet = [list] list of the HMM alphabet, e.g., {1, 2, 3, 4}
 - emissionMatrix = [matrix of size numStates x numAlphabet] the emission matrix.  
     Element eij = the probability of state i emitting letter j., e.g., {{0.4, 0.1, 0.1, 0.4}, {0.05, 0.4, 0.5, 0.05}}
 - transitionMatrix = [matrix of size numStates x numStates] the transition matrix.
     Element tij = the probability of state i transitioning to state j, e.g., {{0.99, 0.01}, {0.01, 0.99}}

*)

(*Note: this is not exactly how I would hav written readHMM stylewise, but it works so I won't change it for now. -MRB *)

readHMM[file_] := 
	Module[{a, numStates, alphabet, numAlphabet, firstStateIndex, lastStateIndex,
		states, firstStateProbIndex, lastStateProbIndex, initialStateProbs, 
		firstEmissionIndex, lastEmissionIndex, emissionList, emissionMatrix,
		firstTransitionIndex, lastTransitionIndex, transitionList, transitionMatrix}, 
		
	a = Import[file, {"Text", "Words"}];
	
	numStates = ToExpression[a[[1]]]; (* Use ToExpression to convert from character to number *)

	alphabet = Characters[a[[2]]];
	numAlphabet = Length[alphabet];

	firstStateIndex = 3;
	lastStateIndex = firstStateIndex + numStates - 1;
	states = a[[firstStateIndex ;; lastStateIndex]];

	firstStateProbIndex = lastStateIndex + 1;
	lastStateProbIndex = firstStateProbIndex + numStates - 1;
	initialStateProbs = ToExpression[a[[firstStateProbIndex ;; lastStateProbIndex]]];

	firstEmissionIndex = lastStateProbIndex + 1;
	lastEmissionIndex = firstEmissionIndex + numStates*numAlphabet - 1;
	emissionList = ToExpression[a[[firstEmissionIndex ;; lastEmissionIndex]]];
	emissionMatrix = Partition[emissionList, numAlphabet];

	firstTransitionIndex = lastEmissionIndex + 1;
	lastTransitionIndex = firstTransitionIndex + numStates*numStates - 1;
	transitionList = ToExpression[a[[firstTransitionIndex ;; lastTransitionIndex]]];
	transitionMatrix = Partition[transitionList, numStates];
	
	{states, alphabet, emissionMatrix, transitionMatrix, initialStateProbs}

];

	
(* readFasta reads a fasta file and outputs the nucleotide sequence converted to numbers
INPUT
- fastaFile is a string representing the path to fasta file

OUTPUT
- input is a list of bases in the file indicated by fastaFile.  
  bases are translated form ACGT to 1234.
  e.g., {1,3,2,4,2}
*)
readFasta[fastaFile_]:=
	Flatten[Map[Characters, Import[fastaFile]] 
		   /. {"A"->1, "C"->2, "G"->3, "T"->4}
		   ]