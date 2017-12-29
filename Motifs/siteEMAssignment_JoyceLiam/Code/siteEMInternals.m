(* Put your code for sequencePosteriors, updateMotifPrior, and updatePFMCounts,
   along with any other functions you write to implement them, in this file.
   Don't forget that you can use your implementation of sitePosterior as part of your
   implementation of sequencePosteriors or, if you prefer, you can request a correct
   version from the TAs. *)
  
 	(* You have to write the code for sequencePosteriors. They are the posteriors calculated according to EM.
    They are only unnormalized in the sense that the window normalization "hack" has not yet been applied. *)	
  sequencePosteriors[fullSequence_, oldMotifPrior_, oldPFM_, backgroundFreqs_] :=
	Module[{k, shreddedSequences = {}, motifLength = Length[oldPFM]}, 
	(*make the shredded sequences. There will be 1+length[sequence]-motifLength shredded sequencesS, each with with motifLength. *)
	For[k = 1, k <= (1+Length[fullSequence]-motifLength), k++, 
		shreddedSequences = Append[shreddedSequences, 
									Take[fullSequence, 
										{k, k + motifLength - 1}]
									]
	];
	
	(*map siteposterior over each shredded sequence*)
	Map[sitePosterior[#, oldMotifPrior, 1 - oldMotifPrior, oldPFM, backgroundFreqs] 
		&, shreddedSequences]
	]
 
   (* You have to write the code for updateMotifPrior. The motif prior is the probability of finding a motif
	in a given position, prior to considering the sequence at that position. *)
   (*thing is a list of normalized posteriors. output is one number*)
   updateMotifPrior[normalizedPosteriors_]:=
   	Module[{},
   		(* take sum of values from sublists, which is at level 2. Then total that. That's the numerator.
   		find the lenghts of normalizedPosteriors then total that for the denominator *)
   			Total[	Total[normalizedPosteriors, {2}]	] 
			/ Total[	Map[Length, normalizedPosteriors]	]
   	]
   	
   	
   (* *** UPDATE PFM COUNTS ISN'T COMPLETE, PLEASE DON'T GRADE CODING STYLE*** *)
   (* You have to write the code for updatePFMCounts. This is the maximum likelihood estimate of the PFM
		    based on the expected counts of the nucleotides at each position in the PFM. *)
   (* max liklihood is like products of Pr(ith sequence | posteriors) ????*)
   (*output new counts of probabilities in list of lists form *)
   (*probability each nucleotide occurs at a given position*)
   updatePFMCounts[motifLength_, input_, normalizedPosteriors_, motifPseudocounts_, erasers_]:=
   	Module[{k, j, shreddedSequences={}, infoTheoryArray
   		},
   		
   		(*infoTheoryArray is a way to show input code in a different format. we multiply by 
   		erasers here. infoTheoryArray as one more level than input (3 instead of 2)*)
   		infoTheoryArray = erasers*Map[
   			Which[# == 1, {1,0,0,0}, # == 2, {0,1,0,0}, 
   				  # == 3,{0,0,1,0}, # == 4, {0,0,0,1}] &, input, {2}];
   				  
   	(*BELOW IS UNFINISHED*)		  
(*	Take[
		For[k = 1, k <= (1+Length[input]-motifLength), k++, 
		shreddedSequences = Append[shreddedSequences, 
									Take[input, {k, k + motifLength - 1}]]], {1, Length[input]}]*)
									
(*		Take[ 
			Take[ ], {1, Length]*)
   		(*MapThread[ #2 * [[#1]] &, {infoTheoryArray, normalizedPosteriors}]*)
   		(*Times[infoTheoryArray, Level[normalizedPosteriors,1]]*)
		(*Map[ # &, normalizedPosteriors]*)
(*   		infoTheoryArray;
		For[k = 1, k <= (1+Length[Flatten[shreddedSequences,1]]-motifLength), k++, 
		shreddedSequences = Append[shreddedSequences, 
									Take[infoTheoryArray, {k, k + motifLength - 1}]]
			];
		input
		 MapIndexed[ Do[ Take[infoTheoryArray, {infoTheoryArray[[#]], infoTheoryArray[[#]]+motifLength-1 }  ] , 
		 		(Length[infoTheoryArray] -motifLength+1)]
		 	&,infoTheoryArray, {4}]*)

   		(*Map[# &, infoTheoryArray]*)
   		(*if{erasers==False, doSomething;, erasers==1};*)
(*   		if{TrueQ[erasers], doNothing, erasers=1};
   		(*only multiple in the erasers if the erasers are true*)
   		normalizedPosteriors*)
   	]   
   (*A draw is the observed bases of a drawn sequence, which is subsequently replaced.
  sitePosterior calculates the posterior probability of a bound site versus a non-bound site, based on the
  observed sequence in the draw, the proportion of bound vs. non-bound sequences in the bag, and the 
  probabilities of observing each base in a sequence for bound and non-bound sites.
  The single number returned is the posterior probability of a bound site.*)
sitePosterior[sequence_, sitePrior_, backgroundPrior_, siteProbs_, backgroundProbs_] := 
Module[{ siteLiklihoodAndPrior, backgroundLiklihoodAndPrior },
		(*find product of liklihood and prior for site and background groups*)
		
		siteLiklihoodAndPrior = sitePrior * Apply[Times, 
			(*the value of a number at sequence indicie is the new indicie indicator at siteProbs. So seq = {3,_,_} means
			we pick the third number in the first list of SiteProbs. if seq={_,4,_}, we pick the 4th number in the 2nd list of
			siteProbs*)
				MapThread[ #2[[#1]] &, {sequence, siteProbs}] 
				]; 		
		backgroundLiklihoodAndPrior = backgroundPrior * Apply[Times,
			(*the sequence value in the list indicates the probability of that base according to the background Probs list*)
				Map[backgroundProbs[[#1]]&, sequence]];
		(* if x is 0, then our sequence must have come from the background!*)
		If[ siteLiklihoodAndPrior==0,
			(*if we have no sequences, then spit out an error message*)
			If[ backgroundLiklihoodAndPrior ==0, (*Print["ahh, you broke me! Check your inputs"]*)
				(siteLiklihoodAndPrior/ (siteLiklihoodAndPrior+backgroundLiklihoodAndPrior)) 
					, 0],
			siteLiklihoodAndPrior / (siteLiklihoodAndPrior+backgroundLiklihoodAndPrior)	
		]
]
