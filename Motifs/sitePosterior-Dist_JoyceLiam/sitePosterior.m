
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

