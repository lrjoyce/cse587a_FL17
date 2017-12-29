(*siteSample[siteProb, backgroundProb, siteFreqs, backgroundFreqs, numDraws] simulates a process in which 
  sites (subsequences of a promoter) are drawn at random from a bag, recorded, and returned to the bag.
  siteProb and backgroundProb are the probabilities of bound sequences and non-bound sequences, respectively. 
  siteFreqs is a list of lists giving the probabilities of seeing the bases A,C,G or T in each position in 
  a bound sequence, and backgroundFreqs lists giving the probabilities of seeing A,C,G or T in an unbound 
  sequence.
  
  backgroundFreqs must be of length 4, while siteFreqs can be of any length, with each of its sublists being
  of length 4. //L - background is non-motif sequences 
  
  numDraws is an integer indicating how many different times a sequence is drawn from the bag,
  recorded, and returned to the bag.
  
  You will need to figure out how long each output sequence should be; it will be equal 
  to the length of siteFreqs.
  
  The return value is a list of lists. Each sublist has length equal to the length of
  siteFreqs and contains integers representing the bases in one drawn sequence. 
  *)
siteSample[siteProb_, backgroundProb_, siteFreqs_, backgroundFreqs_, numDraws_] := 
 Module[{numberOfBases = Length[siteFreqs]},
 		Table[	
 			If[RandomReal[] <= backgroundProb, 
 				(*if a random number is less than the background prob,  
 				create list of bases (numberOfBases) with the prob dist of backgroundFreqs.
 				There are only 4 bases (ATCG) so that's why the empirical dist only goes from 1-4.*)
 						Table[RandomVariate[EmpiricalDistribution[backgroundFreqs->Range[4]]], 
 						numberOfBases ],
 				(*if random number falls in siteProb, then create a list with probabilities
 				from the siteFreqs variable. Map over siteFreqs to get the probability of
 				bases for that specific base position.*)
 				Map[	Table[RandomVariate[EmpiricalDistribution[#->Range[4]]], 
 						numberOfBases ]&, 
 					siteFreqs]
 			]
 		(* the outside list (which contains inside lists) should be done numDraws times *)
 		,numDraws]
 	
 	]
    	    
    	    

