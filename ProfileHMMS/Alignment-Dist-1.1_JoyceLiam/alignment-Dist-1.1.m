(* 
In this lab, you will implement the Smith-Waterman algorithm for local alignment of
two DNA sequences. 

INPUT
 - The actual inputs to smithWaterman are file paths to two fasta files containing
   DNA sequences and one file containing a scoring matrix. smithWaterman reads these
   files in and sets seq1, seq2, and scoringMatrix to the results.
 - seq1 and seq2 are lists of integers representing DNA sequences, e.g., {2,3,4,1,2}
 - scoringMatrix is a 5x5 symmteric matrix giving the -log2 probabilities of each an 
   alignment column containing each pair consisting of two DNA bases or one base and 
   an insertion symbols, represented by number 5. The score for a pair of two insertion
   symbols should be -Infinity.
OUTPUT
- A matrix whose columns are printed representations of alignment columns. The matrix
  should be displated by passing it to prettyPrintAlignment. 
  *)

smithWaterman[fastaFilePath1_, fastaFilePath2_, scoringMatrixPath_] := 
	Module[{seq1, seq2, scoringMatrix},
		seq1=readFasta[fastaFilePath1];
		seq2=readFasta[fastaFilePath2];
		scoringMatrix=readScoringMatrix[scoringMatrixPath];
		alignmentDisplayMatrix[
			swTraceback[
				swBuildMatrix[seq1, seq2, scoringMatrix],
						seq1, seq2, scoringMatrix]
						]

	];

(*swBuildMatrix returns a list whose first element is the coordinates of a top-scoring cell in the 
  Smith-Waterman matrix. The second element is the Smith Waterman matrix itself. No traceback
  pointers are kept.*)
swBuildMatrix[seq1_, seq2_, scoringMatrix_] :=
	Module[{matrix, index1, index2, topScoringCell,
		i,j, indexScore, above, left, corner},
		(*The SW matrix is larger than sequence by 1 in each dimension to accomodate the
		  first row and column. seq1 is rows, seq2 is columns*)
		matrix=ConstantArray[0, {Length[seq1]+1, Length[seq2]+1}];
		(*turn scoringMatrix into an actual matrix (not a list) with one dimension as 5; 4 bases and one empty*)
		(*scoringMatrixReshape = ArrayReshape[scoringMatrix, {Length[scoringMatrix]/5, 5}];*)
		For[i=2, i<=Length[seq1]+1, i++,
			For[j=2, j<=Length[seq2]+1, j++,
				index1 = seq1[[i-1]];
				index2 = seq2[[j-1]];
				indexScore = scoringMatrix[[index1,index2]];
				(*calculates scores up and left one cell*)
				corner = matrix[[i-1, j-1]] + indexScore;
				(*calculates score above current cell, and left of current cell*)
				(*if the calculation is below 0, replace it with 0 otherwise leave it alone*)
				above  = matrix[[i, j-1]]   + scoringMatrix[[5, index2]];
				If[above<0, above = 0, above];
				left   = matrix[[i-1, j]]   + scoringMatrix[[index1, 5]];
				If[left<0, left=0, left];
				(*replace the bottom corner cell (in a 4x4 matrix for example) with the max from the top,
				left, or corner cell*)
				matrix[[i, j]] = Max[corner, above, left];

				(*find the position of the highest value in the matrix*)
				topScoringCell = Flatten[Position[matrix, Max[matrix]]];
				];
			];
		(*Print[MatrixForm[matrix]];*)
		Return[{topScoringCell, matrix}];
	];

(* swTraceback returns a list of pairs representing alignment columns. The elements of each pair are the 
   integers 1-5, with 5 representing a gap in the alignment.
   
   Implementation: Starting topScoringCell, trace back by checking whether the score in the current cell
   is what you would have gotten from each of the previous cells. The current cell should be updated to
   a previous cell that could yield the score of the current cell. 
   
   It is possible that more than one of the three previous adjacent cells would yield the score in the 
   current cell. If an alignment without gap and an alignment with a gap are available, always prefer the 
   one without a gap. If a gap in either sequence is possible, prefer to put the gap in seq2 over seq1.
   
   You may find AppendTo useful for adding alignment columns to the end of the list of columns. AppendTo
   is much faster than PrependTo but you'll have to use Reverse to reverse the order of the columns
   before returning it. If you really want it to be fast you can look up and use Reap and Sow but this
   is not required.
 *)
swTraceback[{topScoringCell_, swMatrix_}, seq1_, seq2_, scoringMatrix_]:=
	Module[{alignmentColumns={}, index1=topScoringCell[[1]], index2=topScoringCell[[2]], 
			currentCellScore=Extract[swMatrix, topScoringCell]
			, above, left, corner, maxScore,i=0},
		If[Length[swMatrix] <= 3,
			index1 = index1+1;
			index2 = index2+1;
		];
		
		While[ currentCellScore >0,
			(*only calculated corner, above, left cell scores if we still have bases left in the sequence*)
			If[index1 >2 && index2>2,
				corner = scoringMatrix[[
					seq1[[index1-2]], seq2[[index2-2]] ]] + swMatrix[[index1-1, index2-1]];
				above = scoringMatrix[[
					seq1[[index1-2]], seq2[[index2-1]] ]] + swMatrix[[index1-1, index2]];
				left = scoringMatrix[[
					seq1[[index1-1]], seq2[[index2-2]] ]]  + swMatrix[[index1, index2-1]];
				maxScore = Max[corner, above, left];
			(*if there are no bases left, end the loop*)
			,currentCellScore =0;];	

			Which[
				maxScore == corner,
					currentCellScore = swMatrix[[index1-1, index2-1]];
					(* if previous append (i) didn't happen, then append*)
					If[i ==0,
						AppendTo[alignmentColumns, {seq1[[index1-1]], seq2[[index2-1]]}];
					];
					(*make sure to reduce the indicies appropriately*)
					index1--;
					index2--;
					i=0;
				,
				maxScore == above,
					currentCellScore = swMatrix[[index1-1, index2]];
					AppendTo[alignmentColumns, {seq1[[index1-1]], seq2[[index2-1]]}]; (*append prev seq*)
					i=1;
					index1--;
				,
				maxScore == left,
					currentCellScore = swMatrix[[index1, index2-1]];
					AppendTo[alignmentColumns, {seq1[[index1-1]], seq2[[index2-1]]}]; (*append prev seq*)
					i=2;
					index2--;
			];
			
			If[i==1, AppendTo[alignmentColumns, {seq1[[index1-1]], 5}];,
				If[i==2, AppendTo[alignmentColumns, {seq1[[index1-1]], 5}];
			]];
		];
		Return[Reverse[alignmentColumns]];
	];

(*prettyPrintAlignment prints an alignment of two DNA sequences in the tradition, BLAST-like format.*)
prettyPrintAlignment[displayMatrix_]:=
	Print[Grid[displayMatrix, Spacings -> {0, 0}]];
 
(* alignmentDisplayMatrix converts a list of alignment columns into the three three rows needed to 
	print it in the traditional, BLAST-like format. This matrix is passed into prettyPrintAlignment.
*)
alignmentDisplayMatrix[columns_]:=
	{
	columns[[All, 1]] /. {1->"A", 2->"C", 3->"G", 4->"T", 5->"_"},
	Map[If[#[[1]]==#[[2]], "|", " "] &,columns],
	columns[[All, 2]] /. {1->"A", 2->"C", 3->"G", 4->"T", 5->"_"}
	}



(* readScoreMatrix takes a pathname of a tab separated file that should contain a 5x5 matrix,
   with one line for each row of the matrix.*)
readScoringMatrix[filePath_] := 
	Module[{matrix}, 		
	matrix = Import[filePath];
	(*Probably should put a bunch of checks for the right dimensions and contents here.*)
	Return[matrix];
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