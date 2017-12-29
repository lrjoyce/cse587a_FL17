
(* proceduralRace is a procedural implementation of the car racing simulation.
   It is procedural because (1) it modifies the values of the list currentPositions, 
   and (2) it modifies the values of loop counters. *)

proceduralRace[totalTime_, initPositions_]:=
	(* Module sets up local variables that are not visible to code outside this function.
	   Note the syntax: Module[{variable1, variable2, ...}, code]. The variables can be 
	   initialized with an = or left unitialized.*)
	Module[{currentPositions=initPositions, timeRemaining, carNum},
	  (* Outer loop counts down the time remaining in the race. *)
	  For[timeRemaining=totalTime, timeRemaining > 0, timeRemaining--,
	  	  (*At each time, start by printing out the time and a character graphic representing the positions.*)
	  	  drawState[timeRemaining, currentPositions];
	  	  (*Inner loop goes through the 'cars' -- i.e. slots in the list of car positions.*)
	  	  For[carNum=1, carNum <= Length[initPositions], carNum++,
	  	  	(* One third of the time, increment (modify!) the position of the current car.*)
	  	  	If[RandomReal[{0, 1}] < 1/3, 
 		  	   currentPositions[[carNum]]++
 		    ]]]]

drawState[time_, positions_]:=
	(*Here Module is just a convenient way to let Mathematica know that all the print statements are part of 
	  the function drawState. It is necessary to make this explicit because function definitions
	  do not have their own scope delimeters. A pair of parentheses around the function body would also work.*) 
	Module[{},
		Print[];
		Print["Positions with ", time, " minutes remaining"];
		(* Here I used Map because it was the simplest way construct a graphic for each positions in the list.
		   # indicates the position to which graphic-constructing function is being applied. & marks it as a
		   function. Note that this function has no name -- it is only used here.*)
 		Print[Column[Map[StringJoin[Table["*", {#}]] &, 
 			             positions]]]
	]
	
(* Now you implement a 'mostly functional' version. It still relies on printing out the results at each step,
   which is a technically a side-effect that makes this not fully functional. *)

(* When writing functional code, you want to think about the most basic operations first -- the ones that
   would live inside the innermost loop of a procedural program. Generally each time you use a piece of information
   to produce a new piece of information, you will want to write a separate function. Fine-grained, with lots of 
   small functions, is good.
   
   To get started, we've given you the outlines of a function that takes in the list of positions at any given time
   step and returns the list of positions at the next time step. We call this runStepOfRace..
   
   You need to write the anonymous function that is mapped over the input list of positions. This replaces the inner
   'For' loop of the procedural implementation. *)

runStepOfRace[positions_]:=
 	Map[ (* Insert missing code here; implements a function that is applied to the each position in the input positions.
 	       It should use RandomReal and return the original position with probability 2/3 and the position plus one
 	       with probability 1/3. Like with any anonymous function, # refers to the argument passed in & indicates that it is
 	       a function. *)
 	       
 	       If[RandomReal[] < 1/3
 	       	, #+1
 	       	, #]
 		 &,
 		positions]
 
(* To eliminate the outer 'For' loop of the procedural implementation, which counted down the remaining time, you will 
   use a recursive function call. Meaning that the body of the function mostlyFunctionalRace will call mostlyFunctionalRace. 
   At each call to mostlyFunctionalRace, the time remaining is decremented and the most recent postions-list is passed in.*)

mostlyFunctionalRace[time_, positions_]:=
	Module[{},
		   drawState[time, positions];
		    (* Insert a recursive call to mostlyFunctionalRace here, reduce the time by one and update the positions within
		   	      the arguments to mostlyFunctionalRace. *)
		   If[time > 0, 
		  mostlyFunctionalRace[time-1, 
		   		runStepOfRace[positions]], positions]
		  ]

(*NOTICE: The reason you needed a recursive function call to eliminate the outer loop rather than just a Map is that 
  the computational at each time point depends on the positions determined for the previous time point. Map only works
  when the computation on each element of a list is independent of the results of on all other elements of the list. *)	
 
 (*Here, you implement a fully functional version that accumulates the results of the various steps as a matrix.
  Because the matrix is the return value of totallyFunctionalRace, it doesn't have to be assigned to a variable.
  This version contains no assignment and no side effects. The entire result is returned from the function call.*)

totallyFunctionalRace[timeRemaining_, positionsMatrix_]:=
	  If[timeRemaining <= 0,
	  	positionsMatrix,
 	    (*The first arguments to 'If' deal with the base case -- when no time remains, the simulation is over and the results,
	      which have been accumulalted during the computation, are returned.*)
	  	(*Insert missing line of code here. It is a recursive function call. The second argument should run one step
	  	  of the simulation, starting from the state at the end of the positions matrtix, and append the results to 
	  	  the end of posistionsMatrix. The built-in functions Append and Last will be useful here. *)
	  	  totallyFunctionalRace[timeRemaining-1,
	  	  	Append[positionsMatrix, runStepOfRace[Last[positionsMatrix]]
	  	  	] 
	  	  ]
	   	 ]

(* You don't have to do anything here, but please note the use of MapThread to map a function of more than one
   variable.*)	   	 
displayRaceResults[positionsMatrix_]:=
	MapThread[drawState, {Range[Length[positionsMatrix] - 1,
		                        0,
		                        -1], 
		                  positionsMatrix}];

(* If you choose to implement the bonus question, move the line below outside of the comment and use it as your starting point.

compositionalFunctionalRace[timeRemaining_, numberOfCars_]:=

*) 