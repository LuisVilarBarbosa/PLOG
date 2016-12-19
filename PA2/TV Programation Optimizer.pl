/*
The number of available series is always greater than the number of slots to fill.

series(Id, Name, Price, Duration, Restricted, DisplayableAnyTimesPerDay).	% Duration is multiple of 30
slot(Id, Day, Hour).

Row = serieId
Column = slotId
preferences([
			[NumVotes, NumVotes, ...],
			[NumVotes, NumVotes, ...],
 			[NumVotes, NumVotes, ...],
 			...
]).
*/

:- use_module(library(clpfd)).
:- use_module(data).

betterSol(SeriesBySlots) :-
	findall(SlotId, slot(SlotId, _, _), SlotsIds),
	length(SlotsIds, Length),
	length(SeriesBySlots, Length).
	/*reset_timer,
	labeling([], X),
	print_time.*/


reset_timer :- statistics(walltime,_).
print_time :-
	statistics(walltime,[_,T]),
	TS is ((T//10)*10)/1000,
	nl, write('Time: '), write(TS), write('s'), nl, nl.
