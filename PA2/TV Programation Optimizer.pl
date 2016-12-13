% The number of available series is always greater than the number of slots to fill.

% series(Id, Name, Price, Duration, Restricted, DisplayableAnyTimesPerDay).	% Duration is multiple of 30
% slot(Day, Hour).
% preference(SeriesId, Day, Hour, NumVotes).

:- use_module(library(clpfd)).
:- use_module(data).

betterSol :-
	findall(SeriesId, series(SeriesId, _, _, _, _, _), SeriesIds),
	length(SeriesIds, Length),
	length(L, Length).
	/*reset_timer,
	labeling([], X),
	print_time.*/


reset_timer :- statistics(walltime,_).	
print_time :-
	statistics(walltime,[_,T]),
	TS is ((T//10)*10)/1000,
	nl, write('Time: '), write(TS), write('s'), nl, nl.
