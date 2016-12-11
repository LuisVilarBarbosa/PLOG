% The number of available series is always greater than the number of slots to fill.

% series(Id, Name, Price, Duration, Restricted, DisplayableAnyTimesPerDay).	% Duration is multiple of 30
% slot(Day, Hour).
% preference(SeriesId, Day, Hour, NumVotes).

betterSol :-
	findall(SeriesId, series(SeriesId, _, _, _, _, _), SeriesIds),
	length(SeriesIds, Length),
	length(L, Length).
