/*
The number of available series must be always greater than the number of slots to fill.
The facts stored must be ordered by id and the first id must be 1.

series(SeriesId, Price, Duration, MinHour, MaxHour).	% Duration is multiple of 30 and MinHour < MaxHour.
noSameDay(SerieId1, SerieId2).
seriesName(SeriesId, SeriesName).
slot(SlotId, DayId, Hour, Minute).	% The duration of a slot is 30 minutes.
slotDay(DayId, DayName).

Row = serieId
Column = slotId
preferences([
			[NumVotes, NumVotes, ...],
			[NumVotes, NumVotes, ...],
 			[NumVotes, NumVotes, ...],
 			...
]).	% NumVotes must be greater than 0
*/

:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- load_files(dataM).

betterSol :-
	reset_timer,
	flatData(Series, Slots, Preferences),
	% Variables declaration
	length(Slots, SlotsLength),
	NumSlots is SlotsLength // 4,	% 4 = number of arguments of slot()
	length(SeriesBySlot, NumSlots),	% SeriesBySlot -> list of SeriesId which index is the SlotId
	% Variables domain declaration
	length(Series, SeriesLength),
	Max is SeriesLength // 5,	% 5 = number of arguments of series()
	domain(SeriesBySlot, 1, Max),
	% Restrictions declaration
	numSeriesOccurs(Series, SeriesBySlot),
	contiguousSeries(SeriesBySlot, SeriesBySlot, Series, Slots),
	restrictSchedule(SeriesBySlot, Series, Slots),
	restrictNoSameDay(SeriesBySlot, Slots),
	% Evaluation function
	calcValues(SeriesBySlot, Series, Preferences, AllSlotsValue),
	print_time,
	% Search for solutions
	write('Labeling beginning'),
	nl,
	reset_timer,
	labeling([minimize(AllSlotsValue), occurrence, all], SeriesBySlot),
	print_time,
	fd_statistics,
	nl,
	show(SeriesBySlot),
	format("Value: ~d~n", [AllSlotsValue]),
	nl,
	fail.	% due to the use of 'all' in 'labeling'
betterSol.

flatData(Series, Slots, Preferences) :-
	findall([A,B,C,D,E], series(A,B,C,D,E),SeriesMatrix),
	findall([A,B,C,D], slot(A,B,C,D), SlotsMatrix),
	preferences(PreferencesMatrix),
	append(SeriesMatrix, Series),
	append(SlotsMatrix, Slots),
	append(PreferencesMatrix, Preferences).

% limits the number of occurrences of the id of a series in the SeriesBySlot list (0 or NumSlots occurrences)
numSeriesOccurs([SeriesId, _, Duration, _, _ | OtherSeries], SeriesBySlot) :-
	NumSlots #= Duration // 30,
	count(SeriesId, SeriesBySlot, #=, X),
	X #= _N * NumSlots,	% _N allows a series to be presented 0, 1 or more times (_N is always greater or equal to 0)
	numSeriesOccurs(OtherSeries, SeriesBySlot).
numSeriesOccurs([], _).

% calculate the sum of ratios Price / NumVotes that must be minimized by 'labeling'
calcValues(SeriesBySlot, Series, Preferences, AllSlotsValue) :-
	length(SeriesBySlot, Length),
  calcValuesAux(SeriesBySlot, Length, Series, Preferences, 0, AllSlotsValue).

calcValuesAux(SeriesBySlot, SeriesBySlotPos, Series, Preferences, TempAllSlotsValue, AllSlotsValue) :-
	SeriesBySlotPos > 0,
  calcValue(SeriesBySlot, SeriesBySlotPos, Series, Preferences, SlotValue),
  TempAllSlotsValue2 #= TempAllSlotsValue + SlotValue,
	SeriesBySlotPos2 is SeriesBySlotPos - 1,
  calcValuesAux(SeriesBySlot, SeriesBySlotPos2, Series, Preferences, TempAllSlotsValue2, AllSlotsValue).
calcValuesAux(_, 0, _, _, AllSlotsValue, AllSlotsValue).

calcValue(SeriesBySlot, SlotId, Series, Preferences, Value) :-
	element(SlotId, SeriesBySlot, SeriesId),
	% position of the actual series price on Series
	Pos #= (SeriesId - 1) * 5 + 2,
	element(Pos, Series, Price),
	% position of the actual series and slot number of votes on Preferences
	length(SeriesBySlot, NumSlots),
	Pos2 #= (SeriesId - 1) * NumSlots + SlotId,
  element(Pos2, Preferences, NumVotes),
	% calculate the ratio - we want to minimize the Price and maximize NumVotes, so we must minimize the ratio
  Value #= Price / NumVotes.

% make a series be contiguous
contiguousSeries([SeriesId | RestSeriesBySlot], SeriesBySlot, Series, Slots) :-
	myElement(PosList, SeriesBySlot, SeriesId),
	contiguousSeriesAux1(PosList, SeriesBySlot, Slots, 0, Exist),
	% must exist at least 1 pair of slots with 30 minutes of difference or the series occupies 0 or 1 slots (for both cases Exist #\= 0)
	Exist #\= 0,
	contiguousSeries(RestSeriesBySlot, SeriesBySlot, Series, Slots).
contiguousSeries([], _, _, _).

contiguousSeriesAux1([SlotId1|OtherSlotIds], SeriesBySlot, Slots, TmpExist, Exist) :-
	contiguousSeriesAux2(SlotId1, OtherSlotIds, SeriesBySlot, Slots, TmpExist2, Exist),
	TmpExist3 #= TmpExist + TmpExist2,
	contiguousSeriesAux1(OtherSlotIds, SeriesBySlot, Slots, TmpExist3, Exist).
contiguousSeriesAux1([], _, _, Exist, Exist).

contiguousSeriesAux2(SlotId1, [SlotId2|OtherSlotIds], SeriesBySlot, Slots, TmpExist, Exist) :-
	% verify if the positions are valid indexes (see myElement()), if not give a valid index (1) that will be ignored
	(SlotId1 #= 0 #/\ TmpSlotId1 #= 1) #\/ (SlotId1 #\= 0 #/\ TmpSlotId1 #= SlotId1),
	(SlotId2 #= 0 #/\ TmpSlotId2 #= 1) #\/ (SlotId2 #\= 0 #/\ TmpSlotId2 #= SlotId2),
	% position of the slot day in Slots
	Pos1 #= (TmpSlotId1 - 1) * 4 + 2,
	element(Pos1, Slots, Day1),
	% position of the slot hour in Slots
	Pos2 #= (TmpSlotId1 - 1) * 4 + 3,
	element(Pos2, Slots, Hour1),
	% position of the slot minute in Slots
	Pos3 #= (TmpSlotId1 - 1) * 4 + 4,
	element(Pos3, Slots, Minute1),
	% position of the slot day in Slots
	Pos4 #= (TmpSlotId2 - 1) * 4 + 2,
	element(Pos4, Slots, Day2),
	% position of the slot hour in Slots
	Pos5 #= (TmpSlotId2 - 1) * 4 + 3,
	element(Pos5, Slots, Hour2),
	% position of the slot minute in Slots
	Pos6 #= (TmpSlotId2 - 1) * 4 + 4,
	element(Pos6, Slots, Minute2),
	% Minute = 0 or Minute = 30
	((SlotId1 #= 0 #\/ SlotId2 #= 0) #\/	% when the series occupies 0 or 1 slots
	(Day1 #= Day2 #/\ Hour1 #= Hour2 #/\ Minute1 #\= Minute2) #\/
	(Day1 #= Day2 #/\ Hour1 #\= Hour2 #/\ Minute1 #\= Minute2) #\/
	(Day1 #\= Day2 #/\ Hour1 #\= Hour2 #/\ Minute1 #\= Minute2)) #<=> Tmp,
	TmpExist2 #= TmpExist + Tmp,
	contiguousSeriesAux2(SlotId1, OtherSlotIds, SeriesBySlot, Slots, TmpExist2, Exist).
contiguousSeriesAux2(_, [], _, _, Exist, Exist).

% restrict the schedule of a series (the chosen slot must be between MinHour and MaxHour)
restrictSchedule(SeriesBySlot, Series, Slots) :-
	length(SeriesBySlot, Length),
	restrictScheduleAux(SeriesBySlot, Length, Series, Slots).

restrictScheduleAux(SeriesBySlot, SeriesBySlotPos /* = SlotId */, Series, Slots) :-
	SeriesBySlotPos > 0,
	element(SeriesBySlotPos, SeriesBySlot, SeriesId),
	% positions of MaxHour and MinHour on Series
	Pos1 #= (SeriesId - 1) * 5 + 4,
	Pos2 #= (SeriesId - 1) * 5 + 5,
	element(Pos1, Series, MinHour),
	element(Pos2, Series, MaxHour),
	% position of Hour on Slots
	Pos3 #= (SeriesBySlotPos - 1) * 4 + 3,
	element(Pos3, Slots, Hour),
	MinHour #=< Hour #/\ MaxHour #> Hour,
	SeriesBySlotPos2 is SeriesBySlotPos - 1,
	restrictScheduleAux(SeriesBySlot, SeriesBySlotPos2, Series, Slots).
restrictScheduleAux(_, 0, _, _).

% restrict the series that must not occur in the same day
restrictNoSameDay(SeriesBySlot, Slots) :-
	findall(SeriesId1-SeriesId2, noSameDay(SeriesId1, SeriesId2), List),
	restrictNoSameDayAux1(List, SeriesBySlot, Slots).

restrictNoSameDayAux1([SeriesId1-SeriesId2 | RestNoSameDay], SeriesBySlot, Slots) :-
	myElement(PosList1, SeriesBySlot, SeriesId1),
	myElement(PosList2, SeriesBySlot, SeriesId2),
	restrictNoSameDayAux2(PosList1, PosList2, Slots),
	restrictNoSameDayAux1(RestNoSameDay, SeriesBySlot, Slots).
restrictNoSameDayAux1([], _, _).

restrictNoSameDayAux2([Pos|RestPosList1], PosList2, Slots) :-
	restrictNoSameDayAux3(Pos, PosList2, Slots),
	restrictNoSameDayAux2(RestPosList1, PosList2, Slots).
restrictNoSameDayAux2([], _, _).

restrictNoSameDayAux3(Pos1 /* = SlotId1*/, [Pos2|RestPosList2], Slots) :-
	% verify if the positions are valid indexes (see myElement()), if not give a valid index (1) that will be ignored
	(Pos1 #= 0 #/\ Pos3 #= 1) #\/ (Pos1 #\= 0 #/\ Pos3 #= Pos1),
	(Pos2 #= 0 #/\ Pos4 #= 1) #\/ (Pos2 #\= 0 #/\ Pos4 #= Pos2),
	% position of Day on Slots
	Pos5 #= (Pos3 - 1) * 4 + 2,
	element(Pos5, Slots, Day1),
	Pos6 #= (Pos4 - 1) * 4 + 2,
	element(Pos6, Slots, Day2),
	Pos1 #= 0 #\/ Pos2 #= 0 #\/ Day1 #\= Day2,
	restrictNoSameDayAux3(Pos1, RestPosList2, Slots).
restrictNoSameDayAux3(_, [], _).

% find all the positions where Elem occurs in L (a list of domain variables)
myElement(PosList, L, Elem) :-
	length(L, Length),
	myElementAux(PosList, L, Elem, Length, []).

myElementAux(PosList, L, Elem, Pos, TmpPosList) :-
	Pos > 0,
	element(Pos, L, E),
	E #= Elem #<=> Exist,
	% insert 0 in the list if the position does not have the element (element() never gives Pos #= 0)
	(Exist #= 0 #/\ Tmp #= 0) #\/ (Exist #= 1 #/\ Tmp #= Pos),
	Pos2 is Pos - 1,
	myElementAux(PosList, L, Elem, Pos2, [Tmp|TmpPosList]).
myElementAux(PosList, _, _, 0, PosList).

reset_timer :- statistics(walltime,_).
print_time :-
	statistics(walltime,[_,T]),
	TS is ((T//10)*10)/1000,
	nl, write('Time: '), write(TS), write('s'), nl, nl.

show(SeriesBySlot) :-
	showAux(SeriesBySlot, 1).

showAux([SeriesId | Rest], SlotId) :-
	seriesName(SeriesId, Name),
	slot(SlotId, DayId, Hour, Minute),
	slotDay(DayId, Day),
	format('~s ~s ~dh~dm~N', [Name, Day, Hour, Minute]),
	SlotId2 is SlotId + 1,
	showAux(Rest, SlotId2).
showAux([], _).
