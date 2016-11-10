:-use_module(library(lists)).
:-use_module(library(random)).

:- dynamic player/1.
:- dynamic state/2.

/*
Game legend:
- -> road
| -> road
X -> conduits
O -> space -> sp
A -> player 1 node -> n1
B -> player 1 unit -> u1
S -> player 2 node -> n2
T -> player 2 unit -> u2
 */

/* Players */
player(p1).
player(p2).

/* Board */
board([
	[' ', ' ', u1, u1, n1, u1, u1, ' ', ' '],
	[' ',  sp, sp, u1, u1, u1, sp,  sp, ' '],
	[ sp,  sp, sp, sp, u1, sp, sp,  sp,  sp],
	[ sp,  sp, sp, sp, sp, sp, sp,  sp,  sp],
	[ sp,  sp, sp, sp, sp, sp, sp,  sp,  sp],
	[ sp,  sp, sp, sp, sp, sp, sp,  sp,  sp],
	[ sp,  sp, sp, sp, u2, sp, sp,  sp,  sp],
	[' ',  sp, sp, u2, u2, u2, sp,  sp, ' '],
	[' ', ' ', u2, u2, n2, u2, u2, ' ', ' ']
	]).

intermediate_board([
	[' ', ' ', sp, sp, sp, u1, sp, ' ', ' '],
	[' ',  sp, u1, u1, n1, u1, sp,  sp, ' '],
	[ sp,  sp, sp, sp, u1, sp, sp,  sp,  sp],
	[ sp,  u1, sp, sp, sp, sp, sp,  u1,  sp],
	[ sp,  sp, sp, sp, u1, sp, sp,  sp,  sp],
	[ sp,  sp, sp, sp, sp, sp, sp,  sp,  sp],
	[ sp,  sp, sp, u2, u2, sp, sp,  sp,  sp],
	[' ',  sp, sp, u2, n2, u2, sp,  sp, ' '],
	[' ', ' ', u2, u2, sp, u2, u2, ' ', ' ']
	]).

final_board([
	[' ', ' ', sp, sp, sp, sp, sp, ' ', ' '],
	[' ',  u1, sp, sp, sp, u1, sp,  sp, ' '],
	[ sp,  sp, u1, sp, u1, n1, sp,  sp,  sp],
	[ sp,  sp, sp, u1, n2, u1, sp,  sp,  sp],
	[ sp,  sp, sp, sp, u1, sp, sp,  sp,  sp],
	[ sp,  sp, u2, u1, sp, sp, sp,  u2,  sp],
	[ sp,  sp, u2, sp, sp, sp, sp,  sp,  sp],
	[' ',  sp, u2, u2, sp, sp, u2,  sp, ' '],
	[' ', ' ', sp, u2, sp, u2, sp, ' ', ' ']
	]).

/* State */
state(_Player, _Board).

/* Display */
display_board(Board) :-
	board(Board), 
	display_board_rows(Board, Board),
	nl,
	nl.
	
display_board_rows([Row | []], _Board) :-
	display_board_middle_bottom_row(Row),
	nl,
	display_board_row_pieces(Row).
display_board_rows([Row | Other_rows], Board) :-	/* Display board up half */
	length(Board, All_board_length),
	length(Other_rows, Left_board_length),
	Left_board_length1 is Left_board_length + 1,
	Half_board_length is All_board_length >> 1,		/* integer division by 2 */
	Left_board_length1 > Half_board_length,
	display_board_row_pieces(Row), 
	nl, 
	display_board_middle_up_row(Row),
	nl,
	display_board_rows(Other_rows, Board).
display_board_rows([Row | Other_rows], Board) :-	/* Display board middle */
	length(Board, All_board_length),
	length(Other_rows, Left_board_length),
	Left_board_length1 is Left_board_length + 1,
	Half_board_length is All_board_length >> 1,		/* integer division by 2 */
	Left_board_length1 == Half_board_length,
	display_board_row_pieces(Row), 
	nl,
	display_board_rows(Other_rows, Board).
display_board_rows([Row | Other_rows], Board) :-	/* Display board bottom half */
	length(Board, All_board_length),
	length(Other_rows, Left_board_length),
	Left_board_length1 is Left_board_length + 1,
	Half_board_length is All_board_length >> 1,		/* integer division by 2 */
	Left_board_length1 < Half_board_length,
	display_board_middle_bottom_row(Row),
	nl, 
	display_board_row_pieces(Row), 
	nl,
	display_board_rows(Other_rows, Board).

translate(sp, 'O').
translate(n1, 'A').
translate(u1, 'B').
translate(n2, 'S').
translate(u2, 'T').
translate(X, X).
	
display_board_row_pieces([Piece | []]) :-
	translate(Piece, TPiece),
	write(TPiece).
display_board_row_pieces([' ' | Other_pieces]) :-
	write('  '),
	display_board_row_pieces(Other_pieces).
display_board_row_pieces([Piece | [' ' | _]]) :-
	translate(Piece, TPiece),
	write(TPiece).
display_board_row_pieces([Piece | Other_pieces]) :-
	translate(Piece, TPiece),
	write(TPiece),
	write('-'),
	display_board_row_pieces(Other_pieces).

display_board_middle_up_row([_Piece | []]) :-
	write('|').
display_board_middle_up_row([Piece | [' ' | _]]) :-
	Piece \== ' ',
	write('|X|\\').
display_board_middle_up_row([' ' | [Other_piece | Other_pieces]]) :-
	Other_piece \== ' ',
	write(' /'),
	display_board_middle_up_row(Other_pieces).
display_board_middle_up_row([' ' | Other_pieces]) :-
	write('  '),
	display_board_middle_up_row(Other_pieces).
display_board_middle_up_row([_Piece | Other_pieces]) :-
	write('|X'),
	display_board_middle_up_row(Other_pieces).

display_board_middle_bottom_row([_Piece | []]) :-
	write('|').
display_board_middle_bottom_row([Piece | [' ' | _]]) :-
	Piece \== ' ',
	write('|X|/').
display_board_middle_bottom_row([' ' | [Other_piece | Other_pieces]]) :-
	Other_piece \== ' ',
	write(' \\'),
	display_board_middle_bottom_row(Other_pieces).
display_board_middle_bottom_row([' ' | Other_pieces]) :-
	write('  '),
	display_board_middle_bottom_row(Other_pieces).
display_board_middle_bottom_row([_Piece | Other_pieces]) :-
	write('|X'),
	display_board_middle_bottom_row(Other_pieces).

/* Game logic */
game(Type) :-
%	write('Player 1 (younger): '),
%	read(Player1),
%	write('Player 2 (older): '),
%	read(Player2),
%	retract(player(p1)),
%	assert(player(Player1)),
%	retract(player(p2)),
%	assert(player(Player2)),
	board(Board),
	retract(state(_, _)),
	assert(state(p1, Board)),	/* the youngest player begins the game */
	repeat,
		play(Type),
		verify_game_over,
	state(Player, _),
	next_player(Player, Winner),	/* 'play' changed the current player to the next, it is necessary to recover the prior player */
	show_results(Winner).

play(cc) :-
	retract(state(Player, Actual_board)),
	best_move(Player, Actual_board, Best_board),
	display_board(Best_board),
	next_player(Player, Next),
	assert(state(Next, Best_board)),
	! .

% play(hh) :-

% play(ch) :-

check_signal(Board, Player, Piece_x, Piece_y) :-
	/* Find the positions of the nodes */
	get_piece(Board, Node1_x, Node1_y, n1),
	get_piece(Board, Node2_x, Node2_y, n2),
	/* Node 1 */
	(((check_signal_vertical(Piece_x, Piece_y, Node1_x, Node1_y, Signal_direction1);
	check_signal_horizontal(Piece_x, Piece_y, Node1_x, Node1_y, Signal_direction1);
	check_signal_diagonal(Piece_x, Piece_y, Node1_x, Node1_y, Signal_direction1)),
	check_enemies_interruptingsignal(Board, Player, Piece_x, Piece_y, Node1_x, Node1_y, Signal_direction1));
	/* Node 2 */
	((check_signal_vertical(Piece_x, Piece_y, Node2_x, Node2_y, Signal_direction2);
	check_signal_horizontal(Piece_x, Piece_y, Node2_x, Node2_y, Signal_direction2);
	check_signal_diagonal(Piece_x, Piece_y, Node2_x, Node2_y, Signal_direction2)),
	check_enemies_interruptingsignal(Board, Player, Piece_x, Piece_y, Node2_x, Node2_y, Signal_direction2))).
		
check_signal_horizontal(Piece_x, Piece_y, Node_x, Node_y, Signal_direction) :-
	Distance is Node_y - Piece_y,
	Node_x = Piece_x,
	((Distance > 1, Signal_direction = right);
	(Distance < 1, Signal_direction = left)).
		
check_signal_vertical(Piece_x, Piece_y, Node_x, Node_y, Signal_direction) :-
	Distance is Node_x - Piece_x,
	Node_y = Piece_y,
	((Distance > 1, Signal_direction = up);
	(Distance < 1, Signal_direction = left)).
		
check_signal_diagonal(Piece_x, Piece_y, Node_x, Node_y, Signal_direction) :-
	Xdiference is Node_x - Piece_x,
	Ydiference is Node_y - Piece_y,
	AbsXdiference is abs(Xdiference),
	AbsYdiference is abs(Ydiference),
	AbsXdiference = AbsYdiference,
	((Xdiference > 1, Ydiference > 1, Signal_direction = diagonal_upright);
	(Xdiference < 1, Ydiference > 1, Signal_direction = diagonal_upleft);
	(Xdiference > 1, Ydiference < 1, Signal_direction = diagonal_downright);
	(Xdiference < 1, Ydiference < 1, Signal_direction = diagonal_downleft)).
	
check_enemies_interruptingsignal(Board, Player, Piece_x, Piece_y, Node_x, Node_y, Signal_direction) :-
	%como se verifica se é o p1 ou o p2?
	(((Signal_direction = up; Signal_direction = down), 
	check_enemies_interruptingsignal_vertical(Board, Player, Piece_x, Piece_y, Node_x, Node_y, Signal_direction));	
	((Signal_direction = left; Signal_direction = right), 
	check_enemies_interruptingsignal_horizontal(Board, Player, Piece_x, Piece_y, Node_x, Node_y, Signal_direction));
	((Signal_direction = diagonal_downleft; Signal_direction = diagonal_downright; Signal_direction = diagonal_upleft; Signal_direction = diagonal_upright),
	check_enemies_interruptingsignal_diagonal(Board, Player, Piece_x, Piece_y, Node_x, Node_y, Signal_direction))).
		
check_enemies_interruptingsignal_vertical(Board, Player, Piece_x, Piece_y, Node_x, Node_y, Signal_direction) :-
	Piece_x =\= Node_x,
	get_piece(Board, Node_x, Node_y, Enemy),
	((Player = p1, Enemy = u2); (Player = p2, Enemy = u1)),
	!, %nao tenho a certeza de como se usa isto
	((Signal_direction = up, Node_x2 is Node_x - 1);
	(Signal_direction = down, Node_x2 is Node_x + 1)),
	check_enemies_interruptingsignal_vertical(Board, Player, Piece_x, Piece_y, Node_x2, Node_y, Signal_direction).
		
check_enemies_interruptingsignal_horizontal(Board, Player, Piece_x, Piece_y, Node_x, Node_y, Signal_direction) :-
	Piece_y =\= Node_y,
	get_piece(Board, Node_x, Node_y, Enemy),
	((Player = p1, Enemy = u2); (Player = p2, Enemy = u1)),
	!, %nao tenho a certeza de como se usa isto
	((Signal_direction = left, Node_y2 is Node_y - 1);
	(Signal_direction = right, Node_y2 is Node_y + 1)),
	check_enemies_interruptingsignal_horizontal(Board, Player, Piece_x, Piece_y, Node_x, Node_y2, Signal_direction).
	
check_enemies_interruptingsignal_diagonal(Board, Player, Piece_x, Piece_y, Node_x, Node_y, Signal_direction) :-
	Piece_x =\= Node_x,
	Piece_y =\= Node_y,
	get_piece(Board, Node_x, Node_y, Enemy),
	((Player = p1, Enemy = u2); (Player = p2, Enemy = u1)),
	!, %nao tenho a certeza de como se usa isto
	((Signal_direction = diagonal_downleft, Node_y2 is Node_y + 1, Node_x2 is Node_x - 1);
	(Signal_direction = diagonal_downright, Node_y2 is Node_y + 1, Node_x2 is Node_x + 1);
	(Signal_direction = diagonal_upleft, Node_y2 is Node_y - 1, Node_x2 is Node_x - 1);
	(Signal_direction = diagonal_upright, Node_y2 is Node_y - 1, Node_x2 is Node_x + 1)),
	check_enemies_interruptingsignal_diagonal(Board, Player, Piece_x, Piece_y, Node_x2, Node_y2, Signal_direction).

rule(move_up, Player, Piece_orig_x, Piece_orig_y, Board, New_board) :-
	Piece_new_y is Piece_orig_y - 1,
	rule_aux(Player, Piece_orig_x, Piece_orig_y, Piece_orig_x, Piece_new_y, Board, New_board).
			
rule(move_down, Player, Piece_orig_x, Piece_orig_y, Board, New_board) :-
	Piece_new_y is Piece_orig_y + 1,
	rule_aux(Player, Piece_orig_x, Piece_orig_y, Piece_orig_x, Piece_new_y, Board, New_board).
	
rule(move_left, Player, Piece_orig_x, Piece_orig_y, Board, New_board) :-
	Piece_new_x is Piece_orig_x - 1,
	rule_aux(Player, Piece_orig_x, Piece_orig_y, Piece_new_x, Piece_orig_y, Board, New_board).
	
rule(move_right, Player, Piece_orig_x, Piece_orig_y, Board, New_board) :-
	Piece_new_x is Piece_orig_x + 1,
	rule_aux(Player, Piece_orig_x, Piece_orig_y, Piece_new_x, Piece_orig_y, Board, New_board).
	
rule(jump_up_enemy_unit, Player, Piece_orig_x, Piece_orig_y, Board, New_board) :-
	Enemy_y is Piece_orig_y - 1,
	Piece_new_y is Piece_orig_y - 2,
	rule_jump_aux(Player, Piece_orig_x, Piece_orig_y, Piece_orig_x, Piece_new_y, Piece_orig_x, Piece_new_y, Board, New_board).
	
rule(jump_down_enemy_unit, Player, Piece_orig_x, Piece_orig_y, Board, New_board) :-
	Enemy_y is Piece_orig_y + 1,
	Piece_new_y is Piece_orig_y + 2,
	rule_jump_aux(Player, Piece_orig_x, Piece_orig_y, Piece_orig_x, Piece_new_y, Piece_orig_x, Piece_new_y, Board, New_board).
	
rule(jump_left_enemy_unit, Player, Piece_orig_x, Piece_orig_y, Board, New_board) :-
	Enemy_x is Piece_orig_x - 1,
	Piece_new_x is Piece_orig_x - 2,
	rule_jump_aux(Player, Piece_orig_x, Piece_orig_y, Piece_new_x, Piece_orig_y, Enemy_x, Piece_orig_y, Board, New_board).
	
rule(jump_right_enemy_unit, Player, Piece_orig_x, Piece_orig_y, Board, New_board) :-
	Enemy_x is Piece_orig_x + 1,
	Piece_new_x is Piece_orig_x + 2,
	rule_jump_aux(Player, Piece_orig_x, Piece_orig_y, Piece_new_x, Piece_orig_y, Enemy_x, Piece_orig_y, Board, New_board).

rule_aux(Player, Piece_orig_x, Piece_orig_y, Piece_new_x, Piece_new_y, Board, New_board) :-
	/* Applicability pre-conditions verifications */
	verify_inside_borders(Board, Piece_orig_x, Piece_orig_y),
	verify_inside_borders(Board, Piece_new_x, Piece_new_y),
	check_signal(Board, Player, Piece_orig_x, Piece_orig_y),
	get_piece(Board, Piece_orig_x, Piece_orig_y, Piece),
	((Player = p1, Piece = u1); (Player = p2, Piece = u2)),
	get_piece(Board, Piece_new_x, Piece_new_y, sp),
	/* action / movement */
	set_piece(Board, Piece_orig_x, Piece_orig_y, sp, New_board2),
	set_piece(New_board2, Piece_new_x, Piece_new_y, Piece, New_board).
	
rule_jump_aux(Player, Piece_orig_x, Piece_orig_y, Piece_new_x, Piece_new_y, Enemy_x, Enemy_y, Board, New_board) :-
	/* Applicability pre-conditions verifications */
	verify_inside_borders(Board, Piece_orig_x, Piece_orig_y),
	verify_inside_borders(Board, Enemy_x, Enemy_y),
	verify_inside_borders(Board, Piece_new_x, Piece_new_y),
	check_signal(Board, Player, Piece_orig_x, Piece_orig_y),
	get_piece(Board, Piece_orig_x, Piece_orig_y, Piece),
	((Player = p1, Piece = u1); (Player = p2, Piece = u2)),
	((Piece = u2, Next_piece = u1); (Piece = u1, Next_piece = u2)),
	get_piece(Board, Enemy_x, Enemy_y, Next_piece),
	get_piece(Board, Piece_new_x, Piece_new_y, sp),
	/* action / movement */
	set_piece(Board, Piece_orig_x, Piece_orig_y, sp, New_board2),
	set_piece(New_board2, Piece_new_x, Piece_new_y, Piece, New_board).


get_piece(Board, X, Y, Piece) :-
	nth1(Y, Board, Line),
	nth1(X, Line, Piece).

verify_inside_borders(Board, X, Y) :-
	X > 1,
	Y > 1,
	length(Board, Length_y),
	Y =< Length_y,
	nth1(Y, Board, Line),
	length(Line, Length_x),
	X =< Length_x.

best_move(Player, Board, Best) :-
	length(Board, Length_y),
	random(1, Length_y, Rand_y),
	nth1(Rand_y, Board, Line),
	length(Line, Length_x),
	random(1, Length_x, Rand_x),
	bagof(Aux_board, rule(_, Player, Rand_x, Rand_y, Board, Aux_board), Possible_boards),
	select_best(Player, Possible_boards, Best).

select_best(Player, [Board | Other_boards], Best) :-
	quality(Board, Player, Value).
%	select_best_aux(List_boards_values, Best_board2, Best_value2),
%	select_best(Player, Other_boards, Best_board2).

select_best_aux([[_Board, Value] | Other_boards_values], Best_board, Best_value) :-
	select_best_aux(Other_boards_values, Best_board2, Best_value2),
	Value =< Best_value2,
	Best_value is Best_value2,
	Best_board = Best_board2.
select_best_aux([[Board, Value] | Other_boards_values], Best_board, Best_value) :-
	select_best_aux(Other_boards_values, _Best_board2, Best_value2),
	Value > Best_value2,
	Best_value is Value,
	Best_board = Board.
select_best_aux([[Board, Value]], Board, Value).

quality(Board, Player, Value) :-
	quality_aux_1(Board, Player, Value).
%	quality_aux_1(Board, Other_player, Value3),
%	Value is Value2 / Value3.

quality_aux_1([Line | Other_lines], Player, Value) :-
	quality_aux_2(Line, Player, Value2),
	quality_aux_1(Other_lines, Player, Value3),
	Value is Value2 + Value3,
	!.
quality_aux_1([], _, 0).

quality_aux_2([Cell | Other_cells], Player, Value) :-
	\+ check_signal(Cell),
	quality_aux_2(Other_cells, Player, Value),
	!.
quality_aux_2([Cell | Other_cells], Player, Value) :-
	check_signal(Cell),
	quality_aux_2(Other_cells, Player, Value2),
	Value is Value2 + 1,
	!.
quality_aux_2([], _, 0).

verify_game_over :-
	state(_, Board),
	find_node(Board, X, Y, Node),
	verify_blocked(Board, Node, X, Y).

find_node(Board, X, Y, Node) :-
	find_node_aux(Board, 1, X, Y, Node).

find_node_aux([Line | _Other_lines], Pos_y, X, Y, Node) :-
	((nth1(X, Line, n1), Node = n1); (nth1(X, Line, n2), Node = n2)),
	Y = Pos_y.
find_node_aux([_Line | Other_lines], Pos_y, X, Y, Node) :-
	Next_pos_y is Pos_y + 1,
	find_node_aux(Other_lines, Next_pos_y, X, Y, Node).

verify_blocked(Board, Node, X, Y) :-
	((Node = n1, Enemy_unit = u2); (Node = n2, Enemy_unit = u1)),
	verify_blocked_left(Board, Enemy_unit, X, Y),
	verify_blocked_right(Board, Enemy_unit, X, Y),
	verify_blocked_up(Board, Enemy_unit, X, Y),
	verify_blocked_down(Board, Enemy_unit, X, Y).

/* if 'X' or 'Y' out of borders 'yes' will be returned */
verify_blocked_left(Board, Enemy_unit, X, Y) :- X2 is X - 1, (X2 < 1; (nth1(Y, Board, Line), nth1(X2, Line, Enemy_unit))).
verify_blocked_right(Board, Enemy_unit, X, Y) :- X2 is X + 1, (nth1(Y, Board, Line), length(Line, Length_x), (X2 > Length_x; nth1(X2, Line, Enemy_unit))).
verify_blocked_up(Board, Enemy_unit, X, Y) :- Y2 is Y - 1, (Y2 < 1; (nth1(Y2, Board, Line), nth1(X, Line, Enemy_unit))).
verify_blocked_down(Board, Enemy_unit, X, Y) :- Y2 is Y + 1, length(Board, Length_y), (Y2 > Length_y; (nth1(Y2, Board, Line), nth1(X, Line, Enemy_unit))).

next_player(Player, Next) :- player(Player), player(Next), Player \= Next.

show_results(Winner) :- format('~NWinner: ~s~N', Winner).

/* if 'Y' is invalid, the same list will be returned */
set_piece([Line | Other_lines], X, Y, New_piece, [Line | Other_new_lines]) :-
	Y =\= 1,
	Next_Y is Y - 1,
	set_piece(Other_lines, X, Next_Y, New_piece, Other_new_lines).
set_piece([Line | Other_lines], X, 1, New_piece, [New_line | Other_new_lines]) :-
	set_cell(X, New_piece, Line, New_line),
	Next_Y is 0,
	set_piece(Other_lines, X, Next_Y, New_piece, Other_new_lines).
set_piece([], _, _, _, []).

/* if 'X' is invalid, the same list will be returned */
set_cell(X, New_piece, [Piece | Rest_line], [Piece | Rest_new_line]) :-
	X =\= 1,
	Next_X is X - 1,
	set_cell(Next_X, New_piece, Rest_line, Rest_new_line).
set_cell(1, New_piece, [_Piece | Rest_line], [New_piece | Rest_new_line]) :-
	Next_X is 0,
	set_cell(Next_X, New_piece, Rest_line, Rest_new_line).
set_cell(_, _, [], []).
