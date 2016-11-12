:- use_module(library(lists)).
:- use_module(library(random)).

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
	length(Board, Length),
	write('  '),
	display_board_cols_indexes(1, Length),
	nl,
	display_board_rows(Board, Board, 1),
	nl,
	nl.

display_board_cols_indexes(Index, Max_index) :-
	Index =< Max_index,
	format('~d ', [Index]),
	Index2 is Index + 1,
	display_board_cols_indexes(Index2, Max_index).
display_board_cols_indexes(Index, Max_index) :- Index > Max_index.
	
display_board_rows([Row | []], _Board, Index) :-
	display_board_middle_bottom_row(Row),
	nl,
	format('~d ', [Index]),
	display_board_row_pieces(Row).
display_board_rows([Row | Other_rows], Board, Index) :-	/* Display board up half */
	length(Board, All_board_length),
	length(Other_rows, Left_board_length),
	Left_board_length1 is Left_board_length + 1,
	Half_board_length is All_board_length >> 1,		/* integer division by 2 */
	Left_board_length1 > Half_board_length,
	format('~d ', [Index]),
	display_board_row_pieces(Row), 
	nl, 
	write('  '),
	display_board_middle_up_row(Row),
	nl,
	Index2 is Index + 1,
	display_board_rows(Other_rows, Board, Index2).
display_board_rows([Row | Other_rows], Board, Index) :-	/* Display board middle */
	length(Board, All_board_length),
	length(Other_rows, Left_board_length),
	Left_board_length1 is Left_board_length + 1,
	Half_board_length is All_board_length >> 1,		/* integer division by 2 */
	Left_board_length1 == Half_board_length,
	format('~d ', [Index]),
	display_board_row_pieces(Row),
	nl,
	write('  '),
	Index2 is Index + 1,
	display_board_rows(Other_rows, Board, Index2).
display_board_rows([Row | Other_rows], Board, Index) :-	/* Display board bottom half */
	length(Board, All_board_length),
	length(Other_rows, Left_board_length),
	Left_board_length1 is Left_board_length + 1,
	Half_board_length is All_board_length >> 1,		/* integer division by 2 */
	Left_board_length1 < Half_board_length,
	display_board_middle_bottom_row(Row),
	nl, 
	format('~d ', [Index]),
	display_board_row_pieces(Row), 
	nl,
	write('  '),
	Index2 is Index + 1,
	display_board_rows(Other_rows, Board, Index2).

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
game(Type, Mode) :-
%	write('Player 1 (younger): '),
%	read(Player1),
%	write('Player 2 (older): '),
%	read(Player2),
%	retract(player(p1)),
%	assert(player(Player1)),
%	retract(player(p2)),
%	assert(player(Player2)),
	(
		check_game_type(Type),
		check_game_mode(Mode),
		board(Board),
		verify_board_dimensions(Board),	/* We will assume that the board is squared */
		retract(state(_Player, _Board)),
		assert(state(p1, Board)),	/* the youngest player begins the game */
		repeat,
			state(Player, _Board),
			format('Player: ~s~N', Player),
			play(Type, Mode),
			verify_game_over,
		next_player(Player, Winner),	/* 'play' changed the current player to the next, it is necessary to recover the prior player */
		show_results(Winner)
	);
	(write('Wrong game values. Please check if you typed correctly the Type and Mode of the Game.'),
	nl,
	write('Type can be: cc, ch or hh.'),
	nl,
	write('Mode can be: easy or hard.'),
	nl).
	
/* Checks if the Mode is valid */
check_game_mode(Mode) :-
	(Mode = easy; Mode = hard).

/* Checks if the Type is valid */
check_game_type(Type) :-
	(Type = cc; Type = ch; Type = hh).

verify_board_dimensions(Board) :-
	length(Board, Length_y),
	(
		verify_board_dimensions_aux(Board, Length_y);
		(format('Invalid board dimensions.~N', []), fail)
	).

verify_board_dimensions_aux([Row | Other_rows], Length) :-
	length(Row, Length_x),
	Length = Length_x,
	verify_board_dimensions_aux(Other_rows, Length).
verify_board_dimensions_aux([], _Length).


play(cc, Mode) :-
	retract(state(Player, Actual_board)),
	best_move(Player, Mode, Actual_board, Best_board),
	display_board(Best_board),
	next_player(Player, Next),
	assert(state(Next, Best_board)),
	!.

play(hh, _Mode) :-
	state(_Player, Board),
	display_board(Board),
	format('Possible moves:~n1-move up~n2-move down~n3-move left~n4-move right~n5-jump enemy unit up~n6-jump enemy unit down~n7-jump enemy unit left~n8-jump enemy unit right~nWhich piece do you want to move?~n', []),
	repeat,
		retract(state(Player, Actual_board)),
		write('X: '),
		read(X),
		write('Y: '),
		read(Y),
		write('Option: '),
		read(Num),
		(
			(Num = 1, rule(move_up, Player, X, Y, Actual_board, New_board));
			(Num = 2, rule(move_down, Player, X, Y, Actual_board, New_board));
			(Num = 3, rule(move_left, Player, X, Y, Actual_board, New_board));
			(Num = 4, rule(move_right, Player, X, Y, Actual_board, New_board));
			(Num = 5, rule(move_enemy_unit_up, Player, X, Y, Actual_board, New_board));
			(Num = 6, rule(move_enemy_unit_down, Player, X, Y, Actual_board, New_board));
			(Num = 7, rule(move_enemy_unit_left, Player, X, Y, Actual_board, New_board));
			(Num = 8, rule(move_enemy_unit_right, Player, X, Y, Actual_board, New_board));
			(New_board = Actual_board, write('Unable to apply the specified rule.\n'))
		),
		display_board(New_board),
		assert(state(_Player, New_board)),
		get_piece(New_board, X, Y, Piece),
		((Player = p1, Piece = n1); (Player = p2, Piece = n2)),
	next_player(Player, Next),
	assert(state(Next, _Board)),
	!.


play(ch, Mode) :-
	state(p1, _Board),
	play(cc, Mode).

play(ch, Mode) :-
	state(p2, _Board),
	play(hh, Mode).

/* Signal functions */

/* Check if the Piece is receiving the signal from a node */
check_signal(Board, Player, Piece_x, Piece_y) :-
	/* Node 1 */
	(
		(
			get_piece(Board, Node1_x, Node1_y, n1),		/* Find the position of the node */
			(
				check_signal_vertical(Piece_x, Piece_y, Node1_x, Node1_y, Signal_direction1);
				check_signal_horizontal(Piece_x, Piece_y, Node1_x, Node1_y, Signal_direction1);
				check_signal_diagonal(Piece_x, Piece_y, Node1_x, Node1_y, Signal_direction1)
			),
			\+ check_enemies_interrupting_signal(Board, Player, Piece_x, Piece_y, Node1_x, Node1_y, Signal_direction1)
		);
	/* Node 2 */
		(
			get_piece(Board, Node2_x, Node2_y, n2),		/* Find the position of the node */
			(
				check_signal_vertical(Piece_x, Piece_y, Node2_x, Node2_y, Signal_direction2);
				check_signal_horizontal(Piece_x, Piece_y, Node2_x, Node2_y, Signal_direction2);
				check_signal_diagonal(Piece_x, Piece_y, Node2_x, Node2_y, Signal_direction2)
			),
			\+ check_enemies_interrupting_signal(Board, Player, Piece_x, Piece_y, Node2_x, Node2_y, Signal_direction2)
		)
	).

/* Check if the Piece is receiving the signal from a Node that is on the same row */
check_signal_horizontal(Piece_x, Piece_y, Node_x, Node_y, Signal_direction) :-
	Node_y = Piece_y,
	Distance is Node_x - Piece_x,
	(
		(Distance > 0, Signal_direction = right);
		(Distance < 0, Signal_direction = left)
	).

/* Check if the Piece is receiving the signal from a Node that is on the same column */
check_signal_vertical(Piece_x, Piece_y, Node_x, Node_y, Signal_direction) :-
	Node_x = Piece_x,
	Distance is Node_y - Piece_y,
	(
		(Distance > 0, Signal_direction = down);
		(Distance < 0, Signal_direction = up)
	).

/* Check if the Piece is receiving the signal from a Node through a conduit */
check_signal_diagonal(Piece_x, Piece_y, Node_x, Node_y, Signal_direction) :-
	X_diference is Node_x - Piece_x,
	Y_diference is Node_y - Piece_y,
	Abs_X_diference is abs(X_diference),
	Abs_Y_diference is abs(Y_diference),
	Abs_X_diference = Abs_Y_diference,
	(
		(X_diference > 0, Y_diference > 0, Signal_direction = diagonal_down_right);
		(X_diference < 0, Y_diference > 0, Signal_direction = diagonal_down_left);
		(X_diference > 0, Y_diference < 0, Signal_direction = diagonal_up_right);
		(X_diference < 0, Y_diference < 0, Signal_direction = diagonal_up_left)
	).

/* Check if the signal from a Node is being blocked by an enemy unit */
check_enemies_interrupting_signal(Board, Player, Piece_x, Piece_y, Node_x, Node_y, Signal_direction) :-
	(
		(
			(Signal_direction = up; Signal_direction = down), 
			check_enemies_interrupting_signal_vertical(Board, Player, Piece_x, Piece_y, Node_x, Node_y, Signal_direction)
		);	
		(
			(Signal_direction = left; Signal_direction = right),
			check_enemies_interrupting_signal_horizontal(Board, Player, Piece_x, Piece_y, Node_x, Node_y, Signal_direction)
		);
		(
			(Signal_direction = diagonal_down_left; Signal_direction = diagonal_down_right; Signal_direction = diagonal_up_left; Signal_direction = diagonal_up_right),
			check_enemies_interrupting_signal_diagonal(Board, Player, Piece_x, Piece_y, Node_x, Node_y, Signal_direction)
		)
	).

/* Check if the signal from a Node is being blocked by an enemy unit on the same row */
check_enemies_interrupting_signal_horizontal(Board, Player, Piece_x, Piece_y, Other_piece_x, Other_piece_y, Signal_direction) :-
	Piece_x =\= Other_piece_x,
	verify_enemy_player(Board, Player, Other_piece_x, Other_piece_y),
	(
		(
			(Signal_direction = left, Other_piece_x2 is Other_piece_x - 1);
			(Signal_direction = right, Other_piece_x2 is Other_piece_x + 1)
		),
		check_enemies_interrupting_signal_horizontal(Board, Player, Piece_x, Piece_y, Other_piece_x2, Other_piece_y, Signal_direction)
	).

/* Check if the signal from a Node is being blocked by an enemy unit on the same column */
check_enemies_interrupting_signal_vertical(Board, Player, Piece_x, Piece_y, Other_piece_x, Other_piece_y, Signal_direction) :-
	Piece_y =\= Other_piece_y,
	verify_enemy_player(Board, Player, Other_piece_x, Other_piece_y),
	(
		(Signal_direction = up, Other_piece_y2 is Other_piece_y - 1);
		(Signal_direction = down, Other_piece_y2 is Other_piece_y + 1)
	),
	check_enemies_interrupting_signal_vertical(Board, Player, Piece_x, Piece_y, Other_piece_x, Other_piece_y2, Signal_direction).
		
/* Check if the signal from a Node is being blocked by an enemy unit through a conduit */	
check_enemies_interrupting_signal_diagonal(Board, Player, Piece_x, Piece_y, Other_piece_x, Other_piece_y, Signal_direction) :-
	Piece_x =\= Other_piece_x,
	Piece_y =\= Other_piece_y,
	verify_enemy_player(Board, Player, Other_piece_x, Other_piece_y),
	(
		(Signal_direction = diagonal_down_left, Other_piece_y2 is Other_piece_y + 1, Other_piece_x2 is Other_piece_x - 1);
		(Signal_direction = diagonal_down_right, Other_piece_y2 is Other_piece_x + 1, Other_piece_x2 is Other_piece_x + 1);
		(Signal_direction = diagonal_up_left, Other_piece_y2 is Other_piece_y - 1, Other_piece_x2 is Other_piece_x - 1);
		(Signal_direction = diagonal_up_right, Other_piece_y2 is Other_piece_y - 1, Other_piece_x2 is Other_piece_x + 1)
	),
	check_enemies_interrupting_signal_diagonal(Board, Player, Piece_x, Piece_y, Other_piece_x2, Other_piece_y2, Signal_direction).

/* Movement functions */

/* Move up */
rule(move_up, Player, Piece_orig_x, Piece_orig_y, Board, New_board) :-
	Piece_new_y is Piece_orig_y - 1,
	rule_aux(Player, Piece_orig_x, Piece_orig_y, Piece_orig_x, Piece_new_y, Board, New_board).

/* Move down */	
rule(move_down, Player, Piece_orig_x, Piece_orig_y, Board, New_board) :-
	Piece_new_y is Piece_orig_y + 1,
	rule_aux(Player, Piece_orig_x, Piece_orig_y, Piece_orig_x, Piece_new_y, Board, New_board).

/* Move left */
rule(move_left, Player, Piece_orig_x, Piece_orig_y, Board, New_board) :-
	Piece_new_x is Piece_orig_x - 1,
	rule_aux(Player, Piece_orig_x, Piece_orig_y, Piece_new_x, Piece_orig_y, Board, New_board).

/* Move right */
rule(move_right, Player, Piece_orig_x, Piece_orig_y, Board, New_board) :-
	Piece_new_x is Piece_orig_x + 1,
	rule_aux(Player, Piece_orig_x, Piece_orig_y, Piece_new_x, Piece_orig_y, Board, New_board).

/* Jump up */
rule(jump_up_enemy_unit, Player, Piece_orig_x, Piece_orig_y, Board, New_board) :-
	Enemy_y is Piece_orig_y - 1,
	Piece_new_y is Piece_orig_y - 2,
	rule_jump_aux(Player, Piece_orig_x, Piece_orig_y, Piece_orig_x, Piece_new_y, Piece_orig_x, Enemy_y, Board, New_board).

/* Jump down */
rule(jump_down_enemy_unit, Player, Piece_orig_x, Piece_orig_y, Board, New_board) :-
	Enemy_y is Piece_orig_y + 1,
	Piece_new_y is Piece_orig_y + 2,
	rule_jump_aux(Player, Piece_orig_x, Piece_orig_y, Piece_orig_x, Piece_new_y, Piece_orig_x, Enemy_y, Board, New_board).

/* Jump left */
rule(jump_left_enemy_unit, Player, Piece_orig_x, Piece_orig_y, Board, New_board) :-
	Enemy_x is Piece_orig_x - 1,
	Piece_new_x is Piece_orig_x - 2,
	rule_jump_aux(Player, Piece_orig_x, Piece_orig_y, Piece_new_x, Piece_orig_y, Enemy_x, Piece_orig_y, Board, New_board).

/* Jump right */
rule(jump_right_enemy_unit, Player, Piece_orig_x, Piece_orig_y, Board, New_board) :-
	Enemy_x is Piece_orig_x + 1,
	Piece_new_x is Piece_orig_x + 2,
	rule_jump_aux(Player, Piece_orig_x, Piece_orig_y, Piece_new_x, Piece_orig_y, Enemy_x, Piece_orig_y, Board, New_board).

/* Auxiliar function to the move functions */
rule_aux(Player, Piece_orig_x, Piece_orig_y, Piece_new_x, Piece_new_y, Board, New_board) :-
	/* Applicability pre-conditions verifications */
	verify_inside_borders(Board, Piece_orig_x, Piece_orig_y),
	verify_inside_borders(Board, Piece_new_x, Piece_new_y),
	verify_piece_player(Board, Player, Piece_orig_x, Piece_orig_y, Piece),
	get_piece(Board, Piece_new_x, Piece_new_y, sp),
	check_signal(Board, Player, Piece_orig_x, Piece_orig_y),
	/* action / movement */
	set_piece(Board, Piece_orig_x, Piece_orig_y, sp, New_board2),
	set_piece(New_board2, Piece_new_x, Piece_new_y, Piece, New_board).

/* Auxiliar function to the jump functions */
rule_jump_aux(Player, Piece_orig_x, Piece_orig_y, Piece_new_x, Piece_new_y, Enemy_x, Enemy_y, Board, New_board) :-
	/* Applicability pre-conditions verifications */
	verify_inside_borders(Board, Piece_orig_x, Piece_orig_y),
	verify_inside_borders(Board, Enemy_x, Enemy_y),
	verify_inside_borders(Board, Piece_new_x, Piece_new_y),
	verify_piece_player(Board, Player, Piece_orig_x, Piece_orig_y, Piece),
	verify_enemy_player(Board, Player, Enemy_x, Enemy_y),
	get_piece(Board, Piece_new_x, Piece_new_y, sp),
	check_signal(Board, Player, Piece_orig_x, Piece_orig_y),
	/* action / movement */
	set_piece(Board, Piece_orig_x, Piece_orig_y, sp, New_board2),
	set_piece(New_board2, Piece_new_x, Piece_new_y, Piece, New_board).

/* Get the symbol on the position X,Y of the Board */
get_piece(Board, X, Y, Piece) :-
	nth1(Y, Board, Line),
	nth1(X, Line, Piece).

/* Check if the position X,Y is valid, is inside the borders of the Board */
verify_inside_borders(Board, X, Y) :-
	X >= 1,
	Y >= 1,
	length(Board, Length),
	Y =< Length,
	X =< Length.

/* Check if the Piece belongs to the Player */
verify_piece_player(Board, Player, Piece_x, Piece_y, Piece) :-
	((Player = p1, Piece = u1); (Player = p2, Piece = u2)),
	get_piece(Board, Piece_x, Piece_y, Piece).

/* Check if the Piece is an Enemy of the Player */	
verify_enemy_player(Board, Player, Enemy_x, Enemy_y) :-
	((Player = p1, Piece = u2); (Player = p2, Piece = u1)),
	get_piece(Board, Enemy_x, Enemy_y, Piece).	

/* Calculates the best move possible */
best_move(Player, Mode, Board, Best) :-
	burst_move(Player, Mode, Board, Best).

find_player_pieces(Board, Player, [X, Y]) :-
	get_piece(Board, X, Y, Piece_to_move),
	((Player = p1, (Piece_to_move = u1; Piece_to_move = n1)); (Player = p2, (Piece_to_move = u2; Piece_to_move = n2))).

burst_move(Player, Mode, Board, Best) :-
	findall(Aux_coords, find_player_pieces(Board, Player, Aux_coords), Possible_coords),
	random_member([X, Y], Possible_coords),
	try_move(Player, Mode, Board, X, Y, New_board),
	(
		(New_board = [], Best = Board);
		(
			(Piece_to_move = n1; Piece_to_move = n2),
			Best = New_board
		);
		burst_move(Player, Mode, New_board, Best)
	),
	!.

try_move(Player, Mode, Board, Piece_x, Piece_y, Best) :-
	findall(Aux_board, (rule(_Move, Player, Piece_x, Piece_y, Board, Aux_board)), Possible_boards),
	(
		(Mode = easy, nth1(1, Possible_boards, Best));
		(Mode = hard, select_best(Player, Possible_boards, Best))
	).

select_best(Player, Possible_boards, Best) :-
	select_best_aux(Player, Possible_boards, Best, _Best_value).

select_best_aux(Player, [Board | Other_boards], Best_board, Best_value) :-
	select_best_aux(Player, Other_boards, Best_board2, Best_value2),
	quality(Board, Player, Value),
	(
		(Value =< Best_value2,
		Best_value is Value,
		Best_board = Board);
		(Value > Best_value2,
		Best_value is Best_value2,
		Best_board = Best_board2)
	).
select_best_aux(_Player, [], [], 100000000000000).	% risky

quality(Board, Player, Value) :-
	length(Board, Length),
	((Player = p1, Enemy_node = n2, My_unit = u1); (Player = p2, Enemy_node = n1, My_unit = u2)),
	get_piece(Board, Node_x, Node_y, Enemy_node),
	quality_aux_1(Board, My_unit, Length, Length, Node_x, Node_y, 0, Value).

quality_aux_1(Board, Piece, Max_x, Y, Node_x, Node_y, Temp_value, Value) :-
	Y >= 1,
	nth1(Y, Board, Line),
	quality_aux_2(Line, Piece, Max_x, Y, Node_x, Node_y, 0, Temp_value2),
	Y2 is Y - 1,
	Temp_value3 is Temp_value + Temp_value2,
	quality_aux_1(Board, Piece, Max_x, Y2,  Node_x, Node_y, Temp_value3, Value).
quality_aux_1(_Board, _Piece, _Max_x, 0, _Node_x, _Node_y, Value, Value).

quality_aux_2(Line, Piece, X, Y, Node_x, Node_y, Temp_value, Value) :-
	X >= 1,
	(
		(
			nth1(X, Line, Piece),
			X_distance is Node_x - X,
			Y_distance is Node_y - Y,
			Abs_X_distance is abs(X_distance),
			Abs_Y_distance is abs(Y_distance),
			Temp_value2 is Abs_X_distance + Abs_Y_distance
		);
		(
			\+ nth1(X, Line, Piece),
			Temp_value2 = 0
		)
	),
	X2 is X - 1,
	Temp_value3 is Temp_value + Temp_value2,
	quality_aux_2(Line, Piece, X2, Y, Node_x, Node_y, Temp_value3, Value).
quality_aux_2(_Line, _Piece, 0, _Y, _Node_x, _Node_y, Value, Value).

verify_game_over :-
	state(_Player, Board),
	get_piece(Board, X, Y, Node),
	verify_blocked(Board, Node, X, Y).

verify_blocked(Board, Node, X, Y) :-
	((Node = n1, Enemy_unit = u2); (Node = n2, Enemy_unit = u1)),
	verify_blocked_left(Board, Enemy_unit, X, Y),
	verify_blocked_right(Board, Enemy_unit, X, Y),
	verify_blocked_up(Board, Enemy_unit, X, Y),
	verify_blocked_down(Board, Enemy_unit, X, Y).

/* If 'X2' or 'Y2' out of borders 'yes' will be returned, as desired */
verify_blocked_left(Board, Enemy_unit, X, Y) :- X2 is X - 1, (X2 < 1; (nth1(Y, Board, Line), nth1(X2, Line, Enemy_unit))).
verify_blocked_right(Board, Enemy_unit, X, Y) :- X2 is X + 1, (nth1(Y, Board, Line), length(Line, Length_x), (X2 > Length_x; nth1(X2, Line, Enemy_unit))).
verify_blocked_up(Board, Enemy_unit, X, Y) :- Y2 is Y - 1, (Y2 < 1; (nth1(Y2, Board, Line), nth1(X, Line, Enemy_unit))).
verify_blocked_down(Board, Enemy_unit, X, Y) :- Y2 is Y + 1, length(Board, Length_y), (Y2 > Length_y; (nth1(Y2, Board, Line), nth1(X, Line, Enemy_unit))).

next_player(Player, Next) :- state(Player, _Board), player(Next), Player \= Next, !.

show_results(Winner) :- format('~NWinner: ~s~N', Winner).

/* If 'Y' is invalid, the same list will be returned */
set_piece([Line | Other_lines], X, Y, New_piece, [Line | Other_new_lines]) :-
	Y =\= 1,
	Next_Y is Y - 1,
	set_piece(Other_lines, X, Next_Y, New_piece, Other_new_lines).
set_piece([Line | Other_lines], X, 1, New_piece, [New_line | Other_new_lines]) :-
	set_cell(X, New_piece, Line, New_line),
	Next_Y is 0,
	set_piece(Other_lines, X, Next_Y, New_piece, Other_new_lines).
set_piece([], _X, _Y, _New_piece, []).

/* If 'X' is invalid, the same list will be returned */
set_cell(X, New_piece, [Piece | Rest_line], [Piece | Rest_new_line]) :-
	X =\= 1,
	Next_X is X - 1,
	set_cell(Next_X, New_piece, Rest_line, Rest_new_line).
set_cell(1, New_piece, [_Piece | Rest_line], [New_piece | Rest_new_line]) :-
	Next_X is 0,
	set_cell(Next_X, New_piece, Rest_line, Rest_new_line).
set_cell(_X, _New_piece, [], []).
