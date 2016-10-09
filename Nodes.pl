/* Game pieces */
road('-').
conduits('X').
node('N').
unit('U').
space('O').

/* Board */
board([
	[' ', ' ', 'O', 'O', 'O', 'O', 'O', ' ', ' '],
	[' ', 'O', 'O', 'O', 'O', 'O', 'O', 'O', ' '],
	['O', 'O', 'O', 'O', 'O', 'O', 'O', 'O', 'O'],
	['O', 'O', 'O', 'O', 'O', 'O', 'O', 'O', 'O'],
	['O', 'O', 'O', 'O', 'O', 'O', 'O', 'O', 'O'],
	['O', 'O', 'O', 'O', 'O', 'O', 'O', 'O', 'O'],
	['O', 'O', 'O', 'O', 'O', 'O', 'O', 'O', 'O'],
	[' ', 'O', 'O', 'O', 'O', 'O', 'O', 'O', ' '],
	[' ', ' ', 'O', 'O', 'O', 'O', 'O', ' ', ' ']
	]).

/* Display */
display_board(board) :-	/* board is not a variable so no question is asked */
	board(Board), display_board_lines(Board).

display_board_lines([Line | Other_lines]) :-
	display_board_line_pieces(Line), nl, display_board_lines(Other_lines).
display_board_lines([]).

display_board_line_pieces([Piece | Other_pieces]) :-
	write(Piece), write('-'), display_board_line_pieces(Other_pieces).
display_board_line_pieces([]).
