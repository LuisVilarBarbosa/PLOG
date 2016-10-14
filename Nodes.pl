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
	display_board_line_pieces(Line), nl, display_board_lines_mid(Line), display_board_lines(Other_lines).
display_board_lines([]).

display_board_line_pieces([Piece | Other_pieces]) :-
	write(Piece), write('-'), display_board_line_pieces(Other_pieces).
display_board_line_pieces([]).

display_board_lines_mid([Piece | Other_pieces]) :-
	write(' '),write('|'), write('X'),write('|'), write('X'),write('|'), write('X'),write('|'), write('X'), write('|'), write('X'), write('|'), write('X'), write('|'), write('X'), write('|'), write('X'),nl.
display_board_lines_mid([]).


/* 
recomendaçoes do prof: 
	colocar numeros nas linhas e colunas
	fazer translate do board
	
read(X).
write(X).
get_char(X).	'X'
get_code(X).	96
put_char(X).
put_code(X).

print_board([]). para acabar de imprimir o tabuleiro
print_board([Line|Rest]):-
	nl,
	print_line(Line),
	print_board(Rest).

print_line([]).
print_line([Elem|Rest]):-
	translate(Elem,Elem), %este traduz o Elem e guarda em Elem
	write(Elem),
	print_line(Rest).
	
translate(0,' ').
translate(1,'X').
translate(2,'0').
*/