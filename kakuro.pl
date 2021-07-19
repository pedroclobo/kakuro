% Pedro Lobo - ist199115

:- [codigo_comum].

% combinacoes_soma(N, Els, Soma, Combs)
/* Combs eh a lista ordenada cujos elementos sao as combinacoes N a N, dos
 * elementos de Els cuja soma eh Soma. */
combinacoes_soma(N, Els, Soma, Combs) :-
	setof(Comb, (combinacao(N, Els, Comb), sum_list(Comb, Soma)), Combs).


% permutacoes_soma(N, Els, Soma, Perms)
/* Perms eh a lista ordenada cujos elementos sao as permutacoes das combinacoes
 * N a N, dos elementos de Els cuja soma eh Soma. */
permutacoes_soma(N, Els, Soma, Perms) :-

	% Encontra as permutacoes de cada combinacao
	findall(Perm,
	(combinacoes_soma(N, Els, Soma, Combs), member(Comb, Combs), permutation(Comb, Perm)),
	Temp),

	% Ordena as permutacoes
	setof(Perm, member(Perm, Temp), Perms).


% espaco_fila(Fila, Esp, H_V)
/* em que Fila eh uma fila (linha ou coluna) de um puzzle e H_V eh um dos
 * atomos h ou v, conforme se trate de uma fila horizontal ou vertical,
 * respectivamente, significa que Esp eh um espaco de Fila. */
espaco_fila(Fila, Esp, 'h') :-

	% Iterando sobre todas as listas da fila Agrupada,
	% cria o espaco com a lista e variaveis correspondentes
	obtem_indices(Fila, Inds), obtem_intervalos(Inds, Ints), agrupa_fila(Fila, Fila_Ag, Ints),
	member(M, Fila_Ag),
	M = [Nums|Vars],
	nth1(2, Nums, Num),
	Esp = espaco(Num, Vars).

espaco_fila(Fila, Esp, 'v') :-
	obtem_indices(Fila, Inds), obtem_intervalos(Inds, Ints), agrupa_fila(Fila, Fila_Ag, Ints),
	member(M, Fila_Ag),
	M = [Nums|Vars],
	nth1(1, Nums, Num),
	Esp = espaco(Num, Vars).


% obtem_indices(Fila, Inds)
/* Fila eh uma fila e Inds eh os indices ocupados pelas listas validas de Fila */
obtem_indices(Fila, Inds) :-

	% Lista com todas as listas invalidas
	findall(M, (nextto(M, N, Fila), is_list(M), is_list(N)), Inv),

	% Indices das listas validas, acrescentada com o tamanho da lista+1
	findall(Pos, (nth1(Pos, Fila, O), is_list(O), \+member(O, Inv)), Temp),
	length(Fila, L), L_mais_1 is L+1, append(Temp, [L_mais_1], Inds).


% obtem_intervalos(Inds, Ints)
/* Inds eh uma lista de indices e Ints eh a lista formada pelos intervalos
 * dos indices. ex. Inds = [1,2,3], Ints = [[1,2], [2,3]] */
obtem_intervalos(Inds, Ints) :-
	findall(Int, obtem_intervalos_aux(Inds, Int), Ints).

obtem_intervalos_aux(Inds, Int) :-

	% Itera sobre todos os indices, prefazendo pares de indices
	length(Inds, L), L_menos_1 is L-1, between(1, L_menos_1, Ind),
	nth1(Ind, Inds, Ind1), Ind_mais_1 is Ind+1, nth1(Ind_mais_1, Inds, Ind2),
	Int = [Ind1, Ind2].


% agrupa_fila(Fila, Fila_S, Ints)
/* Fila eh uma fila, Ints eh uma lista de intervalos e Fila_S eh a fila com
 * listas agrupadas de listas e variaveis */
agrupa_fila(Fila, Fila_Ag, Ints) :-
	bagof(Fila_S, agrupa_fila_aux(Fila, Fila_S, Ints), Fila_Ag).

agrupa_fila_aux(Fila, Fila_S, Ints) :-

	% Itera sobre todos os pares, definindo um limite inferior e superior de indices
	member(Par, Ints), nth1(1, Par, Inf), nth1(2, Par, Sup),

	% Forma a lista com todas as variaveis entre as duas listas com indices
	% Inf e Sup
	bagof(M, Pos^(nth1(Pos, Fila, M), \+is_list(M), Inf < Pos, Pos < Sup), Vars),
	nth1(Inf, Fila, Lst),

	Fila_S = [Lst|Vars].


% espacos_fila(H_V, Fila, Espacos)
/* em que Fila eh uma fila (linha ou coluna) de uma grelha e H_V eh um dos
 * atomos h ou v, significa que Espacos eh a lista de todos os espacos de
 * Fila, da esquerda para a direita */
espacos_fila(H_V, Fila, Espacos) :-
	bagof(Esp, espaco_fila(Fila, Esp, H_V), Espacos).


% espacos_puzzle(Puzzle, Espacos)
/* em que Puzzle eh um puzzle, significa que Espacos eh a lista de espacos de
 * Puzzle */
espacos_puzzle(Puzzle, Espacos) :-

	% Obter espacos do puzzle
	bagof(Esps, espacos_puzzle_aux('h', Puzzle, Esps), Espacos1),
	mat_transposta(Puzzle, Transp),
	bagof(Esps, espacos_puzzle_aux('v', Transp, Esps), Espacos2),

	% Junta os espacos numa so lista
	append(Espacos1, Espacos2, Temp),
	append(Temp, Espacos).

espacos_puzzle_aux(H_V, Puzzle, Esps) :-

	% Descobre todos os espacos do puzzle
	member(Fila, Puzzle),
	espacos_fila(H_V, Fila, Esps).


% espacos_com_posicoes_comuns(Espacos, Esp, Esps_com)
/* em que Espacos eh uma lista de espacos e Esp eh um espaco, significa que
 * Esps_com eh a lista de espacos com variaveis em comum com Esp, exceptuando Esp */
espacos_com_posicoes_comuns(Espacos, Esp, Esps_com) :-
	bagof(Esp_com, espacos_com_posicoes_comuns_aux(Espacos, Esp, Esp_com), Esps_com).

espacos_com_posicoes_comuns_aux(Espacos, Esp, Esp_com) :-
	member(M, Espacos),
	eh_espacos_com_posicoes_comuns(Esp, M),
	Esp_com = M.


% eh_espacos_com_posicoes_comuns(Esp1, Esp2)
/* em que Esp1 e Esp2 sao dois espacos distintos com posicoes comuns */
eh_espacos_com_posicoes_comuns(espaco(N1, Vars1), espaco(N2, Vars2)) :-
	espaco(N1, Vars2) \== espaco(N2, Vars2),
	\+disjuntas(Vars1, Vars2).


% membro(El, Lst)
/* em que El eh uma elemento e Lst eh uma lista,
 * equivalente ao predicado member, porem sem a unificacao */
membro(El, [P|_]) :- El == P.
membro(El, [_|R]) :- membro(El, R).


% disjuntas(Lst1, Lst2)
/* em que Lst1 e Lst2 sao duas listas disjuntas */
disjuntas([], _) :- !.
disjuntas(_, []) :- !.
disjuntas([P|_], Lst2) :-
	membro(P, Lst2),
	!, fail.
disjuntas([_|R], Lst2) :- disjuntas(R, Lst2).


% permutacoes_soma_espacos(Espacos, Perms_soma)
/* em que Espacos eh uma lista de espacos, significa que Perms_soma eh a lista
 * de listas de 2 elementos, em que o 1o elemento eh um espaco de Espacos e o
 * 2o eh a lista ordenada de permutacoes cuja soma eh igual a soma do espaco */
permutacoes_soma_espacos(Espacos, Perms_soma) :-
	bagof(Perm_soma, permutacoes_soma_espacos_aux(Espacos, Perm_soma), Perms_soma).

permutacoes_soma_espacos_aux(Espacos, Perm_soma) :-
	member(Esp, Espacos), Esp = espaco(Soma, Vars), length(Vars, L),
	permutacoes_soma(L, [1,2,3,4,5,6,7,8,9], Soma, Perms),
	Perm_soma = [Esp, Perms].


% numeros_comuns(Lst_Perms, Numeros_comuns)
/* Lst_Perms eh uma lista de permutacoes e Numeros_comuns eh uma lista de pares
 * (pos, numero), em que as listas de Lst_Perms contem numero na posicao pos. */
numeros_comuns(Lst_Perms, Numeros_comuns) :-

	% Lista de pares (pos, numero) de todas as permutacoes
	findall((Pos, Num), (member(L, Lst_Perms), nth1(Pos, L, Num)), Temp),

	% Lista de pares comuns a todas as permutacoes
	% Caso nada seja comum, Numeros_comuns eh a lista vazia
	length(Lst_Perms, N),
	setof(Par, Aux ^
	(member(Par, Temp), intersection(Temp, [Par], Aux), length(Aux, N)),
	Numeros_comuns); Numeros_comuns = [].
