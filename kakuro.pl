% Pedro Lobo - ist199115

:- [codigo_comum].
:- [puzzles_publicos].

% combinacoes_soma(N, Els, Soma, Combs)
/* Combs eh a lista ordenada de combinacoes N a N, dos elementos de Els cuja
 * soma eh Soma. */
combinacoes_soma(N, Els, Soma, Combs) :-
	setof(Comb, (combinacao(N, Els, Comb), sum_list(Comb, Soma)), Combs).


% permutacoes_soma(N, Els, Soma, Perms)
/* Perms eh a lista ordenada de permutacoes das combinacoes N a N, dos
 * elementos de Els cuja soma eh Soma. */
permutacoes_soma(N, Els, Soma, Perms) :-

	findall(Perm,
	(combinacoes_soma(N, Els, Soma, Combs), member(Comb, Combs), permutation(Comb, Perm)),
	Temp),

	% Ordena as permutacoes
	setof(Perm, member(Perm, Temp), Perms).


% espaco_fila(Fila, Esp, H_V)
/* Espaco eh um espaco da fila Fila na direcao de H_V:
 * h - horizontal
 * v - vertical */
espaco_fila(Fila, Esp, 'h') :-

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
/* Inds eh a lista de indices ocupados pelas listas validas de Fila */
obtem_indices(Fila, Inds) :-

	% Listas invalidas
	findall(M, (nextto(M, N, Fila), is_list(M), is_list(N)), Inv),

	% Indices das listas validas
	findall(Pos, (nth1(Pos, Fila, O), is_list(O), \+member(O, Inv)), Temp),
	length(Fila, L), L_mais_1 is L+1, append(Temp, [L_mais_1], Inds).


% obtem_intervalos(Inds, Ints)
/* Inds eh uma lista de indices e Ints eh a lista formada pelos intervalos
 * dos indices. ex. Inds = [1,2,3], Ints = [[1,2], [2,3]] */
obtem_intervalos(Inds, Ints) :-
	findall(Int, obtem_intervalos_aux(Inds, Int), Ints).

obtem_intervalos_aux(Inds, Int) :-

	length(Inds, L), L_menos_1 is L-1, between(1, L_menos_1, Ind),
	nth1(Ind, Inds, Ind1), Ind_mais_1 is Ind+1, nth1(Ind_mais_1, Inds, Ind2),
	Int = [Ind1, Ind2].


% agrupa_fila(Fila, Fila_S, Ints)
/* Ints eh uma lista de intervalos e Fila_S eh a fila com listas agrupadas de
 * listas e variaveis */
agrupa_fila(Fila, Fila_Ag, Ints) :-
	bagof(Fila_S, agrupa_fila_aux(Fila, Fila_S, Ints), Fila_Ag).

agrupa_fila_aux(Fila, Fila_S, Ints) :-

	member(Par, Ints), nth1(1, Par, Inf), nth1(2, Par, Sup),

	bagof(M, Pos^(nth1(Pos, Fila, M), \+is_list(M), Inf < Pos, Pos < Sup), Vars),
	nth1(Inf, Fila, Lst),

	Fila_S = [Lst|Vars].


% espacos_fila(H_V, Fila, Espacos)
/* Espacos eh a lista de espacos da fila Fila na direcao H_V */
espacos_fila(H_V, Fila, Espacos) :-
	bagof(Esp, espaco_fila(Fila, Esp, H_V), Espacos).


% espacos_puzzle(Puzzle, Espacos)
/* Espacos eh a lista de espacos do puzzle Puzzle */
espacos_puzzle(Puzzle, Espacos) :-

	bagof(Esps, espacos_puzzle_aux('h', Puzzle, Esps), Espacos1),
	mat_transposta(Puzzle, Transp),
	bagof(Esps, espacos_puzzle_aux('v', Transp, Esps), Espacos2),

	append(Espacos1, Espacos2, Temp),
	append(Temp, Espacos).

espacos_puzzle_aux(H_V, Puzzle, Esps) :-

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
membro(El, [P|R]) :-
	El == P;
	membro(El, R).


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


% permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma)
/* em que Perm eh uma permutacao, Esp eh um espaco, Espacos eh uma lista de
 * espacos, e Perms_soma eh uma lista de listas tal como obtida pelo predicado
 * anterior, significa que Perm eh uma permutacao possivel para o espaco Esp */
permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma) :-

	% Encontra as permutacoes soma para os espacos em comum
	espacos_com_posicoes_comuns(Espacos, Esp, Esp_Com),
	permutacoes_soma_espacos(Esp_Com, Temp),
	intersection(Perms_soma, Temp, Perms_soma_Com),

	% Encontra as permutacoes para o espaco Esp
	permutacoes_soma_espacos([Esp], [[_, Esp_Perms]]),

	% Itera sobre todas as permutacoes de Esp, ate conseguir aquela que nao
	% apresenta incompatibilidades
	member(Perm, Esp_Perms),
	include(eh_permutacao_impossivel(Esp, Perm, 1), Perms_soma_Com, N),
	length(N, 0).


% eh_permutacao_impossivel(Esp, Perm, N, Perms_soma_Com)
eh_permutacao_impossivel(espaco(_, [Var1|_]), Perm, N, [espaco(_, Vars), Perms]) :-

	% Verifica se as variaveis do espaco sao iguais
	membro(Var1, Vars),

	% Lista de permutacoes
	append(Perms, Perms_L),

	% Extrai o nesimo numero da permutacao e verifica que esta nao eh uma
	% permutacao valida
	nth1(N, Perm, Num), !,
	\+ member(Num, Perms_L).


% Itera sobre todas as variaveis do espaco
eh_permutacao_impossivel(espaco(S1, [_|R1]), Perm, N, [espaco(S2, Vars), Perms]) :-
	N_mais_1 is N + 1,
	eh_permutacao_impossivel(espaco(S1, R1), Perm, N_mais_1, [espaco(S2, Vars), Perms]).


% permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss)
/* em que Espacos eh uma lista de espacos, Perms_soma eh uma lista de listas
 * tal como obtida pelo predicado permutacoes_soma_espacos, e Esp eh um
 * espaco, significa que Perms_poss eh uma lista de 2 elementos em que o
 * primeiro eh a lista de variaveis de Esp e o segundo e a lista ordenada de
 * permutacoes possiveis para o espaco Esp. */
permutacoes_possiveis_espaco(Espacos, Perms_soma, espaco(S, Vars), Perms_poss) :-

	% Encontra permutacoes possiveis
	bagof(Perm, permutacao_possivel_espaco(Perm, espaco(S, Vars), Espacos, Perms_soma), Perms),

	Perms_poss = [Vars, Perms].


% permutacoes_possiveis_espaco(Espacos, Perms_poss_esps)
permutacoes_possiveis_espacos(Espacos, Perms_poss_esps) :-
	permutacoes_soma_espacos(Espacos, Perms_soma),

	bagof(Perms_poss, (member(Esp, Espacos), permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss)), Perms_poss_esps).


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
	Numeros_comuns), !; Numeros_comuns = [].


% atribui_comuns(Perms_Possiveis)
/* em que Perms_Possiveis eh uma lista de permutacoes possiveis, atualiza esta
 * lista atribuindo a cada espaco numeros comuns a todas as permutacoes
 * possiveis para esse espaco. */
atribui_comuns(Perms_Possiveis) :-
	maplist(atribui_comuns_aux, Perms_Possiveis).

atribui_comuns_aux([Vars, Perms]) :-
	numeros_comuns(Perms, Comuns),
	atribui_comuns_aux(Vars, Comuns).

atribui_comuns_aux(_, []).
atribui_comuns_aux(Esp, [(Pos, Val) | R]) :-
	nth1(Pos, Esp, Val),
	atribui_comuns_aux(Esp, R).


% simplifica(Perms_Possiveis, Novas_Perms_Possiveis)
/* em que Perms_Possiveis eh uma lista de permutacoes possiveis, significa que
 * Novas_Perms_Possiveis eh o resultado de simplificar Perms_Possiveis. */
simplifica(Perms_Possiveis, Novas_Perms_Possiveis) :-
	atribui_comuns(Perms_Possiveis),
	retira_impossiveis(Perms_Possiveis, P),
	Perms_Possiveis \== P,
	simplifica(P, Novas_Perms_Possiveis).

simplifica(Perms_Possiveis, Novas_Perms_Possiveis) :-
	atribui_comuns(Perms_Possiveis),
	retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis),
	Perms_Possiveis == Novas_Perms_Possiveis.


% inicializa(Puzzle, Perms_Possiveis)
/* em que Puzzle eh um puzzle, significa que Perms_Possiveis eh a lista de
 * permutacoes simplificada para Puzzle */
inicializa(Puzzle, Perms_Possiveis) :-
	espacos_puzzle(Puzzle, Espacos),
	permutacoes_possiveis_espacos(Espacos, Perms_P),
	simplifica(Perms_P, Perms_Possiveis).


% escolhe_menos_alternativas(Perms_Possiveis, Escolha)
/* Escolha eh o elemento de Perms_Possiveis com lista de permutacoes com mais
 * que uma permutacao e menor numero de permutacoes entre as que teem mais que
 * uma */
escolhe_menos_alternativas(Perms_Possiveis, [Vars, Perms]) :-
	member([Vars, Perms], Perms_Possiveis),
	any(var, Vars),
	all(mais_curto(Perms), Perms_Possiveis), !.

menor_comprimento(Lst1, Lst2) :-
	length(Lst1, L1), length(Lst2, L2),
	L1 =< L2.

any(Goal, [El | _]) :-
	call(Goal, El), !.
any(Goal, [_ | Resto]) :- any(Goal, Resto).

all(_, []) :- !.
all(Goal, [El | Resto]) :-
	call(Goal, El),
	all(Goal, Resto).


% experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis)
/* Novas_Perms_Possiveis eh o resultado de substituir em Perms_Possiveis o
 * espaco correspondente ao espaco de Escolha por Escolha, unificando o espaco
 * de Escolha com uma permutacao de Escolha */
experimenta_perm([Esp, Lst_Perms], Perms_Possiveis, Novas_Perms_Possiveis) :-
	member(Esp, Lst_Perms),
	append([Pre, [[Esp, Lst_Perms]], Post], Perms_Possiveis),
	append([Pre, [[Esp, [Esp]]], Post], Novas_Perms_Possiveis).


% resolve_aux(Perms, Novas_Perms)
/* Novas_Perms_Possiveis eh o resultado de resolver o puzzle representado
 por Perms_Possiveis */
resolve_aux(Perms, Novas_Perms) :-
	escolhe_menos_alternativas(Perms, Escolha), !,
	experimenta_perm(Escolha, Perms, Perms_Testadas),
	simplifica(Perms_Testadas, Perms_Simples),
	resolve_aux(Perms_Simples, Novas_Perms).

resolve_aux(Perms, Perms_Simples) :-
	all(all(\=([])), Perms),
	simplifica(Perms, Perms_Simples), !.


% resolve(Puzzle)
/* Unifica as variaveis em Puzzle de forma a inicializar e a resolver o Puzzle */
resolve(Puzzle) :-
	inicializa(Puzzle, Perms_Possiveis),
	resolve_aux(Perms_Possiveis, _).
