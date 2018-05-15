letra(["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z",
	   "A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","_"]).

digito(["0","1","2","3","4","5","6","7","8","9"]).

octdigito(["0", "1", "2", "3", "4", "5", "6", "7"]).

hexdigito(["0","1","2","3","4","5","6","7","8","9", "a","b","c","d","e","f", "A","B","C","D","E","F"]).

imprimiveis(["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z",
	         "A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","_",
	         "0","1","2","3","4","5","6","7","8","9", " ", "!", "#", "$", "%", "&", "(", ")", "*", "+", ",", "-", ".", "/", 
	         ":", ";", "<", "=", ">", "?", "@", "[", "\\", "]", "^", "`", "{", "|", "}", "~"]).


espaco(" ").
espaco("\n").
espaco("\t").

comL("//").

delimitadorC("<=").
delimitadorC(">=").
delimitadorC("==").
delimitadorC("*=").
delimitadorC("/=").
delimitadorC("+=").
delimitadorC("-=").
delimitadorC("++").
delimitadorC("--").
delimitadorC("!=").
delimitadorC("&&").
delimitadorC("||").
delimitadorC("%=").
delimitadorC("&=").
delimitadorC("|=").
delimitadorC("^=").
delimitadorC(">>").
delimitadorC("<<").
delimitadorC("->").

delimitadorC2(">>=").
delimitadorC2("<<=").

delimitadorS("<").
delimitadorS(">").
delimitadorS("=").
delimitadorS("+").
delimitadorS("-").
delimitadorS("*").
delimitadorS("/").
delimitadorS("(").
delimitadorS(")").
delimitadorS(":").
delimitadorS(",").
delimitadorS(";").
delimitadorS("%").
delimitadorS("{").
delimitadorS("}").
delimitadorS(":").
delimitadorS("&").
delimitadorS(".").
delimitadorS("[").
delimitadorS("]").
delimitadorS("?").
delimitadorS("'").
delimitadorS("\"").

token(S,0,""   ,[]).
token(S,0,Token,[Token]).


token(S,_, Token  ,[Token]) :- sub_string(Sub1,S,0,2), 
				comL(Sub1).

token(S,T,""   ,L) :- sub_string(Sub1,S,0,1), 
				espaco(Sub1),
				T1 is T - 1, 
				sub_string(Sub2,S,1,T1), 
				token(Sub2,T1,"",L).
token(S,T,Token,[Token|L]) :- sub_string(Sub1,S,0,1), 
					espaco(Sub1),
					T1 is T - 1, 
					sub_string(Sub2,S,1,T1), 
					token(Sub2,T1,"",L).

token(S,T,""   ,[Sub1|L]) :- sub_string(Sub1,S,0,2), 
					delimitadorC(Sub1),
					T1 is T - 2, 
					sub_string(Sub2,S,2,T1), 
					token(Sub2,T1,"",L).
token(S,T,Token,[Token,Sub1|L]) :- sub_string(Sub1,S,0,2), 
						delimitadorC(Sub1),
						T1 is T - 2, 
						sub_string(Sub2,S,2,T1), 
						token(Sub2,T1,"",L).

token(S,T,""   ,[Sub1|L]) :- sub_string(Sub1,S,0,1), 
					delimitadorS(Sub1),
					T1 is T - 1, 
					sub_string(Sub2,S,1,T1), 
					token(Sub2,T1,"",L).
token(S,T,Token,[Token,Sub1|L]) :- sub_string(Sub1,S,0,1), 
						delimitadorS(Sub1),
						T1 is T - 1, 
						sub_string(Sub2,S,1,T1), 
						token(Sub2,T1,"",L).

token(S,T,Token,L) :- sub_string(Sub1,S,0,1), 
				T1 is T - 1, 
				Token1 is Token + Sub1,
				sub_string(Sub2,S,1,T1), 
				token(Sub2,T1,Token1,L).

concatenacao([],L2,L2).
concatenacao(L1,[],L1).
concatenacao([H|T],L2,[H|L]):- concatenacao(T,L2,L).


lerLinha(File,[]):- feof(File).
lerLinha(File,L):- Line is readln(File), 
                        token(Line,str_length(Line),"",L1),
					lerLinha(File,L2),
                        concatenacao(L1,L2,L).

token_lista(S,0,[]).
token_lista(S,T,[Sub1|L]) :- sub_string(Sub1,S,0,1), 
					T1 is T - 1, 
					sub_string(Sub2,S,1,T1), 
					token_lista(Sub2,T1,L).

membro(X,[X]).
membro(X,[X|T]).
membro(X,[H|T]):- membro(X,T).

%--------------------simbolos terminais-------------------------%
t_letra(H):- letra(L), membro(H,L).
t_digito(H):- digito(L), membro(H,L).
t_oct(H):- octdigito(L), membro(H, L).
t_hex(H):- hexdigito(L), membro(H, L).
t_imprimivel(H):- imprimiveis(L), membro(H, L).

t_zero("0").
t_x("x").

t_void(["void"|O], O).
t_char(["char"|O], O).
t_short(["short"|O], O).
t_int(["int"|O], O).
t_long(["long"|O], O).
t_float(["float"|O], O).
t_double(["double"|O], O).
t_signed(["signed"|O], O).
t_unsigned(["unsigned"|O], O).
t_struct(["struct"|O], O).
t_union(["union"|O], O).
t_enum(["enum"|O], O).
t_typedef(["typedef"|O], O).

t_const(["const"|O], O).

t_class(["auto"|O], O).
t_class(["register"|O], O).
t_class(["static"|O], O).
t_class(["extern"|O], O).
t_class(["const"|O], O).
t_class(["typedef"|O], O).
t_class(["volatile"|O], O).

t_main(["main"|O], O).
t_if(["if"|O], O).
t_else(["else"|O], O).
t_while(["while"|O], O).
t_for(["for"|O], O).
t_do(["do"|O], O).
t_case(["case"|O], O).
t_default(["default"|O ], O).
t_break(["break"|O], O).
t_continue(["continue"|O], O).
t_switch(["switch"|O], O).
t_goto(["goto"|O], O).
t_return(["return"|O], O).

t_sizeof(["sizeof"|O], O).

t_pontovirgula([";"|O], O).
t_virgula([","|O], O).

t_parenteseA(["("|O], O).
t_parenteseF([")"|O], O).

t_chaveA(["{"|O], O).
t_chaveF(["}"|O], O).

t_colcheteA(["["|O], O).
t_colcheteF(["]"|O], O).

t_interrogacao(["?"|O], O).
t_doisP([":"|O], O).

t_atribuicao(["="|O], O).
t_multAtribuicao(["*="|O], O).
t_divAtribicao(["/="|O], O).
t_resAtribicao(["%="|O], O).
t_somaAtribuicao(["+="|O], O).
t_subAtribuicao(["-="|O], O).
t_shiftAE(["<<="|O], O).
t_shiftAD([">>="|O], O).
t_andA(["&="|O], O).
t_xorA(["^="|O], O).
t_orA(["|="|O], O).

t_or(["||"|O], O).
t_and(["&&"|O], O).
t_igual(["=="|O], O).
t_diferente(["!="|O], O).
t_menor(["<"|O], O).
t_maior([">"|O], O).
t_menorIgual(["<="|O], O).
t_maiorIgual([">="|O], O).

t_mult(["*"|O], O).
t_soma(["+"|O], O).
t_subtracao(["-"|O], O).
t_div(["/"|O], O).
t_resto(["%"|O], O).

t_unarioI(["++"|O], O).
t_unarioD(["--"|O], O).
t_unarioE(["&"|O], O).
t_unarioT(["~"|O], O).
t_unarioN(["!"|O], O).

t_shiftE(["<<"|O], O).
t_shiftD([">>"|O], O).

t_binor(["|"|O], O).
t_binxor(["^"|O], O).

t_ponto(["."|O], O).
t_seta(["->"|O], O).

t_apost(["'"|O], O).
t_aspas(["\""|O], O).
%#t_qualifier(["const"|O], O).
%#t_qualifier(["const"|O], O).
%-------------Regras de formacao------------%
programa(I, O):- dcls(I, O).
programa(I, O):- erro_.

erro_:- O is G_pilha, balanceamento(O).
erro_:- write("erro\n").
balanceamento([H]):- write("nao esperado "), write(G_O), nl.
balanceamento([H|T]):- write("nao esperado "), write(G_O), nl.


%--reconhecer um id----%
id([H|O], O):- L is str_length(H), token_lista(H, L, [H1]), t_letra(H1).
id([H|O], O):- L is str_length(H), token_lista(H, L, [H1|T]), t_letra(H1), idtail(T).
idtail([H]):- t_letra(H).
idtail([H]):- t_digito(H).
idtail([H|T]):- t_letra(H), idtail(T).
idtail([H|T]):- t_digito(H), idtail(T).
%?- id(["_"], O).

%----Reconhecer um numero----
num([H|O], O):- L is str_length(H), token_lista(H, L, [H1]), t_digito(H1).
num([H|O], O):- L is str_length(H), token_lista(H, L, [H1|T]), t_digito(H1), digit_list(T).
num([H|O], O):- L is str_length(H), token_lista(H, L, [H1]), t_zero(H1).
num([H|O], O):- L is str_length(H), token_lista(H, L, [H1|T]), t_zero(H1), octlist(T).
num([H|O], O):- L is str_length(H), token_lista(H, L, [H1, H2|T]), t_zero(H1), t_x(H2), hexlist(T).


digit_list([H]):- t_digito(H).
digit_list([H|T]):- t_digito(H), digit_list(T).

octlist([H]):- t_oct(H).
octlist([H|T]):- t_oct(H), octlist(T).

hexlist([H]):- t_hex(H).
hexlist([H|T]):- t_hex(H), hexlist(T).

%--reconhecer um caracter--
string_([H|O], O):- L is str_length(H), token_lista(H, L, [H1]), t_imprimivel(H1).
string_([H|O], O):- L is str_length(H), token_lista(H, L, [H1|T]), t_imprimivel(H1), string_list(T).

string_list([H]):- t_imprimivel(H).
string_list([H|L]):- t_imprimivel(H), string_list(L).
%?-num(["0x21"], O).

dcls([], []).
dcls(I, O):- dcl(I, O2), dcls(O2, O).

dcl(I, O):- funcdcl(I, O).
dcl(I, O):- funcproto(I, O).
dcl(I, O):- structdcl(I, O).
dcl(I, O):- uniondcl(I, O).
dcl(I, O):- enumdcl(I, O).
dcl(I, O):- vardcl(I, O).
dcl(I, O):- typedefdcl(I, O).

%function declaration
funcproto(I, O):- funcid(I, O1), t_parenteseA(O1, O2), P is G_pilha, push(O1, P, P1), G_pilha := P1, types(O2, O3), t_parenteseF(O3, O4), pop(P1, P2),G_pilha := P2, t_pontovirgula(O4, O).
funcproto(I, O):- funcid(I, O1),  t_parenteseA(O1, O2),  P is G_pilha, pop(P, P_), push(O1, P_, P1), G_pilha := P1, params(O2, O3), t_parenteseF(O3, O4), pop(P1, P2), G_pilha := P2, t_pontovirgula(O4, O).
funcproto(I, O):- funcid(I, O1), t_parenteseA(O1, O2),  P is G_pilha, pop(P, P_), push(O1, P_, P1), G_pilha := P1, t_parenteseF(O2, O3),pop(P1, P2), G_pilha := P2, t_pontovirgula(O3, O).

funcdcl(I, O):- funcid(I, O1), t_parenteseA(O1, O2),  P is G_pilha, push(O1, P, P1), params(O2, O3), t_parenteseF(O3, O4), pop(P1, P2),G_pilha := P2, block(O4, O).
funcdcl(I, O):- funcid(I, O1), t_parenteseA(O1, O2),  P is G_pilha, pop(P, P_), push(O1, P_, P1), idlist(O2, O3), t_parenteseF(O3, O4), pop(P1, P2),G_pilha := P2, structdef(O4, O).
funcdcl(I, O):- block(I, O).
funcdcl(I, O):- funcid(I, O1), t_parenteseA(O1, O2),  P is G_pilha, pop(P, P_), push(O1, P_, P1), t_parenteseF(O2, O3), pop(P1, P2),G_pilha := P2, block(O3, O).

params(I, O):- param(I, O1), t_virgula(O1, O2), params(O2, O).
params(I, O):- param(I, O).

param(I, O):- t_const(I, O1), type(O1, O2), id(O2, O).
param(I, O):- type(I, O1), id(O1, O).

types(I, O):- type(I, O1), t_virgula(O1, O2), types(O2, O).
types(I, O):- type(I, O).

idlist(I, O):- id(I, O1), t_virgula(O1, O2), idlist(O2, O).
idlist(I, O):- id(I, O).

funcid(I, O):- type(I, O1), id(O1, O).
funcid(I, O):- id(I, O).

%--fim

%Type declaration
typedefdcl(I, O):- t_typedef(I, O1), type(O1, O2), id(O2, O3), t_pontovirgula(O3, O).
structdcl(I, O):- t_struct(I, O1), id(O1, O2), t_chaveA(O2, O3), structdef(O3, O4), t_chaveF(O4, O5), t_pontovirgula(O5, O).
uniondcl(I, O):- t_union(I, O1), id(O1, O2), t_chaveA(O2, O3), structdef(O3, O4), t_chaveF(O4, O5), t_pontovirgula(O5, O).

structdef(I, O):- vardcl(I, O1), structdef(O1, O).
structdef(I, O):- vardcl(I, O).

%--fim

%-var dcl

vardcl(I, O):- mod_(I, O1), type(O1, O2), variavel(O2, O3), varlist(O3, O4), t_pontovirgula(O4, O).
vardcl(I, O):- type(I, O2), variavel(O2, O3),  varlist(O3, O4), t_pontovirgula(O4, O).
vardcl(I, O):- mod_(I, O1), variavel(O1, O3),  varlist(O3, O4), t_pontovirgula(O4, O).

/* types */
type(I, O):- base(I, O1), pointers(O1, O).

base(I, O):- sign(I ,O1), scalar(O1, O).

sign(I, O):- t_signed(I, O).
sign(I, O):- t_unsigned(I, O).
sign(I, I).

scalar(I ,O):- t_char(I, O).
scalar(I ,O):- t_int(I, O).
scalar(I ,O):- t_short(I, O).
scalar(I ,O):- t_long(I, O).
scalar(I ,O):- t_float(I, O).
scalar(I ,O):- t_double(I, O).
scalar(I ,O):- t_void(I, O).

pointers(I, O):- t_mult(I, O1), pointers(O1, O).
pointers(I, I).

/* fim */

variavel(I, O):- id(I, O1), array_(O1, O2), t_atribuicao(O2, O3), op_if(O3, O). 
variavel(I, O):- id(I, O1), array_(O1, O).

array_(I, O):-  t_colcheteA(I, O1), expr(O1, O2), t_colcheteF(O2, O).
array_(I, O):-  t_colcheteA(I, O1), t_colcheteF(O1, O).
array_(I, I).


varlist(I, O):- t_virgula(I, O2), varitem(O2, O3), varlist(O3, O).
varlist(I, I).

varitem(I, O):- pointers(I, O1), var(O1, O).

mod_(I, O):- t_class(I, O).

stm(I, O):- vardcl(I, O).
stm(I, O):- id(I, O1), t_doisP(O1, O).
stm(I, O):- t_if(I, O1), t_parenteseA(O1, O2), P is G_pilha, push(O1, P, P1),expr(O2, O3), t_parenteseF(O3, O4), pop(P1, P2), G_pilha := P2,stm(O4, O).
stm(I, O):- t_if(I, O1), t_parenteseA(O1, O2), P is G_pilha, pop(P, P_), push(O1, P_, P1), expr(O2, O3), t_parenteseF(O3, O4), pop(P1, P2), G_pilha := P2,thenstm(O4, O5),t_else(O5, O6), stm(O6, O).
stm(I, O):- t_while(I, O1), t_parenteseA(O1, O2), P is G_pilha, pop(P, P_), push(O1, P_, P1), expr(O2, O3), t_parenteseF(O3, O4), pop(P1, P2), G_pilha := P2,stm(O4, O).
stm(I, O):- t_for(I, O1), t_parenteseA(O1, O2), P is G_pilha, pop(P, P_), push(O1, P_, P1), arg(O2, O3), t_pontovirgula(O3, O4), arg(O4, O5), t_pontovirgula(O5, O6), arg(O6, O7), t_parenteseF(O7, O8), pop(P1, P2), G_pilha := P2,stm(O8, O).
stm(I, O):- normalstm(I, O).

thenstm(I, O):- t_if(I, O1), t_parenteseA(O1, O2),P is G_pilha,  push(O1, P, P1),expr(O2, O3), t_parenteseF(O3, O4), pop(P1, P2),G_pilha := P2, thenstm(O4, O5), t_else(O5, O6), thenstm(O6, O).
thenstm(I, O):- t_while(I, O1), t_parenteseA(O1, O2),P is G_pilha, pop(P, P_), push(O1, P_, P1), expr(O2, O3), t_parenteseF(O3, O4), pop(P1, P2), G_pilha := P2, thenstm(O4, O).
thenstm(I, O):- t_for(I, O1), t_parenteseA(O1, O2),P is G_pilha, pop(P, P_), push(O1, P_, P1), arg(O2, O3), t_pontovirgula(O3, O4), arg(O4, O5), t_pontovirgula(O5, O6), arg(O6, O7), t_parenteseF(O7, O8), pop(P1, P2), G_pilha := P2, thenstm(O8, O).
thenstm(I, O):- normalstm(I, O).


normalstm(I, O):- t_do(I, O1), stm(O1, O2), t_while(O2, O3), t_parenteseA(O3, O4), P is G_pilha,  push(O3, P, P1), expr(O4, O5), t_parenteseF(O5, O), pop(P1, P2),G_pilha := P2.
normalstm(I, O):- t_switch(I, O1), t_parenteseA(O1, O2), P is G_pilha, pop(P, P_), push(O1, P_, P1), expr(O2, O3), t_parenteseF(O3, O4), pop(P1, P2),G_pilha := P2, t_chaveA(O4, O5), casestm(O5, O6), t_chaveF(O6, O).
normalstm(I, O):- block(I, O).
normalstm(I, O):- expr(I, O1), t_pontovirgula(O1, O).
normalstm(I, O):- t_goto(I, O1), id(O1, O2), t_pontovirgula(O2, O).
normalstm(I, O):- t_break(I, O1), t_pontovirgula(O1, O).
normalstm(I, O):- t_continue(I, O1),t_pontovirgula(O1, O).
normalstm(I, O):- t_return(I, O1), expr(O1, O2),t_pontovirgula(O2, O).
normalstm(I, O):- t_pontovirgula(I, O).

arg(I, O):- expr(I, O).
arg(I, I).

casestm(I, O):- t_case(I, O1), value(O1, O2), t_doisP(O2, O3), stmlist(O3, O4), casestm(O4, O).
casestm(I, O):- t_default(I, O1), t_doisP(O1, O2), stmlist(O2 , O).
casestm(I, I).

block(I, O):- t_chaveA(I, O1), stmlist(O1, O2), t_chaveF(O2, O).

stmlist(I, O):- stm(I, O1), stmlist(O1, O).
stmlist(I, I).

/* Operator precedence */

expr(I, O):- opassign(I, O).
expr(I, O):- opassign(I, O1), t_virgula(O1, O2), expr(O2, O).


opassign(I, O):- op_if(I, O).
opassign(I, O):- op_if(I, O1),  operador_atribuicao(O1, O2), opassign(O2, O).

op_if(I ,O):- op_or(I, O).
op_if(I, O):- op_or(I, O1), t_interrogacao(O1, O2), op_if(O2, O3), t_doisP(O3, O4), op_if(O4, O).

op_or(I, O):- op_and(I, O).
op_or(I, O):- op_and(I, O1),  t_or(O1, O2), op_or(O2, O).

op_and(I, O):- op_binor(I, O).
op_and(I, O):- op_binor(I, O1), t_and(O1, O2), op_and(O2, O).

op_binor(I, O):- op_binxor(I, O).
op_binor(I, O):- op_binxor(I, O1),  t_binor(O1, O2), op_binor(O2, O).

op_binxor(I, O):- op_binand(I, O).
op_binxor(I, O):- op_binand(I, O1),  t_binxor(O1, O2), op_binxor(O2, O).

op_binand(I, O):- op_equate(I, O).
op_binand(I, O):- op_equate(I, O1),  t_unarioE(O1, O2), op_binand(O2, O).

op_equate(I, O):- op_compare(I, O).
op_equate(I, O):- op_compare(I, O1), operador_igualdade(O1, O2), op_equate(O2, O).

op_compare(I, O):- op_shift(I, O).
op_compare(I, O):- op_shift(I, O1), operador_relacao(O1, O2), op_compare(O2, O).

op_shift(I, O):- op_add(I, O).
op_shift(I, O):- op_add(I, O1), operador_shift(O1, O2), op_shift(O2, O).

op_add(I, O):- op_mult(I, O).
op_add(I, O):- op_mult(I, O1), operador_adicao(O1, O2), op_add(O2, O).

op_mult(I, O):- op_unary(I, O).
op_mult(I, O):- op_unary(I, O1), operador_mult(O1, O2), op_mult(O2, O).

op_unary(I, O):- op_pointer(I, O).
op_unary(I, O):- operador_unario(I, O1), op_pointer(O1, O).
op_unary(I, O):- op_pointer(I, O1), operador_unario(O1, O).
op_unary(I, O):- t_parenteseA(I, O1), P is G_pilha,  push(O3, P, P1), type(O1, O2), t_parenteseF(O2,O3), pop(P1, P2),G_pilha := P2, op_unary(O3, O).
op_unary(I, O):- t_sizeof(I, O1), t_parenteseA(O1, O2), P is G_pilha, pop(P, P_), push(O1, P_, P1),type(O2, O3), t_parenteseF(O3, O), pop(P1, P2),G_pilha := P2.
op_unary(I, O):- t_sizeof(I, O1), t_parenteseA(O1, O2), P is G_pilha, pop(P, P_), push(O1, P_, P1), id(O2, O3), poniters(O3, O4), t_parenteseF(O4, O), pop(P1, P2),G_pilha := P2.

op_pointer(I, O):- value(I, O).
op_pointer(I, O):- value(I, O1), operador_ponto(O1, O2), op_pointer(O2, O).
op_pointer(I, O):- value(I, O1), t_colcheteA(O1, O2), expr(O2, O3), t_colcheteF(O3, O).

value(I, O):- id(I, O).
value(I, O):- num(I, O).
value(I, O):- t_aspas(I, O1), string_(O1, O2), t_aspas(O2, O).
value(I, O):- t_apost(I, O1), string_(O1, O2), t_apost(O2, O).
value(I, O):- t_parenteseA(I, O1),P is G_pilha,  push(O3, P, P1), expr(O1, O2), t_parenteseF(O2, O), pop(P1, P2),G_pilha := P2.
value(I, O):- id(I, O1), t_parenteseA(O1, O2), P is G_pilha, pop(P, P_), push(O1, P_, P1),expr(O2, O3), t_parenteseF(O3, O), pop(P1, P2),G_pilha := P2.
value(I, O):- id(I, O1), t_parenteseA(O1, O2),P is G_pilha, pop(P, P_), push(O1, P_, P1),t_parenteseF(O2, O), pop(P1, P2),G_pilha := P2.

%-- operadores de atribuicao----%
operador_atribuicao(I, O):- t_atribuicao(I, O).
operador_atribuicao(I, O):- t_somaAtribuicao(I, O).
operador_atribuicao(I, O):- t_subAtribuicao(I, O).
operador_atribuicao(I, O):- t_multAtribuicao(I, O).
operador_atribuicao(I, O):- t_divAtribicao(I, O).
operador_atribuicao(I, O):- t_resAtribicao(I, O).
operador_atribuicao(I, O):- t_shiftAE(I, O).
operador_atribuicao(I, O):- t_shiftAD(I, O).
operador_atribuicao(I, O):- t_andA(I, O).
operador_atribuicao(I, O):- t_xorA(I, O).
operador_atribuicao(I, O):- t_orA(I, O).

%---operadores de igualdade----%
operador_igualdade(I, O):- t_igual(I, O).
operador_igualdade(I, O):- t_diferente(I, O).

%---Operadores de relacao de ordem----
operador_relacao(I, O):- t_menor(I, O).
operador_relacao(I, O):- t_maior(I, O).
operador_relacao(I, O):- t_menorIgual(I, O).
operador_relacao(I, O):- t_maiorIgual(I, O).

%--Operadores de adicao----
operador_adicao(I, O):- t_soma(I, O).
operador_adicao(I, O):- t_subtracao(I, O).

%--operadores de multiplicao---
operador_mult(I, O):- t_mult(I, O).
operador_mult(I, O):- t_div(I, O).
operador_mult(I, O):- t_resto(I, O).

%--operadores unarios---
operador_unario(I, O):- t_unarioI(I, O).
operador_unario(I, O):- t_unarioD(I, O).
operador_unario(I, O):- t_unarioE(I, O).
operador_unario(I, O):- t_unarioT(I, O).
operador_unario(I, O):- t_unarioN(I, O).
operador_unario(I, O):- t_mult(I, O).
operador_unario(I, O):- t_subtracao(I, O).
%--operadores de shift----
operador_shift(I, O):- t_shiftE(I, O).
operador_shift(I, O):- t_shiftD(I, O).


%---operadores de ponto---
operador_ponto(I, O):-t_ponto(I, O).
operador_ponto(I, O):-t_seta(I, O).

%--G_pilha
push([H|T], [], [H]).
push([H|T], P, [H|P]).
pop([], []).
pop([H|T], T).

%?- Line is "float b = 0;", token(Line, str_length(Line),"",L), write(L), nl, vardcl(L, O), write(O).
?- G_pilha := [],G_O := [],FILE is open("lixo.c", "r"),lerLinha(FILE, L),write(L), nl, programa(L, O),nl, write(G_pilha).