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

token(S,0,""   ,[], Linha).
token(S,0,Token,[t_(Token, Linha)], Linha).
token(S,_, Token  ,[Tok], Linha) :- sub_string(Sub1,S,0,2), 
				Tok is t_(Token, Linha),
				comL(Sub1).

token(S,T,""   ,L, Linha) :- sub_string(Sub1,S,0,1), 
				espaco(Sub1),
				T1 is T - 1, 
				sub_string(Sub2,S,1,T1), 
				token(Sub2,T1,"",L, Linha).
token(S,T,Token,[Tok|L], Linha) :- sub_string(Sub1,S,0,1), 
					espaco(Sub1),
					Tok is t_(Token, Linha),
					T1 is T - 1, 
					sub_string(Sub2,S,1,T1), 
					token(Sub2,T1,"",L, Linha).

token(S,T,""   ,[Tok|L], Linha) :- sub_string(Sub1,S,0,2), 
					delimitadorC(Sub1),
					T1 is T - 2,
					Tok is t_(Sub1, Linha),
					sub_string(Sub2,S,2,T1), 
					token(Sub2,T1,"",L, Linha).
token(S,T,Token,[Tok1,Tok|L], Linha) :- sub_string(Sub1,S,0,2), 
						delimitadorC(Sub1),
						Tok1 is t_(Token, Linha),
						Tok is t_(Sub1, Linha),
						T1 is T - 2, 
						sub_string(Sub2,S,2,T1), 
						token(Sub2,T1,"",L, Linha).

token(S,T,""   ,[Tok|L], Linha) :- sub_string(Sub1,S,0,1), 
					delimitadorS(Sub1),
					Tok is t_(Sub1, Linha),
					T1 is T - 1, 
					sub_string(Sub2,S,1,T1), 
					token(Sub2,T1,"",L, Linha).
token(S,T,Token,[Tok1,Tok|L], Linha) :- sub_string(Sub1,S,0,1), 
						delimitadorS(Sub1),
						Tok1 is t_(Token, Linha),
						Tok is t_(Sub1, Linha),
						T1 is T - 1, 
						sub_string(Sub2,S,1,T1), 
						token(Sub2,T1,"",L, Linha).

token(S,T,Token,L, Linha) :- sub_string(Sub1,S,0,1), 
				T1 is T - 1, 
				Token1 is Token + Sub1,
				sub_string(Sub2,S,1,T1), 
				token(Sub2,T1,Token1,L, Linha).

concatenacao([],L2,L2).
concatenacao(L1,[],L1).
concatenacao([H|T],L2,[H|L]):- concatenacao(T,L2,L).


lerLinha(File,[], Linha):- feof(File).
lerLinha(File,L, Linha):- Line is readln(File), 
						token(Line,str_length(Line),"",L1, Linha),
						Linha2 is Linha + 1,
						lerLinha(File,L2, Linha2),
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

t_void([t_("void", L)|O], O).
t_char([t_("char", L)|O], O).
t_short([t_("short", L)|O], O).
t_int([t_("int", L)|O], O).
t_long([t_("long", L)|O], O).
t_float([t_("float", L)|O], O).
t_double([t_("double", L)|O], O).
t_signed([t_("signed", L)|O], O).
t_unsigned([t_("unsigned", L)|O], O).
t_struct([t_("struct", L)|O], O).
t_union([t_("union", L)|O], O).
t_enum([t_("enum", L)|O], O).
t_typedef([t_("typedef", L)|O], O).

t_const([t_("const", L)|O], O).

t_class([t_("auto", L)|O], O).
t_class([t_("register", L)|O], O).
t_class([t_("static", L)|O], O).
t_class([t_("extern", L)|O], O).
t_class([t_("const", L)|O], O).
t_class([t_("volatile", L)|O], O).

t_main([t_("main", L)|O], O).
t_if([t_("if", L)|O], O).
t_else([t_("else", L)|O], O).
t_while([t_("while", L)|O], O).
t_for([t_("for", L)|O], O).
t_do([t_("do", L)|O], O).
t_case([t_("case", L)|O], O).
t_default([t_("default", L)|O ], O).
t_break([t_("break", L)|O], O).
t_continue([t_("continue", L)|O], O).
t_switch([t_("switch", L)|O], O).
t_goto([t_("goto", L)|O], O).
t_return([t_("return", L)|O], O).

t_sizeof([t_("sizeof", L)|O], O).

t_pontovirgula([t_(";", L)|O], O).
t_virgula([t_(",", L)|O], O).

t_parenteseA([t_("(", L)|O], O).
t_parenteseF([t_(")", L)|O], O).

t_chaveA([t_("{", L)|O], O).
t_chaveF([t_("}", L)|O], O).

t_colcheteA([t_("[", L)|O], O).
t_colcheteF([t_("]", L)|O], O).

t_interrogacao([t_("?", L)|O], O).
t_doisP([t_(":", L)|O], O).

t_atribuicao([t_("=", L)|O], O).
t_multAtribuicao([t_("*=", L)|O], O).
t_divAtribicao([t_("/=", L)|O], O).
t_resAtribicao([t_("%=", L)|O], O).
t_somaAtribuicao([t_("+=", L)|O], O).
t_subAtribuicao([t_("-=", L)|O], O).
%t_shiftAE(["<<="|O], O).
%t_shiftAD([">>="|O], O).
t_andA([t_("&=", L)|O], O).
t_xorA([t_("^=", L)|O], O).
t_orA([t_("|=", L)|O], O).

t_or([t_("||", L)|O], O).
t_and([t_("&&", L)|O], O).
t_igual([t_("==", L)|O], O).
t_diferente([t_("!=", L)|O], O).
t_menor([t_("<", L)|O], O).
t_maior([t_(">", L)|O], O).
t_menorIgual([t_("<=", L)|O], O).
t_maiorIgual([t_(">=", L)|O], O).

t_mult([t_("*", L)|O], O).
t_soma([t_("+", L)|O], O).
t_subtracao([t_("-", L)|O], O).
t_div([t_("/", L)|O], O).
t_resto([t_("%", L)|O], O).

t_unarioI([t_("++", L)|O], O).
t_unarioD([t_("--", L)|O], O).
t_unarioE([t_("&", L)|O], O).
t_unarioT([t_("~", L)|O], O).
t_unarioN([t_("!", L)|O], O).

t_shiftE([t_("<<", L)|O], O).
t_shiftD([t_(">>", L)|O], O).

t_binor([t_("|", L)|O], O).
t_binxor([t_("^", L)|O], O).

t_ponto([t_(".", L)|O], O).
t_seta([t_("->", L)|O], O).

t_apost([t_("'", L)|O], O).
t_aspas([t_("\"", L)|O], O).

%----------------------------%
exibe_erro_linha([t_(H, X)|T]):- writeq(H), write(" na linha "), write(X), terminate("\nEncerrado\n").
%----------------------------%

%-------------Regras de formacao------------%

programa(I, O):- dcls(I, O);(writeq(programa),nl, write(O)).

%--reconhecer um id----%
id([t_(H, Linha)|O], O):- L is str_length(H), token_lista(H, L, [H1]), t_letra(H1).
id([t_(H, Linha)|O], O):- L is str_length(H), token_lista(H, L, [H1|T]), t_letra(H1), idtail(T).
idtail([H]):- t_letra(H).
idtail([H]):- t_digito(H).
idtail([H|T]):- t_letra(H), idtail(T).
idtail([H|T]):- t_digito(H), idtail(T).
%?- id(["_"], O).

%----Reconhecer um numero----
num([t_(H, Linha)|O], O):- L is str_length(H), token_lista(H, L, [H1]), t_digito(H1).
num([t_(H, Linha)|O], O):- L is str_length(H), token_lista(H, L, [H1|T]), t_digito(H1), digit_list(T).
num([t_(H, Linha)|O], O):- L is str_length(H), token_lista(H, L, [H1]), t_zero(H1).
num([t_(H, Linha)|O], O):- L is str_length(H), token_lista(H, L, [H1|T]), t_zero(H1), octlist(T).
num([t_(H, Linha)|O], O):- L is str_length(H), token_lista(H, L, [H1, H2|T]), t_zero(H1), t_x(H2), hexlist(T).


digit_list([H]):- t_digito(H).
digit_list([H|T]):- t_digito(H), digit_list(T).

octlist([H]):- t_oct(H).
octlist([H|T]):- t_oct(H), octlist(T).

hexlist([H]):- t_hex(H).
hexlist([H|T]):- t_hex(H), hexlist(T).

%--reconhecer um caracter--
string_([t_(H, Linha)|O], O):- L is str_length(H), token_lista(H, L, [H1]), t_imprimivel(H1).
string_([t_(H, Linha)|O], O):- L is str_length(H), token_lista(H, L, [H1|T]), t_imprimivel(H1), string_list(T).

string_list([H]):- t_imprimivel(H).
string_list([H|L]):- t_imprimivel(H), string_list(L).
%?-num(["0x21"], O).



dcls([], []).
dcls(I, O):- (dcl(I, O2);(writeq("erro dcl"),nl,exibe_erro_linha(I))),
			 (dcls(O2, O);(writeq("erro dlcs"),nl,exibe_erro_linha(O2))).


dcl(I, O):- funcdcl(I, O).
dcl(I, O):- funcproto(I, O).
dcl(I, O):- structdcl(I, O).
dcl(I, O):- uniondcl(I, O).
dcl(I, O):- enumdcl(I, O).
dcl(I, O):- vardcl(I, O).
dcl(I, O):- (typedefdcl(I, O);writeq("erro dcl"),nl,exibe_erro_linha(I)).

%function declaration
funcproto(I, O):- funcid(I, O1),  t_parenteseA(O1, O2), types(O2, O3), t_parenteseF(O3, O4), t_pontovirgula(O4, O).
funcproto(I, O):- funcid(I, O1),  t_parenteseA(O1, O2), params(O2, O3), t_parenteseF(O3, O4), t_pontovirgula(O4, O).
funcproto(I, O):- funcid(I, O1),  t_parenteseA(O1, O2), t_parenteseF(O2, O3), t_pontovirgula(O3, O).

funcdcl(I, O):- funcid(I, O1), t_parenteseA(O1, O2), params(O2, O3), t_parenteseF(O3, O4), block(O4, O).
funcdcl(I, O):- funcid(I, O1), t_parenteseA(O1, O2), idlist(O2, O3), t_parenteseF(O3, O4), structdef(O4, O5), block(O5, O).
funcdcl(I, O):- funcid(I, O1), t_parenteseA(O1, O2), t_parenteseF(O2, O3), block(O3, O).

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
vardcl(I, O):- type(I, O2), variavel(O2, O3), varlist(O3, O4), t_pontovirgula(O4, O).
vardcl(I, O):- mod_(I, O1), variavel(O1, O3), varlist(O3, O4), t_pontovirgula(O4, O).

/* types */
type(I, O):- base(I, O1), pointers(O1, O).

base(I, O):- sign(I ,O1), scalar(O1, O).
base(I, O):- t_struct(I ,O1), id(O1, O).
base(I, O):- t_union(I, O1), id(O1, O).
base(I, O):- t_struct(I ,O1), t_chaveA(O1, O2), structdef(O2, O3), t_chaveF(O3, O).
base(I, O):- t_union(I, O1), t_chaveA(O1, O2), structdef(O2, O3), t_chaveF(O3, O) .

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

array_(I, O):- t_colcheteA(I, O1), expr(O1, O2), t_colcheteF(O2, O).
array_(I, O):- t_colcheteA(I, O1), t_colcheteF(O1, O).
array_(I, I).


varlist(I, O):- t_virgula(I, O2), varitem(O2, O3), varlist(O3, O).
varlist(I, I).

varitem(I, O):- pointers(I, O1), variavel(O1, O).

mod_(I, O):- t_class(I, O).

stm(I, O):- vardcl(I, O).
stm(I, O):- id(I, O1), t_doisP(O1, O).
stm(I, O):- t_if(I, O1), t_parenteseA(O1, O2), expr(O2, O3), t_parenteseF(O3, O4), stm(O4, O).
stm(I, O):- t_if(I, O1), t_parenteseA(O1, O2), expr(O2, O3), t_parenteseF(O3, O4), thenstm(O4, O5),t_else(O5, O6), stm(O6, O).
stm(I, O):- t_while(I, O1), t_parenteseA(O1, O2), expr(O2, O3), t_parenteseF(O3, O4), stm(O4, O).
stm(I, O):- t_for(I, O1), t_parenteseA(O1, O2), arg(O2, O3), t_pontovirgula(O3, O4), arg(O4, O5), t_pontovirgula(O5, O6), arg(O6, O7), t_parenteseF(O7, O8), stm(O8, O).
stm(I, O):- normalstm(I, O).

thenstm(I, O):- t_if(I, O1), t_parenteseA(O1, O2), expr(O2, O3), t_parenteseF(O3, O4), thenstm(O4, O5), t_else(O5, O6), thenstm(O6, O).
thenstm(I, O):- t_while(I, O1), t_parenteseA(O1, O2), expr(O2, O3), t_parenteseF(O3, O4), thenstm(O4, O).
thenstm(I, O):- t_for(I, O1), t_parenteseA(O1, O2), arg(O2, O3), t_pontovirgula(O3, O4), arg(O4, O5), t_pontovirgula(O5, O6), arg(O6, O7), t_parenteseF(O7, O8), thenstm(O8, O).
thenstm(I, O):- normalstm(I, O).

normalstm(I, O):- t_do(I, O1), stm(O1, O2), t_while(O2, O3), t_parenteseA(O3, O4), expr(O4, O5), t_parenteseF(O5, O).
normalstm(I, O):- t_switch(I, O1), t_parenteseA(O1, O2), expr(O2, O3), t_parenteseF(O3, O4), t_chaveA(O4, O5), casestm(O5, O6), t_chaveF(O6, O).
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
opassign(I, O):- op_if(I, O1), operador_atribuicao(O1, O2), opassign(O2, O).

op_if(I ,O):- op_or(I, O).
op_if(I, O):- op_or(I, O1), t_interrogacao(O1, O2), op_if(O2, O3), t_doisP(O3, O4), op_if(O4, O).

op_or(I, O):- op_and(I, O).
op_or(I, O):- op_and(I, O1), t_or(O1, O2), op_or(O2, O).

op_and(I, O):- op_binor(I, O).
op_and(I, O):- op_binor(I, O1), t_and(O1, O2), op_and(O2, O).

op_binor(I, O):- op_binxor(I, O).
op_binor(I, O):- op_binxor(I, O1), t_binor(O1, O2), op_binor(O2, O).

op_binxor(I, O):- op_binand(I, O).
op_binxor(I, O):- op_binand(I, O1), t_binxor(O1, O2), op_binxor(O2, O).

op_binand(I, O):- op_equate(I, O).
op_binand(I, O):- op_equate(I, O1), t_unarioE(O1, O2), op_binand(O2, O).

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
op_unary(I, O):- t_parenteseA(I, O1), type(O1, O2), t_parenteseF(O2,O3), op_unary(O3, O).
op_unary(I, O):- t_sizeof(I, O1), t_parenteseA(O1, O2), type(O2, O3), t_parenteseF(O3, O).
op_unary(I, O):- t_sizeof(I, O1), t_parenteseA(O1, O2), id(O2, O3), poniters(O3, O4), t_parenteseF(O4, O).

op_pointer(I, O):- value(I, O).
op_pointer(I, O):- value(I, O1), operador_ponto(O1, O2), op_pointer(O2, O).
op_pointer(I, O):- value(I, O1), t_colcheteA(O1, O2), expr(O2, O3), t_colcheteF(O3, O).

value(I, O):- id(I, O).
value(I, O):- num(I, O).
value(I, O):- t_aspas(I, O1), string_(O1, O2), t_aspas(O2, O).
value(I, O):- t_apost(I, O1), string_(O1, O2), t_apost(O2, O).
value(I, O):- t_parenteseA(I, O1), expr(O1, O2), t_parenteseF(O2, O).
value(I, O):- id(I, O1), t_parenteseA(O1, O2), expr(O2, O3), t_parenteseF(O3, O).
value(I, O):- id(I, O1), t_parenteseA(O1, O2), t_parenteseF(O2, O).

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
push(t_(H, L), [], [t_(H, L)]).
push(t_(H, L), P, [t_(H, L)|P]).

pop([], []).
pop([t_(H, L)|T], T).

igual([t_(A,_)], t_(A,_)).
igual([t_(A,_)|O], t_(A,_)).

balanceamento_geral([]).
balanceamento_geral([t_("(", L)|T]):- P is G_pilha, push(t_("(", L), P, P1), G_pilha := P1, balanceamento_geral(T).
balanceamento_geral([t_("[", L)|T]):- P is G_pilha, push(t_("[", L), P, P1), G_pilha := P1, balanceamento_geral(T).
balanceamento_geral([t_("{", L)|T]):- P is G_pilha, push(t_("{", L), P, P1), G_pilha := P1, balanceamento_geral(T).
balanceamento_geral([t_(")", L)|T]):- P is G_pilha, igual(P, t_("(", L)), pop(P, A), G_pilha := A, balanceamento_geral(T).
balanceamento_geral([t_("]", L)|T]):- P is G_pilha, igual(P, t_("[", L)), pop(P, A), G_pilha := A, balanceamento_geral(T).
balanceamento_geral([t_("}", L)|T]):- P is G_pilha, igual(P, t_("{", L)), pop(P, A), G_pilha := A, balanceamento_geral(T).
balanceamento_geral([t_(")", L)|T]):- P is G_pilha, not(igual(P, t_("(", L))), push(t_(")", L), P, P1), G_pilha := P1.
balanceamento_geral([t_("]", L)|T]):- P is G_pilha, not(igual(P, t_("[", L))), push(t_("]", L), P, P1), G_pilha := P1.
balanceamento_geral([t_("}", L)|T]):- P is G_pilha, not(igual(P, t_("{", L))), push(t_("}", L), P, P1), G_pilha := P1.
balanceamento_geral([H|T]):- balanceamento_geral(T).

pre_programa(L, [t_(H, Linha)|O]):- writeq(H), write(" nao esperado na linha "), write(Linha).
pre_programa(L, [t_(H, Linha)]):- writeq(H), write(" nao esperado na linha "), write(Linha).
pre_programa(L, []):- programa(L, O).

%?- Line is "float b = 0;", token(Line, str_length(Line),"",L), write(L), nl, vardcl(L, O), write(O).
?- G_pilha := [],G_O := [],
	FILE is open("lixo.c", "r"),
	lerLinha(FILE, L, 1), /* write(L), nl, */
	balanceamento_geral(L),
	P is  G_pilha,
	%write(G_pilha),
	pre_programa(L, P), write(L).
