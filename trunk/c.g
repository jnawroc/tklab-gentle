--i:



'type' IDENT

'type' PROG
	prog(DECLLIST, CODE, RET)

'type' DECL
	decl(TYPE, IDENT, value: INT)

'type' DECLLIST
	decllist(DECL, DECLLIST)
	nil

'type' TYPE
	int
	uint
	array(TYPE, count: INT)


'type' CODE
	code(STAT_LIST)
	emptycode()

'type' STAT
	comp_stat(COMP_STAT) -- compound statement
	expr_stat(EXPR_STAT) -- expression statement
	sel_stat(SEL_STAT)   -- selection statement
	iter_stat(ITER_STAT) -- iteration statement

'type' COMP_STAT
	empty_stat()  -- "{}"
	stat_list(STAT_LIST)	-- "{ statement list } "

'type' EXPR
	unary_assign_expr(EXPR, EXPR) -- "unary_expr = epxr"

--'type' LOGIC_OR_EXPR
	logic_or_and_expr(EXPR,EXPR) -- "or_expr || and_expr"

--'type' LOGIC_AND_EXPR
	and_equal_expr(EXPR, EXPR) -- "logic_and_expr && equal_expr"

--'type' EQUAL_EXPR
	equal_relation_expr(EXPR, EXPR) -- "equal == relat"
	nequal_relation_expr(EXPR, EXPR) -- "equal != relat"

--'type' RELATION_EXPR
	relat_lt_additive(EXPR, EXPR) -- "rel < add"
	relat_gt_additive(EXPR, EXPR) -- "rel > add"
	relat_le_additive(EXPR, EXPR) -- "rel <= add"
	relat_ge_additive(EXPR, EXPR) -- "rel >= add"

--'type' ADDITIVE_EXPR
	add_plus_multiplic_expr(EXPR, EXPR) --"add + mult"
	add_minus_multiplic_expr(EXPR, EXPR) --"add - mult"

--'type' MULTIPLIC_EXPR
	multiplic_unary_expr(EXPR, EXPR) --"mult * unary"

--'type' UNARY_EXPR
	plusplus_unary_expr(EXPR) --"++ unary"
	minusminus_unary_expr(EXPR) --"-- unary"
	unary_oper_unary_expr(UNARY_OPERATOR, EXPR)

--'type' POSTFIX_EXPR
	postfix_expr_expr(EXPR, EXPR) -- "post [ expr ]"
	postfix_expr_plusplus(EXPR) -- "post ++"
	postfix_expr_minusminus(EXPR) -- "post --"

--'type' PRIMARY_EXPR
	ident(IDENT)
	number(INT)
	primary_expr_expr(EXPR) -- "( expr )"


'type' UNARY_OPERATOR
	unary_plus() --"+"
	unary_minus() --"-"
	unary_neg() --"!"


'type' EXPR_STAT
	empty_expr() -- ";"
	expr(EXPR)		-- "expr ;"

'type' SEL_STAT
	if(EXPR, STAT) -- "if ( expr ) stat"
	ifelse(EXPR, STAT, STAT) -- "if ( expr ) stat else stat"

'type' ITER_STAT
	while(EXPR, STAT) -- "while ( expr ) stat"
	

'type' STAT_LIST
	statlist(STAT, STAT_LIST)
	stat(STAT)

'type' RET
	return(EXPR)	


'root' Prog(->X) print(X)

'nonterm'  Prog(-> PROG)
	'rule' Prog(-> prog(DeclList, C,  Ret )):
		Declarations(-> DeclList) Code(->C) Return(->Ret)



----------- czesc deklaracji --------

'nonterm' Declarations(-> DECLLIST)
	'rule' Declarations(-> decllist(D, Ds)):
		Declaration(-> D) ";" Declarations(->Ds)
	'rule' Declarations(-> decllist(D, nil)):
		Declaration(-> D) ";"

'nonterm' Declaration(-> DECL)
	'rule' Declaration(-> decl(T, Id, 0)):
		Type(-> T) Ident(-> Id)
	'rule' Declaration(-> decl(T, Id, Val)):
		Type(-> T) Ident(-> Id) "=" Number(-> Val)
	'rule' Declaration(-> decl(array(T, Count), Id, 0)):
		Type(-> T) Ident(-> Id) "[" Number(-> Count) "]"

'nonterm' Type(-> TYPE)
	'rule' Type(-> int): "int"
	'rule' Type(-> uint): "unsigned int"

------- koniec czesci deklaracji ----



----------- czesc kodu --------------

'nonterm' Code(-> CODE)
	'rule' Code(-> code(Ss)):
		StatList(-> Ss)
	'rule' Code(-> emptycode()):

'nonterm' StatList(-> STAT_LIST)
	'rule' StatList(-> statlist(S, Ss)):
		Stat(-> S) StatList(-> Ss)
	'rule' StatList(-> stat(S)):
		Stat(-> S)

'nonterm' Stat(-> STAT)
	'rule' Stat(-> comp_stat(S)):
		CompStat(-> S)
	'rule' Stat(-> expr_stat(S)):
		ExprStat(-> S)
	'rule' Stat(-> sel_stat(X)):
		SelStat(-> X)
	'rule' Stat(-> iter_stat(X)):
		IterStat(-> X)

'nonterm' SelStat(-> SEL_STAT)
	'rule' SelStat(-> if(X,Y)):
		"if" "(" Expr(-> X) ")" Stat(-> Y)
	'rule' SelStat(-> ifelse(X,Y,Z)):
		"if" "(" Expr(->X) ")" Stat(-> Y) "else" Stat(->Z)

'nonterm' IterStat(-> ITER_STAT)
	'rule' IterStat(-> while(X,Y)):
		"while" "(" Expr(-> X)")" Stat(->Y)


'nonterm' ExprStat(-> EXPR_STAT)
	'rule' ExprStat(-> empty_expr()):
		";"
	'rule' ExprStat(-> expr(E)):
		Expr(-> E) ";"

'nonterm' Expr(-> EXPR)
	'rule' Expr(-> E):
		LogicOrExpr(-> E)
	'rule' Expr(-> unary_assign_expr(UE, E)):
		UnaryExpr(-> UE) "=" Expr(-> E)

'nonterm' UnaryExpr(-> EXPR)
	'rule' UnaryExpr(-> X): 
		PostfixExpr(-> X)
	'rule' UnaryExpr(-> plusplus_unary_expr(X)):
		"++" UnaryExpr(-> X)
	'rule' UnaryExpr(-> minusminus_unary_expr(X)):
		"--" UnaryExpr(-> X)
	'rule' UnaryExpr(-> unary_oper_unary_expr(unary_minus(),X)):
		"-" UnaryExpr(->X)
	'rule' UnaryExpr(-> unary_oper_unary_expr(unary_plus(),X)):
		"+" UnaryExpr(->X)
	'rule' UnaryExpr(-> unary_oper_unary_expr(unary_neg(),X)):
		"!" UnaryExpr(->X)


'nonterm' PostfixExpr(-> EXPR)
	'rule' PostfixExpr(-> X):
		PrimaryExpr(-> X)
	'rule' PostfixExpr(-> postfix_expr_expr(X,Y)):
		PostfixExpr(-> X)"[" Expr(-> Y) "]"
	'rule' PostfixExpr(-> postfix_expr_plusplus(X)):
		PostfixExpr(-> X) "++"
	'rule' PostfixExpr(-> postfix_expr_minusminus(X)):
		PostfixExpr(-> X) "--"

'nonterm' PrimaryExpr(-> EXPR)
	'rule' PrimaryExpr(-> ident(X)):
		Ident(-> X)
	'rule' PrimaryExpr(-> number(X)):
		Number(-> X)
	'rule' PrimaryExpr(-> primary_expr_expr( X)):
		"("Expr(-> X)")"

'nonterm' LogicOrExpr(->EXPR)
	'rule' LogicOrExpr(-> LAE):
		LogicAndExpr(-> LAE)
	'rule' LogicOrExpr(-> logic_or_and_expr(LOE, LAE)):
		LogicOrExpr(-> LOE) "||" LogicAndExpr(-> LAE)

'nonterm' LogicAndExpr(-> EXPR)
	'rule' LogicAndExpr(->X):
		EqualExpr(-> X)
	'rule' LogicAndExpr(-> and_equal_expr(X,Y))
	 	LogicAndExpr(-> X) "&&" EqualExpr(-> Y)

'nonterm' EqualExpr(-> EXPR)
	'rule' EqualExpr(-> X):
		RelationExpr(-> X)
	'rule' EqualExpr(-> equal_relation_expr(X,Y)):
		EqualExpr(-> X) "==" RelationExpr(-> Y)
	'rule' EqualExpr(-> nequal_relation_expr(X,Y)):
		EqualExpr(-> X) "!=" RelationExpr(-> Y)

'nonterm' RelationExpr(-> EXPR)
	'rule' RelationExpr(-> X):
		AdditiveExpr(-> X)
	'rule' RelationExpr(-> relat_lt_additive(X,Y)):
		RelationExpr(-> X) "<" AdditiveExpr(-> Y)
	'rule' RelationExpr(-> relat_gt_additive(X,Y)):
		RelationExpr(-> X) ">" AdditiveExpr(-> Y)
	'rule' RelationExpr(-> relat_le_additive(X,Y)):
		RelationExpr(-> X) "<=" AdditiveExpr(-> Y)
	'rule' RelationExpr(-> relat_ge_additive(X,Y)):
		RelationExpr(-> X) ">=" AdditiveExpr(-> Y)

'nonterm' AdditiveExpr(-> EXPR)
	'rule' AdditiveExpr(-> X):
		MultiplicExpr(-> X)
	'rule' AdditiveExpr(-> add_plus_multiplic_expr(X,Y)):
		AdditiveExpr(-> X) "+" MultiplicExpr(-> Y)
	'rule' AdditiveExpr(-> add_minus_multiplic_expr(X,Y)):
		AdditiveExpr(-> X) "-" MultiplicExpr(-> Y)

'nonterm' MultiplicExpr(-> EXPR)
	'rule' MultiplicExpr(-> X):
		UnaryExpr(-> X)
	'rule' MultiplicExpr(-> multiplic_unary_expr(X,Y)):
		MultiplicExpr(-> X) "*" UnaryExpr(-> Y)


'nonterm' CompStat(-> COMP_STAT)
	'rule' CompStat(-> empty_stat()):
		"{" "}"
	'rule' CompStat(-> stat_list(S)):
		"{" StatList(-> S) "}"


-------- koniec czesci kodu ----------



----- czesc zwracania wartosci -------

'nonterm' Return(-> RET)
	'rule' Return(-> return(X)): 
		"return" Expr(-> X) ";"
		



-- koniec czesci zwracania wartosci --


/*'nonterm' declarations(-> Decs)
	'rule' declarations(-> X): declaration (-> X)
	'rule' declarations(->dec_append (X,Y)): declarations(-> X) declaration(-> Y)

'nonterm' declaration
	'rule' declaration: int_dec ";"
	'rule' declaration: int_dec int_init ";"

	'rule' declaration: int_tab_dec ";"

'nonterm' int_dec
	'rule' int_dec : "int" var_name
	'rule' int_dec : "unsigned" var_name
	'rule' int_dec : "unsigned" "int" var_name

'nonterm' int_init
	'rule' int_init : "=" basic_expression

'nonterm' int_tab_dec 
	'rule' int_tab_dec : int_dec "[" tab_length "]"



'nonterm' declaration
	'rule
	'rule' declaration : dec_header  ";"
	'rule' declaration : dec_header dec_init ";"

'nonterm' dec_init
	'rule' dec_init : "=" expression ";" 

'nonterm' dec_header
	'rule' dec_header: dectype_name var_name;
	'rule' dec_header: dectype_name var_name "[" table_length "]"

'nonterm' code
	'rule' code: instruction;
	'rule' code: code instruction;

'nonterm'  ret
	'rule' ret(-> return(X)) : "return" bl unint_c(-> X) bl ";"
	'rule' ret(-> return(X)) : "return" bl unint_l(-> X) bl ";"*/


'token' Ident(-> IDENT)
'token' Number(-> INT)
