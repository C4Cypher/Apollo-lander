:- module ast.

:- interface.

:- import_module list.
:- import_module assoc_list.

:- type chunk
	---> 	chunk(statements)
	;	chunk(statements, last_statement.
	
:- type block == chunk.


:-  type statement
	---> 	assign(vars, expressions)
	;	call(expression, vars)
	;	do(block)
	;	while(expression, block)
	;	repeat(block, expression)
	;	if(expression, block)
	;	if(expression, block, block)
	;	for(var, expression, expression, expression, block)
	;	for(vars, expressions, block)
	;	local(var)
	;	local(vars).
	
:- type statements == list(statement).
	
:- type last_statement
	--->	return(vars)
	;	break.

:- type vars == list(var).


:- type var
	--->	name(string)
	;	var(scope, int).
	
:- type expression
	--->	some [T] (value(T))
	;	'...'
	;	true
	;	function(vars, block)
	;	expression ^ expression
	;	table
	;	table(expressions, assoc_list(var, expression))
	;	exp + exp
	;	exp - exp
	;	exp * exp
	;	exp / exp
	;	exp pow exp
	;	exp mod exp	
	;	exp .. exp
	;	exp < exp
	;	exp =< exp
	;	exp == exp
	;	exp ~= exp
	;
	
	
	
	
	
	
