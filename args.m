

:- module args.

:- interface args.

:- import_module list.

:- type arg == (func) = T.
:- inst arg == (func) = T is semidet.

:- type args == list(arg).
	

:- func arg(T) = arg.
:- func args(list(_)) = args.

:- implementation.

arg(This) = (func(This) = T :- dynamic_cast(This,T)).

args([]) = [].

args([H | T]) = [arg(H) | args(T)].
