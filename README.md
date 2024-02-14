Apollo-lander
=============

Mercury is a purely declarative logic programming language. It is related to 
both Prolog and Haskell.[1] It features a strong, static, polymorphic 
type system, as well as a strong mode and determinism system.

Apollo, the ancient Greek name for the planet Mercury, when observed just before
dawn as a morning star.

The Apollo spacecraft was composed of three parts designed to accomplish the 
American Apollo program's goal of landing astronauts on the Moon by the end of 
the 1960s and returning them safely to Earth.

Lua - A lightweight programming language with dynamic typing.
From the Portuguese lua (“moon”). The inventors of the language were Brazilian.

=============

If you are reading this, you should no doubt be aware that Mercury is a
strict language.  This is not merely about having a strict static type
or mode system. Mercury was designed with a very specific set of semantics
in mind.  

Mercury is intended to be a purely declarative language, without 
side-effects, and the Melbourne Mercury Compiler was designed specifically 
to optimize code written with those strict, declarative semantics in mind.  
While the language does allow you to deviate from those semantics, there 
is a price to be paid in efficiency and ease-of use.  To do so is generally
considered to be a 'bad idea'.

In contrast, the Lua programming language is known for flexibility,
extendability and for ease of code customization on the fly.  Using syntax
influenced by Ada and Eiffel, and borrowing a few functional programming
features from Lisp, Lua is simple, lightweight and easy to understand.

One of Lua's most notable features is it's extendability.  There are a
multitude of ways to extend Lua's syntax and modify it's behavior,
allowing one to define and use Lua with their own semantics, tailored
to the programmer's needs.
This, coupled with a similarly simple stack based C API, makes Lua a very
popular choice for embedding in or binding to foreign languages and
environments.

For a Mercury programmer, to attempt to embed Lua in Mercury might seem
counter-intuitive (if not outright insane).  Trying to work around
Lua's dynamic, impure and unpredictable nature would run counter to
Mercury's strengths and would be a nightmare to implement.

At the same time, the way Lua's C API is implemented, if Lua were to load
and invoke exported Mercury predicates and functions from a dynamic library,
Mercury would be able to interact with the calling Lua state and it's
instantiated variables as immutable values and data structures in a manner
that preserves Mercury's pure declarative semantics.  Furthermore, 
synergies with Lua's C API and language features would allow Mercury code 
to define methods for Lua to interact with Mercury values passed to Lua as 
if they were native Lua objects.  

This would allow Lua programmers the ability to take advantage of the speed 
and stability of compiled Mercury modules. It would also make it easier
to embed Mercury in foreign code, and bind Mercury to foreign libraries and
languages.
 
For these reasons, this module is intended to provide a simple way to pass 
both data and procedure between Mercury 
and Lua in an efficient and seamless manner.

=============

apollo.m
--------

Top level language binding written to facilitate Mercury style IO passing to manipulate Lua 
variables, values and the Lua state

apollo.api.m
----

Primitive (semipure and impure) bindings directly to the Lua C api.
It is advised that this module not be directly used unless one understand the workings
of the Lua Stack and the Lua C api.


apollo.state.m
--------------

Facilitates types and methods to facilitate handling the Lua state as if it were a 
Mercury style IO variable, including the possibility for backtracking certain
changes to the Lua state

trail.m
-------

Methods to further facilitate trailing and backtracking via the Lua state


	
	
		






