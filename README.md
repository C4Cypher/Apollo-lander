Apollo-lander
=============

Mercury is a purely declarative logic language. It is related to both Prolog and
Haskell.[1] It features a strong, static, polymorphic type system, as well as a
strong mode and determinism system.

Apollo, the ancient Greek name for the planet Mercury, when observed just before
dawn as a morning star

The Apollo spacecraft was composed of three parts designed to accomplish the 
American Apollo program's goal of landing astronauts on the Moon by the end of 
the 1960s and returning them safely to Earth.

Lua - A lightweight programming language with dynamic typing.
From the Portuguese lua (“moon”). The inventors of the language were Brazilian.

%%----------------------------------------------------------------------------%%

This library is intended to provide a simple, easy to use interface between the
Mercury functional/logic compiled programming language and the Lua Virtual 
Machine in such a manner that it preserves the advantages of both environments.

Mercury and Lua are vastly different programming languages.  

Mercury is derived from Prolog, yet it is strongly-typed, functionally pure 
language that is compiled to C and then to native binary code. Any imperitive 
changes to the program state must be threaded through a state variable to 
preserve purity, or otherwise explicitly marked so that it can be handled safely
by the compiler.  The logical and functional purity of Mercury allow it's
compiler to perform exstensive optimization, and to catch potential problems at
compile-time that other languages would let through.

Lua is an imperitave dynamically-typed scripting language that is compiled to 
mid-level bytecode in realtime so that the interpreter (referred to as the Lua 
Virtual Machine) can execute Lua scripts on the fly.  The only native data 
structure is an efficiently implented hash-table.  Functions are treated as 
first-class variables, and Lua has an exstensive C-API that easily handles 
foreign data making it easy to bind to foreign code. This, coupled with the fact
that it is small and very fast makes it a superb choice for an embedded 
scripting language, or as glue-code between different environments.

Lua may not be the best choice for embedding in a Mercury program, it's
imperitive nature and the maleability of it's code would sacrifice much of the
advantage offered by Mercury's purity.  However, in order to interact with the
real world, Mercury provides language constructs for interacting with mutable
state that can be clunky and akward.

Apollo-lander is intended to allow Lua code to make calls on Mercury predicates
in a controlled manner, allowing Mercury to query the state of the Lua VM
without modifying it, and then returning values back to Lua in a manner that
capitalizes on Lua's capability to use varadic function calls and return values.

Lua functions may be called from Mercury, but only if the function in question
originated in a controlled manner from the Apollo interface.  Lua functions
passed as arguments to Mercury should be opaque, either passed back in return
values.

Planned features:

-Allow outside coders to define additional types that can be easily
	passed to Lua as return values, using either userdata,
	metatables and/or Lua's existing primitive types, without
	compromising type-safety or having to resort to the Lua API.

-Allow Lua to make calls on non-deterministic predicates via iterators
	and Mercury multithreading.

-Initial support is for Lua 5.1, eventual support for Lua 5.2

-Support for Lua coroutines
