Futamura projections
====================

> {-# OPTIONS_GHC -Wno-missing-export-lists #-}
> {-# LANGUAGE PartialTypeSignatures #-}
>
> module Futamura where

Definitions
===========

Source of some program
----------------------

> newtype S a = S a

Executable program
------------------

> newtype P a = P { run :: a }

Sample program
--------------

> prog :: P (a -> r)
> prog = undefined
> 
> prog' :: S (a -> r)
> prog' = undefined

> -- >>> :t run prog

Interpreter
-----------

> interpreter :: P (S (a -> r) -> a -> r)
> interpreter = undefined
> 
> interpreter' :: S (S (a -> r) -> a -> r)
> interpreter' = undefined

> -- >>> :t run interpreter prog'

Compiler
--------

> compiler :: P (S (a -> r) -> P (a -> r))
> compiler = undefined
> 
> compiler' :: S (S (a -> r) -> P (a -> r))
> compiler' = undefined

> -- >>> :t run compiler prog'

> -- >>> :t run (run compiler prog')

Partial evaluator
=================

> mix :: P (S (a -> b -> r) -> a -> P (b -> r))
> mix = undefined
> 
> mix' :: S (S (a -> b -> r) -> a -> P (b -> r))
> mix' = undefined
 
What can we pass as argument to `mix`?
----------

> -- >>> :t interpreter'

> -- >>> :t mix'

Projection #1
=============
 
> -- >>> :t run mix interpreter'

What does it look like?
-----------------------

> -- >>> :t run compiler

> compiler2 :: P (S (a -> r) -> P (a -> r))
> compiler2 = P (run mix interpreter')

> -- >>> :t run compiler prog'

Projection #2
=============

> -- >>> :t run mix mix' interpreter'

Helpers
-------

> type Interpreter a r = S (a -> r) ->    a -> r
> type Compiler    a r = S (a -> r) -> P (a -> r)

> -- >>> :t interpreter :: P (Interpreter a r)
>
> -- >>> :t interpreter' :: S (Interpreter a r)
>
> -- >>> :t compiler :: P (Compiler a r)
>
> -- >>> :t compiler' :: S (Compiler a r)

Generator of compilers
----------------------

> genCompiler :: P (S (Interpreter a r) -> P (Compiler a r))
> genCompiler = P (run mix mix')

Projection #3
=============

> -- >>> :t run mix mix' mix'

What is `run mix mix'`?
-----------------------

> gen :: P (S (a -> b -> r) -> P (a -> P (b -> r)))
> gen = P (run mix mix')

Let's try `interpreter'`
------------------------

> -- >>> :t run gen interpreter'

> genCompiler2 :: P (S (Interpreter a r) -> P (Compiler a r))
> genCompiler2 = run gen mix'

Let's try `mix'`
----------------

> -- >>> :t run gen mix'

Helpers
-------

> type Mix a b r = S (a -> b -> r) ->    a -> P (b -> r)
> type Gen a b r = S (a -> b -> r) -> P (a -> P (b -> r))

Generator of generators
-----------------------

> genGen :: P (S (Mix a b r) -> P (Gen a b r))
> genGen = run gen mix'

Projection #X
=============

Can we go further?
------------------

> gen2 :: P (Gen a b r)
> gen2 = run genGen mix'

> genGen2 :: P (S (Mix a b r) -> P (Gen a b r))
> genGen2 = run gen2 mix'

Can we go further?
------------------

> gen3 :: P (Gen a b r)
> gen3 = run genGen2 mix'

What about evaluation?
----------------------

> -- gen3 = run genGen2 mix'
> --      = run (run gen2 mix') mix'
> --      = run (run (run genGen mix') mix') mix'
> --      = run (run (run (run gen mix') mix') mix') mix'
> --      = run (run (run (run (run mix mix') mix') mix') mix') mix'

...
---

All `genX` are the same as `generator` from Projection #3
