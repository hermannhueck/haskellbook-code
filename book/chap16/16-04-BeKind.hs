-- 16-04-BeKind.hs
--
-- 16.4 Let’s talk about f, baby, page 631
-- Exercises: Be Kind, page 634
--
module BeKind where
{-
    Given a type signature, determine the kinds of each type variable:

    1. What’s the kind of a?
    a -> a
    Answer: a :: *

    2. What are the kinds of b and T ? (The T is capitalized on purpose!)
    a -> b a -> T (b a)
    Answer: b :: * -> *, T :: * -> *
    
    3. What’s the kind of c?
    c a b -> c b a
    Answer: c :: * -> * -> *
    
-}
