-- 05-08b-DoesItCompile.hs
--
-- 5.8 Chapter Exercises, page 147
-- Does it compile?, page 150
--
module DoesItCompile where

-- For each expression, figure out, which expression, if any, causes
-- the compiler to squak a you and why. Fix it if you can.
--
-- 1.
bigNum = (^) 5 $ 10 -- compiles

-- wahoo = bigNum $ 10 -- error: bigNum is not a function
wahoo = bigNum -- fixed

-- 2.
x = print -- ok

y = print "woohoo!" -- ok

z = x "hello world" -- ok

-- 3.
a = (+) -- ok

b = 5 -- ok

-- c = b 10 -- error: b is not a function
c = b `a` 10 -- fixed

-- d = c 200 -- error: c is not a function
d = c `a` 200 -- fixed

-- 4.
a4 = 12 + b4
b4 = 10000 * c4 -- error: c4 is not in scope
c4 = 3 -- fixed: c4 declared with some value
