-- 21-07-AxingCode.hs
--
-- 21.7 Axing tedious code, page 848
--

module AxingCode where

-- Thanks for the great example, Alex
data Query = Query
data SomeObj = SomeObj
data IoOnlyObj = IoOnlyObj
data Err = Err

-- There's a decoder function that makes some object from String
decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

-- There's a query, that runs against the DB and returns array of strings
fetchFn :: Query -> IO [String]
fetchFn = undefined

-- an additional "context initializer", that also has IO
makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

-- before
pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query
  case sequence (map decodeFn a) of
    (Left err) -> return $ Left $ err
    (Right res) -> do
      a <- makeIoOnlyObj res
      return $ Right a

{-
      The objective was to clean up this code.
      A few things made them suspicious:

      1. The use of sequence and map.
      2. Manually casing on the result of sequence and map
      3. Binding monadically over the Either only to perform
         another monadic (IO) action inside of that.
      
      We pared the pipeline function down to this:
-}

pipelineFn' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn' query = do
  a <- fetchFn query
  traverse makeIoOnlyObj (mapM decodeFn a)

--    Thanks to merijn on the IRC channel for helping with this.
--    We can make it pointfree if we want to:

pipelineFn'' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn'' = (traverse makeIoOnlyObj . mapM decodeFn =<<) . fetchFn

-- And since mapM is just traverse with a slightly different type:

pipelineFn''' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn''' = (traverse makeIoOnlyObj . traverse decodeFn =<<) . fetchFn

-- I tried to do the axing in smaller steps
-- in order to really understand the code and the refactoring.

-- My own refactoring, no 1
-- this is just a small simplification compared to the above code.
-- I removed unnecessary parentheses and $ in Left $ err
myPipelineFn1 :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
myPipelineFn1 query = do
  a <- fetchFn query          -- why call it a? Is there no better name?
  case traverse decodeFn a of
    Left err -> return $ Left err
    Right res -> do
      a <- makeIoOnlyObj res  -- why call it a again?
      return $ Right a

-- My own refactoring, no 2
-- renamed variables renamed and types added to make the code more understandable.
-- This is more code but for me it was a helpful intermediate step.
myPipelineFn2 :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
myPipelineFn2 query = do
  ioStrings <- fetchFn query :: IO [String]
  let errOrObjects = traverse decodeFn ioStrings :: Either Err [SomeObj]
  case errOrObjects of
    Left err -> return $ Left err
    Right res -> do
      let objects = res :: [SomeObj]
      ioObjectTuples <- makeIoOnlyObj objects :: IO [(SomeObj, IoOnlyObj)]
      return $ Right ioObjectTuples

-- My own refactoring, no 3
-- in the 3rd step I replaced the inner do construct
myPipelineFn3 :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
myPipelineFn3 query = do
  ioStrings <- fetchFn query :: IO [String]
  let errOrObjects = traverse decodeFn ioStrings :: Either Err [SomeObj]
  case errOrObjects of
    Left err -> return $ Left err
    Right res -> traverse makeIoOnlyObj $ Right res     -- can use mapM instead of traverse

-- My own refactoring, no 4
-- in the 4th step it is easy to remove casing
myPipelineFn4 :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
myPipelineFn4 query = do
  ioStrings <- fetchFn query :: IO [String]
  let errOrObjects = traverse decodeFn ioStrings :: Either Err [SomeObj]
  traverse makeIoOnlyObj errOrObjects     -- can use mapM instead of traverse

-- My own refactoring, no 5
-- in the 5th step I removed the variable errOrObjects thus joining the two invocations of traverse
myPipelineFn5 :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
myPipelineFn5 query = do
  ioStrings <- fetchFn query :: IO [String]
  traverse makeIoOnlyObj $ (traverse decodeFn ioStrings :: Either Err [SomeObj])

-- My own refactoring, no 6
-- In step 6 I removed the the remaining types and introduce composition (instead of $)
-- This gave me nearly the same solution as Chris got in his 1st step
myPipelineFn6 :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
myPipelineFn6 query = do
  ioStrings <- fetchFn query
  (traverse makeIoOnlyObj . traverse decodeFn) ioStrings

-- My own refactoring, no 7
-- In step 7 I introduced the bind operator, whaqt allowed me to remove 'ioStrings' and the outer 'do' construct.
myPipelineFn7 :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
myPipelineFn7 query = fetchFn query >>=
  (traverse makeIoOnlyObj . traverse decodeFn)

-- My own refactoring, no 8
-- bind (>>=) replaced by flipped bind (=<<)
myPipelineFn8 :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
myPipelineFn8 query =
  (traverse makeIoOnlyObj . traverse decodeFn) =<< fetchFn query

-- My own refactoring, no 9
-- Composed fetchFn to the traverse invocations
myPipelineFn9 :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
myPipelineFn9 query =
  ((traverse makeIoOnlyObj . traverse decodeFn =<<) . fetchFn) query

-- My own refactoring, no 10
-- Now with 'query' at the end we can make it pointless ending up with the same solution as Chris
myPipelineFn10 :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
myPipelineFn10 =
  (traverse makeIoOnlyObj . traverse decodeFn =<<) . fetchFn
