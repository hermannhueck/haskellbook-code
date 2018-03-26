-- 02-02-SayHello.hs

-- 2.2 Interacting with Haskell code

sayHello :: String -> IO ()
sayHello x =
  putStrLn ("Hello, " ++ x ++ "!")
