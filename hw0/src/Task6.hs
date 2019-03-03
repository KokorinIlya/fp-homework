module Task6
  ( firstWhnf
  , secondWhnf
  ) where
  
firstWhnf :: (Either String b, Either String c)
firstWhnf = (Left ("harold" ++ " hide " ++ "the " ++ "pain"), Left ("harold" ++ " hide " ++ "the " ++ "pain"))

secondWhnf :: Bool
secondWhnf = False
