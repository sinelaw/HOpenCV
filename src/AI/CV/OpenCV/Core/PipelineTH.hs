module AI.CV.OpenCV.Core.PipelineTH (mkPiped, mkAux) where
import Control.Monad (when)
import Data.List (foldl')
import Language.Haskell.TH

countArrows :: Type -> Int
countArrows (ForallT _ _ t) = countArrows t
countArrows ArrowT = 1
countArrows (AppT t1 t2) = countArrows t1 + countArrows t2
countArrows _ = 0

getArity :: Exp -> Q Int
getArity (VarE n) = do VarI _ t _ _ <- reify n
                       return $ countArrows t
getArity _ = error "getArity called with non VarE expression"

-- Declare an alias of a variable @foo@ named @fooAux@ that is
-- semantically equivalent to the original.
mkAux :: Q Exp -> Q [Dec]
mkAux q = do VarE n <- q
             VarI _ t _ _ <- reify n
             let n' = mkName $ nameBase n ++ "Aux"
                 spec = InlineSpec True False (Just (True,1))
             return [ SigD n' t
                    , FunD n' [Clause [] (NormalB (VarE n)) []] 
                    , PragmaD $ InlineP n' spec ]

-- Takes variables bound to safe and the unsafe functions. Generates
-- an auxiliary function semantically equivalent to the safe function
-- with the name "fooAux" (for a function named "foo"), and a
-- pipelined function of the form @fooPiped x y = pipeline (unsafe x y)
-- (fooAux x y)@. The safe function's type must be no less general
-- than the unsafe function's.
mkPiped :: Q Exp -> Q [Dec]
mkPiped pair = do TupE [f1,f2] <- pair
                  arity <- getArity f1
                  arity' <- getArity f2
                  when (arity /= arity')
                       (error $ "Arities of functions passed to "++
                                "mkPipe do not match")
                  let VarE f1Name = f1
                      -- VarE f2Name = f2
                  VarI _ t1 _ _ <- reify f1Name
                  --VarI _ t2 _ _ <- reify f2Name
                  let nameAux = mkName $ nameBase f1Name ++ "Aux"
                      namePiped = mkName $ nameBase f1Name ++ "Piped"
                      auxSpec = InlineSpec True False (Just (True, 1))
                      pipeSpec = InlineSpec True True Nothing
                      fAux = VarE nameAux
                  names <- mapM newName (take (arity - 1) (repeat "x"))
                  let app1 = foldl' AppE fAux (map VarE names)
                      app2 = foldl' AppE f2 (map VarE names)
                      pipe = AppE (AppE (VarE (mkName "pipeline")) app2) app1
                  return [ SigD nameAux t1
                         , FunD nameAux [Clause [] (NormalB f1) []]
                         , PragmaD $ InlineP nameAux auxSpec
                         --, SigD namePiped t2
                         , FunD namePiped [Clause (map VarP names) 
                                                  (NormalB pipe) []]
                         , PragmaD $ InlineP namePiped pipeSpec ]
