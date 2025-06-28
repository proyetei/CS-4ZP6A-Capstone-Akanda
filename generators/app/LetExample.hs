import Panbench.Bwd (Bwd)
import Panbench.Bwd qualified as Bwd
import Panbench

-- [TODO: Reed M, 28/06/2025] Experimenting with a DSL a bit.
lets_ :: [(String, Expr)] -> (Bwd Expr -> Expr) -> Expr
lets_ = Bwd.scoped (\(x, e) -> Let [LocDefFun x Nothing [] e]) (\(x, _) -> Var x)

main :: IO ()
main = panbenchMain "LetExample" \n ->
  Module "LetExample" [ImportLib NatMod] $
  [ DefTVar "n" (Just nat) $
    lets_ [ ('x' : show n, if i == 1 then Nat 1 else Var ('x' : show (i - 1))) | i <- [1..n] ] \es ->
      Bwd.last es
  ]
