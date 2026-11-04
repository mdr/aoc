def liftIO (t : ExceptT String Id α) : IO α :=
  match t.run with
  | .ok r => pure r
  | .error e => throw (IO.userError e)

instance : MonadLift (ExceptT String Id) IO where
  monadLift := liftIO
