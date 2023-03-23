module Housekeeper.Main where

import Housekeeper.App.Env (mkEnv)
import Housekeeper.App.Handler (app)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

main :: IO ()
main = do
  env <- mkEnv
  let runApp = run 8080 $ logStdoutDev $ app env
  runApp
