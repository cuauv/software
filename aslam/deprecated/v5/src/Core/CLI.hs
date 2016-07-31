import           Options.Applicative

import qualified Core.Daemon         as Daemon

data Options = Options deriving (Show)

parser = info (helper <*> options) (fullDesc <> progDesc "Adaptive SLAM" <> header "Adaptive SLAM")

options ∷ Parser Options
options = undefined

go ∷ Options → IO ()
go opts = undefined

main ∷ IO ()
main = do
  Daemon.go
