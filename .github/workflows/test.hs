import System.Environment
import System.Process

main =
  do
    ghc <- getEnv "ghc"

    let constraints = case ghc of
            "8.2.2"  -> [ "--constraint=bytestring == 0.10.6.0"
                        , "--constraint=path == 0.5.0"
                        , "--constraint=safe-excecptions == 0.1.5.0"
                        , "--constraint=text == 1.2.2.0"
                        ]
            "8.4.3"  -> []
            "8.6.1"  -> []
            "8.8.1"  -> []
            "8.10.1" -> [ "--constraint=bytestring == 0.10.12.1"
                        , "--constraint=path == 0.7.0"
                        , "--constraint=safe-exceptions == 0.1.7.1"
                        , "--constraint=text == 1.2.4.1"
                        ]

    callProcess "cabal" ("build" : "all" : constraints)
