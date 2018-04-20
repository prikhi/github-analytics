#!/usr/bin/env stack
{-  stack
    --resolver lts-11.5
    script
    --package typed-process
-}

import Control.Monad (void)
import System.Environment (getArgs, unsetEnv)
import System.Process.Typed

main = do
    args <- getArgs
    unsetEnv "GHC_PACKAGE_PATH"
    runProcess $ shell
        "find client server common -iname .hdevtools.sock -exec rm -f {} \\;"
    case args of
        -- Production Build
        ["nix-build"] ->
            run "nix-build" ["-A", "all"]
        -- Incremental Dev Builds
        ["ghc-build"] ->
            run "nix-shell"
                [ "-A", "shells.ghc"
                , "--run", "cabal new-build all"
                ]
        ["ghcjs-build"] ->
            run "nix-shell"
                [ "-A", "shells.ghcjs"
                , "--run"
                , "cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs new-build all"
                ]
        -- REPLs
        ["client-repl"] ->
            run "nix-shell"
                [ "-A", "shells.ghc"
                , "--run", "cabal new-repl client"
                ]
        -- Cleanup
        ["clean"] ->
            run "rm" [ "-rf" , "result*", "dist*" ]
        _ ->
            mapM_ putStrLn
                [ "./manage.hs: Available Commands:\n"
                , "nix-build"
                , "ghc-build"
                , "ghcjs-build"
                , "client-repl"
                , "clean"
                , ""
                ]
    where
        run e a =
            void $ runProcess (proc e a)
