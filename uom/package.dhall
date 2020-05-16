    let defs = ./../defaults.dhall

in    defs
    ⫽ { name =
          "thoralf-plugin"
      , synopsis =
          "An extensible GHC typechecker plugin based on Z3"
      , description =
          ''
          Thoralf is a small yet extensible typechecker plugin based on the
          Z3 SMT solver. It is designed to be easy to extend with new
          theories.
          ''
      , category =
          "Development"
      , github =
          "bgamari/the-thoralf-plugin"
      , ghc-options =
          [ "-Wall", "-fno-warn-partial-type-signatures" ]
      , dependencies =
            defs.dependencies
          # [ "containers", "ghc", "ghc-prim", "hashable", "mtl", "simple-smt" ]
      , library =
          { source-dirs =
              "src"
          , exposed-modules =
              [ "ThoralfPlugin.Plugin"
              , "ThoralfPlugin.ThoralfPlugin"
              , "ThoralfPlugin.Convert"
              , "ThoralfPlugin.Variables"
              , "ThoralfPlugin.Singletons.Symbol"
              , "ThoralfPlugin.Singletons.Nat"
              , "ThoralfPlugin.Theory.DisEq"
              , "ThoralfPlugin.Theory.FiniteMap"
              , "ThoralfPlugin.Theory.UoM"
              , "ThoralfPlugin.Theory.Bool"
              ]
          }
      , tests =
          { units =
              { dependencies =
                  [ "base"
                  , "QuickCheck"
                  , "singletons"
                  , "thoralf-plugin"
                  , "tasty"
                  , "tasty-hunit"
                  , "tasty-quickcheck"
                  , "tasty-th"
                  ]
              , ghc-options =
                  [ "-Wall", "-fplugin ThoralfPlugin.Plugin" ]
              , main =
                  "Main.hs"
              , source-dirs =
                  "test-suite-units"
              }
          }
      }
