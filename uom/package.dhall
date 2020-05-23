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
          # [ "containers"
            , "deepseq"
            , "ghc"
            , "ghc-prim"
            , "hashable"
            , "mtl"
            , "simple-smt"
            , "template-haskell"
            , "units-parser"
            ]
      , library =
          { source-dirs =
              "src"
          , exposed-modules =
              [ "ThoralfPlugin.Plugin"
              , "ThoralfPlugin.ThoralfPlugin"
              , "ThoralfPlugin.Convert"
              , "ThoralfPlugin.Variables"
              , "ThoralfPlugin.Singletons.Symbol"
              , "ThoralfPlugin.Theory.DisEq"
              , "ThoralfPlugin.Theory.UoM"
              , "Data.UnitsOfMeasure"
              , "Data.UnitsOfMeasure.Convert"
              , "Data.UnitsOfMeasure.Defs"
              , "Data.UnitsOfMeasure.DefsManual"
              , "Data.UnitsOfMeasure.Internal"
              , "Data.UnitsOfMeasure.Read"
              , "Data.UnitsOfMeasure.Show"
              , "Data.UnitsOfMeasure.Singleton"
              ]
          }
      , tests =
          { thoralf =
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
                  "test-suite-thoralf"
              }
          , units =
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
                  "Tests.hs"
              , source-dirs =
                  "test-suite-units"
              }
          }
      }
