{ name = "my-project"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "integers"
  , "lists"
  , "node-buffer"
  , "node-fs"
  , "prelude"
  , "psci-support"
  , "stringutils"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
