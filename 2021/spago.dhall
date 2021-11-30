{ name = "my-project"
, dependencies =
  [ "console", "effect", "lists", "prelude", "psci-support", "test-unit" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
