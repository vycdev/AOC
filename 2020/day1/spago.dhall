
{ name = "my-project"
, dependencies =
  [ "arrays", "console", "effect", "enums", "node-fs-aff", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
