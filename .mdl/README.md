# Mudyla Build Configuration

This directory contains the [mudyla](https://github.com/7mind/mudyla) build configuration for izumi-reflect.

## Structure

- `defs/actions.md` - Main build action definitions
- `resources/` - Supporting files used by build actions

## Available Actions

Run actions using `mdl :action-name` (from within nix shell) or `nix develop --command mdl :action-name`:

- `:gen` - Generate build files using sbtgen
- `:test` - Run tests and binary compatibility checks
- `:build` - Full build pipeline (gen + test)
- `:publish-scala` - Publish Scala artifacts to Sonatype
- `:publish-ziodocs` - Publish documentation to NPM

## Environment Variables

The following environment variables are used by build actions:

- `SCALA_VERSION` - Scala version to build for
- `JAVA_VERSION` - Java version to use
- `SONATYPE_USERNAME` - Sonatype credentials (for publishing)
- `SONATYPE_PASSWORD` - Sonatype credentials (for publishing)
- `NODE_AUTH_TOKEN` - NPM token (for docs publishing)
- `CI_BRANCH_TAG` - Current git branch/tag (for conditional publishing)

## GitHub Actions Integration

Use the `--github-actions` flag when running in CI:

```bash
nix develop --command mdl --github-actions :gen :test
```

This enables proper output formatting and progress reporting for GitHub Actions.
