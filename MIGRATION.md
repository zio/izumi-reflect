# Migration from Mobala to Mudyla

This document describes the migration from mobala to mudyla build system.

## What Changed

### Build Tool
- **Before**: mobala (`.mobala/` directory with bash scripts)
- **After**: mudyla (`.mdl/` directory with markdown action definitions)

### File Structure

**Removed**:
- `.mobala/` directory (all mobala configuration)
- `run` script (now use `nix develop --command mdl` directly)

**Added**:
- `.mdl/defs/actions.md` - Main build action definitions
- `.mdl/resources/` - Supporting files used by build actions
- `.mdl/README.md` - Documentation for the mudyla configuration

**Modified**:
- `flake.nix` - Added mudyla as a flake input and included in devShell
- `flake.lock` - Updated with mudyla dependency
- `run` - Simplified to use `mdl` command from nix environment
- `.github/workflows/build.yml` - Updated to use mudyla commands

## Available Actions

Run actions using `nix develop --command mdl :action-name` (or just `mdl :action-name` if you're already in the nix shell):

### Environment Setup Actions

| Action | Description |
|--------|-------------|
| `:setup-jdk` | Setup JDK path based on JAVA_VERSION |
| `:setup-jvm-options` | Setup JVM options and optimizations |
| `:setup-scala` | Setup Scala version variables |
| `:setup-env` | Complete environment setup (combines all above) |

### Build Actions

| Action | Description |
|--------|-------------|
| `:gen` | Generate build files using sbtgen |
| `:test` | Run tests and binary compatibility checks |
| `:build` | Full build pipeline (gen + test) |
| `:publish-scala` | Publish Scala artifacts to Sonatype |
| `:publish-ziodocs` | Publish documentation to NPM |

**Note**: All build actions automatically depend on the environment setup actions, so you don't need to explicitly run setup actions.

## Command Equivalents

| Old (mobala) | New (mudyla) |
|--------------|--------------|
| `./run --nix :gen :test` | `nix develop --command mdl --github-actions :gen :test` |
| `./run --nix :gen :publish-scala` | `nix develop --command mdl --github-actions :gen :publish-scala` |
| `./run --nix :gen :publish-ziodocs` | `nix develop --command mdl --github-actions :gen :publish-ziodocs` |

## GitHub Actions Changes

The workflow now uses:
- `--github-actions` flag instead of `--nix` flag
- Sets `CI_BRANCH_TAG` environment variable for conditional publishing

## Local Development

### Option 1: Using nix develop directly
```bash
nix develop --command mdl --list-actions  # List all actions
nix develop --command mdl :gen :test      # Run actions
nix develop --command mdl --dry-run :gen  # Dry run
```

### Option 2: Enter nix shell first (recommended)
```bash
nix develop  # or use direnv
mdl --list-actions
mdl :gen :test
mdl --dry-run :gen
```

## Benefits of Mudyla

- **Declarative**: Actions defined in markdown with clear dependencies
- **Type-safe**: Return values are typed (Bool, Int, String, File, Directory)
- **Parallel execution**: Independent actions run concurrently
- **Better debugging**: Use `--keep-run-dir` to inspect execution artifacts
- **Cleaner syntax**: No more nested shell functions and flow scripts
