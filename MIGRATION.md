# Migration from Mobala to Mudyla

This document describes the migration from mobala to mudyla build system.

## What Changed

### Build Tool
- **Before**: mobala (`.mobala/` directory with bash scripts)
- **After**: mudyla (`.mdl/` directory with markdown action definitions)

### File Structure

**Removed**:
- `.mobala/` directory (all mobala configuration)

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

Run actions using `./run :action-name`:

| Action | Description |
|--------|-------------|
| `:gen` | Generate build files using sbtgen |
| `:test` | Run tests and binary compatibility checks |
| `:build` | Full build pipeline (gen + test) |
| `:publish-scala` | Publish Scala artifacts to Sonatype |
| `:publish-ziodocs` | Publish documentation to NPM |

## Command Equivalents

| Old (mobala) | New (mudyla) |
|--------------|--------------|
| `./run --nix :gen :test` | `./run --github-actions :gen :test` |
| `./run --nix :gen :publish-scala` | `./run --github-actions :gen :publish-scala` |
| `./run --nix :gen :publish-ziodocs` | `./run --github-actions :gen :publish-ziodocs` |

## GitHub Actions Changes

The workflow now uses:
- `--github-actions` flag instead of `--nix` flag
- Sets `CI_BRANCH_TAG` environment variable for conditional publishing

## Local Development

1. Ensure you're using the nix flake environment (via direnv or `nix develop`)
2. Run `./run --list-actions` to see all available actions
3. Run `./run :action-name` to execute specific actions
4. Run `./run --dry-run :action-name` to see what would be executed

## Benefits of Mudyla

- **Declarative**: Actions defined in markdown with clear dependencies
- **Type-safe**: Return values are typed (Bool, Int, String, File, Directory)
- **Parallel execution**: Independent actions run concurrently
- **Better debugging**: Use `--keep-run-dir` to inspect execution artifacts
- **Cleaner syntax**: No more nested shell functions and flow scripts
