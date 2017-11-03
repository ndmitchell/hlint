# The `codeclimate-hlint` Engine

## Building

To build the `codeclimate/codeclimate-hlint` engine from source:

```console
./cc/build
```

The built image will now be used by the `codeclimate` CLI.

## Usage

Add the following to `.codeclimate.yml`:

```yaml
engines:
  hlint:
    enabled: true
```

## Configuration

To pass additional flags to `hlint`:

```yaml
engines:
  hlint:
    enabled: true
    config:
      flags:
        - --hint
        - my-hints.yml
```

See `hlint lint --help`.

To exclude files from analysis, use the `exclude_paths` options

```yaml
# Exclude from all engines
engines:
  # ...

exclude_paths:
  - problematic-file.hs

# Exclude from just hlint
engines:
  hlint:
    # ...
    exclude_paths:
      - problematic-file.hs
```

See https://docs.codeclimate.com/docs/excluding-files-and-folders.
