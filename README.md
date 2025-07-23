# rime-ice-modular

A modular version of rime-ice input method built with Haskell's Shake build system, enabling flexible configuration and component selection.

## Build Instructions

### Using Nix (Recommended)

To build rime-ice-modular using Nix:

```bash
nix build '.?submodules=1'
```

This will build the complete rime-ice-modular package with all submodules.

### Using Shake

To build using the Shake build system directly:

```bash
shake
```

This will compile rime-ice-modular using the Haskell Shake configuration defined in `Shakefile.hs`.

## Usage Instructions

1.  **Copy to Rime user directory**: Copy the built files to your Rime user configuration directory.

2.  **Configure default.custom.yaml**: Add the following configuration to your `default.custom.yaml` file:

    - pinyin:

      ```yaml
      patch:
        __include: rime_ice_default_patch:/
        __merge:
          schema_list:
            - schema: rime_ice
      ```

    - double-pinyin:

      ```yaml
      patch:
        __include: rime_ice_double_pinyin_default_patch:/
        __merge:
          schema_list:
            - schema: rime_ice_double_pinyin_flypy
      ```

**Important**: Make sure to embed this content directly in your `default.custom.yaml` file rather than referencing it as a separate file.

## Project Structure

This project uses a modular approach to organize the rime-ice input method:

- `Shakefile.hs`: Main build configuration using Haskell Shake
- `Shakefile/`: Additional Shake modules for components and rendering
- `components.json` & `components.nix`: Component definitions and Nix packaging
- `externals/rime-ice/`: The original rime-ice repository as a submodule
- `flake.nix`: Nix flake configuration for reproducible builds

## Development

For development, you can use the provided Nix development shell:

```bash
nix develop .
```

This will set up the necessary development environment with all required dependencies.