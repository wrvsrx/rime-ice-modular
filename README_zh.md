# rime-ice-modular

使用 Haskell Shake 构建系统的模块化 rime-ice 输入法，支持灵活的配置和组件选择。

## 编译说明

### 使用 Nix（推荐）

使用 Nix 编译 rime-ice-modular：

```bash
nix build '.?submodules=1'
```

这将编译包含所有子模块的完整 rime-ice-modular 包。

### 使用 Shake

直接使用 Shake 构建系统编译：

```bash
shake
```

这将使用 `Shakefile.hs` 中定义的 Haskell Shake 配置编译 rime-ice-modular。

## 使用说明

1.  **复制到 Rime 用户目录**：将编译好的文件复制到您的 Rime 用户配置目录。

2.  **配置 default.custom.yaml**：在您的 `default.custom.yaml` 文件中添加以下配置：

    - 全拼:

      ```yaml
      patch:
        __include: rime_ice_default_patch:/
        __merge:
          schema_list:
            - schema: rime_ice
      ```

    - 双拼:

      ```yaml
      patch:
        __include: rime_ice_double_pinyin_default_patch:/
        __merge:
          schema_list:
            - schema: rime_ice_double_pinyin_flypy
      ```

**重要提示**：请确保将此内容直接嵌入到您的 `default.custom.yaml` 文件中，而不是作为单独文件引用。

## 项目结构

本项目采用模块化方法来组织 rime-ice 输入法：

- `Shakefile.hs`：使用 Haskell Shake 的主要构建配置
- `Shakefile/`：组件和渲染的附加 Shake 模块
- `components.json` 和 `components.nix`：组件定义和 Nix 打包
- `externals/rime-ice/`：作为子模块的原始 rime-ice 仓库
- `flake.nix`：用于可重现构建的 Nix flake 配置

## 开发

对于开发，您可以使用提供的 Nix 开发环境：

```bash
nix develop
```

这将设置包含所有必需依赖项的开发环境。