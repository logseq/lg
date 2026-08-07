# let-go 与 lg：设计对比及 lg 可借鉴方向

> 分析日期：2026-07-27  
> `let-go`：`/Users/tiensonqin/Codes/projects/let-go`，`main`，提交 `e5d560d1339a73d7bb55a562424a4eae3d620773`  
> `lg`：`/Users/tiensonqin/Codes/projects/lg`，`main`，提交 `a29abbf898d16e275d7c0b26ded60f7623f8de75`，分析的是包含本地未提交修改的当前工作树

## 结论先行

`let-go` 和 `lg` 虽然都使用 Clojure 语法、都能生成原生程序，但它们解决的不是同一个问题。

- `let-go` 的核心目标是：**用 Go 实现一个高度兼容、可嵌入、可动态求值的 Clojure 运行时**。它选择统一的动态值模型、字节码栈 VM、运行时 Var/协议分派，并在保持这些语义的前提下增加 IR 到原生 Go 的 AOT 快速路径。
- `lg` 的核心目标是：**提供一种以 Clojure 语法书写、以 OCaml 静态类型系统为最终裁判的语言**。它选择静态推断/检查、闭合代数数据类型、结构化语义 IR、OCaml Parsetree 和 OCaml 原生模块系统，明确拒绝把类型冲突悄悄装箱进通用动态值。

因此，`lg` 不应把 `let-go` 当成编译器内核的模板。`let-go` 最值得借鉴的是它外围的工程体系：

1. 兼容性套件和逐项能力矩阵；
2. 跨后端语义一致性检查；
3. 生成物新鲜度与可复现性闸门；
4. 相对基准、历史快照和性能趋势页；
5. “原始宿主绑定 + 语言层友好封装”的互操作分层；
6. 单文件分发、资源打包、REPL/nREPL 等产品化体验；
7. 明确标记权威设计文档、过期文档和人工复核日期。

`lg` 不应借鉴的部分同样明确：统一 `Value`、任意值装箱、运行时 Var 注册表、反射式协议/记录分派、VM 兜底、通过动态求值保留兼容性。这些机制与 `docs/design.md` 的静态设计契约直接冲突。

## 1. let-go 的设计

### 1.1 产品定位

`let-go` 是一个用 Go 编写的 Clojure 方言，主要面向 CLI、脚本、Web 服务、嵌入式脚本和 WASM。它强调：

- 单个约 13 MB 的解释器二进制；
- 约 10–11 ms 的冷启动；
- 无 JVM；
- 运行时 `eval`、REPL 和 nREPL；
- 编译为 `.lgb` 字节码、独立二进制或自包含 WASM 页面；
- Go 函数、结构体、channel 和包的双向互操作。

这些数字和 `5621 / 5621` 条 jank Clojure 测试断言通过，是仓库在当前提交中的公开结果；本次分析没有重新运行完整测试和基准。

### 1.2 编译与执行架构

默认执行链路可以概括为：

```text
Clojure form
  -> reader
  -> macro expansion / compileForm
  -> stack bytecode + constant pool + source map
  -> Frame-based stack VM
  -> vm.Value
```

`pkg/compiler/compiler.go` 的 `Context` 在编译时直接维护栈深、局部槽位、闭包捕获、`recur` 点和源码位置，生成 `vm.CodeChunk`。VM 在 `pkg/vm/vm.go` 中解释 opcode。

项目又增加了一条 IR/AOT 链路：

```text
form
  -> block-parameter SSA IR
  -> optimization passes
  -> bytecode lowering，或
  -> Go AST lowering
  -> go build
```

IR 和主要 lowering/optimization pass 本身大量用 `.lg` 编写，位于 `pkg/rt/core/ir/`。它包含常量折叠、DCE、CSE、内联、liveness、type inference、legalize、lambda lifting、循环优化和 Go lowering 等机制。AOT 生成代码仍保留 `vm.Value`、`Var` 和运行时 helper，从而允许编译代码与解释代码相互调用。

这个“双路径、同运行时”模型是 `let-go` 最关键的架构选择：

- 字节码 VM 永远是一等公民，负责 REPL、`eval`、动态特性和嵌入；
- Go AOT 是附加快速路径，不能改变语言语义；
- 编译函数注册回同一套 Var；
- 未能静态直调的调用仍通过 `LookupVar`、`Deref`、`vm.Value` 参数数组完成。

它换来了动态兼容性，但也保留了装箱、间接调用和运行时分派成本。

### 1.3 运行时值模型

`pkg/vm/value.go` 定义统一接口：

```go
type Value interface {
    fmt.Stringer
    Type() ValueType
    Unbox() any
}
```

序列、集合、函数、索引、查找、可调用对象等能力继续通过 Go interface 表示。`Int`、`String`、`Keyword`、持久化 vector/map/set、record、Var、Fn 等都是 `Value`。

这使任意 Clojure 值可以：

- 放进同一个 collection；
- 传给同一个函数调用入口；
- 在运行时查询类型和协议；
- 通过反射或适配器与 Go 值互转；
- 被动态 Var 替换并立即影响后续调用。

这也是 `let-go` 高兼容性的根基，而不是偶然的实现细节。

### 1.4 Clojure 兼容策略

`let-go` 追求“绝大多数惯用 Clojure 代码直接运行”，已覆盖：

- 宏、lazy seq、transducer；
- protocol、record、deftype、reify、multimethod；
- BigInt、BigDecimal、ratio；
- atoms、metadata、regex；
- `clojure.string`、`set`、`walk`、`edn`、`pprint`、`test`、`core.async`；
- reader conditional 和 `.cljc`；
- Babashka pods。

它明确接受一些 Go 宿主差异，例如：

- `go` block 是真实 goroutine，不是 IOC 状态机；
- channel 操作总是阻塞；
- regex 使用 RE2；
- 数值塔是 Go 友好的实用子集；
- `ref`/agent 目前是兼容性别名，不提供完整 STM/异步 agent 语义。

这里的原则是“尽量保持 Clojure 可观察行为，必要时公开记录宿主差异”。

### 1.5 Go 互操作

`let-go` 有两层互操作：

1. **宿主嵌入 API**：Go 程序通过 `pkg/api` 创建 let-go 环境、注册值和函数、运行用户代码。
2. **包绑定生成器**：`cmd/lginterop` 扫描 Go package，生成原始 namespace；再用 `.lg` 编写符合 Clojure 风格的薄封装。

原始绑定可使用反射和 `vm.MustBox`，语言自身的热点 primitive 则生成无反射的类型化 adapter，并向 AOT lowering 注册直接调用信息。

这种“机械生成原始宿主面，手写语言惯用面”的分层很成熟，也很适合迁移到 `lg`，但 `lg` 必须用静态签名和具体 OCaml 类型实现，不能复制反射装箱。

### 1.6 工程治理

`let-go` 的工程体系比单纯的语言实现更值得关注：

- 文档有 `status`、`authoritative-for`、`supersedes`、`last-verified`、`human-verified` 元数据；
- 文档索引明确说明哪个设计文件是当前权威；
- 提交生成代码时有 stale artifact 检查；
- 字节码和 Go-lowered bootstrap 有 parity 检查；
- jank Clojure 套件作为端到端兼容基线；
- benchmark ratchet 使用 CPU anchor 做相对归一化，减少不同 CI 机器之间的噪声；
- 保存历史 release 基线和 main 分支时间序列；
- 同时跟踪冷启动、热运行、RSS、产物大小；
- bytecode 带 opcode-set signature，避免不兼容 VM 错误执行旧字节码。

## 2. lg 的设计

### 2.1 产品定位

`lg` 是静态类型的 Clojure-family 语言，目标是把 Clojure 风格语法直接 elaboration 成 OCaml，而不是重建 JVM Clojure 的动态对象模型。

当前主链路是：

```text
located Lisp AST
  -> lg inference/checking
  -> typed Semantic_ir
  -> Semantic_lowering
  -> Ocaml_ir
  -> OCaml Parsetree
  -> compiler-libs typecheck
  -> native OCaml / Melange / js_of_ocaml
```

`lg` 自己负责：

- Clojure 表面语义；
- homogeneous collection；
- structural map/record row；
- 静态 protocol dispatch；
- nullable/option；
- 源码级错误；
- 闭合 variant/record 建模。

OCaml 负责最终的：

- 模块、签名和 functor；
- 参数化类型；
- constructor payload；
- pattern exhaustiveness；
- host package 函数类型；
- Typedtree、warnings 和部分工具能力。

这与 ReasonML 的思路接近：`lg` 是 OCaml 的另一种静态语法和语义前端，而不是独立动态运行时。

### 2.2 静态类型原则

`docs/design.md` 的核心约束是：

- 推断失败必须报错，不能自动退化成 `Runtime_dynamic.t`；
- 已知异构域必须用闭合 variant；
- `nil` 表示 `option`；
- 声明的类型变量是刚性的静态类型；
- collection 更新不能扩大到 dynamic；
- record、callback、DataScript、query、transaction 和 storage 的字段必须保持具体类型；
- 禁止 `Obj.magic` 和任何等价隐藏；
- 禁止公开 `to_dynamic` / `of_dynamic`、`__lg_dynamic` 等逃生口；
- Java/JVM 互操作明确不属于语言边界；
- OCaml 互操作必须通过显式 package/module/signature 和具体宿主类型。

`Semantic_type.ty` 直接表达 int、float、keyword、option、tuple、array、ref、list、vector、set、seq、function、overload、record 和 named record。`Semantic_ir` 保留类型标注，进入 `Ocaml_ir` 时才明确擦除这些标注。

需要区分“设计合同”和“当前迁移状态”：当前工作树的 `src/`、`runtime/`、`test/datascript_runtime/` 中仍能搜索到若干 `Runtime_dynamic` 使用，其中部分属于编译器/运行时内部兼容边界。它们不能被理解成公共语言模型；按设计合同，应继续缩小到有文档依据的最小边界。

### 2.3 数据结构与 DataScript

`lg` 的 DataScript 方向进一步体现了两项目标的差异：

- `let-go` 依赖统一动态值来接近 Clojure 原始表示；
- `lg` 为 DataScript value、query source/result/input、transaction entry、serialization、storage 等建立闭合 sum 和具体 record；
- 静态表示可以替代原始动态表示，但不得改变上游算法的 branch order、cursor movement、transaction ordering 和公开 API。

这是一条难度更高但边界更清晰的路线：**保持行为兼容，不保持动态表示兼容**。

### 2.4 当前能力边界

相较 `let-go`，`lg` 已有更强的静态模块和工具链能力：

- OCaml module/signature/functor；
- records、variants、option/result、tuple；
- 编译期 protocol dispatch；
- Native、Melange、js_of_ocaml 共享 `.cljc`；
- compiler-libs 最终类型检查；
- Typedtree-backed LSP；
- 跨文件增量分析和依赖分量重分析；
- 原生 DataScript 静态 port 和跨目标 benchmark。

但 Clojure 产品面明显较小：

- 没有通用宏系统；
- 没有完整 namespace/import 生态；
- 标准库覆盖较少；
- 没有通用动态 `eval`；
- 没有完整 Clojure 数值塔；
- 没有 let-go 那样成熟的单文件 bundle、WASM 页面、nREPL 和嵌入式脚本体验。

这些并不全是缺陷。有些是静态语言的有意边界，有些则是产品成熟度差距。

## 3. 核心差异

| 维度 | let-go | lg |
|---|---|---|
| 首要目标 | 高 Clojure 兼容、动态执行、Go 嵌入 | 静态安全、OCaml 互操作、可预测表示 |
| 类型模型 | 统一 `vm.Value`，运行时类型与能力接口 | `Semantic_type.ty` + OCaml 类型系统 |
| 异构 collection | 原生支持，元素统一为 `Value` | 必须显式闭合 sum |
| `nil` | 一等动态 singleton | 静态 `option`/nullable |
| 函数调用 | `Fn.Invoke([]Value)`、Var 间接调用、可动态替换 | 静态函数类型、编译期 elaboration、OCaml 调用 |
| protocol | 运行时注册和类型分派 | 编译期 witness/静态分派 |
| record | 动态运行时值，可反射/注册 | 具体 OCaml record，名义或结构 row |
| 宏 | 运行时系统的重要部分 | 当前不支持通用宏 |
| 编译器后端 | stack bytecode VM；可选 SSA IR→Go AOT | typed semantic IR→OCaml Parsetree |
| AOT 语义 | 保留 VM runtime/Value/Var 语义 | 直接生成静态 OCaml 程序 |
| 动态求值 | 核心能力 | 非目标，不能成为类型逃生口 |
| 宿主互操作 | 反射、boxing、生成 wrapper、Go channel | 显式 OCaml package/module/signature |
| 并发 | goroutine/channel 映射 | 交给具体 OCaml 库和显式类型绑定 |
| Clojure 兼容 | 广，jank suite 5621/5621（项目自述） | 选择性静态兼容，API 面仍在扩展 |
| 工具 | REPL、nREPL、bundle、WASM、pods | compiler、CLI、增量编译、较强 LSP |
| 优化重点 | VM dispatch、boxing、IR passes、AOT direct call | 静态表示、OCaml codegen、DataScript hot path |

最本质的区别是：

> `let-go` 先保留动态 Clojure 世界，再尽量优化；`lg` 先限定一个可静态表达的世界，再要求所有代码留在这个世界里。

## 4. lg 可以借鉴的地方

### P0：立即值得做

#### 4.1 建立“语言兼容性总账”

参考 `let-go` 的 Clojure compatibility 文档和 jank suite 接入，为 `lg` 建立机器可检查的能力矩阵：

- reader/form/core function/collection/protocol/module/interop 分类别；
- 每项标记 `supported`、`static alternative`、`intentionally unsupported`、`partial`；
- 每个 `partial` 必须链接具体测试；
- Native、Melange、js_of_ocaml 分列；
- DataScript 继续使用独立的上游 API manifest 和 differential matrix。

`lg` 已经在 DataScript 上做了 API manifest、上游状态表和 differential fixture；应把同样方法提升到整个语言，不要另造一套机制。

收益：语言边界不再只存在于长篇文档和测试文件中，新增兼容行为也不会因静态化困难而被无声删除。

#### 4.2 把“多后端相同语义”做成正式 parity gate

`let-go` 对 bytecode 与 `gogen_ir` 做 bootstrap parity。`lg` 对应的检查应是：

```text
同一 fixture
  -> Native
  -> Melange
  -> js_of_ocaml（适用时）
  -> 规范化公开结果
  -> 完全一致
```

重点覆盖：

- reader conditional；
- collection 顺序、hash/equality；
- lazy seq 重复消费；
- exception/error；
- DataScript query/pull/transaction/serialization；
- storage `Strong | Weak`；
- 生成代码中的 target-specific runtime helper。

不要比较内部表示；比较公开行为。对浮点、错误路径和无序集合使用显式规范化规则。

#### 4.3 引入生成物与设计约束闸门

可直接借鉴的 CI 检查：

- 重新生成后工作树必须为空；
- public API manifest 必须是最新的；
- sidecar/signature/generated Parsetree fixture 必须可复现；
- 同一输入编译两次必须产生相同 OCaml；
- 禁止生成物出现 `Obj.magic`、`__lg_dynamic`、`to_dynamic`、`of_dynamic`；
- DataScript hot path 禁止出现未批准的 `Runtime_dynamic`；
- compiler/runtime opcode 或 cache schema 变化时更新显式 version/signature。

最后一项借鉴的是 `let-go` bytecode opcode signature 的思想，不是让 `lg` 引入 bytecode：任何持久化编译缓存、LSP cache 或序列化编译状态都应带 schema/compiler fingerprint，宁可拒绝旧缓存，也不要错误读取。

#### 4.4 建立跨机器更稳定的性能 ratchet

`lg` 已有很强的 DataScript benchmark，但可以借鉴 `let-go` 的 anchor-relative 和历史快照机制：

- Native 与 Melange 分开记录；
- 每条 benchmark 同时记录时间、分配、峰值 RSS、产物大小；
- 使用稳定的 target-local anchor 归一化；
- 保存 release baseline 和 main timeline；
- PR 使用宽松回归阈值，定时任务运行完整样本；
- 对小幅领先（例如当前设计文档提到的 Melange `pull-many` 窄优势）设专门回归边界；
- 原始结果流式写入，单个 benchmark 失败时保留此前数据。

不能只比较相对比例；DataScript 仍应保留绝对时间和规模曲线，防止 anchor 一起退化或小数据掩盖复杂度问题。

#### 4.5 整理权威文档元数据

`lg` 已经有权威的 `docs/design.md`，但其他文档可以借鉴 `let-go` 的元数据：

```yaml
status: active | planning | shipped | superseded | archived
authoritative-for:
supersedes:
last-verified:
human-verified:
```

并增加一个短索引，说明：

- 静态语言合同看 `docs/design.md`；
- 当前能力差异看 `docs/differences.md`；
- DataScript 上游对齐看哪个文件；
- 哪些 roadmap 已过期；
- benchmark baseline 在哪里。

这能减少“旧计划仍像当前设计”的歧义。

### P1：高价值，但需要设计

#### 4.6 为 OCaml 互操作建立“原始绑定 + 惯用封装”两层

借鉴 `lginterop` 的分层，但保持全静态：

```text
OCaml package metadata / .cmi / explicit manifest
  -> 生成 lg sidecar/signature 和薄绑定
  -> 手写 lg 惯用 API
```

建议：

- 原始层保留 OCaml 原名、labelled argument、result/option、record/variant；
- 生成具体类型，禁止 `Runtime_dynamic.t`；
- 对真正无法自动表达的 GADT、first-class module、开放 object type 明确拒绝并解释；
- 惯用层提供 kebab-case、参数顺序调整、资源生命周期 helper；
- 生成头记录输入 package 版本、命令和 fingerprint；
- round-trip regeneration 必须 byte-identical。

这会显著降低接入 OCaml 库的成本，同时完全符合 `lg` 的互操作合同。

#### 4.7 改善分发体验

`let-go` 在这一点上明显领先。`lg` 可以保持静态架构，同时提供：

- `lg build`：从 `.cljc` 到原生可执行文件；
- `lg build --target melange`：生成可部署 JS bundle；
- `lg build --target js-of-ocaml`：生成对应产物；
- resource manifest 和资源嵌入；
- 可复现的 package/findlib dependency lock；
- `lg run` 自动管理临时 build，而不要求用户手写 `ocamlopt` 参数。

优先级应高于增加动态语言特性，因为它直接改善现有静态语言的可用性，不改变语义边界。

#### 4.8 增加面向静态语言的 REPL，而不是动态 VM

可以借鉴 `let-go` 的交互体验，但实现应沿用 `lg` 已有的 incremental compiler state：

- 每个输入 chunk 走完整 parse → infer/check → Parsetree → OCaml typecheck；
- 成功后链接/执行新 phrase；
- 保留类型、模块、protocol 和 package state；
- 失败 chunk 不污染状态；
- 提供 `:type`、`:source`、`:reload`、`:target`；
- nREPL 只做协议适配，不改变求值模型。

不要为 REPL 引入 `vm.Value`、通用动态 environment 或运行时 Var。

#### 4.9 建立静态宏边界

`let-go` 的宏覆盖是其 Clojure 兼容度的重要原因，但其宏依赖动态运行时。`lg` 如果需要宏，应借鉴“兼容性价值”，而不是复制实现：

- 宏只在 parse 与 inference 之间展开；
- 输入输出是闭合、带 source span 的 form AST；
- 展开结果必须经过普通类型检查；
- 宏不能构造或传递 runtime dynamic 值；
- 宏依赖和 expansion cache 必须确定性、可版本化；
- 优先实现一组受控、可测试的库宏，再考虑用户宏；
- 保持 `if`、`match`、`loop/recur` 等类型敏感 form 为 compiler form。

若做不到确定性和可靠 source mapping，宁可继续扩充 compiler-recognized form，也不要引入半动态宏系统。

### P2：条件成熟后再做

#### 4.10 标准库采用“宿主 primitive + lg veneer”

`let-go` 用 Go primitive 提供底层能力、用 `.lg` 提供用户 API。`lg` 可采用：

- 小而具体的 OCaml primitive；
- 静态 sidecar；
- 大部分组合逻辑写在 `.cljc`；
- Native/Melange 共用实现，仅在宿主能力不同处用 reader conditional；
- 对每个 primitive 保留直接调用和目标特定测试。

这有利于逐步自举标准库，但不意味着让编译器自举。`lg` 依赖 compiler-libs、Typedtree 和 OCaml 模块系统，自举编译器本身未必有收益。

#### 4.11 增加真实项目兼容 corpus

在 DataScript 之外选取若干不同类型的真实项目：

- 纯函数/collection 库；
- parser；
- CLI；
- 数据处理；
- OCaml interop 示例；
- 跨 Native/Melange 小应用。

每个项目记录：

- 原始 API；
- 需要的静态改写；
- 被拒绝的动态模式；
- 推荐的 closed sum/record 替代；
- 编译时间、运行时间和产物大小。

目标不是让所有 Clojure 项目无修改运行，而是验证 `lg` 的静态替代方案是否足够自然。

## 5. 明确不应借鉴的设计

### 5.1 不引入统一动态值

不要把 `Semantic_type.ty` 或已知异构域降成类似 `vm.Value`。这会同时破坏：

- homogeneous collection；
- rigid type variable；
- option；
- record/tuple 具体表示；
- OCaml direct call；
- DataScript hot path；
- 可预测的 equality/hash/serialization。

### 5.2 不把 AOT 做成“动态运行时的快速路径”

`let-go` 的 AOT 必须保留 Var、boxing 和运行时 fallback，因为它需要动态 `eval`。`lg` 已经直接生成静态 OCaml，没有必要再添加一套 SSA→OCaml/Go 的平行语义实现。

优化应优先依赖：

- 更精确的 elaboration；
- 简单、结构化的 OCaml AST；
- flambda/OCaml 编译器优化；
- 针对已测热点的类型化 fast path；
- 保持上游控制流的局部表示优化。

只有当 profiling 证明 OCaml 后端无法表达关键优化时，才考虑增加新的中间优化层。

### 5.3 不引入运行时 Var、协议或 record 注册表

动态 Var 和 runtime protocol registry 是 `let-go` 兼容 `with-redefs`、`eval`、任意 extension 的必要机制，却与 `lg` 的编译期 protocol witness 和具体 record 冲突。

动态绑定的 Var 在 `lg` 中应继续保持“值可重绑定但类型不变”，而不是“类型和值都可替换”。

### 5.4 不复制反射式宿主互操作

`vm.MustBox` 和反射 method dispatch 对动态 Go 嵌入很实用，但 `lg` 应始终：

- 生成或声明具体函数类型；
- 显式表达 `option`/`result`；
- 对资源和 callback 使用具体 record；
- 在边界立即校验并进入闭合静态表示；
- 拒绝无法安全映射的开放宿主面。

### 5.5 不追求表面的 100% Clojure 兼容

`let-go` 的 `5621 / 5621` 是其动态路线的重要成绩，但不应成为 `lg` 的直接 KPI。`lg` 更合适的指标是：

- 支持的兼容项在所有 target 上行为一致；
- 不支持项有明确静态替代或清晰拒绝；
- 不通过 dynamic escape hatch 提高通过率；
- DataScript 等重点项目在静态表示下保持完整公开行为。

## 6. 建议实施顺序

### 第一阶段：2–4 周

1. 建立全语言 capability matrix，并关联现有测试。
2. 增加 Native/Melange 规范化 parity runner。
3. 增加 deterministic codegen 和 stale generated artifact 检查。
4. 为设计/roadmap 文档增加权威状态索引。

### 第二阶段：4–8 周

1. 将现有 DataScript benchmark 升级为可保存历史的 ratchet。
2. 增加冷启动、RSS、产物大小和编译时间指标。
3. 设计并实现一个小型 OCaml package binding generator 原型。
4. 用一个真实 OCaml 库验证“原始绑定 + lg veneer”。

### 第三阶段：8 周以后

1. 提供 `lg build` 和资源打包。
2. 基于增量 compiler state 提供静态 REPL/nREPL。
3. 用真实项目 corpus 决定是否需要受控静态宏。
4. 扩展跨目标标准库，但继续禁止动态兜底。

## 7. 最终判断

`let-go` 展示了一个 Clojure 方言如何从“语言实现”成长为“可安装、可嵌入、可测试、可分发、可长期维护的产品”。这是 `lg` 最值得学习的地方。

`lg` 的真正优势则是 `let-go` 无法轻易获得的：静态类型、闭合表示、OCaml 模块系统、Typedtree 工具链，以及 DataScript 热路径中可预测的具体数据布局。借鉴时应守住这个优势。

最合适的策略不是让 `lg` 变成“OCaml 上的 let-go”，而是：

> 保留 `lg` 的静态内核，吸收 `let-go` 的兼容性验证、工程治理、互操作生成和分发体验。

## 8. 主要证据入口

### let-go

- `README.md`：定位、兼容性、benchmark、分发方式
- `pkg/compiler/compiler.go`：直接字节码编译器
- `pkg/vm/value.go`：统一动态值与能力接口
- `pkg/vm/vm.go`：opcode 与 stack VM
- `pkg/rt/core/ir/`：SSA IR、优化、bytecode/Go lowering
- `docs/contribution-policy.md`：权威架构合同和 CI checkpoint
- `docs/design/go-aot-backend.md`：VM 与 AOT 共存设计
- `docs/guide/clojure-compatibility.md`：兼容边界
- `docs/guide/embedding-in-go.md`：嵌入 API
- `docs/guide/go-interop.md`：绑定生成与 veneer 分层
- `docs/perf/ratchet.md`：相对性能基线

### lg

- `docs/design.md`：静态设计权威合同
- `README.md`：当前 pipeline、Parsetree、增量编译和 LSP
- `docs/differences.md`：与 Clojure 的差异
- `src/semantic_type.ml`：源类型模型
- `src/semantic_ir.ml`：带类型语义 IR
- `src/semantic_lowering.ml`：显式类型擦除边界
- `src/toolchain.ml`：frontend、typecheck、Parsetree、compiler-libs 链路
- `test/datascript/UPSTREAM.md`：DataScript 上游基线
- `docs/agent-guide/003-datascript-upstream-alignment.md`：静态 port 与差分验证计划

