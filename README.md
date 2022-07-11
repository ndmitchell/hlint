# HLint [![Hackage version](https://img.shields.io/hackage/v/hlint.svg?label=Hackage)](https://hackage.haskell.org/package/hlint) [![Stackage version](https://www.stackage.org/package/hlint/badge/nightly?label=Stackage)](https://www.stackage.org/package/hlint) [![Build status](https://img.shields.io/github/workflow/status/ndmitchell/hlint/ci/master.svg)](https://github.com/ndmitchell/hlint/actions)

HLint is a tool for suggesting possible improvements to Haskell code. These suggestions include ideas such as using alternative functions, simplifying code and spotting redundancies. This document is structured as follows:

* [Installing and running HLint](./README.md#installing-and-running-hlint)
* [FAQ](./README.md#faq)
* [Customizing the hints](./README.md#customizing-the-hints)
* [Hacking HLint](./README.md#hacking-hlint)

### Bugs and limitations

Bugs can be reported [on the bug tracker](https://github.com/ndmitchell/hlint/issues). There are some issues that I do not intend to fix:

* HLint operates on each module at a time in isolation, as a result HLint does not know about types or which names are in scope. This decision is deliberate, allowing HLint to parallelise and be used incrementally on code that may not type-check. If fixities are required to parse the code properly, they [can be supplied](./README.md#why-doesnt-hlint-know-the-fixity-for-my-custom--operator).
* The presence of `seq` may cause some hints (i.e. eta-reduction) to change the semantics of a program.
* Some transformed programs may require [additional type signatures](https://stackoverflow.com/questions/16402942/how-can-eta-reduction-of-a-well-typed-function-result-in-a-type-error/), particularly if the transformations trigger the monomorphism restriction or involve rank-2 types. In rare cases, there might be [nowhere to write](https://stackoverflow.com/questions/19758828/eta-reduce-is-not-always-held-in-haskell) the required type signature.
* Sometimes HLint will change the code in a way that causes values to default to different types, which may change the behaviour.
* HLint assumes duplicate identical expressions within in a single expression are used at the same type.
* The `RebindableSyntax` extension can cause HLint to suggest incorrect changes.
* HLint can be configured with knowledge of C Pre Processor flags, but it can only see one conditional set of code at a time.
* HLint turns on many language extensions so it can parse more documents, occasionally some break otherwise legal syntax - e.g. `{-#INLINE foo#-}` doesn't work with `MagicHash`, `foo $bar` means something different with `TemplateHaskell`. These extensions can be disabled with `-XNoMagicHash` or `-XNoTemplateHaskell` etc.
* HLint doesn't run any custom preprocessors, e.g. [markdown-unlit](https://hackage.haskell.org/package/markdown-unlit) or [record-dot-preprocessor](https://hackage.haskell.org/package/record-dot-preprocessor), so code making use of them will usually fail to parse.
* Some hints, like `Use const`, don't work for non-lifted (i.e. unlifted and unboxed) types.

## Installing and running HLint

Installation follows the standard pattern of any Haskell library or program: type `cabal update` to update your local hackage database, then `cabal install hlint` to install HLint.

Once HLint is installed, run `hlint source` where `source` is either a Haskell file, or a directory containing Haskell files. A directory will be searched recursively for any files ending with `.hs` or `.lhs`. For example, running HLint over darcs would give:

```console
$ hlint darcs-2.1.2

darcs-2.1.2\src\CommandLine.lhs:94:1: Warning: Use concatMap
Found:
    concat $ map escapeC s
Perhaps:
    concatMap escapeC s

darcs-2.1.2\src\CommandLine.lhs:103:1: Suggestion: Use fewer brackets
Found:
    ftable ++ (map (\ (c, x) -> (toUpper c, urlEncode x)) ftable)
Perhaps:
    ftable ++ map (\ (c, x) -> (toUpper c, urlEncode x)) ftable

darcs-2.1.2\src\Darcs\Patch\Test.lhs:306:1: Warning: Use a more efficient monadic variant
Found:
    mapM (delete_line (fn2fp f) line) old
Perhaps:
    mapM_ (delete_line (fn2fp f) line) old

... lots more hints ...
```

Each hint says which file/line the hint relates to, how serious an issue it is, a description of the hint, what it found, and what you might want to replace it with. In the case of the first hint, it has suggested that instead of applying `concat` and `map` separately, it would be better to use the combination function `concatMap`.

The first hint is marked as an warning, because using `concatMap` in preference to the two separate functions is always desirable. In contrast, the removal of brackets is probably a good idea, but not always. Reasons that a hint might be a suggestion include requiring an additional import, something not everyone agrees on, and functions only available in more recent versions of the base library.

Any configuration can be done via [.hlint.yaml](./README.md#customizing-the-hints) file.

**Bug reports:** The suggested replacement should be equivalent - please report all incorrect suggestions not mentioned as known limitations.

### Suggested usage

HLint usage tends to proceed in three distinct phases:

1. Initially, run `hlint . --report` to generate `report.html` containing a list of all issues HLint has found. Fix those you think are worth fixing and keep repeating.
1. Once you are happy, run `hlint . --default > .hlint.yaml`, which will generate a settings file ignoring all the hints currently outstanding. Over time you may wish to edit the list.
1. For larger projects, add [custom hints or rules](./README.md#customizing-the-hints).

Most hints are intended to be a good idea in most circumstances, but not universally - judgement is required. When contributing to someone else's project, HLint can identify pieces of code to look at, but only make changes you consider improvements - not merely to adhere to HLint rules.

### Running with Continuous Integration

On CI you might wish to run `hlint .` (or `hlint src` if you only want to check the `src` directory). To avoid the cost of compilation you may wish to fetch the [latest HLint binary release](https://github.com/ndmitchell/hlint/releases/latest).

For the CI systems [Travis](https://travis-ci.org/), [Appveyor](https://www.appveyor.com/) and [Azure Pipelines](https://azure.microsoft.com/en-gb/services/devops/pipelines/) add the line:

```sh
curl -sSL https://raw.github.com/ndmitchell/hlint/master/misc/run.sh | sh -s .
```

The arguments after `-s` are passed to `hlint`, so modify the final `.` if you want other arguments. This command works on Windows, Mac and Linux.

### Integrations

HLint is integrated into lots of places:

* Lots of editors have HLint plugins (quite a few have more than one HLint plugin).
* HLint is part of the multiple Haskell IDEs, [haskell-language-server](https://github.com/haskell/haskell-language-server), [ghc-mod](https://hackage.haskell.org/package/ghc-mod) and [Intero](https://github.com/commercialhaskell/intero).
* [HLint Source Plugin](https://github.com/ocharles/hlint-source-plugin) makes HLint available as a GHC plugin.
* [Splint](https://github.com/tfausak/splint) is another source plugin that doesn't require reparsing the GHC source if you are on the latest GHC version.
* [Code Climate](https://docs.codeclimate.com/v1.0/docs/hlint) is a CI for analysis which integrates HLint.
* [Danger](http://allocinit.io/haskell/danger-and-hlint/) can be used to automatically comment on pull requests with HLint suggestions.
* [Restyled](https://restyled.io) includes an HLint Restyler to automatically run `hlint --refactor` on files changed in GitHub Pull Requests.
* [hlint-test](https://hackage.haskell.org/package/hlint-test) helps you write a small test runner with HLint.
* [hint-man](https://github.com/apps/hint-man) automatically submits reviews to opened pull requests in your repositories with inline hints.
* [CircleCI](https://circleci.com/orbs/registry/orb/haskell-works/hlint) has a plugin to run HLint more easily.
* [haskell/actions](https://github.com/haskell/actions) has `hlint-setup` and `hlint-run` actions for GitHub.

### Automatically Applying Hints

HLint can automatically apply some suggestions using the `--refactor` flag. If passed, instead of printing out the hints, HLint will output the refactored file on stdout. For `--refactor` to work it is necessary to have the `refactor` executable from the [`apply-refact`](https://github.com/mpickering/apply-refact) package on your `$PATH`. HLint uses that tool to perform the refactoring.

When using `--refactor` you can pass additional options to the `refactor` binary using `--refactor-options` flag. Some useful flags include `-i` (which replaces the original file) and `-s` (which asks for confirmation before performing a hint). The `--with-refactor` flag can be used to specify an alternative location for the `refactor` binary. Simple bindings for [Vim](https://github.com/mpickering/hlint-refactor-vim), [Emacs](https://github.com/mpickering/hlint-refactor-mode) and [Atom](https://github.com/mpickering/hlint-refactor-atom) are available.

While the `--refactor` flag is useful, not all hints support refactoring. See [hints.md](hints.md) for which hints don't support refactoring.

### Reports

HLint can generate a lot of information, making it difficult to search for particular types of errors. The `--report` flag will cause HLint to generate a report file in HTML, which can be viewed interactively. Reports are recommended when there are more than a handful of hints.

### Language Extensions

HLint enables most Haskell extensions, disabling only those which steal too much syntax (e.g. Arrows, TransformListComp and TypeApplications). Individual extensions can be enabled or disabled with, for instance, `-XArrows`, or `-XNoMagicHash`. The flag `-XHaskell2010` selects Haskell 2010 compatibility. You can also pass them via `.hlint.yaml` file. For example: `- arguments: [-XArrows]`.

### Emacs Integration

Emacs integration has been provided by [Alex Ott](http://xtalk.msk.su/~ott/). The integration is similar to compilation-mode, allowing navigation between errors. The script is at [hs-lint.el](https://raw.githubusercontent.com/ndmitchell/hlint/master/data/hs-lint.el), and a copy is installed locally in the data directory. To use, add the following code to the Emacs init file:

```guile
(require 'hs-lint)
(defun my-haskell-mode-hook ()
    (local-set-key "\C-cl" 'hs-lint))
(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)
```

### GHCi Integration

GHCi integration has been provided by Gwern Branwen. The integration allows running `:hlint` from the GHCi prompt. The script is at [hlint.ghci](https://raw.githubusercontent.com/ndmitchell/hlint/master/data/hlint.ghci), and a copy is installed locally in the data directory. To use, add the contents to your [GHCi startup file](https://www.haskell.org/ghc/docs/latest/html/users_guide/ghci.html#the-ghci-and-haskeline-files).

### Parallel Operation

To run HLint on 4 processors append the flags `-j4`. HLint will usually perform fastest if n is equal to the number of physical processors, which can be done with `-j` alone.

If your version of GHC does not support the GHC threaded runtime then install with the command: `cabal install --flags="-threaded"`

### C preprocessor support

HLint runs the [cpphs C preprocessor](http://hackage.haskell.org/package/cpphs) over all input files, by default using the current directory as the include path with no defined macros. These settings can be modified using the flags `--cpp-include` and `--cpp-define`. To disable the C preprocessor use the flag `-XNoCPP`. There are a number of limitations to the C preprocessor support:

* HLint will only check one branch of an `#if`, based on which macros have been defined.
* Any missing `#include` files will produce a warning on the console, but no information in the reports.

## FAQ

### Usage

#### Why doesn't the compiler automatically apply the optimisations?

HLint doesn't suggest optimisations, it suggests code improvements - the intention is to make the code simpler, rather than making the code perform faster. The [GHC compiler](http://haskell.org/ghc/) automatically applies many of the rules suggested by HLint, so HLint suggestions will rarely improve performance.

#### Why do I sometimes get a "Note" with my hint?

Most hints are perfect substitutions, and these are displayed without any notes. However, some hints change the semantics of your program - typically in irrelevant ways - but HLint shows a warning note. HLint does not warn when assuming typeclass laws (such as `==` being symmetric). Some notes you may see include:

* __Increases laziness__ - for example `foldl (&&) True` suggests `and` including this note. The new code will work on infinite lists, while the old code would not. Increasing laziness is usually a good idea.
* __Decreases laziness__ - for example `(fst a, snd a)` suggests `a` including this note. On evaluation the new code will raise an error if a is an error, while the old code would produce a pair containing two error values. Only a small number of hints decrease laziness, and anyone relying on the laziness of the original code would be advised to include a comment.
* __Removes error__ - for example `foldr1 (&&)` suggests `and` including the note `Removes error on []`. The new code will produce `True` on the empty list, while the old code would raise an error. Unless you are relying on the exception thrown by the empty list, this hint is safe - and if you do rely on the exception, you would be advised to add a comment.

#### What is the difference between error/warning/suggestion?

Every hint has a severity level:

* __Error__ - by default only used for parse errors.
* __Warning__ - for example `concat (map f x)` suggests `concatMap f x` as a "warning" severity hint. From a style point of view, you should always replace a combination of `concat` and `map` with `concatMap`.
* __Suggestion__ - for example `x !! 0` suggests `head x` as a "suggestion" severity hint. Typically `head` is a simpler way of expressing the first element of a list, especially if you are treating the list inductively. However, in the expression `f (x !! 4) (x !! 0) (x !! 7)`, replacing the middle argument with `head` makes it harder to follow the pattern, and is probably a bad idea. Suggestion hints are often worthwhile, but should not be applied blindly.

The difference between warning and suggestion is one of personal taste, typically my personal taste. If you already have a well developed sense of Haskell style, you should ignore the difference. If you are a beginner Haskell programmer you may wish to focus on warning hints before suggestion hints.

#### Why do I get a parse error?

HLint enables/disables a set of extensions designed to allow as many files to parse as possible, but sometimes you'll need to enable an additional extension (e.g. Arrows, QuasiQuotes, ...), or disable some (e.g. MagicHash) to enable your code to parse.

You can enable extensions by specifying additional command line arguments in [.hlint.yaml](./README.md#customizing-the-hints), e.g.: `- arguments: [-XQuasiQuotes]`.

#### How do I only run hlint on changed files?

If you're using git, it may be helpful to only run hlint on changed files. This can be a considerable speedup on very large codebases.

```bash
{ git diff --diff-filter=d --name-only $(git merge-base HEAD origin/master) -- "***.hs" && git ls-files -o --exclude-standard -- "***.hs"; } | xargs hlint
```

Because hlint's `--refactor` option only works when you pass a single file, this approach is also helpful to enable refactoring many files in a single command:

```bash
{ git diff --diff-filter=d --name-only $(git merge-base HEAD origin/master) -- "***.hs" && git ls-files -o --exclude-standard -- "***.hs"; } | xargs -I file hlint file --refactor --refactor-options="--inplace --step"
```

### Configuration

#### Why doesn't HLint know the fixity for my custom !@%$ operator?

HLint knows the fixities for all the operators in the base library, as well as operators whose fixities are declared in the module being linted, but no others. HLint works on a single file at a time, and does not resolve imports, so cannot see fixity declarations from imported modules. You can tell HLint about fixities by putting them in a hint file named `.hlint.yaml` with the syntax `- fixity: "infixr 5 !@%$"`. You can also use `--find` to automatically produce a list of fixity declarations in a file.

#### Which hints are ignored?

Some hints are off-by-default. Some are ignored by the configuration settings. To see all hints pass `--show`. This feature is often useful in conjunction with `--report` which shows the hints in an interactive web page, allowing them to be browsed broken down by hint.

#### Which hints are used?

HLint uses the `hlint.yaml` file it ships with by default (containing things like the `concatMap` hint above), along with the first `.hlint.yaml` file it finds in the current directory or any parent thereof. To include other hints, pass `--hint=filename.yaml`.

#### Are there any extra hints available?

There are a few groups of hints that are shipped with HLint, but disabled by default. These are:

* `future`, which suggests switching `return` for `pure`.
* `extra`, which suggests replacements which introduce a dependency on the [`extra` library](https://hackage.haskell.org/package/extra).
* `use-lens`, which suggests replacements which introduce a dependency on the [`lens` library](https://hackage.haskell.org/package/lens).
* `use-th-quotes`, which suggests using `[| x |]` where possible.
* `generalise`, which suggests more generic methods, e.g. `fmap` instead of `map`.
* `generalise-for-conciseness`, which suggests more generic methods, but only when they are shorter, e.g. `maybe True` becomes `all`.
* `dollar` which suggests `a $ b $ c` is replaced with `a . b $ c`.
* `teaching` which encourages a simple beginner friendly style, learning about related functions.

These can be enabled by passing `--with-group=future` or adding the following to your `.hlint.yaml` file:

```yaml
- group: {name: future, enabled: true}
```

### Design

#### Why are hints not applied recursively?

Consider:

```haskell
foo xs = concat (map op xs)
```

This will suggest eta reduction to `concat . map op`, and then after making that change and running HLint again, will suggest use of `concatMap`. Many people wonder why HLint doesn't directly suggest `concatMap op`. There are a number of reasons:

* HLint aims to both improve code, and to teach the author better style. Doing modifications individually helps this process.
* Sometimes the steps are reasonably complex, by automatically composing them the user may become confused.
* Sometimes HLint gets transformations wrong. If suggestions are applied recursively, one error will cascade.
* Some people only make use of some of the suggestions. In the above example using concatMap is a good idea, but sometimes eta reduction isn't. By suggesting them separately, people can pick and choose.
* Sometimes a transformed expression will be large, and a further hint will apply to some small part of the result, which appears confusing.
* Consider `f $ (a b)`. There are two valid hints, either remove the $ or remove the brackets, but only one can be applied.

#### Is it possible to use pragma annotations in code that is read by `ghci` (conflicts with `OverloadedStrings`)?

Short answer: yes, it is!

If the language extension `OverloadedStrings` is enabled, `ghci` may however report error messages such as:

```console
Ambiguous type variable ‘t0’ arising from an annotation
prevents the constraint ‘(Data.Data.Data t0)’ from being solved.
```

In this case, a solution is to add the `:: String` type annotation.  For example:

```haskell
{-# ANN someFunc ("HLint: ignore Use fmap" :: String) #-}
```

See discussion in [issue #372](https://github.com/ndmitchell/hlint/issues/372).

## Customizing the hints

To customize the hints given by HLint, create a file `.hlint.yaml` in the root of your project. For a suitable default run:

```console
hlint --default > .hlint.yaml
```

This default configuration shows lots of examples (as `# comments`) of how to:

* Add command line arguments to all runs, e.g. `--color` or `-XNoMagicHash`.
* Ignore certain hints, perhaps within certain modules/functions.
* Restrict the use of GHC flags/extensions/functions, e.g. banning `Arrows` and `unsafePerformIO`.
* Add additional project-specific hints.

You can see the output of `--default` for a clean lint [here](https://github.com/ndmitchell/hlint/blob/master/data/default.yaml) but for a dirty project `--default` output includes an extra warnings section that counts and ignores any hints it finds:

  ```yaml
  # Warnings currently triggered by your code
  - ignore: {name: "Redundant $"} # 20 hints
  - ignore: {name: "Unused LANGUAGE pragma"} # 29 hints
  ```

If you wish to use the [Dhall configuration language](https://github.com/dhall-lang/dhall-lang) to customize HLint, there [is an example](https://kowainik.github.io/posts/2018-09-09-dhall-to-hlint) and [type definition](https://github.com/kowainik/relude/blob/master/hlint/Rule.dhall).

### Finding the name of a hint

Hints are named with the string they display in their help message

For example, if hlints outputs a warning like

```
./backend/tests/api-tests/src/Main.hs:116:51: Warning: Redundant ==
Found:
  regIsEnabled rr == True
Perhaps:
  regIsEnabled rr
```

the name of the lint is `Redundant ==`.

You can use that name to refer to the lint in the configuration file and `ANN` pragmas, see the following sections.

### Ignoring hints

Some of the hints are subjective, and some users believe they should be ignored. Some hints are applicable usually, but occasionally don't always make sense. The ignoring mechanism provides features for suppressing certain hints. Ignore directives can either be written as pragmas in the file being analysed, or in the hint files. Examples of pragmas are:

* `{-# ANN module "HLint: ignore" #-}` or `{-# HLINT ignore #-}` or `{- HLINT ignore -}` - ignore all hints in this module (use `module` literally, not the name of the module).
* `{-# ANN module "HLint: ignore Eta reduce" #-}` or `{-# HLINT ignore "Eta reduce" #-}` or `{- HLINT ignore "Eta reduce" -}` - ignore all eta reduction suggestions in this module.
* `{-# ANN myDef "HLint: ignore" #-}` or `{-# HLINT ignore myDef #-}` or `{- HLINT ignore myDef -}` - don't give any hints in the definition `myDef`. This may be combined with hint names, `{- HLINT ignore myDef "Eta reduce" -}`, to only ignore that hint in that definition.
* `{-# ANN myDef "HLint: error" #-}` or `{-# HLINT error myDef #-}` or `{- HLINT error myDef -}` - any hint in the definition `myDef` is an error.
* `{-# ANN module "HLint: error Use concatMap" #-}` or `{-# HLINT error "Use concatMap" #-}` or `{- HLINT error "Use concatMap" -}` - the hint to use `concatMap` is an error (you may also use `warn` or `suggest` in place of `error` for other severity levels).

For `ANN` pragmas it is important to put them _after_ any `import` statements. If you have the `OverloadedStrings` extension enabled you will need to give an explicit type to the annotation, e.g. `{-# ANN myDef ("HLint: ignore" :: String) #-}`. The `ANN` pragmas can also increase compile times or cause more recompilation than otherwise required, since they are evaluated by `TemplateHaskell`.

For `{-# HLINT #-}` pragmas GHC may give a warning about an unrecognised pragma, which can be suppressed with `-Wno-unrecognised-pragmas`.

For `{- HLINT -}` comments they are likely to be treated as comments in syntax highlighting, which can lead to them being overlooked.

Ignore directives can also be written in the hint files:

* `- ignore: {name: Eta reduce}` - suppress all eta reduction suggestions.
* `- ignore: {name: Eta reduce, within: [MyModule1, MyModule2]}` - suppress eta reduction hints in the `MyModule1` and `MyModule2` modules.
* `- ignore: {within: MyModule.myDef}` - don't give any hints in the definition `MyModule.myDef`.
* `- error: {within: MyModule.myDef}` - any hint in the definition `MyModule.myDef` is an error.
* `- error: {name: Use concatMap}` - the hint to use `concatMap` is an error (you may also use `warn` or `suggest` in place of `error` for other severity levels).

These directives are applied in the order they are given, with later hints overriding earlier ones.

You can choose to ignore all hints with `- ignore: {}` then selectively enable the ones you want (e.g. `- warn: {name: Use const}`), but it isn't a totally smooth experience (see [#747](https://github.com/ndmitchell/hlint/issues/747) and [#748](https://github.com/ndmitchell/hlint/issues/748)).

Finally, `hlint` defines the `__HLINT__` preprocessor definition (with value `1`), so problematic definitions (including those that don't parse) can be hidden with:

```haskell
#ifndef __HLINT__
foo = ( -- HLint would fail to parse this
#endif
```

### Adding hints

The hint suggesting `concatMap` can be defined as:

```yaml
- warn: {lhs: concat (map f x), rhs: concatMap f x}
```

This line can be read as replace `concat (map f x)` with `concatMap f x`. All single-letter variables are treated as substitution parameters. For examples of more complex hints see the supplied `hlint.yaml` file in the data directory. This hint will automatically match `concat . map f` and `concat $ map f x`, so there is no need to give eta-reduced variants of the hints. Hints may tagged with `error`, `warn` or `suggest` to denote how severe they are by default. In addition, `hint` is a synonym for `suggest`. If you come up with interesting hints, please submit them for inclusion.

You can search for possible hints to add from a source file with the `--find` flag, for example:

```console
$ hlint --find=src/Utils.hs
-- hints found in src/Util.hs
- warn: {lhs: "null (intersect a b)", rhs: "disjoint a b"}
- warn: {lhs: "dropWhile isSpace", rhs: "trimStart"}
- fixity: "infixr 5 !:"
```

These hints are suitable for inclusion in a custom hint file. You can also include Haskell fixity declarations in a hint file, and these will also be extracted. If you pass only `--find` flags then the hints will be written out, if you also pass files/folders to check, then the found hints will be automatically used when checking.

Hints can specify more advanced aspects, with names and side conditions. To see examples and descriptions of these features look at [the default hint file](https://github.com/ndmitchell/hlint/blob/master/data/hlint.yaml) and [the hint interpretation module comments](https://github.com/ndmitchell/hlint/blob/master/src/Hint/Match.hs).

### Restricting items

HLint can restrict what Haskell code is allowed, which is particularly useful for larger projects which wish to enforce coding standards - there is a short example in the [HLint repo itself](https://github.com/ndmitchell/hlint/blob/master/.hlint.yaml#L10-L32). As an example of restricting extensions:

```yaml
- extensions:
  - default: false
  - name: [DeriveDataTypeable, GeneralizedNewtypeDeriving]
  - {name: CPP, within: CompatLayer}
```

The above block declares that GHC extensions are not allowed by default, apart from `DeriveDataTypeable` and `GeneralizedNewtypeDeriving` which are available everywhere. The `CPP` extension is only allowed in the module `CompatLayer`. Much like `extensions`, you can use `flags` to limit the `GHC_OPTIONS` flags that are allowed to occur. You can also ban certain functions:

```yaml
- functions:
  - {name: nub, within: []}
  - {name: unsafePerformIO, within: CompatLayer}
```

This declares that the `nub` function can't be used in any modules, and thus is banned from the code. That's probably a good idea, as most people should use an alternative that isn't _O(n^2)_ (e.g. [`nubOrd`](https://hackage.haskell.org/package/extra/docs/Data-List-Extra.html#v:nubOrd)). We also whitelist where `unsafePerformIO` can occur, ensuring that there can be a centrally reviewed location to declare all such instances. Function names can be given qualified, e.g. `Data.List.head`, but note that functions available through multiple exports (e.g. `head` is also available from `Prelude`) should be listed through all paths they are likely to be obtained, as the HLint qualified matching is unaware of re-exports.

Finally, we can restrict the use of modules with:

```yaml
- modules:
  - {name: [Data.Set, Data.HashSet], as: Set}
  - {name: Control.Arrow, within: []}
  - {name: Control.Monad.State, badidents: [modify, get, put], message: "Use Control.Monad.State.Class instead"}
  - {name: Control.Exception, only: [Exception], message: "Use UnliftIO.Exception instead"}
```

This fragment adds the following hints:
* Requires that all imports of `Set` must be `qualified Data.Set as Set`, enforcing consistency
* Ensures the module `Control.Arrow` can't be used anywhere
* Prevents explicit imports of the given identifiers from `Control.Monad.State` (e.g. to prevent people from importing reexported identifiers).
* Prevents all imports from `Control.Exception`, except `Exception`

You can customize the `Note:` for restricted modules, functions and extensions, by providing a `message` field (default: `may break the code`).

Other options are available:

- `asRequired`: boolean.

  If true, `as` alias is required. Ignored if `as` is empty.
- `importStyle`: one of `'qualified'`, `'unqualified'`, `'explicit'`,
  `'explicitOrQualified'`, `'unrestricted'`.

  The preferred import style.

  `explicitOrQualified` accepts both `import Foo (a,b,c)` and `import qualified Foo`, but not `import Foo` or `import Foo hiding (x)`.

  `explicit` is basically the same, but doesn't accept `import qualified`.

  `qualified` and `unqualified` do not care about the import list at all.
- `qualifiedStyle`: either `'pre'`, `'post'` or `'unrestricted'`; how should the module be qualified? This option also affects how suggestions are formatted.

For example:

```yaml
- modules:
  - {name: [Data.Set, Data.HashSet], as: Set, asRequired: true}
  - {name: Debug, importStyle: explicitOrQualified}
  - {name: Unsafe, importStyle: qualified, qualifiedStyle: post, as: Unsafe}
  - {name: Prelude, importStyle: unqualified}
```

This:
* Requires `Data.Set` and `Data.HashSet` to be imported with alias `Set`; if imported without alias, a warning is generated.
* Says that `Debug` must be imported either qualified with post-qualification, i.e. `import Debug qualified`, or with an explicit import list, e.g. `Debug (debugPrint)`.
* Requires that `Unsafe` must always be imported qualified, and can't be aliased.
* Forbids `import qualified Prelude` and `import Prelude qualified` (with or without explicit import list).

You can match on module names using [glob](https://en.wikipedia.org/wiki/Glob_(programming))-style wildcards. Module names are treated like file paths, except that periods in module names are like directory separators in file paths. So `**.*Spec` will match `Spec`, `PreludeSpec`, `Data.ListSpec`, and many more. But `*Spec` won't match `Data.ListSpec` because of the separator. See [the filepattern library](https://hackage.haskell.org/package/filepattern) for a more thorough description of the matching.

Restrictions are unified between wildcard and specific matches. With `asRequired`, `importStyle` and `qualifiedStyle` fields, the more specific option takes precedence. The list fields are merged. With multiple wildcard matches, the precedence between them is not guaranteed (but in practice, names are sorted in the reverse lexicograpic order, and the first one wins -- which hopefully means the more specific one more often than not)

If the same module is specified multiple times, for `asRequired`, `importStyle`
and `qualifiedStyle` fields, only the first definition will take effect.

```yaml
- modules:
  - {name: [Data.Map, Data.Map.*], as: Map}
  - {name: Test.Hspec, within: **.*Spec }
  - {name: '**', importStyle: post}
```

## Hacking HLint

Contributions to HLint are most welcome, following [my standard contribution guidelines](https://github.com/ndmitchell/neil/blob/master/README.md#contributions).

### How to run tests

You can run the tests either from within a `ghci` session by typing `:test` or by running the standalone binary's tests via `cabal run -- hlint --test` or `stack run -- hlint --test`. After changing hints, you will need to regenerate the [hints.md](hints.md) file with `hlint --generate-summary`.

### How to add tests

New tests for individual hints can be added directly to source and hint files by adding annotations bracketed in `<TEST></TEST>` code comment blocks. Here are some examples:

```haskell
{-
    Tests to check the zipFrom hint works

<TEST>
zip [1..length x] x -- zipFrom 1 x
zip [1..length y] x
zip [1..length x] x -- ??? @Warning
</TEST>
-}
```

The general syntax is `lhs -- rhs` with `lhs` being the expression you expect to be rewritten as `rhs`. The absence of `rhs` means you expect no hints to fire. In addition `???` lets you assert a warning without a particular suggestion, while `@` tags require a specific severity -- both these features are used less commonly.

### Printing abstract syntax

Getting started on problems in HLint often means wanting to inspect a GHC parse tree to get a sense of what it looks like (to see how to match on it for example). Given a source program `Foo.hs` (say), you can get GHC to print a textual representation of `Foo`'s AST via the `-ddump-parsed-ast` flag e.g. `ghc -fforce-recomp -ddump-parsed-ast -c Foo.hs`.

When you have an [`HsSyn`](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/hs-syn-type) term in your program, it's quite common to want to print it (e.g. via `Debug.Trace.trace`). Types in `HsSyn` aren't in [`Show`](https://hoogle.haskell.org/?hoogle=Show). Not all types in `HsSyn` are [`Outputable`](https://hoogle.haskell.org/?hoogle=Outputable) but when they are you can call `ppr` to get `SDoc`s. This idiom is common enough that there exists [`unsafePrettyPrint`](https://hackage.haskell.org/package/ghc-lib-parser-ex-8.10.0.16/docs/Language-Haskell-GhclibParserEx-GHC-Utils-Outputable.html#v:unsafePrettyPrint). The function [`showAstData`](https://hoogle.haskell.org/?hoogle=showAstData) can be called on any `HsSyn` term to get output like with the `dump-parsed-ast` flag. The `showAstData` approach is preferable to `ppr` when both choices exist in that two ASTs that differ only in fixity arrangements will render differently with the former.

### Generating the hints summary

The hints summary is an auto-generated list of hlint's builtin hints. This can be generated with `hlint --generate-summary`, which will output the summary to `hints.md`.

### Acknowledgements

Many improvements to this program have been made by [Niklas Broberg](http://www.nbroberg.se) in response to feature requests. Additionally, many people have provided help and patches, including Lennart Augustsson, Malcolm Wallace, Henk-Jan van Tuyl, Gwern Branwen, Alex Ott, Andy Stewart, Roman Leshchinskiy, Johannes Lippmann, Iustin Pop, Steve Purcell, Mitchell Rosen and others.
