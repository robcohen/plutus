{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {};
    package = {
      specVersion = "2.4";
      identifier = { name = "language-plutus-core"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "vanessa.mchale@iohk.io";
      author = "Vanessa McHale";
      homepage = "";
      url = "";
      synopsis = "Language library for Plutus Core";
      description = "Pretty-printer, parser, and typechecker for Plutus Core.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = "";
      dataFiles = [];
      extraSrcFiles = [
        "src/costModel.json"
        "language-plutus-core/src/costModel.json"
        "budgeting-bench/csvs/*.csv"
        "language-plutus-core/budgeting-bench/csvs/*.csv"
        "budgeting-bench/*.R"
        "language-plutus-core/budgeting-bench/*.R"
        ];
      extraTmpFiles = [];
      extraDocFiles = [ "README.md" ];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."algebraic-graphs" or (errorHandler.buildDepError "algebraic-graphs"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."barbies" or (errorHandler.buildDepError "barbies"))
          (hsPkgs."bimap" or (errorHandler.buildDepError "bimap"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."composition-prelude" or (errorHandler.buildDepError "composition-prelude"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."dependent-map" or (errorHandler.buildDepError "dependent-map"))
          (hsPkgs."dependent-sum" or (errorHandler.buildDepError "dependent-sum"))
          (hsPkgs."dependent-sum-template" or (errorHandler.buildDepError "dependent-sum-template"))
          (hsPkgs."deriving-aeson" or (errorHandler.buildDepError "deriving-aeson"))
          (hsPkgs."deriving-compat" or (errorHandler.buildDepError "deriving-compat"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
          (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
          (hsPkgs."monoidal-containers" or (errorHandler.buildDepError "monoidal-containers"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nonempty-containers" or (errorHandler.buildDepError "nonempty-containers"))
          (hsPkgs."parser-combinators" or (errorHandler.buildDepError "parser-combinators"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."prettyprinter-configurable" or (errorHandler.buildDepError "prettyprinter-configurable"))
          (hsPkgs."recursion-schemes" or (errorHandler.buildDepError "recursion-schemes"))
          (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
          (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."th-lift" or (errorHandler.buildDepError "th-lift"))
          (hsPkgs."th-lift-instances" or (errorHandler.buildDepError "th-lift-instances"))
          (hsPkgs."th-utilities" or (errorHandler.buildDepError "th-utilities"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        build-tools = [
          (hsPkgs.buildPackages.alex or (pkgs.buildPackages.alex or (errorHandler.buildToolDepError "alex")))
          (hsPkgs.buildPackages.happy or (pkgs.buildPackages.happy or (errorHandler.buildToolDepError "happy")))
          ];
        buildable = true;
        modules = [
          "Language/PlutusCore/Pretty/ConfigName"
          "Language/PlutusCore/Core/Type"
          "Language/PlutusCore/Core/Plated"
          "Language/PlutusCore/Core/Instance/Eq"
          "Language/PlutusCore/Core/Instance/Pretty/Classic"
          "Language/PlutusCore/Core/Instance/Pretty/Common"
          "Language/PlutusCore/Core/Instance/Pretty/Default"
          "Language/PlutusCore/Core/Instance/Pretty/Plc"
          "Language/PlutusCore/Core/Instance/Pretty/Readable"
          "Language/PlutusCore/Core/Instance/Pretty"
          "Language/PlutusCore/Core/Instance/Recursive"
          "Language/PlutusCore/Core/Instance"
          "Language/PlutusCore/Constant/Apply"
          "Language/PlutusCore/Constant/Dynamic/BuiltinName"
          "Language/PlutusCore/Constant/Dynamic/Call"
          "Language/PlutusCore/Constant/Dynamic/Emit"
          "Language/PlutusCore/Constant/Dynamic/OffChain"
          "Language/PlutusCore/Constant/Function"
          "Language/PlutusCore/Constant/Name"
          "Language/PlutusCore/Constant/Typed"
          "Language/PlutusCore/Lexer/Type"
          "Language/PlutusCore/Eq"
          "Language/PlutusCore/Mark"
          "Language/PlutusCore/Pretty/Classic"
          "Language/PlutusCore/Pretty/Default"
          "Language/PlutusCore/Pretty/Plc"
          "Language/PlutusCore/Pretty/PrettyConst"
          "Language/PlutusCore/Pretty/Readable"
          "Language/PlutusCore/Pretty/Utils"
          "Language/PlutusCore/Universe/Core"
          "Language/PlutusCore/Universe/Default"
          "Language/PlutusCore/Error"
          "Language/PlutusCore/Size"
          "Language/PlutusCore/TypeCheck/Internal"
          "Language/PlutusCore/TypeCheck"
          "Language/PlutusCore/Analysis/Definitions"
          "Language/PlutusCore/Examples/Data/InterList"
          "Language/PlutusCore/Examples/Data/TreeForest"
          "Language/PlutusCore/Examples/Data/Vec"
          "Language/PlutusCore/Generators/Internal/Denotation"
          "Language/PlutusCore/Generators/Internal/Dependent"
          "Language/PlutusCore/Generators/Internal/Entity"
          "Language/PlutusCore/Generators/Internal/TypeEvalCheck"
          "Language/PlutusCore/Generators/Internal/TypedBuiltinGen"
          "Language/PlutusCore/Generators/Internal/Utils"
          "Language/PlutusIR/Analysis/Dependencies"
          "Language/PlutusIR/Analysis/Usages"
          "Language/PlutusIR/Compiler/Error"
          "Language/PlutusIR/Compiler/Let"
          "Language/PlutusIR/Compiler/Datatype"
          "Language/PlutusIR/Compiler/Provenance"
          "Language/PlutusIR/Compiler/Recursion"
          "Language/PlutusIR/Compiler/Types"
          "Language/PlutusIR/Compiler/Lower"
          "Data/Aeson/THReader"
          "Data/Functor/Foldable/Monadic"
          "Language/PlutusCore"
          "Language/PlutusCore/Quote"
          "Language/PlutusCore/MkPlc"
          "Language/PlutusCore/Evaluation/Machine/Ck"
          "Language/PlutusCore/Evaluation/Machine/Cek"
          "Language/PlutusCore/Evaluation/Machine/ExBudgeting"
          "Language/PlutusCore/Evaluation/Machine/ExBudgetingDefaults"
          "Language/PlutusCore/Evaluation/Machine/Exception"
          "Language/PlutusCore/Evaluation/Machine/ExMemory"
          "Language/PlutusCore/Evaluation/Evaluator"
          "Language/PlutusCore/Evaluation/Result"
          "Language/PlutusCore/Check/Value"
          "Language/PlutusCore/Check/Normal"
          "Language/PlutusCore/CBOR"
          "Language/PlutusCore/Constant"
          "Language/PlutusCore/Constant/Dynamic"
          "Language/PlutusCore/Universe"
          "Language/PlutusCore/Rename/Internal"
          "Language/PlutusCore/Rename/Monad"
          "Language/PlutusCore/Rename"
          "Language/PlutusCore/Normalize"
          "Language/PlutusCore/Normalize/Internal"
          "Language/PlutusCore/Pretty"
          "Language/PlutusCore/View"
          "Language/PlutusCore/Subst"
          "Language/PlutusCore/Name"
          "Language/PlutusCore/Core"
          "Language/PlutusCore/DeBruijn"
          "Language/PlutusCore/Check/Uniques"
          "Language/PlutusCore/FsTree"
          "Language/PlutusCore/StdLib/Data/Bool"
          "Language/PlutusCore/StdLib/Data/ChurchNat"
          "Language/PlutusCore/StdLib/Data/Function"
          "Language/PlutusCore/StdLib/Data/Integer"
          "Language/PlutusCore/StdLib/Data/List"
          "Language/PlutusCore/StdLib/Data/Nat"
          "Language/PlutusCore/StdLib/Data/Sum"
          "Language/PlutusCore/StdLib/Data/Unit"
          "Language/PlutusCore/StdLib/Data/ScottUnit"
          "Language/PlutusCore/StdLib/Everything"
          "Language/PlutusCore/StdLib/Meta"
          "Language/PlutusCore/StdLib/Meta/Data/Tuple"
          "Language/PlutusCore/StdLib/Meta/Data/Function"
          "Language/PlutusCore/StdLib/Type"
          "Language/PlutusCore/Examples/Everything"
          "Language/PlutusCore/Generators"
          "Language/PlutusCore/Generators/AST"
          "Language/PlutusCore/Generators/Interesting"
          "Language/PlutusCore/Generators/Test"
          "Language/PlutusCore/Lexer"
          "Language/PlutusCore/Parser"
          "Language/PlutusIR"
          "Language/PlutusIR/Compiler"
          "Language/PlutusIR/Compiler/Names"
          "Language/PlutusIR/Compiler/Definitions"
          "Language/PlutusIR/Generators/AST"
          "Language/PlutusIR/Parser"
          "Language/PlutusIR/MkPir"
          "Language/PlutusIR/Value"
          "Language/PlutusIR/Optimizer/DeadCode"
          "Language/PlutusIR/Transform/Substitute"
          "Language/PlutusIR/Transform/ThunkRecursions"
          "Language/PlutusIR/Transform/Rename"
          "Language/PlutusIR/Transform/NonStrict"
          "Language/PlutusIR/Transform/LetFloat"
          "PlutusPrelude"
          "Common"
          "Data/ByteString/Lazy/Hash"
          "PlcTestUtils"
          "Crypto"
          "Data/Text/Prettyprint/Doc/Custom"
          ];
        hsSourceDirs = [
          "src"
          "prelude"
          "stdlib"
          "examples"
          "generators"
          "common"
          "plutus-ir"
          ];
        };
      exes = {
        "language-plutus-core-generate-evaluation-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."language-plutus-core" or (errorHandler.buildDepError "language-plutus-core"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          hsSourceDirs = [ "generate-evaluation-test" ];
          mainPath = [ "Main.hs" ];
          };
        "plc" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."language-plutus-core" or (errorHandler.buildDepError "language-plutus-core"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            ];
          buildable = true;
          hsSourceDirs = [ "exe" ];
          mainPath = [ "Main.hs" ];
          };
        };
      tests = {
        "language-plutus-core-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."language-plutus-core" or (errorHandler.buildDepError "language-plutus-core"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."tuple" or (errorHandler.buildDepError "tuple"))
            ];
          buildable = true;
          modules = [
            "Evaluation/ApplyBuiltinName"
            "Evaluation/DynamicBuiltins/Common"
            "Evaluation/DynamicBuiltins/Definition"
            "Evaluation/DynamicBuiltins/MakeRead"
            "Evaluation/DynamicBuiltins"
            "Evaluation/Golden"
            "Evaluation/Machines"
            "Evaluation/Spec"
            "Normalization/Check"
            "Normalization/Type"
            "Pretty/Readable"
            "Check/Spec"
            "TypeSynthesis/Spec"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        "plutus-ir-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."language-plutus-core" or (errorHandler.buildDepError "language-plutus-core"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            ];
          buildable = true;
          modules = [ "OptimizerSpec" "TransformSpec" "ParserSpec" "TestLib" ];
          hsSourceDirs = [ "plutus-ir-test" ];
          mainPath = [ "Spec.hs" ];
          };
        "language-plutus-core-test-cost-model" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."barbies" or (errorHandler.buildDepError "barbies"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."language-plutus-core" or (errorHandler.buildDepError "language-plutus-core"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."deriving-aeson" or (errorHandler.buildDepError "deriving-aeson"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."cassava" or (errorHandler.buildDepError "cassava"))
            (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."language-plutus-core" or (errorHandler.buildDepError "language-plutus-core"))
            (hsPkgs."inline-r" or (errorHandler.buildDepError "inline-r"))
            ];
          buildable = true;
          modules = [ "CostModelCreation" ];
          hsSourceDirs = [ "test-cost-model" "cost-model-creation" ];
          mainPath = [ "TestCostModels.hs" ];
          };
        };
      benchmarks = {
        "language-plutus-core-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."language-plutus-core" or (errorHandler.buildDepError "language-plutus-core"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            ];
          buildable = true;
          hsSourceDirs = [ "bench" ];
          };
        "language-plutus-core-weigh" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."language-plutus-core" or (errorHandler.buildDepError "language-plutus-core"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."weigh" or (errorHandler.buildDepError "weigh"))
            ];
          buildable = true;
          hsSourceDirs = [ "weigh" ];
          };
        "language-plutus-core-budgeting-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."language-plutus-core" or (errorHandler.buildDepError "language-plutus-core"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            ];
          buildable = true;
          hsSourceDirs = [ "budgeting-bench" ];
          };
        "language-plutus-core-create-cost-model" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."barbies" or (errorHandler.buildDepError "barbies"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."language-plutus-core" or (errorHandler.buildDepError "language-plutus-core"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."inline-r" or (errorHandler.buildDepError "inline-r"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."deriving-aeson" or (errorHandler.buildDepError "deriving-aeson"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."cassava" or (errorHandler.buildDepError "cassava"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."deriving-aeson" or (errorHandler.buildDepError "deriving-aeson"))
            (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            ];
          buildable = true;
          modules = [ "CostModelCreation" ];
          hsSourceDirs = [ "create-cost-model" "cost-model-creation" ];
          };
        };
      };
    } // rec {
    src = (pkgs.lib).mkDefault ./language-plutus-core;
    }