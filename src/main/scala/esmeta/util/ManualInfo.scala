package esmeta.util

import esmeta.MANUALS_DIR
import esmeta.analyzer.tychecker.TyChecker.Ignore
import esmeta.spec.Spec
import esmeta.test262.util.ManualConfig
import esmeta.ty.TyModel
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import java.io.File

/** manual information helpers */
object ManualInfo {

  /** A list of algorithms to ignore for tycheck */
  lazy val tycheckIgnore: Ignore = Ignore(s"$MANUALS_DIR/tycheck-ignore.json")

  /** manual algorithm files */
  lazy val algoFiles: List[String] = getFileNames(algoFilter)

  /** manual IR function files */
  lazy val funcFiles: List[String] = getFileNames(irFilter)

  /** manual compilation rule */
  lazy val compileRule: CompileRule =
    readJson[CompileRule](s"$MANUALS_DIR/rule.json")
  type CompileRule = Map[String, Map[String, String]]

  /** bugfix patch map */
  lazy val bugfixPatchMap: Map[String, String] = (for {
    file <- getFiles(patchFilter)
    name = file.getName
    pattern = "(.*).patch".r
    hash <- name match
      case pattern(hash) => Some(hash)
      case _             => None
  } yield hash -> file.toString).toMap

  /** type model */
  lazy val tyModel: TyModel =
    import esmeta.ty.util.JsonProtocol.given
    TyModel.from(
      """
      // -----------------------------------------------------------------------------
      // Symbol
      // -----------------------------------------------------------------------------

      // https://tc39.es/ecma262/#sec-ecmascript-language-types-symbol-type
      type Symbol {
        Description: String | Undefined;
      }

      // https://tc39.es/ecma262/#table-globalsymbolregistry-record-fields
      type GlobalSymbolRegistryRecord {
        Key: String;
        Symbol: Record[Symbol];
      }

      // -----------------------------------------------------------------------------
      // Completion Records
      // -----------------------------------------------------------------------------

      // https://tc39.es/ecma262/#table-completion-record-fields
      type CompletionRecord {
        Type: Enum[~normal~, ~break~, ~continue~, ~return~, ~throw~];
        Value;
        Target: String | Enum[~empty~];
      }

      // https://tc39.es/ecma262/#sec-completion-record-specification-type
      type NormalCompletion extends CompletionRecord {
        Type: Enum[~normal~];
        Value;
        Target: Enum[~empty~];
      }

      // https://tc39.es/ecma262/#sec-completion-record-specification-type
      type AbruptCompletion extends CompletionRecord {
        Type: Enum[~break~, ~continue~, ~return~, ~throw~];
        Value: ESValue | Enum[~empty~];
        Target: String | Enum[~empty~];
      }

      type BreakCompletion extends AbruptCompletion {
        Type: Enum[~break~];
        Value: Enum[~empty~];
        Target: String | Enum[~empty~];
      }

      type ContinueCompletion extends AbruptCompletion {
        Type: Enum[~continue~];
        Value: Enum[~empty~];
        Target: String | Enum[~empty~];
      }

      type ReturnCompletion extends AbruptCompletion {
        Type: Enum[~return~];
        Value: ESValue;
        Target: Enum[~empty~];
      }

      type ThrowCompletion extends AbruptCompletion {
        Type: Enum[~throw~];
        Value: ESValue;
        Target: Enum[~empty~];
      }

      // -----------------------------------------------------------------------------
      // Reference Records
      // -----------------------------------------------------------------------------

      // https://tc39.es/ecma262/#table-reference-record-fields
      type ReferenceRecord {
        Base: ESValue | Record[EnvironmentRecord] | Enum[~unresolvable~];
        // TODO: `Record[Symbol] | String` to `ESValue` in latest version
        ReferencedName: Record[Symbol | PrivateName] | String;
        Strict: Boolean;
        ThisValue: ESValue | Enum[~empty~];
      }

      // MANUAL MODELING
      type IdentifierReferenceRecord extends ReferenceRecord {
        Base: Record[EnvironmentRecord];
        ReferencedName: String;
      }

      // MANUAL MODELING
      type PropertyReferenceRecord extends ReferenceRecord {
        Base: ESValue;
      }

      // MANUAL MODELING
      type UnresolvableReferenceRecord extends ReferenceRecord {
        Base: Enum[~unresolvable~];
        ReferencedName: String;
      }

      // MANUAL MODELING
      type PrivateReferenceRecord extends ReferenceRecord {
        ReferencedName: Record[PrivateName];
      }

      // https://tc39.es/ecma262/#super-reference-record
      type SuperReferenceRecord extends ReferenceRecord {
        Base: ESValue;
        ThisValue: ESValue;
      }

      // https://tc39.es/ecma262/#sec-private-names
      type PrivateName {
        Description: String;
      }

      // -----------------------------------------------------------------------------
      // Property Descriptor
      // -----------------------------------------------------------------------------

      // https://tc39.es/ecma262/#table-object-property-attributes
      type PropertyDescriptor {
        Value?: ESValue;
        Writable?: Boolean;
        Get?: Record[Object] | Undefined;
        Set?: Record[Object] | Undefined;
        Enumerable?: Boolean;
        Configurable?: Boolean;
      }

      // https://tc39.es/ecma262/#table-privateelement-fields
      type PrivateElement {
        Key: Record[PrivateName];
        Kind: Enum[~field~, ~method~, ~accessor~];
        Value?: ESValue;
        Get?: Record[FunctionObject] | Undefined;
        Set?: Record[FunctionObject] | Undefined;
      }

      // -----------------------------------------------------------------------------
      // ClassFieldDefinition Record
      // -----------------------------------------------------------------------------

      // https://tc39.es/ecma262/#table-classfielddefinition-fields
      type ClassFieldDefinitionRecord {
        Name: Record[PrivateName | Symbol] | String;
        Initializer: Record[ECMAScriptFunctionObject] | Enum[~empty~];
      }

      // -----------------------------------------------------------------------------
      // ClassStaticBlockDefinition Record
      // -----------------------------------------------------------------------------

      // https://tc39.es/ecma262/#table-classstaticblockdefinition-record-fields
      type ClassStaticBlockDefinitionRecord {
        BodyFunction: Record[FunctionObject];
      }

      // -----------------------------------------------------------------------------
      // Environment Record
      // -----------------------------------------------------------------------------

      // https://tc39.es/ecma262/#sec-environment-records
      type EnvironmentRecord {
        abstract def HasBinding;
        abstract def CreateMutableBinding;
        abstract def CreateImmutableBinding;
        abstract def InitializeBinding;
        abstract def SetMutableBinding;
        abstract def GetBindingValue;
        abstract def DeleteBinding;
        abstract def HasThisBinding;
        abstract def HasSuperBinding;
        abstract def WithBaseObject;
        OuterEnv: Record[EnvironmentRecord] | Null;
        __MAP__: Map[String -> Record[Binding]];
      }

      // https://tc39.es/ecma262/2024/#sec-declarative-environment-records
      type DeclarativeEnvironmentRecord extends EnvironmentRecord {
        def CreateImmutableBinding;
        def CreateMutableBinding;
        def DeleteBinding;
        def GetBindingValue;
        def HasBinding;
        def HasSuperBinding;
        def HasThisBinding;
        def InitializeBinding;
        def SetMutableBinding;
        def WithBaseObject;
      }

      // https://tc39.es/ecma262/2024/#sec-object-environment-records
      type ObjectEnvironmentRecord extends EnvironmentRecord {
        def CreateMutableBinding;
        def DeleteBinding;
        def GetBindingValue;
        def HasBinding;
        def HasSuperBinding;
        def HasThisBinding;
        def InitializeBinding;
        def SetMutableBinding;
        def WithBaseObject;
        BindingObject: Record[Object];
        IsWithEnvironment: Boolean;
      }

      // https://tc39.es/ecma262/2024/#sec-function-environment-records
      type FunctionEnvironmentRecord extends DeclarativeEnvironmentRecord {
        def BindThisValue;
        def GetSuperBase;
        def GetThisBinding;
        def HasSuperBinding;
        def HasThisBinding;
        FunctionObject: Record[ECMAScriptFunctionObject];
        NewTarget: Record[Object] | Undefined;
        ThisBindingStatus: Enum[~initialized~, ~lexical~, ~uninitialized~];
        ThisValue: ESValue;
      }

      // https://tc39.es/ecma262/2024/#sec-global-environment-records
      type GlobalEnvironmentRecord extends EnvironmentRecord {
        def CanDeclareGlobalFunction;
        def CanDeclareGlobalVar;
        def CreateGlobalFunctionBinding;
        def CreateGlobalVarBinding;
        def CreateImmutableBinding;
        def CreateMutableBinding;
        def DeleteBinding;
        def GetBindingValue;
        def GetThisBinding;
        def HasBinding;
        def HasLexicalDeclaration;
        def HasRestrictedGlobalProperty;
        def HasSuperBinding;
        def HasThisBinding;
        def HasVarDeclaration;
        def InitializeBinding;
        def SetMutableBinding;
        def WithBaseObject;
        DeclarativeRecord: Record[DeclarativeEnvironmentRecord];
        GlobalThisValue: Record[Object];
        ObjectRecord: Record[ObjectEnvironmentRecord];
        VarNames: List[String];
      }

      // https://tc39.es/ecma262/2024/#sec-module-environment-records
      type ModuleEnvironmentRecord extends DeclarativeEnvironmentRecord {
        def CreateImportBinding;
        def GetBindingValue;
        def GetThisBinding;
        def HasThisBinding;
      }

      type Binding {
        BoundValue: ESValue;
        initialized: Boolean;
        strict: Boolean;
        mutable: Boolean;
      }

      type ImmutableBinding extends Binding {
        strict: Boolean;
        mutable: False;
      }

      type MutableBinding extends Binding {
        strict: False;
        mutable: True;
      }

      // -----------------------------------------------------------------------------
      // Objects
      // -----------------------------------------------------------------------------

      // https://tc39.es/ecma262/2024/#sec-object-internal-methods-and-internal-slots
      type Object {
        abstract def GetPrototypeOf;
        abstract def SetPrototypeOf;
        abstract def IsExtensible;
        abstract def PreventExtensions;
        abstract def GetOwnProperty;
        abstract def DefineOwnProperty;
        abstract def HasProperty;
        abstract def Get;
        abstract def Set;
        abstract def Delete;
        abstract def OwnPropertyKeys;
        Prototype?: Record[Object] | Null;
        Extensible?: Boolean;
        PrivateElements: List[Record[PrivateElement]];
        __MAP__: Map[Record[Symbol] | String -> Record[PropertyDescriptor]];
      }

      // https://tc39.es/ecma262/2024/#function-object
      type FunctionObject = Object { Call; }

      // https://tc39.es/ecma262/2024/#constructor
      type Constructor = FunctionObject { Construct; }

      // https://tc39.es/ecma262/2024/#ordinary-object
      type OrdinaryObject extends Object {
        def GetPrototypeOf;
        def SetPrototypeOf;
        def IsExtensible;
        def PreventExtensions;
        def GetOwnProperty;
        def DefineOwnProperty;
        def HasProperty;
        def Get;
        def Set;
        def Delete;
        def OwnPropertyKeys;
        Prototype: Record[Object] | Null;
        Extensible: Boolean;
        ParameterMap?: Record[Object] | Undefined;
      }

      // https://tc39.es/ecma262/2024/#sec-ecmascript-function-objects
      type ECMAScriptFunctionObject extends OrdinaryObject {
        def Call;
        def Construct?;
        Environment: Record[EnvironmentRecord];
        PrivateEnvironment: Record[PrivateEnvironmentRecord] | Null;
        FormalParameters: Ast;
        ECMAScriptCode: Ast;
        ConstructorKind?: Enum[~base~, ~derived~];
        Realm: Record[RealmRecord];
        ScriptOrModule: Record[ModuleRecord | ScriptRecord] | Null; // TODO: PR - Null
        ThisMode: Enum[~global~, ~lexical~, ~strict~];
        Strict: Boolean;
        HomeObject: Record[Object] | Undefined; // TODO: PR - Undefined
        SourceText: String;
        Fields: List[Record[ClassFieldDefinitionRecord]];
        PrivateMethods: List[Record[PrivateElement]];
        ClassFieldInitializerName: Record[PrivateName | Symbol] | Enum[~empty~] | String;
        IsClassConstructor: Boolean;
      }

      // https://tc39.es/ecma262/2024/#sec-built-in-function-objects
      type BuiltinFunctionObject extends OrdinaryObject {
        def Call;
        def Construct?;
        Realm: Record[RealmRecord];
        InitialName: String | Null;
        __CODE__: Clo[(ESValue, List[ESValue], Record[Constructor] | Undefined) => Normal[ESValue] | Throw];
      }

      // https://tc39.es/ecma262/2024/#sec-promise.all-resolve-element-functions
      type PromiseAllResolveElementFunction extends BuiltinFunctionObject {
        Index: NonNegInt;
        Values: List[ESValue];
        Capability: Record[PromiseCapabilityRecord];
        RemainingElements: Record[{ Value: Int }];
        AlreadyCalled: Boolean;
      }

      type ExoticObject extends Object {
        Prototype?: Record[Object] | Null;
      }

      // https://tc39.es/ecma262/2024/#sec-bound-function-exotic-objects
      type BoundFunctionExoticObject extends ExoticObject {
        def GetPrototypeOf = Record[OrdinaryObject].GetPrototypeOf;
        def SetPrototypeOf = Record[OrdinaryObject].SetPrototypeOf;
        def IsExtensible = Record[OrdinaryObject].IsExtensible;
        def PreventExtensions = Record[OrdinaryObject].PreventExtensions;
        def GetOwnProperty = Record[OrdinaryObject].GetOwnProperty;
        def DefineOwnProperty = Record[OrdinaryObject].DefineOwnProperty;
        def HasProperty = Record[OrdinaryObject].HasProperty;
        def Get = Record[OrdinaryObject].Get;
        def Set = Record[OrdinaryObject].Set;
        def Delete = Record[OrdinaryObject].Delete;
        def OwnPropertyKeys = Record[OrdinaryObject].OwnPropertyKeys;
        def Call;
        def Construct?;
        BoundTargetFunction: Record[FunctionObject];
        BoundThis: ESValue;
        BoundArguments: List[ESValue];
      }

      // https://tc39.es/ecma262/2024/#sec-array-exotic-objects
      type Array extends ExoticObject {
        def GetPrototypeOf = Record[OrdinaryObject].GetPrototypeOf;
        def SetPrototypeOf = Record[OrdinaryObject].SetPrototypeOf;
        def IsExtensible = Record[OrdinaryObject].IsExtensible;
        def PreventExtensions = Record[OrdinaryObject].PreventExtensions;
        def GetOwnProperty = Record[OrdinaryObject].GetOwnProperty;
        def DefineOwnProperty;
        def HasProperty = Record[OrdinaryObject].HasProperty;
        def Get = Record[OrdinaryObject].Get;
        def Set = Record[OrdinaryObject].Set;
        def Delete = Record[OrdinaryObject].Delete;
        def OwnPropertyKeys = Record[OrdinaryObject].OwnPropertyKeys;
        Prototype: Record[Object];
      }

      // https://tc39.es/ecma262/2024/#sec-string-exotic-objects
      type StringExoticObject extends ExoticObject {
        def GetPrototypeOf = Record[OrdinaryObject].GetPrototypeOf;
        def SetPrototypeOf = Record[OrdinaryObject].SetPrototypeOf;
        def IsExtensible = Record[OrdinaryObject].IsExtensible;
        def PreventExtensions = Record[OrdinaryObject].PreventExtensions;
        def GetOwnProperty;
        def DefineOwnProperty;
        def HasProperty = Record[OrdinaryObject].HasProperty;
        def Get = Record[OrdinaryObject].Get;
        def Set = Record[OrdinaryObject].Set;
        def Delete = Record[OrdinaryObject].Delete;
        def OwnPropertyKeys;
        StringData: String;
      }

      // https://tc39.es/ecma262/2024/#sec-arguments-exotic-objects
      type ArgumentsExoticObject extends ExoticObject {
        def GetPrototypeOf = Record[OrdinaryObject].GetPrototypeOf;
        def SetPrototypeOf = Record[OrdinaryObject].SetPrototypeOf;
        def IsExtensible = Record[OrdinaryObject].IsExtensible;
        def PreventExtensions = Record[OrdinaryObject].PreventExtensions;
        def GetOwnProperty;
        def DefineOwnProperty;
        def HasProperty = Record[OrdinaryObject].HasProperty;
        def Get;
        def Set;
        def Delete;
        def OwnPropertyKeys = Record[OrdinaryObject].OwnPropertyKeys;
        ParameterMap: Record[Object];
      }

      // https://tc39.es/ecma262/#sec-typedarray-exotic-objects
      type TypedArray extends ExoticObject {
        def GetPrototypeOf = Record[OrdinaryObject].GetPrototypeOf;
        def SetPrototypeOf = Record[OrdinaryObject].SetPrototypeOf;
        def IsExtensible = Record[OrdinaryObject].IsExtensible;
        def PreventExtensions = Record[OrdinaryObject].PreventExtensions;
        def GetOwnProperty;
        def DefineOwnProperty;
        def HasProperty;
        def Get;
        def Set;
        def Delete;
        def OwnPropertyKeys;
        ViewedArrayBuffer: Record[ArrayBuffer | SharedArrayBuffer];
        ArrayLength: NonNegInt | Enum[~auto~];
        ByteOffset: NonNegInt;
        ContentType: Enum[~bigint~, ~number~];
        TypedArrayName: String;
        ByteLength: NonNegInt | Enum[~auto~];
      }

      // https://tc39.es/ecma262/#sec-module-namespace-exotic-objects
      type ModuleNamespaceExoticObject extends ExoticObject {
        def GetPrototypeOf;
        def SetPrototypeOf;
        def IsExtensible;
        def PreventExtensions;
        def GetOwnProperty;
        def DefineOwnProperty;
        def HasProperty;
        def Get;
        def Set;
        def Delete;
        def OwnPropertyKeys;
        Module: Record[ModuleRecord];
        Exports: List[String];
      }

      // https://tc39.es/ecma262/#sec-immutable-prototype-exotic-objects
      type ImmutablePrototypeExoticObject extends ExoticObject {
        def GetPrototypeOf = Record[OrdinaryObject].GetPrototypeOf;
        def SetPrototypeOf;
        def IsExtensible = Record[OrdinaryObject].IsExtensible;
        def PreventExtensions = Record[OrdinaryObject].PreventExtensions;
        def GetOwnProperty = Record[OrdinaryObject].GetOwnProperty;
        def DefineOwnProperty = Record[OrdinaryObject].DefineOwnProperty;
        def HasProperty = Record[OrdinaryObject].HasProperty;
        def Get = Record[OrdinaryObject].Get;
        def Set = Record[OrdinaryObject].Set;
        def Delete = Record[OrdinaryObject].Delete;
        def OwnPropertyKeys = Record[OrdinaryObject].OwnPropertyKeys;
      }

      // https://tc39.es/ecma262/#sec-proxy-object-internal-methods-and-internal-slots
      type ProxyExoticObject extends ExoticObject {
        def GetPrototypeOf;
        def SetPrototypeOf;
        def IsExtensible;
        def PreventExtensions;
        def GetOwnProperty;
        def DefineOwnProperty;
        def HasProperty;
        def Get;
        def Set;
        def Delete;
        def OwnPropertyKeys;
        def Call?;
        def Construct?;
        ProxyHandler: Record[Object] | Null;
        ProxyTarget: Record[Object] | Null;
      }

      // -----------------------------------------------------------------------------
      // Executable Code and Execution Contexts
      // -----------------------------------------------------------------------------

      // https://tc39.es/ecma262/#realm-record
      type RealmRecord {
        AgentSignifier: Record[AgentSignifier];
        Intrinsics: Map[String -> Record[Object]];
        GlobalObject: Record[Object] | Undefined;
        GlobalEnv: Record[GlobalEnvironmentRecord];
        TemplateMap: List[Record[{ Site: Ast[TemplateLiteral], Array: Record[Array] }]];
        LoadedModules: List[Record[{ Specifier: String, Module: Record[ModuleRecord] }]];
        HostDefined: Undefined;
      }

      // https://tc39.es/ecma262/#table-state-components-for-all-execution-contexts
      type ExecutionContext {
        // TODO code evaluation state
        Function: Record[FunctionObject] | Null;
        Realm: Record[RealmRecord];
        ScriptOrModule: Record[ModuleRecord | ScriptRecord] | Null;
        LexicalEnvironment?: Record[EnvironmentRecord];
        VariableEnvironment?: Record[EnvironmentRecord];
        PrivateEnvironment?: Record[PrivateEnvironmentRecord] | Null;
        Generator?: Record[Generator | AsyncGenerator];
      }

      // https://tc39.es/ecma262/#ecmascript-code-execution-context
      type ECMAScriptCodeExecutionContext extends ExecutionContext {
        LexicalEnvironment: Record[EnvironmentRecord];
        VariableEnvironment: Record[EnvironmentRecord];
        PrivateEnvironment: Record[PrivateEnvironmentRecord] | Null;
      }

      // https://tc39.es/ecma262/#privateenvironment-record
      type PrivateEnvironmentRecord {
        OuterPrivateEnvironment: Record[PrivateEnvironmentRecord] | Null;
        Names: List[Record[PrivateName]];
      }

      // https://tc39.es/ecma262/#table-additional-state-components-for-generator-execution-contexts
      type GeneratorExecutionContext extends ExecutionContext {
        Generator: Record[Generator | AsyncGenerator]; // TODO: PR - AsyncGenerator
      }

      // MANUAL MODELING FOR JOB QUEUE
      type JobRecord {
        Job: Clo;
        Realm: Record[RealmRecord];
        ScriptOrModule: Record[ScriptRecord | ModuleRecord];
      }

      // https://tc39.es/ecma262/#sec-promisecapability-records
      type JobCallbackRecord {
        Callback: Record[FunctionObject];
        HostDefined: Enum[~empty~];
      }

      // https://tc39.es/ecma262/#agent-record
      type AgentRecord {
        LittleEndian: Boolean;
        CanBlock: Boolean;
        Signifier: Record[AgentSignifier];
        IsLockFree1: Boolean;
        IsLockFree2: Boolean;
        IsLockFree8: Boolean;
        CandidateExecution: Record[CandidateExecutionRecord];
        KeptAlive: List[Record[Object | Symbol]];
      }

      // https://tc39.es/ecma262/#sec-candidate-executions
      type CandidateExecutionRecord {
        EventsRecords: List[Record[AgentEventsRecord]];
        ChosenValues: List[Record[ChosenValueRecord]];
      }

      // https://tc39.es/ecma262/#sec-agent-event-records
      type AgentEventsRecord {
        AgentSignifier: Record[AgentSignifier];
        EventList: List[Record[Event]];
        AgentSynchronizesWith: List[List[Record[SynchronizeEvent]]]; // TODO Pair
      }

      // https://tc39.es/ecma262/#sec-chosen-value-records
      type ChosenValueRecord {
        Event: Record[SharedDataBlockEvent];
        ChosenValue: List[Int]; // TODO Byte
      }

      // -----------------------------------------------------------------------------
      // Iterators
      // -----------------------------------------------------------------------------

      // https://tc39.es/ecma262/#sec-iterator-records
      type IteratorRecord {
        Iterator: Record[Object];
        NextMethod: ESValue;
        Done: Boolean;
      }

      // https://tc39.es/ecma262/#sec-properties-of-for-in-iterator-instances
      type ForInIterator extends OrdinaryObject {
        Object: Record[Object];
        ObjectWasVisited: Boolean;
        VisitedKeys: List[String];
        RemainingKeys: List[String];
      }

      // https://tc39.es/ecma262/#sec-iterator-interface
      type Iterator extends Object

      // -----------------------------------------------------------------------------
      // Scripts and Modules
      // -----------------------------------------------------------------------------

      // https://tc39.es/ecma262/#script-record
      type ScriptRecord {
        Realm: Record[RealmRecord] | Undefined;
        ECMAScriptCode: Ast[Script];
        LoadedModules: List[Record[{ Specifier: String, Module: Record[ModuleRecord] }]];
        HostDefined: Enum[~empty~];
      }

      // https://tc39.es/ecma262/#sec-abstract-module-records
      type ModuleRecord {
        abstract def LoadRequestedModules;
        abstract def GetExportedNames;
        abstract def ResolveExport;
        abstract def Link;
        abstract def Evaluate;
        Realm: Record[RealmRecord];
        Environment: Record[ModuleEnvironmentRecord] | Enum[~empty~];
        Namespace: Record[ModuleNamespaceExoticObject] | Enum[~empty~];
        HostDefined: Undefined;
      }

      // https://tc39.es/ecma262/#resolvedbinding-record
      type ResolvedBindingRecord {
        Module: Record[ModuleRecord];
        BindingName: String | Enum[~namespace~];
      }

      // https://tc39.es/ecma262/#sec-cyclic-module-records
      type CyclicModuleRecord extends ModuleRecord {
        def LoadRequestedModules;
        def Link;
        def Evaluate;
        abstract def InitializeEnvironment;
        abstract def ExecuteModule;
        Status: Enum[~evaluated~, ~evaluating-async~, ~evaluating~, ~linked~, ~linking~, ~unlinked~];
        EvaluationError: Abrupt[throw] | Enum[~empty~];
        DFSIndex: Enum[~empty~] | Int;
        DFSAncestorIndex: Enum[~empty~] | Int;
        RequestedModules: List[String];
        LoadedModules: List[Record[{ Specifier: String, Module: Record[ModuleRecordN] }]];
        CycleRoot: Record[CyclicModuleRecord] | Enum[~empty~];
        HasTLA: Boolean;
        AsyncEvaluation: Boolean;
        TopLevelCapability: Record[PromiseCapabilityRecord] | Enum[~empty~];
        AsyncParentModules: List[Record[CyclicModuleRecord]];
        PendingAsyncDependencies: Enum[~empty~] | Int;
      }

      // https://tc39.es/ecma262/#graphloadingstate-record
      type GraphLoadingStateRecord {
        PromiseCapability: Record[PromiseCapabilityRecord];
        IsLoading: Boolean;
        PendingModulesCount: NonNegInt;
        Visited: List[Record[CyclicModuleRecord]];
        HostDefined: Enum[~empty~];
      }

      // https://tc39.es/ecma262/#sec-source-text-module-records
      type SourceTextModuleRecord extends CyclicModuleRecord {
        def GetExportedNames;
        def ResolveExport;
        def InitializeEnvironment;
        def ExecuteModule;
        ECMAScriptCode: Ast[Module];
        Context: Record[ExecutionContext];
        ImportMeta: Record[Object] | Enum[~empty~];
        ImportEntries: List[Record[ImportEntryRecord]];
        LocalExportEntries: List[Record[ExportEntryRecord]];
        IndirectExportEntries: List[Record[ExportEntryRecord]];
        StarExportEntries: List[Record[ExportEntryRecord]];
      }

      // https://tc39.es/ecma262/#resolvedbinding-record
      type ResolvedBindingRecord {
        Module: Record[ModuleRecord];
        BindingName: String | Enum[~namespace~];
      }

      // https://tc39.es/ecma262/#importentry-record
      type ImportEntryRecord {
        ModuleRequest: String;
        ImportName: String | Enum[~namespace-object~];
        LocalName: String;
      }

      // https://tc39.es/ecma262/#exportentry-record
      type ExportEntryRecord {
        ExportName: String | Null;
        ModuleRequest: String | Null;
        ImportName: String | Null | Enum[~all~, ~all-but-default~];
        LocalName: String | Null;
      }

      // -----------------------------------------------------------------------------
      // Builtin Objects
      // -----------------------------------------------------------------------------

      // https://tc39.es/ecma262/#sec-properties-of-boolean-instances
      type BooleanObject extends OrdinaryObject {
        BooleanData: Boolean;
      }

      // https://tc39.es/ecma262/#sec-properties-of-symbol-instances
      type SymbolObject extends OrdinaryObject {
        SymbolData: Record[Symbol];
      }

      // https://tc39.es/ecma262/#sec-properties-of-error-instances
      type ErrorObject extends OrdinaryObject {
        ErrorData: Undefined;
      }

      // https://tc39.es/ecma262/#sec-properties-of-number-instances
      type NumberObject extends OrdinaryObject {
        NumberData: Number;
      }

      // https://tc39.es/ecma262/#sec-properties-of-bigint-instances
      type BigIntObject extends OrdinaryObject {
        BigIntData: BigInt;
      }

      // https://tc39.es/ecma262/#sec-properties-of-date-instances
      type Date extends OrdinaryObject {
        DateValue: Number[Int, NaN];
      }

      // https://tc39.es/ecma262/#sec-properties-of-regexp-instances
      type RegExp extends OrdinaryObject {
        RegExpMatcher: Clo;
      }

      // https://tc39.es/ecma262/#sec-properties-of-map-instances
      type Map extends OrdinaryObject {
        MapData: List[Record[{ Key: ESValue | Enum[~empty~], Value: ESValue }]];
      }

      // https://tc39.es/ecma262/#sec-properties-of-set-instances
      type Set extends OrdinaryObject {
        SetData: List[ESValue];
      }

      // https://tc39.es/ecma262/#sec-properties-of-weakmap-instances
      type WeakMap extends OrdinaryObject {
        WeakMapData: List[Record[{ Key: ESValue | Enum[~empty~], Value: ESValue }]];
      }

      // https://tc39.es/ecma262/#sec-properties-of-weakset-instances
      type WeakSet extends OrdinaryObject {
        WeakSetData: List[ESValue];
      }

      // https://tc39.es/ecma262/#sec-properties-of-the-arraybuffer-instances
      type ArrayBuffer extends Object {
        ArrayBufferData: Record[DataBlock] | Null;
        ArrayBufferByteLength: NonNegInt;
        ArrayBufferDetachKey: ESValue;
        ArrayBufferMaxByteLength?: NonNegInt;
      }

      // https://tc39.es/ecma262/#sec-properties-of-the-sharedarraybuffer-instances
      type SharedArrayBuffer extends Object {
        ArrayBufferData: Record[SharedDataBlock] | Null;
        ArrayBufferByteLength?: NonNegInt;
        ArrayBufferByteLengthData?: Record[SharedDataBlock];
        ArrayBufferMaxByteLength?: NonNegInt;
      }

      // https://tc39.es/ecma262/#sec-properties-of-dataview-instances
      type DataView extends OrdinaryObject {
        DataView: Undefined;
        ViewedArrayBuffer: Record[ArrayBuffer | SharedArrayBuffer];
        ByteLength: NonNegInt | Enum[~auto~];
        ByteOffset: NonNegInt;
      }

      // TODO https://tc39.es/ecma262/#table-json-serialization-record
      // type JSONSerializationRecord {
      //   ReplacerFunction:	Record[FunctionObject] | Undefined;
      //   PropertyList: List[String] | Undefined;
      //   Gap: String;
      //   Stack: List[Record[Object]];
      //   Indent: String;
      // }

      // https://tc39.es/ecma262/#sec-properties-of-weak-ref-instances
      type WeakRef extends OrdinaryObject {
        WeakRefTarget: Record[Object | Symbol];
      }

      // https://tc39.es/ecma262/#sec-properties-of-finalization-registry-instances
      type FinalizationRegistry extends OrdinaryObject {
        Realm: Record[RealmRecord];
        Cells: List[Record[{ WeakRefTarget: Record[Object | Symbol], HeldValue: ESValue, UnregisterToken: Record[Object | Symbol] | Enum[~empty~] }]];
        CleanupCallback: Record[JobCallbackRecord];
      }

      // https://tc39.es/ecma262/#sec-properties-of-async-from-sync-iterator-instances
      type AsyncFromSyncIterator extends OrdinaryObject {
        SyncIteratorRecord: Record[IteratorRecord];
      }

      // https://tc39.es/ecma262/#sec-properties-of-promise-instances
      type Promise extends OrdinaryObject {
        PromiseState: Enum[~pending~, ~fulfilled~, ~rejected~];
        PromiseResult: ESValue;
        PromiseFulfillReactions: List[Record[PromiseReactionRecord]];
        PromiseRejectReactions: List[Record[PromiseReactionRecord]];
        PromiseIsHandled: Boolean;
      }

      // https://tc39.es/ecma262/#sec-properties-of-generator-instances
      type Generator extends OrdinaryObject {
        GeneratorState: Enum[~completed~, ~executing~, ~suspended-start~, ~suspended-yield~];
        GeneratorContext: Record[ExecutionContext];
        GeneratorBrand: String | Enum[~empty~];
      }

      // https://tc39.es/ecma262/#sec-properties-of-asyncgenerator-intances
      type AsyncGenerator extends OrdinaryObject {
        AsyncGeneratorState: Enum[~suspended-start~, ~suspended-yield~, ~executing~, ~awaiting-return~, ~completed~];
        AsyncGeneratorContext: Record[ExecutionContext];
        AsyncGeneratorQueue: List[Record[AsyncGeneratorRequest]];
        GeneratorBrand: String | Enum[~empty~];
      }

      // -----------------------------------------------------------------------------
      // Records for Control Abstraction
      // -----------------------------------------------------------------------------

      // https://tc39.es/ecma262/#sec-promisereaction-records
      type PromiseReactionRecord {
        Capability: Record[PromiseCapabilityRecord] | Undefined;
        Type: Enum[~fulfill~, ~reject~];
        Handler: Record[JobCallbackRecord] | Enum[~empty~];
      }

      // https://tc39.es/ecma262/#sec-promisecapability-records
      type PromiseCapabilityRecord {
        Promise: Record[Promise];
        Resolve: Record[FunctionObject];
        Reject: Record[FunctionObject];
      }

      // https://tc39.es/ecma262/#sec-asyncgeneratorrequest-records
      type AsyncGeneratorRequest {
        Completion: Normal[ESValue] | Return | Throw;
        Capability: Record[PromiseCapabilityRecord];
      }

      // -----------------------------------------------------------------------------
      // Records for Regular Expressions
      // -----------------------------------------------------------------------------

      // https://tc39.es/ecma262/#sec-match-records
      type MatchRecord {
        StartIndex: NonNegInt;
        EndIndex: NonNegInt;
      }

      // -----------------------------------------------------------------------------
      // Records for Data Blocks
      // -----------------------------------------------------------------------------

      // https://tc39.es/ecma262/#sec-typedarray-with-buffer-witness-records
      type TypedArrayWithBufferWitnessRecord {
        Object: Record[TypedArray];
        CachedBufferByteLength: NonNegInt | Enum[~detached~];
      }

      // https://tc39.es/ecma262/#sec-dataview-with-buffer-witness-records
      type DataViewWithBufferWitnessRecord {
        Object: Record[DataView];
        CachedBufferByteLength: NonNegInt | Enum[~detached~];
      }
      """
    )

  /** get test262 manual configuration */
  lazy val test262Config: ManualConfig = ManualConfig(
    readJson[Map[String, List[String]]](s"$MANUALS_DIR/test262/filtered.json"),
    readJson[List[String]](s"$MANUALS_DIR/test262/yet-categorized.json"),
    readJson[List[String]](s"$MANUALS_DIR/test262/supported-features.json"),
  )

  /** find all files in the manual directory with a filter */
  private def getFiles(filter: String => Boolean): List[File] = (for {
    file <- walkTree(MANUALS_DIR)
    if filter(file.getName)
  } yield file).toList

  /** find all file names in the manual directory with a filter */
  private def getFileNames(filter: String => Boolean): List[String] =
    getFiles(filter).map(_.toString)
}
