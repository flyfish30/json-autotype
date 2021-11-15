{-# LANGUAGE CPP                 #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGuaGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGuaGE DeriveGeneric       #-}
{-# LANGuaGE FlexibleContexts    #-}
-- | Formatting type declarations and class instances for inferred types.
module Data.Aeson.AutoType.CodeGen.ClangFormat(
    displaySplitTypes
  , declSplitTypes
  , normalizeTypeName
  , splitTypeByLabelClang
) where

import           Control.Arrow             ((&&&), first, second)
import           Control.Applicative       ((<$>), (<*>))
import           Control.Lens.TH
import           Control.Lens
import           Control.Monad             (forM, forM_, unless)
import           Control.Exception(assert)
import qualified Data.HashMap.Strict        as Map
import           Data.Monoid
import           Data.Maybe                (fromJust)
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text
import           Data.Text                 (Text)
import           Data.Set                  (Set )
import           Data.List                 (foldl1', foldl', intercalate)
import           Data.Char                 (isAlpha, isDigit)
import           Control.Monad.State.Class
import           Control.Monad.State.Strict(State, runState, get)
import qualified Data.Graph          as Graph
import           GHC.Generics              (Generic)

import           Data.Aeson.AutoType.Type
import           Data.Aeson.AutoType.Extract
import           Data.Aeson.AutoType.Format
import           Data.Aeson.AutoType.Split (toposort)
import           Data.Aeson.AutoType.Util  ()

--import           Debug.Trace -- DEBUG
trace _ x = x

-- | prefix of struct name, function name, variable name for json encode/decode
prefixName = "neu_json_"

shiftWidth = 4

fst3 ::  (t, t1, t2) -> t
fst3 (a, _b, _c) = a

data DeclState = DeclState { _decls   :: [Text]
                           , _counter :: Int
                           }
  deriving (Eq, Show, Ord, Generic)

makeLenses ''DeclState

type DeclM = State DeclState

type Map k v = Map.HashMap k v

stepM :: DeclM Int
stepM = counter %%= (\i -> (i, i+1))

tShow :: (Show a) => a -> Text
tShow = Text.pack . show

tryStripPrefix :: Text -> Text -> Text
tryStripPrefix pre str
  | Text.isPrefixOf pre str = fromJust $ Text.stripPrefix pre str
  | otherwise = str

tryStripSuffix :: Text -> Text -> Text
tryStripSuffix suf str
  | Text.isSuffixOf suf str = fromJust $ Text.stripSuffix suf str
  | otherwise = str

tryDropEndChar :: Char -> Text -> Text
tryDropEndChar c str
  | Text.last str == c = Text.dropEnd 1 str
  | otherwise = str

-- Try strip suffix "s[0-9]*Elt"
tryStripArraySuffix :: Text -> Text
tryStripArraySuffix name
  | Text.isSuffixOf "Elt" name = tryDropEndChar 's' . Text.dropWhileEnd isDigit
                               . fromJust $ Text.stripSuffix "Elt" name
  | otherwise = name

sanitySubEltName :: Text -> Text
sanitySubEltName = stripReqResp . stripElts
  where
    stripReqResp = tryStripPrefix "req_" . tryStripPrefix "resp_"
    stripElts    = tryStripArraySuffix

mergeSubTypeName :: Text -> Text -> Text
mergeSubTypeName sup sub = Text.concat [sup, "_", sanitySubEltName sub]

-- | Wrap a C struct alias.
wrapStructAlias :: Text -> Text -> Text
wrapStructAlias identifier contents = Text.concat ["typedef ", contents, " ", prefixName, identifier, "_t;"]

-- | Wrap a data type declaration
wrapStruct ::  Text -> Text -> Text
wrapStruct identifier contents = Text.unlines [header, contents, tailer]
  where
    header = "typedef struct {"
    tailer = Text.concat ["} ", prefixName, identifier, "_t;"]

-- | Explanatory type alias for making declarations
-- First element of the triple is original JSON identifier,
-- second element of the triple is the mapped identifier name in C language.
-- third element of the triple shows the type in a formatted way
type MappedKey = (Text, Text, Text, Bool)

jtypeHasAllocBuf "object" = True
jtypeHasAllocBuf "value" = True
jtypeHasAllocBuf "str" = True
jtypeHasAllocBuf _ = False

unlinesWithIndent n = Text.unlines . map (Text.replicate n " " `Text.append`)

shiftWithIndent n = map (Text.replicate n " " `Text.append`)

-- | Makes a generic identifier name.
genericIdentifier :: DeclM Text
genericIdentifier = do
  i <- stepM
  return $! "Obj" `Text.append` tShow i

data EndecEnum = Encode
               | Decode
               deriving (Eq)

genEndecElems :: Int -> EndecEnum -> Text -> Text -> [MappedKey] -> [Text]
genEndecElems indent endec identifier valName contents =
    shiftWithIndent indent $ [
        Text.concat ["neu_json_elem_t ", identifier, "[] = {"]
      , "    {"
      ] ++
      innerElems
      ++ [
        "    }"
      , "};"
      ]
  where
    innerElems = intercalate seperator $ map putValue contents
    putValue (jsonId, clangId, typeText, _nullable) =
        shiftWithIndent (indent + shiftWidth) $ [
            Text.concat [".name = ", escapeText jsonId, ","]
          , if typeText == "value" && endec == Encode then
              Text.concat [".t = ", valName, "->t", ","]
            else
              Text.concat [".t = NEU_JSON_", Text.toUpper typeText, ","]
          ] ++
          [valAssign jsonId clangId typeText | endec == Encode]
    seperator =
        shiftWithIndent indent [
            "},"
          , "{"
          ]
    escapeText fieldName | Text.null fieldName = "NULL"
                         | otherwise = Text.concat ["\"", fieldName, "\""]
    getArrayName fieldName = Text.append (tryStripSuffix "s" fieldName) "_array"
    valAssign jsonId clangId typeText
      | typeText == "value"  = Text.concat [".v", " = ", valName, "->" , escapeKeywords clangId, ","]
      | typeText == "object" = Text.concat [".v.val_", typeText, " = ", getArrayName jsonId, ","]
      | Text.null jsonId     = Text.concat [".v.val_", typeText, " = *", valName, ","]
      | otherwise            = Text.concat [".v.val_", typeText, " = ", valName, "->" , jsonId, ","]

genDecodeAssign :: Int -> Text -> Text -> [MappedKey] -> [Text]
genDecodeAssign indent varName elemsName contents =
    concatMap (shiftWithIndent indent) $ zipWith assignValue [0,1..] contents
  where
    assignValue i key@(jsonId, clangId, typeText, _nullable)
      | typeText == "value" = assignUnionValue i key
      | Text.null jsonId    = [
            Text.concat [ "*", varName, " = ", elemsName, "["
                        , Text.pack $ show i, "].v.val_", typeText, ";"]
          ]
      | otherwise           = [
            Text.concat [ varName, "->", jsonId, " = ", elemsName, "["
                        , Text.pack $ show i, "].v.val_", typeText, ";"]
          ]
    assignUnionValue i key@(jsonId, clangId, typeText, _nullable) = [
            Text.concat ["switch (", elemsName, "[", Text.pack $ show i, "].t) {"]
          ] ++
          concatMap (genAssignSingle i key) ["bool", "bit", "int", "double", "str"]
          ++ [
            "default:"
          , "    break;"
          , "}"
          , Text.concat [varName, "->t = ", elemsName, "[",
                         Text.pack $ show i, "].t;"]
        ]
    genAssignSingle i (jsonId, clangId, _, _nullable) typeText = [
          Text.concat ["case NEU_JSON_", Text.toUpper typeText, ":"]
        , Text.concat [ "    ", varName, "->value.val_", typeText, " = ", elemsName
                      , "[", Text.pack $ show i, "].v.val_", typeText, ";"]
        , "    break;"
      ]

-- * Printing a JSON elems encoding source code segment
newEncodeSeg :: [Text] -> Text -> [(Text, Type)] -> DeclM Text
newEncodeSeg names identifier kvs = do
    attrs <- forM kvs $ \(k, v) -> do
      formatted <- formatType v
      return (k, normalizeFieldName identifier k, formatted, isNullable v)
    let decl = Text.unlines $
                  genEndecElems shiftWidth Encode elems_name pVarName attrs  -- generate neu_json_elem_t elems[]
                  ++ [ Text.concat [ "    ret = neu_json_encode_field(json_object, "
                                   , elems_name, ", NEU_JSON_ELEM_SIZE(", elems_name, "));"]
                     ]

    addDecl decl
    return "resp"
  where
    resp_type_decl = Text.concat [prefixName, identifier, "_t"]
    pVarName = last names
    elems_name = "resp_elems"

newEncodeArraySeg :: [Text] -> Text -> [(Text, Type)] -> DeclM Text
newEncodeArraySeg names identifier kvs = do
    attrs <- forM kvs $ \(k, v) -> do
      formatted <- formatType v
      return (k, normalizeFieldName identifier k, formatted, isNullable v)
    let decl = unlinesWithIndent shiftWidth $ [
                    Text.concat ["void *", arrayName, " = NULL;"]
                  , Text.concat [resp_type_decl, " *", pVarName, " = ", varName, ";"]
                  , Text.concat ["for (int i = 0; i < ", countName, "; i++) {"]
                  ] ++
                  genEndecElems shiftWidth Encode elems_name pVarName attrs  -- generate neu_json_elem_t elems[]
                  ++ [
                    Text.concat [ "    ", arrayName, " = neu_json_encode_array(", arrayName, ", "
                                , elems_name, ", NEU_JSON_ELEM_SIZE(", elems_name, "));"]
                  , Text.concat ["    ", pVarName, "++;"]
                  , "}"
                  ]

    addDecl decl
    return arrayName
  where
    resp_type_decl = Text.concat [prefixName, identifier, "_t"]
    arrayName = Text.concat [last names, "_array"]
    varName = Text.intercalate "->" (init names ++ [Text.concat [last names, "s"]])
    countName = Text.intercalate "->" (init names ++ [Text.concat ["n_", last names]])
    pVarName = Text.concat ["p_", last names]
    elems_name = Text.concat [last names, "_elems"]

-- * Printing a JSON elems decoding source code segment
hasSubType (id -> TObj _) = True
hasSubType (id -> TArray _) = True
hasSubType _ = False

newDecodeSeg :: [Text] -> Text -> [(Text, Type)] -> DeclM Text
newDecodeSeg names identifier kvs = do
    let kvs' = filter (not . hasSubType . snd) kvs
    attrs <- forM kvs' $ \(k, v) -> do
      formatted <- formatType v
      return (k, normalizeFieldName identifier k, formatted, isNullable v)
    let decl = Text.unlines $ [
                    Text.concat ["    ", req_type_decl, " *req = calloc(1, sizeof(", req_type_decl, "));"]
                  ] ++
                  genEndecElems shiftWidth Decode elems_name pVarName attrs  -- generate neu_json_elem_t elems[]
                  ++ [
                    Text.concat [ "    ret = neu_json_decode(buf, NEU_JSON_ELEM_SIZE("
                                , elems_name, "), ", elems_name, ");"]
                  , "    if (ret != 0) {"
                  , "        goto decode_fail;"
                  , "    }"
                  , ""
                  ]
                  ++ genDecodeAssign shiftWidth "req" elems_name attrs

    addDecl decl
    return "req"
  where
    req_type_decl = Text.concat [prefixName, identifier, "_t"]
    pVarName = last names
    elems_name = "req_elems"

newDecodeArraySeg :: [Text] -> Text -> [(Text, Type)] -> DeclM Text
newDecodeArraySeg names identifier kvs = do
    attrs <- forM kvs $ \(k, v) -> do
      formatted <- formatType v
      return (k, normalizeFieldName identifier k, formatted, isNullable v)
    let decl = unlinesWithIndent shiftWidth $ [
                    Text.concat [countName, " = neu_json_decode_array_size(buf, \"", fieldName, "\");"]
                  , Text.concat [varName, " = calloc(", countName, ", sizeof(", req_type_decl, "));"]
                  , Text.concat [req_type_decl, " *", pVarName, " = ", varName, ";"]
                  , Text.concat ["for (int i = 0; i < ", countName, "; i++) {"]
                  ] ++
                  genEndecElems shiftWidth Decode elems_name "" attrs  -- generate neu_json_elem_t elems[]
                  ++ [
                    Text.concat [ "    ret = neu_json_decode_array(buf, \"", fieldName
                                , "\", i, NEU_JSON_ELEM_SIZE(", elems_name, "), ", elems_name, ");"]
                  , "    if (ret != 0) {"
                  , "        goto decode_fail;"
                  , "    }"
                  , ""
                  ] ++
                  genDecodeAssign shiftWidth pVarName elems_name attrs
                  ++ [
                    Text.concat ["    ", pVarName, "++;"]
                  , "}"
                  ]

    addDecl decl
    return varName
  where
    req_type_decl = Text.concat [prefixName, identifier, "_t"]
    varName = Text.intercalate "->" (init names ++ [Text.concat [last names, "s"]])
    countName = Text.intercalate "->" (init names ++ [Text.concat ["n_", last names]])
    pVarName = Text.concat ["p_", last names]
    fieldName = Text.concat [last names, "s"]
    elems_name = Text.concat [last names, "_elems"]

-- * Printing a request freeing source code segment
newFreeReqSeg :: Text -> [(Text, Type)] -> DeclM ()
newFreeReqSeg identifier kvs = do
    attrs <- forM kvs $ \(k, v) -> do
      formatted <- formatType v
      return (k, normalizeFieldName identifier k, formatted, isNullable v)
    let decl = Text.unlines [
                   innerFree attrs
                 , Text.concat ["    free(req);"]
                 ]
    addDecl decl
  where
    req_type_decl = Text.concat [prefixName, identifier, "_t"]
    innerFree = unlinesWithIndent shiftWidth . map freeValue
              . filter hasAllocValue
    hasAllocValue (_jsonId, _clangId, typeText, _nullable) = jtypeHasAllocBuf typeText
    freeValue (jsonId, _clangId, _typeText, _nullable) =
            Text.concat ["free(req->", jsonId, ");"]

newFreeReqArraySeg :: [Text] -> Text -> [(Text, Type)] -> DeclM ()
newFreeReqArraySeg names identifier kvs = do
    attrs <- forM kvs $ \(k, v) -> do
      formatted <- formatType v
      return (k, normalizeFieldName identifier k, formatted, isNullable v)
    let decl = if any hasAllocValue attrs then
                 unlinesWithIndent shiftWidth $ [
                     Text.concat [req_type_decl, " *", pVarName, " = ", varName, ";"]
                   , Text.concat ["for (int i = 0; i < ", countName, "; i++) {"]
                   ] ++
                   innerFree attrs
                   ++ [
                     Text.concat ["    ", pVarName, "++;"]
                   , "}"
                   ]
               else ""
    unless (Text.null decl) $ addDecl decl
  where
    req_type_decl = Text.concat [prefixName, identifier, "_t"]
    varName = Text.intercalate "->" (init names ++ [Text.concat [last names, "s"]])
    countName = Text.intercalate "->" (init names ++ [Text.concat ["n_", last names]])
    pVarName = Text.concat ["p_", last names]
    innerFree = shiftWithIndent shiftWidth . concatMap freeValue
              . filter hasAllocValue
    hasAllocValue (_jsonId, _clangId, typeText, _nullable) = jtypeHasAllocBuf typeText
    freeValue (jsonId, clangId, typeText, _nullable)
      -- for primary union value, only string need to free
      | typeText == "value" = [
            Text.concat ["if (", pVarName, "->t == NEU_JSON_STR) {"]
          , Text.concat ["    free(", pVarName, "->", jsonId, ".val_str);"]
          , "}"
          ]
      | otherwise = [
            Text.concat ["free(", pVarName, "->", jsonId, ");"]
          ]

-- * Printing a single data type declaration
newStructDecl :: Text -> [(Text, Type)] -> DeclM Text
newStructDecl identifier kvs = do attrs <- forM kvs $ \(k, v) -> do
                                    formatted <- formatStruct v
                                    return (k, normalizeFieldName identifier k, formatted, isNullable v)
                                  let decl = wrapStruct identifier $ fieldDecls attrs
                                  addDecl decl
                                  return identifier
  where
    fieldDecls attrList = Text.intercalate "\n" $ map fieldDecl attrList
    fieldDecl :: (Text, Text, Text, Bool) -> Text
    fieldDecl (_jsonName, clangId, fType, _nullable)
      | Text.isSuffixOf "Elt*" fType =
          Text.intercalate "\n" [
              Text.concat ["    int  ", arrayLenName clangId, ";"]
            , Text.concat ["    ", arrayTypeName fType, "  ", escapeKeywords clangId, ";"]
          ]
      | Text.isSuffixOf "json_value" fType =
          Text.intercalate "\n" [
              Text.concat ["    enum neu_json_type    t;"]
            , Text.concat ["    ", fType, "  ", escapeKeywords clangId, ";"]
          ]
      | otherwise =
              Text.concat ["    ", fType, "  ", escapeKeywords clangId, ";"]
    arrayLenName name = Text.append "n_"
                      . escapeKeywords $ tryStripSuffix "s" name
    arrayTypeName name = Text.append prefixName
                       . flip Text.append "_t*"
                       . mergeSubTypeName identifier
                       . escapeKeywords $ Text.dropEnd 1 name

-- | Add new type alias for Array type
newStructAlias :: Text -> Type -> DeclM Text
newStructAlias identifier content = do formatted <- formatStruct content
                                       addDecl $ Text.unlines [wrapStructAlias identifier formatted]
                                       return identifier

addDecl decl = decls %%= (\ds -> ((), decl:ds))

-- | Convert a JSON key name given by second argument,
-- from within a dictionary keyed with first argument,
-- into a name of Haskell record field (hopefully distinct from other such selectors.)
normalizeFieldName ::  Text -> Text -> Text
normalizeFieldName identifier = escapeKeywords             .
                                uncapitalize               .
                                normalizeTypeName

keywords ::  Set Text
keywords = Set.fromList [ "if", "else", "for", "do", "while", "switch", "case", "break"
                        , "continue", "goto", "return", "sizeof", "typeof"]

escapeKeywords ::  Text -> Text
escapeKeywords k | k `Set.member` keywords = k `Text.append` "_"
escapeKeywords k                           = k

-- | Format the type within DeclM monad, that records
-- the separate declarations on which this one is dependent.
formatType :: Type -> DeclM Text
formatType  TString                          = return "str"
formatType  TInt                             = return "int"
formatType  TDouble                          = return "double"
formatType  TBool                            = return "bool"
formatType (TLabel l)                        = return $ normalizeTypeName l
formatType (TUnion u)                        = return "value"
formatType (TArray a)                        = do inner <- formatType a
                                                  return "object"
formatType (TObj   o)                        = do ident <- genericIdentifier
                                                  return "object"
  where
    d = Map.toList $ unDict o
formatType  e | e `Set.member` emptySetLikes = return emptyTypeRepr
formatType  t                                = return $ "ERROR: Don't know how to handle: " `Text.append` tShow t

-- | Format the type within DeclM monad, that records
-- the separate declarations on which this one is dependent.
formatStruct :: Type -> DeclM Text
formatStruct  TString                          = return "char*"
formatStruct  TInt                             = return "int64_t"
formatStruct  TDouble                          = return "double"
formatStruct  TBool                            = return "bool"
formatStruct (TLabel l)                        = return $ normalizeTypeName l
formatStruct (TUnion u)                        = return "union neu_json_value"
formatStruct (TArray a)                        = do inner <- formatStruct a
                                                    return $ Text.concat [inner, "*"]
formatStruct (TObj   o)                        = do ident <- genericIdentifier
                                                    newStructDecl ident d
  where
    d = Map.toList $ unDict o
formatStruct  e | e `Set.member` emptySetLikes = return emptyTypeRepr
formatStruct  t                                = return $ "ERROR: Don't know how to handle: " `Text.append` tShow t

emptyTypeRepr :: Text
emptyTypeRepr = "(Maybe Value)" -- default, accepts future extension where we found no data

runDecl ::  DeclM a -> Text
runDecl decl = Text.unlines $ reverse $ finalState ^. decls
  where
    initialState    = DeclState [] 1
    (_, finalState) = runState decl initialState

-- * Splitting object types by label for unification.
type TypeTree    = Map Text [Type]

type TypeTreeM a = State TypeTree a

addType :: Text -> Type -> TypeTreeM ()
addType label typ = modify $ Map.insertWith (++) label [typ]

splitPrimaryTypeByLabel :: Text -> Type -> TypeTreeM Type
splitPrimaryTypeByLabel l typ = if Text.isSuffixOf "Elt" l then do
                                     addType l typ
                                     return $! TLabel l
                                else return typ

splitTypeByLabel' :: Text -> Type -> TypeTreeM Type
splitTypeByLabel' l  TString   = splitPrimaryTypeByLabel l TString
splitTypeByLabel' l  TInt      = splitPrimaryTypeByLabel l TInt
splitTypeByLabel' l  TDouble   = splitPrimaryTypeByLabel l TDouble
splitTypeByLabel' l  TBool     = splitPrimaryTypeByLabel l TBool
splitTypeByLabel' l  TNull     = splitPrimaryTypeByLabel l TNull
splitTypeByLabel' _ (TLabel r) = assert False $ return $ TLabel r -- unnecessary?
splitTypeByLabel' l (TUnion u) = do m <- mapM (splitTypeByLabel' l) $ Set.toList u
                                    return $! TUnion $! Set.fromList m
splitTypeByLabel' l (TArray a) = do m <- Control.Monad.State.Strict.get
                                    let size = Map.size m
                                    t <- splitTypeByLabel' (Text.concat [l, Text.pack (show size), "Elt"]) a
                                    return $! TArray t
splitTypeByLabel' l (TObj   o) = do kvs <- forM (Map.toList $ unDict o) $ \(k, v) -> do
                                       component <- splitTypeByLabel' k v
                                       return (k, component)
                                    addType l (TObj $ Dict $ Map.fromList kvs)
                                    return $! TLabel l

-- | Splits initial type with a given label, into a mapping of object type names and object type structures.
splitTypeByLabelClang :: Text -> Type -> Map Text Type
splitTypeByLabelClang topLabel t = Map.map (foldl1' unifyTypes) finalState
  where
    finalize (TLabel l) = assert (l == topLabel) $ return ()
    finalize  topLevel  = addType topLabel topLevel
    initialState    = Map.empty
    (_, finalState) = runState (splitTypeByLabel' topLabel t >>= finalize) initialState

-- * Generate function for encode json value
declEncodeFunction :: Text -> DeclM ()
declEncodeFunction identifier = addDecl $
    Text.concat ["int ", prefixName, "encode_", identifier,
                 "(void *json_object, void *param);"]

encodeFunctionHeader :: Text -> Text
encodeFunctionHeader identifier =
    Text.unlines [
        Text.concat ["int ", prefixName, "encode_", identifier,
                     "(void *json_object, void *param)"]
      , "{"
      , "    int ret = 0;"
      , Text.concat ["    ", resp_type_decl, " *resp = (", resp_type_decl, "*) param;"]
      ]
  where
    resp_type_decl = Text.concat [prefixName, identifier, "_t"]

encodeFunctionEpilogue :: [Text] -> Text
encodeFunctionEpilogue varNames =
    Text.unlines [
        "    return ret;"
      , "}"
    ]
  where
    freeVarNames = unlinesWithIndent (shiftWidth * 2)
                 $ map (Text.append "free(" . flip Text.append ");")
                 $ reverse varNames

genEncodeFunction :: Text -> [(Text, Type)] -> DeclM ()
genEncodeFunction identifier kvs = do
    addDecl $ encodeFunctionHeader identifier
    varNames <- forM kvs genEncodeVariables
    addDecl $ encodeFunctionEpilogue varNames
  where
    getIdentifier sup sub = if sup == sub
                            then sub
                            else mergeSubTypeName sup sub
    genEncodeVariables (name, TObj o)
      | Text.isSuffixOf "Elt" name =
          newEncodeArraySeg ["resp", tryStripArraySuffix name]
                            (getIdentifier identifier name) (toposort $ unDict o)
      | otherwise =
          newEncodeSeg ["resp"]
                       (getIdentifier identifier name) (toposort $ unDict o)
    genEncodeVariables (name, typ)
      | Text.isSuffixOf "Elt" name =
          newEncodeArraySeg ["resp", tryStripArraySuffix name]
                            (getIdentifier identifier name) [("", typ)]
      | otherwise =
          newEncodeSeg ["resp"]
                       (getIdentifier identifier name) [(tryStripSuffix "Elt" name, typ)]

-- * Generate function for decode json value
declDecodeFunction :: Text -> DeclM ()
declDecodeFunction identifier = addDecl $
    Text.concat ["int ", prefixName, "decode_", identifier,
                 "(char *buf, ", req_type_decl, " **result);"]
  where
    req_type_decl = Text.concat [prefixName, identifier, "_t"]

decodeFunctionHeader :: Text -> Text
decodeFunctionHeader identifier =
    Text.unlines [
        Text.concat [ "int ", prefixName, "decode_", identifier
                    , "(char *buf, ", req_type_decl, " **result)"]
      , "{"
      , "    int ret = 0;"
      ]
  where
    req_type_decl = Text.concat [prefixName, identifier, "_t"]

decodeFunctionEpilogue :: [Text] -> Text
decodeFunctionEpilogue varNames =
    Text.unlines [
        "    *result = req;"
      , "    return ret;"
      , ""
      , "decode_fail:"
      , freeVariables
      , "    return -1;"
      , "}"
    ]
  where
    freeVariables = Text.intercalate "\n"
                  . concatMap freeSingleVar
                  $ reverse varNames
    freeSingleVar name = shiftWithIndent shiftWidth [
          Text.concat ["if (", name, " != NULL) {"]
        , Text.concat ["    free(", name, ");"]
        , "}"
      ]

genDecodeFunction :: Text -> [(Text, Type)] -> DeclM ()
genDecodeFunction identifier kvs = do
    addDecl $ decodeFunctionHeader identifier
    varNames <- forM (reverse kvs) genDecodeVariables
    addDecl $ decodeFunctionEpilogue varNames
  where
    getIdentifier sup sub = if sup == sub
                            then sub
                            else mergeSubTypeName sup sub
    genDecodeVariables (name, TObj o)
      | Text.isSuffixOf "Elt" name =
          newDecodeArraySeg ["req", tryStripArraySuffix name]
                            (getIdentifier identifier name) (toposort $ unDict o)
      | otherwise =
          newDecodeSeg ["req"]
                       (getIdentifier identifier name) (toposort $ unDict o)
    genDecodeVariables (name, typ)
      | Text.isSuffixOf "Elt" name =
          newDecodeArraySeg ["req", tryStripArraySuffix name]
                            (getIdentifier identifier name) [("", typ)]
      | otherwise =
          newDecodeSeg ["req"]
                       (getIdentifier identifier name) [(tryStripSuffix "Elt" name, typ)]

-- * Generate function for free request structure
declFreeReqFunction :: Text -> DeclM ()
declFreeReqFunction identifier = addDecl $
    Text.concat ["void ", prefixName, "decode_", identifier, "_free",
                 "(", req_type_decl, " *req);"]
  where
    req_type_decl = Text.concat [prefixName, identifier, "_t"]

freeReqFunctionHeader :: Text -> Text
freeReqFunctionHeader identifier =
    Text.unlines [
          Text.concat ["void ", prefixName, "decode_", identifier, "_free",
                       "(", req_type_decl, " *req)"]
        , "{"
        ]
  where
    req_type_decl = Text.concat [prefixName, identifier, "_t"]

freeReqFunctionEpilogue :: Text -> Text
freeReqFunctionEpilogue identifier = "}"

genFreeReqFunction :: Text -> [(Text, Type)] -> DeclM ()
genFreeReqFunction identifier kvs = do
    addDecl $ freeReqFunctionHeader identifier
    forM_ kvs genFreeVariables
    addDecl $ freeReqFunctionEpilogue identifier
  where
    getIdentifier sup sub = if sup == sub
                            then sub
                            else mergeSubTypeName sup sub
    genFreeVariables (name, TObj o)
      | Text.isSuffixOf "Elt" name =
          newFreeReqArraySeg ["req", tryStripArraySuffix name]
                            (getIdentifier identifier name) (toposort $ unDict o)
      | otherwise =
          newFreeReqSeg (getIdentifier identifier name) (toposort $ unDict o)
    genFreeVariables (name, typ)
      | Text.isSuffixOf "Elt" name =
          newFreeReqArraySeg ["req", tryStripArraySuffix name]
                            (getIdentifier identifier name) [("", typ)]
      | otherwise =
          newFreeReqSeg (getIdentifier identifier name) [(tryStripSuffix "Elt" name, typ)]

-- * format object and structure
zipNamesKvs :: [Text] -> [(Text, Type)] -> [(Text, [(Text, Type)])]
zipNamesKvs names = reverse . go ([], names)
  where
    go zs@(znkvs, ns) [] = znkvs
    go zs@(znkvs, ns) kvs@(kv:kvs')
      | null ns          = go (updateZnkvsHead znkvs kv, ns) kvs'
      | fst kv `elem` ns = go ((fst kv, [kv]) : znkvs, filter (/= fst kv) ns) kvs'
      | otherwise = go (updateZnkvsHead znkvs kv, ns) kvs'
    updateZnkvsHead [] kv = []
    updateZnkvsHead (znkv:znkvs) kv = second (kv :) znkv : znkvs

getObjectDict :: Type -> Dict
getObjectDict (TObj o) = fromJust $ Just o
getObjectDict _        = fromJust Nothing

-- | Display an environment of types split by name.
displaySplitTypes ::  Map Text Type -> Text
displaySplitTypes dict = trace ("displaySplitTypes: " ++ show (toposort dict)) $ runDecl declarations
  where
    declarations =
      forM (reverse nameKvsPairs) $ \(name, kvs) -> do
        if Text.isSuffixOf "_req" name then do
          genDecodeFunction (normalizeTypeName name) kvs
          genFreeReqFunction (normalizeTypeName name) kvs
          addDecl ""
        else
          genEncodeFunction (normalizeTypeName name) kvs
    nameKvsPairs = zipNamesKvs topFieldNames sortedDict
    topFieldNames = map fst . toposort . unDict . getObjectDict . snd $ head sortedDict
    sortedDict = toposort dict

formatObjectStruct ::  Text -> Text -> Type -> DeclM Text
formatObjectStruct topField identifier (TObj o) = newStructDecl identifier' d
  where
    d = Map.toList $ unDict o
    identifier' = if topField == identifier
                  then identifier
                  else mergeSubTypeName topField identifier
formatObjectStruct topField identifier  other   = newStructAlias identifier' other
  where
    identifier' = mergeSubTypeName topField identifier

-- | Declare an structure of types split by name.
declSplitTypes ::  Map Text Type -> Text
declSplitTypes dict = trace ("declSplitTypes: " ++ show nameKvsPairs) $ runDecl declarations
  where
    declarations =
      forM (reverse nameKvsPairs) $ \(topField, kvs) -> do
        forM_ kvs $ \(name, typ) ->
          formatObjectStruct topField (normalizeTypeName name) typ
        if Text.isSuffixOf "_req" topField then do
          declDecodeFunction topField
          declFreeReqFunction topField
        else
          declEncodeFunction topField
        addDecl ""
    nameKvsPairs = zipNamesKvs topFieldNames sortedDict
    topFieldNames = map fst . toposort . unDict . getObjectDict . snd $ head sortedDict
    sortedDict = toposort dict

-- | Normalize type name by:
-- 1. Treating all characters that are not acceptable in Haskell variable name as end of word.
-- 2. Capitalizing each word, but a first (camelCase).
-- 3. Adding underscore if first character is non-alphabetic.
-- 4. Escaping Haskell keywords if the whole identifier is such keyword.
-- 5. If identifier is empty, then substituting "JsonEmptyKey" for its name.
normalizeTypeName :: Text -> Text
normalizeTypeName = ifEmpty "json_empty_key"                .
                    ensureBeginsWithCapital                 .
                    escapeKeywords                          .
                    escapeFirstNonAlpha
  where
    ifEmpty x ""       = x
    ifEmpty _ nonEmpty = nonEmpty
    ensureBeginsWithCapital x =
      if Text.isPrefixOf "_" x
      then "D" <> x
      else x
    acceptableInVariable c = isAlpha c || isDigit c
    escapeFirstNonAlpha cs                  | Text.null cs =                   cs
    escapeFirstNonAlpha cs@(Text.head -> c) | isAlpha   c  =                   cs
    escapeFirstNonAlpha cs                                 = "_" `Text.append` cs

-- | Computes all type labels referenced by a given type.
allLabels :: Type -> [Text]
allLabels = flip go []
  where
    go (TLabel l) ls = l:ls
    go (TArray t) ls = go t ls
    go (TUnion u) ls = Set.foldr go ls          u
    go (TObj   o) ls = Map.foldr go ls $ unDict o
    go _other     ls = ls
