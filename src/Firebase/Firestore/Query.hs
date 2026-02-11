{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Firebase.Firestore.Query
-- Description : Structured query DSL for Firestore
-- License     : MIT
--
-- A pure builder-pattern DSL for constructing Firestore structured queries.
-- Compose with @(&)@ from "Data.Function":
--
-- @
-- import Data.Function ((&))
--
-- let q = query (CollectionPath \"users\")
--       & where_ (fieldFilter \"age\" OpGreaterThan (IntegerValue 18))
--       & orderBy \"age\" Ascending
--       & limit 10
-- @
module Firebase.Firestore.Query
  ( -- * Query Construction
    StructuredQuery,
    query,
    where_,
    orderBy,
    limit,
    offset,

    -- * Filters
    Filter (..),
    FieldFilter (..),
    CompositeFilter (..),
    CompositeOp (..),
    FilterOp (..),
    fieldFilter,
    compositeAnd,
    compositeOr,

    -- * Ordering
    OrderDirection (..),

    -- * Encoding
    encodeQuery,
  )
where

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Firebase.Firestore.Types (CollectionPath (..), FirestoreValue)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | A structured query builder. Construct with 'query' and refine with
-- 'where_', 'orderBy', 'limit', and 'offset'.
data StructuredQuery = StructuredQuery
  { sqFrom :: !CollectionPath,
    sqWhere :: !(Maybe Filter),
    sqOrderBy :: ![(Text, OrderDirection)],
    sqLimit :: !(Maybe Int),
    sqOffset :: !(Maybe Int)
  }
  deriving (Eq, Show)

-- | A query filter: either a single field filter or a composite of filters.
data Filter
  = FieldFilterF !FieldFilter
  | CompositeFilterF !CompositeFilter
  deriving (Eq, Show)

-- | A filter on a single field.
data FieldFilter = FieldFilter
  { ffField :: !Text,
    ffOp :: !FilterOp,
    ffValue :: !FirestoreValue
  }
  deriving (Eq, Show)

-- | A composite of multiple filters joined by AND or OR.
data CompositeFilter = CompositeFilter
  { cfOp :: !CompositeOp,
    cfFilters :: ![Filter]
  }
  deriving (Eq, Show)

-- | Composite filter operator.
data CompositeOp = OpAnd | OpOr
  deriving (Eq, Show)

-- | Field filter comparison operators.
data FilterOp
  = OpEqual
  | OpNotEqual
  | OpLessThan
  | OpLessThanOrEqual
  | OpGreaterThan
  | OpGreaterThanOrEqual
  | OpArrayContains
  | OpIn
  | OpArrayContainsAny
  | OpNotIn
  deriving (Eq, Show)

-- | Sort direction for 'orderBy'.
data OrderDirection = Ascending | Descending
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Builders
-- ---------------------------------------------------------------------------

-- | Start a query on a collection.
query :: CollectionPath -> StructuredQuery
query cp =
  StructuredQuery
    { sqFrom = cp,
      sqWhere = Nothing,
      sqOrderBy = [],
      sqLimit = Nothing,
      sqOffset = Nothing
    }

-- | Add a filter to the query. Replaces any existing filter.
where_ :: Filter -> StructuredQuery -> StructuredQuery
where_ f sq = sq {sqWhere = Just f}

-- | Add an ordering clause. Multiple calls append in order.
orderBy :: Text -> OrderDirection -> StructuredQuery -> StructuredQuery
orderBy field dir sq = sq {sqOrderBy = sqOrderBy sq ++ [(field, dir)]}

-- | Set the maximum number of results to return.
limit :: Int -> StructuredQuery -> StructuredQuery
limit n sq = sq {sqLimit = Just n}

-- | Set the number of results to skip before returning.
offset :: Int -> StructuredQuery -> StructuredQuery
offset n sq = sq {sqOffset = Just n}

-- | Convenience: create a single field filter.
fieldFilter :: Text -> FilterOp -> FirestoreValue -> Filter
fieldFilter field op val = FieldFilterF (FieldFilter field op val)

-- | Convenience: create an AND composite filter.
compositeAnd :: [Filter] -> Filter
compositeAnd = CompositeFilterF . CompositeFilter OpAnd

-- | Convenience: create an OR composite filter.
compositeOr :: [Filter] -> Filter
compositeOr = CompositeFilterF . CompositeFilter OpOr

-- ---------------------------------------------------------------------------
-- JSON Encoding
-- ---------------------------------------------------------------------------

-- | Encode a 'StructuredQuery' to the JSON format expected by the
-- Firestore REST API's @:runQuery@ endpoint.
encodeQuery :: StructuredQuery -> Aeson.Value
encodeQuery sq =
  Aeson.object
    [ "structuredQuery"
        .= Aeson.object
          ( ["from" .= [encodeCollectionSelector (sqFrom sq)]]
              ++ maybe [] (\f -> ["where" .= encodeFilter f]) (sqWhere sq)
              ++ encodeOrderBy (sqOrderBy sq)
              ++ maybe [] (\n -> ["limit" .= n]) (sqLimit sq)
              ++ maybe [] (\n -> ["offset" .= n]) (sqOffset sq)
          )
    ]

-- | Encode a collection selector.
encodeCollectionSelector :: CollectionPath -> Aeson.Value
encodeCollectionSelector (CollectionPath cp) =
  Aeson.object ["collectionId" .= cp]

-- | Encode a filter to Firestore JSON.
encodeFilter :: Filter -> Aeson.Value
encodeFilter (FieldFilterF ff) =
  Aeson.object
    [ "fieldFilter"
        .= Aeson.object
          [ "field" .= Aeson.object ["fieldPath" .= ffField ff],
            "op" .= encodeFilterOp (ffOp ff),
            "value" .= ffValue ff
          ]
    ]
encodeFilter (CompositeFilterF cf) =
  Aeson.object
    [ "compositeFilter"
        .= Aeson.object
          [ "op" .= encodeCompositeOp (cfOp cf),
            "filters" .= map encodeFilter (cfFilters cf)
          ]
    ]

-- | Encode ordering clauses.
encodeOrderBy :: [(Text, OrderDirection)] -> [(Aeson.Key, Aeson.Value)]
encodeOrderBy [] = []
encodeOrderBy orders =
  [ "orderBy"
      .= [ Aeson.object
             [ "field" .= Aeson.object ["fieldPath" .= field],
               "direction" .= encodeDirection dir
             ]
         | (field, dir) <- orders
         ]
  ]

-- | Encode a filter operator to its Firestore string representation.
encodeFilterOp :: FilterOp -> Text
encodeFilterOp OpEqual = "EQUAL"
encodeFilterOp OpNotEqual = "NOT_EQUAL"
encodeFilterOp OpLessThan = "LESS_THAN"
encodeFilterOp OpLessThanOrEqual = "LESS_THAN_OR_EQUAL"
encodeFilterOp OpGreaterThan = "GREATER_THAN"
encodeFilterOp OpGreaterThanOrEqual = "GREATER_THAN_OR_EQUAL"
encodeFilterOp OpArrayContains = "ARRAY_CONTAINS"
encodeFilterOp OpIn = "IN"
encodeFilterOp OpArrayContainsAny = "ARRAY_CONTAINS_ANY"
encodeFilterOp OpNotIn = "NOT_IN"

-- | Encode a composite operator.
encodeCompositeOp :: CompositeOp -> Text
encodeCompositeOp OpAnd = "AND"
encodeCompositeOp OpOr = "OR"

-- | Encode an order direction.
encodeDirection :: OrderDirection -> Text
encodeDirection Ascending = "ASCENDING"
encodeDirection Descending = "DESCENDING"
