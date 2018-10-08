data CxtNode
  = Bin { key :: Word
        , firstIx :: Int
        , lastIx :: Int
        , cxtLen :: Int
        , ixSeries :: Either Int PosMap
        , left :: CxtNode
        , right :: CxtNode }
  | Tip

data PosMap
  = Empty
  | NonEmpty { key :: Word
             , firstIx :: Int
             , lastIx :: Int
             , cxtLen :: Int
             , ixSeries :: Either Int PosMap
             , subCxt :: CxtNode }

-- Since PosMap inside must be non-empty:

data CxtNode
  = Bin { key :: Word
        , firstIx :: Int
        , lastIx :: Int
        , cxtLen :: Int
        , ixSeries :: Either Int PosMap1
        , left :: CxtNode
        , right :: CxtNode }
  | Tip

newtype PosMap = PosMap { getPosMap :: Maybe PosMap1 }

data PosMap1 = PosMap1 { key :: Word
             , firstIx :: Int
             , lastIx :: Int
             , cxtLen :: Int
             , ixSeries :: Either Int PosMap1
             , subCxt :: CxtNode }

PosMap ~ Maybe PosMap1
PosMap ~ Ptr PosMap1

PosMap1 ~ (V4 Int, Either Int PosMap1, CxtNode)
PosMap1 ~ (V4 Int, Either Int (V4 Int, Either Int PosMap1, CxtNode), CxtNode)
PosMap1 ~ (NonEmpty (V4 Int, CxtNode), Int)
PosMap1 ~ ((V4 Int, CxtNode), [(V4 Int, CxtNode)], Int)
PosMap1 ~ (V5 Int, CxtNode, [(V4 Int, CxtNode)])
PosMap1 ~ (V5 Int, CxtNode, Ptr (V4 Int, CxtNode))

(# key, firstIx, lastIx, cxtLen, ixSeriesLastPeriod, cxtNode, subCxtNodes #)

CxtNode ~ Maybe (V4 Int, Either Int PosMap1, V2 CxtNode)
CxtNode ~ Maybe (V4 Int, (NonEmpty (V4 Int, CxtNode), Int), V2 CxtNode)
CxtNode ~ Maybe (V5 Int, NonEmpty (V4 Int, CxtNode), V2 CxtNode)
CxtNode ~ Maybe (V5 Int, (V4 Int, CxtNode), [(V4 Int, CxtNode)], V2 CxtNode)
CxtNode ~ Maybe (V9 Int, [(V4 Int, CxtNode)], V3 CxtNode)
CxtNode ~ Ptr (V9 Int, Ptr (V4 Int, CxtNode), V3 CxtNode)

newtype IntPosMap = IntPosMap { runIntPosMap :: Maybe IntPosMap1 }

data IntPosMap1 = IntPosMap1
  { key :: !Int
  , firstIx :: !Int
  , lastIx :: !Int
  , cxtLen :: !Int
  , lastPeriod :: !Int
  , ixSeries :: [PosCxt]
  , subCxt :: PosCxtNode

PosCxt = { firstIx lastIx cxtLen, posCxtNode :: PosCxtNode }






PosCxtNode = Maybe PosCxtNode1

PosCxtNode1 = PosCxtNode1 { firstIx :: !Int, lastIx :: !Int, cxtLen :: !Int, _, ixSeries :: [PosCxt]


Nothing
Just (V5 Int, (V4 Int, Nothing) :| [], V2 Nothing Nothing)



-- newtype CxtRec a b = CxtRec { runCxtRec :: Maybe (CxtRec1 a b) }

-- data CxtRec1 a b = CxtRec1 { cxtRec1 :: a, innerCxtRec1 :: NonEmpty (b, CxtRec), leftCxtRec1 :: CxtRec, rightCxtRec1 :: CxtRec }

data CxtRecB a b c = CxtRec1 { cxtRecB :: a, innerCxtRecB :: NonEmpty (b, c), leftCxtRec1 :: c, rightCxtRec1 :: c }

newtype CxtRec a b c = CxtRec { runCxtRec :: Free (CxtRecB a b) c }

CxtNode = CxtRec (V5 Int) (V4 Int) ()


