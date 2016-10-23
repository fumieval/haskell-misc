import Data.Serialize

import Data.Serialize 
import qualified Data.Vector as V
import qualified Data.Map as M
import Linear
import Control.Applicative
import Control.Monad
import Data.Word

type VertexIndex = Int
type BoneIndex = Int

type RGB = V3 Float
type RGBA = V4 Float

data PMX = PMX
  { _header :: Header 
  , _modelInfo :: ModelInfo
  , _vertices :: V.Vector Vertex
  , _surfaces :: V.Vector VertexIndex
  , _textures :: V.Vector FilePath
  , _materials :: V.Vector Material
  , _bones :: V.Vector Bone
  , _morphs :: V.Vector Morph
  , _frames :: V.Vector Frame
  , _rigidBodies :: V.Vector RigidBody
  , _joints :: V.Vector Joint
  }

data RigidBody = RigidBody

data Frame = Frame

data IndexSize = W8 | W16 | W32

instance Serialize IndexSize where
  get = (\r -> case r of
    1 -> W8
    2 -> W16
    4 -> W32) <$> get
  put W8 = put 1
  put W16 = put 2
  put W32 = put 4

data Encoding = UTF16 | UTF8

data Header = Header Float Encoding Int IndexSize IndexSize IndexSize IndexSize IndexSize IndexSize

data ModelInfo = ModelInfo { _modelName :: String, _comment :: String }

data BoneAssoc = BoneSingular BoneIndex
  | BoneDouble BoneIndex BoneIndex Float (Maybe (V3 Float, V3 Float, V3 Float))
  | Bones (M.Map BoneIndex Int)

data Vertex = Vertex
  { _position :: V3 Float
  , _normal :: V3 Float
  , _texCoord :: V2 Float
  , _extraUV :: [V4 Float]
  , _bone :: BoneAssoc
  , _edgeMag :: Float
  }

data RenderMode = RenderMode
  { _renderBothSides :: Bool
  , _renderShadow :: Bool
  , _renderShadowSelf :: Bool
  , _renderSelfShadow :: Bool
  , _renderEdge :: Bool
}

data Material = Material
  { _materialName :: String
  , _diffuse :: RGBA
  , _specular :: RGB
  , _reflectance :: Float
  , _ambient :: RGB
  , _renderMode :: RenderMode
  , _edgeColor :: RGBA
  , _edgeSize :: Float
  , _fromTexture :: Int
  , _materialNote :: String
  , _verticesCount :: Int
  }

data IKLink = IKLink BoneIndex (Maybe Float) (Maybe Float)

data Bone = Bone
  { _boneName :: String
  , _bonePosition :: V3 Float
  , _boneParent :: Maybe BoneIndex
  , _boneLevel :: Int
  , _connection :: Either (V3 Float) BoneIndex
  , _affineLinking :: Maybe (BoneIndex, Float)
  , _fixedAxis :: Maybe (V3 Float)
  , _localAxes :: Maybe (V3 Float, V3 Float)
  , _parentDeform :: Maybe Int
  , _ikLinks :: [IKLink]
  }

data Morph = Morph
data Joint = Joint

getHeader :: Get Header
getHeader = do
  pmx <- replicateM 4 get
  guard $ pmx == "PMX "
  ver <- get
  n <- get
  _ ver

getPMX :: Get Vertex
getPMX = do
  h <- getHeader
  _

getVector :: Get a -> Get (V.Vector a)
getVector g = do
  n <- get
  V.fromList <$> replicateM n g

getVertex :: Int -> Get Vertex
getVertex n = Vertex
  <$> get
  <*> get
  <*> get
  <*> replicateM n get
  <*> m
  <*> get where
    m = do
      i <- get :: Get Word8
      case i of
        0 -> BoneSingular <$> get
        1 -> BoneDouble <$> get <*> get <*> get <*> pure Nothing
        2 -> Bones <$> M.fromList
          <$> (zip <$> replicateM 4 get <*> replicateM 4 get)
        3 -> BoneDouble <$> get <*> get <*> get
          <*> fmap Just ((,,) <$> get <*> get <*> get)

