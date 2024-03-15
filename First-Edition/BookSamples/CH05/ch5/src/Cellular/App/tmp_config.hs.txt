{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances  #-}

module Cellular.App.Config where

import qualified Data.Aeson                as A
import qualified Data.ByteString.Lazy      as LBS


-- N.B., ToJSON and FromJSON instances should be declared using 'nodeConfigJsonOptions'.
data Config rule = Config
    { rule         :: rule                 -- ^ Rule tag.
    -- , nodeScenario :: NodeScenario node    -- ^ Node scenario. It's possible to have several scenarios for node.
    -- , nodeConfig   :: NodeConfig node      -- ^ Node config. Different scenarios have the same config.
    }
    deriving (Generic)

instance
  ( Show node
  , Show (NodeScenario node)
  , Show (NodeConfig node)
  ) =>
  Show (Config node)

instance
  ( FromJSON node
  , FromJSON (NodeScenario node)
  , FromJSON (NodeConfig node)
  ) =>
  FromJSON (Config node)

instance
  ( ToJSON node
  , ToJSON (NodeScenario node)
  , ToJSON (NodeConfig node)
  ) =>
  ToJSON (Config node)

-- | Represents a config type for a particular node.
data family NodeConfig node :: *

-- | Represents a definition of node scenarios available.
class Node node where
    data NodeScenario node :: *
    getNodeScript :: NodeScenario node -> NodeConfig node -> NodeDefinitionL ()
    getNodeTag    :: NodeConfig node -> node

defConfig :: Node node => NodeScenario node -> NodeConfig node -> Config node
defConfig scenario cfg = Config
    { node         = getNodeTag cfg
    , nodeScenario = scenario
    , nodeConfig   = cfg
    , loggerConfig = defaultLoggerConfig
    }

-- | Options for ToJSON / FromJSON instances for configs.
-- These options take care about correct parsing of enum and data types.
nodeConfigJsonOptions :: A.Options
nodeConfigJsonOptions = noLensPrefixJsonConfig

-- | Reads a config file and evals some action with the contents.
withConfig :: FilePath -> (LByteString -> IO ()) -> IO ()
withConfig configName act = act =<< LBS.readFile configName

-- | Tries to parse config according to the type @node@ passed.
tryParseConfig
    :: (FromJSON node, FromJSON (NodeScenario node), FromJSON (NodeConfig node))
    => LByteString
    -> Either ParseException (Config node)
tryParseConfig = A.decodeEither' . LBS.toStrict

getNodeScript' :: Node node => Config node -> NodeDefinitionL ()
getNodeScript' cfg = getNodeScript (nodeScenario cfg) (nodeConfig cfg)

dispatchScenario
    :: FromJSON node
    => FromJSON (NodeScenario node)
    => FromJSON (NodeConfig node)
    => Node node
    => LByteString
    -> Maybe (Config node, NodeDefinitionL ())
dispatchScenario configSrc = case tryParseConfig' configSrc of
    Just cfg -> Just (cfg, getNodeScript' cfg)
    Nothing  -> Nothing

tryParseConfig'
  :: (FromJSON (NodeScenario node), FromJSON (NodeConfig node),
      FromJSON node) =>
      LByteString -> Maybe (Config node)
tryParseConfig' configSrc = case tryParseConfig configSrc of
    Left _    -> Nothing
    Right cfg -> Just cfg


------------ Aeson configs

noLensPrefix :: Options
noLensPrefix = A.defaultOptions { A.fieldLabelModifier = drop 1 }

noLensPrefixJsonConfig :: A.Options
noLensPrefixJsonConfig = noLensPrefix
    { A.unwrapUnaryRecords    = False
    , A.tagSingleConstructors = True
    }
