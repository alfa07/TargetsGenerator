module Art where

data ShardId = ShardId { unShardId :: Int }
    deriving (Eq, Show)

data Site =
      SiteWWW
    | SiteMobile
    | SiteTablet
    deriving (Eq, Show)

newtype Cohort = Cohort { unCohort :: String }
    deriving (Eq, Show)

data Art =
      ArtImage ()
    | ArtImageShard Int Int
    | ArtSprite Int
    | ArtSpriteMap ShardId Site Cohort
    deriving (Eq, Show)