{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH
import Build2
import Art

$(build_p1)

mkTargets ''Art

test :: (Art -> Maybe (Int, Int), (Int, Int) -> Art) -> IO ()
test (fromArt, toArt) = do
    print "test"
    print . fromArt $ ArtImage ()
    print . fromArt $ ArtImageShard 1 2
    print $ toArt (5, 6)

test2 :: (Art -> Maybe (), () -> Art) -> IO ()
test2 (fromArt, toArt) = do
    print "test2"
    print . fromArt $ ArtImage ()
    print . fromArt $ ArtImageShard 1 2
    print $ toArt ()

test3 :: (Art -> Maybe Int, Int -> Art) -> IO ()
test3 (fromArt, toArt) = do
    print "test3"
    print . fromArt $ ArtImage ()
    print . fromArt $ ArtImageShard 1 2
    print . fromArt $ ArtSprite 7
    print $ toArt 5

main = do
    s <- runQ [d| f :: (Art -> Maybe (Int, String), (Int, String) -> Art)
                  f = undefined
                  g :: (Art -> Maybe (), () -> Art)
                  g = undefined
                  r :: (Art -> Maybe Double, Double -> Art)
                  r = undefined
              |]
    print s
    v <- runQ [d|targetImageShard = (
                    \x -> case x of
                        ArtImageShard v w -> Just (v, w)
                        _ -> Nothing
                  , \(v,w) -> ArtImageShard v w
                  )
              |]
    print $(parseType ''Art)
    print $ v
    test targetArtImageShard
    test2 targetArtImage
    test3 targetArtSprite
    --test $(artCase2 'ArtImageShard)
