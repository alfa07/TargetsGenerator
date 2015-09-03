{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Build2 where
import Language.Haskell.TH
import Control.Monad

build_p1 :: Q [Dec]
build_p1 = do
  let p1 = mkName "p1"
  a <- newName "a"
  b <- newName "b"
  return
    [ FunD p1 
             [ Clause [TupP [VarP a,VarP b]] (NormalB (VarE a)) []
             ]
    ]

artCase :: String -> Q Exp
artCase nameStr = do
    let name = mkName nameStr
    x_0 <- newName "x_0"
    v_1 <- newName "v_1"
    tupE [lamE [varP x_0] (caseE (varE x_0) [ match (conP name [varP v_1]) (normalB (appE (conE (mkName "Just")) (varE v_1))) []
                                                     , match wildP (normalB (conE (mkName "Nothing"))) []])
                  ,conE name
                  ]
                  
artCase2 :: Name -> Q Exp
artCase2 name = do
    x_0 <- newName "x_0"
    v_1 <- newName "v_1"
    tupE [lamE [varP x_0] (caseE (varE x_0) [ match (conP name [varP v_1]) (normalB (appE (conE (mkName "Just")) (varE v_1))) []
                                                     , match wildP (normalB (conE (mkName "Nothing"))) []])
                  ,conE name
                  ]

artCase3 :: Name -> Q Exp
artCase3 name = do
    v_1 <- newName "v_1"
    [|(\x -> case x of
                            $(conP name [varP v_1]) -> Just ()
                            _ -> Nothing
                 , $(conE name)
                 )|]

mkTargets :: Name -> Q [Dec]
mkTargets name = do
    TyConI (DataD _ _ _ constructors _) <- reify name
    fmap concat $ forM constructors $ \c -> do
        case c of
            NormalC n args -> do
                let defName = mkName $ "target" ++ nameBase n
                sig <- mkSig name defName args
                f <- valD (varP defName) (normalB $ makePair n args) [] 
                return $ [sig, f]
            _-> error $ "Unable to process constructor: " ++ show c
    
    where
        makePair n args = do
            x_0 <- newName "x_0"
            vs <- forM args $ \_ -> newName "v"
            tupE [lamE [varP x_0] (caseE (varE x_0) [ match (conP n (map varP vs)) (normalB (appE (conE (mkName "Just")) (tupE $ map varE vs))) []
                                                     , match wildP (normalB (conE (mkName "Nothing"))) []])
                  ,lamE [tupP (map varP vs)] (callConE n (reverse vs))
                  ]
        callConE n [] = conE n
        callConE n (v:vs) = do
            appE (callConE n vs) (varE v)

        extractTypesReversed args = go args []
            where
            go [] acc = acc
            go ((_, c):as) acc = go as (c:acc)

        mkTypeTuple [TupleT 0] = return $ TupleT 0
        mkTypeTuple [ConT t] = return $ ConT t
        mkTypeTuple ts = go ts (length ts)
            where
                go [t] n = appT (tupleT n) (return t)
                go (t:ts) n = appT (go ts n) (return t)

        mkSig typeName funName args = do
            let ts = extractTypesReversed args
            sigD funName (appT (appT (tupleT 2)
                                (appT (appT arrowT (conT typeName))
                                    (appT (conT $ mkName "Maybe") (mkTypeTuple ts))))
                                (appT (appT arrowT (mkTypeTuple ts)) (conT typeName)))
                                
            -- [d|$(funName) :: ( $(conT typeName) -> Maybe $(mkTypeTuple ts)
            -- , $(mkTypeTuple ts) -> $(conT typeName))
            -- |]

parseType name = do
    stringE . show =<< reify name
