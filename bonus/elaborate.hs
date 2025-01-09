safeSucc2 :: Maybe Int -> Maybe Int
safeSucc2 = fmap ( + 1)

safeSucc3 :: Maybe Int -> Maybe Int
safeSucc3 e = e >>= (\x -> Just $ x + 1)

safeSucc4 :: Maybe Int -> Maybe Int
safeSucc4 e = do
    x <- e
    return $ x + 1

maybeDo2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeDo2 f a b = a >>= (\x -> b >>= (\y -> Just $ f x y))

maybeDo3 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeDo3 f a b = do
    x <- a
    y <- b
    return $ f x y

maybeDo4 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeDo4 f a b = f <$> a <*> b