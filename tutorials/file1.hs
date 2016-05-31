data StreamF a b = StreamF ( a -> ( b, StreamF a b))

arrow :: ( a -> b ) -> StreamF a b
arrow f = StreamF $ \ a -> ( f a , arrow f)

data Stream a = Stream a (Stream a) deriving Show

apply :: StreamF a b -> Stream a -> Stream b
apply (StreamF sf) (Stream a as) = Stream b bs
    where (b, sf') = sf a
          bs = apply sf' as

sumsf :: Num a =>  StreamF a a
sumsf = sumsffrom 0

sumsffrom :: Num a => a -> StreamF a a
sumsffrom accum = StreamF $ \ a -> (accum + a, sumsffrom (accum + a) )

reactimate :: IO a -> (b -> IO ())
           -> StreamF a b
           -> IO ()
reactimate sensor actor (StreamF sf) = do
    a <- sensor
    let (b, sf') = sf a
    actor b
    reactimate sensor actor sf'