import System.Random (StdGen)
import Control.Monad.RWS (MonadTrans)
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad (liftM)
import Control.Monad (ap)

newtype StateIO s m a = StateIO {runStateT :: ((s, StdGen) -> m (a, (s, StdGen) ))}

instance (Monad m) => Functor (StateIO s m) where
    fmap = liftM


instance (Monad m) => Applicative (StateIO s m) where
    pure = return
    (<*>) = ap

instance (Monad m) => Monad (StateIO s m) where
    return x = StateIO $ \s -> return (x, s)
    (StateIO x) >>= f = StateIO $ 
     \s -> do 
        (out, s2) <- x s
        (StateIO x') <- return $ f out
        x' s2


instance MonadTrans (StateIO s) where
    lift c = StateIO $ \s -> c >>= (\x -> return (x,s))
