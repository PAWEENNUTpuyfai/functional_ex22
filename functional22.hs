import Data.Char
class MonadTrans t where
    lift :: Monad m => m a -> t m a
--mplement a monad transformer for Identity monad:
--IdentityT m a, wrapping m a

newtype IdentityT m a =
  IdentityT { runIdentityT :: m a }

instance MonadTrans IdentityT where
  lift :: Monad m => m a -> IdentityT m a
  lift = IdentityT

instance Functor m => Functor (IdentityT m) where
  fmap f (IdentityT x) = IdentityT (fmap f x)

instance Monad m => Applicative (IdentityT m) where
  pure = IdentityT . pure
  (IdentityT mf) <*> (IdentityT mx) = IdentityT $ do
    f <- mf
    x <- mx
    return (f x)

instance Monad m => Monad (IdentityT m) where
  (IdentityT mx) >>= f = IdentityT $ do
    x <- mx
    runIdentityT (f x)

--implement a monad transformer for Either monad:
--EitherT a m b, wrapping m (Either a b)

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance MonadTrans (EitherT e) where
  lift :: Monad m => m a -> EitherT e m a
  lift = EitherT . fmap Right

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT x) = EitherT . fmap (fmap f) $ x

instance Monad m => Applicative (EitherT e m) where
  pure = lift . pure
  (EitherT mf) <*> (EitherT mx) = EitherT $ do
    f <- mf
    x <- mx
    return (f <*> x)

instance Monad m => Monad (EitherT e m) where
  (EitherT mx) >>= f = EitherT $ do
    x <- mx
    case x of
      Left e  -> return (Left e)
      Right a -> runEitherT (f a)


--implement a monad transformer for the arrow monad:
--ContT r m a, wrapping m (r -> a)


newtype ContT r m a =
  ContT { runContT :: (a -> m r) -> m r }

instance MonadTrans (ContT r) where
  lift :: Monad m => m a -> ContT r m a
  lift ma = ContT $ \k -> ma >>= k

instance Functor m => Functor (ContT r m) where
  fmap f (ContT x) = ContT $ \k -> x (k . f)

instance Monad m => Applicative (ContT r m) where
  pure x = ContT ($ x)
  (ContT mf) <*> (ContT mx) = ContT $ \k ->
    mf (\f -> mx (k . f))

instance Monad m => Monad (ContT r m) where
  (ContT mx) >>= f = ContT $ \k ->
    mx (\x -> runContT (f x) k)

--modify the signup page example to use EitherT transformer
--be sure to give helpful error messages

readEmail :: IO (Maybe String)
readEmail = do
  putStrLn "Please enter your email!"
  str <- getLine
  if '@' `elem` str && '.' `elem` str
    then return $ Just str
    else return Nothing

readPassword :: IO (Maybe String)
readPassword = do
  putStrLn "Please enter your Password!"
  str <- getLine
  if length str < 8 || null (filter isUpper str)
      || null (filter isLower str)
    then return Nothing
    else return $ Just str

type Error = String

readEmail' :: EitherT Error IO String
readEmail' = EitherT $ do
    email <- readEmail 
    return $ case email of
        Just e  -> Right e
        Nothing -> Left "Invalid email"

readPassword' :: EitherT Error IO String
readPassword' = EitherT $ do
    password <- readPassword  
    return $ case password of
        Just p  -> Right p
        Nothing -> Left "password "

signup' :: EitherT Error IO (String, String)
signup' = do
    email <- readEmail'
    password <- readPassword'
    password2 <- readPassword'
    if password == password2
        then return (email, password)
        else EitherT . return $ Left "Passwords do not match"

main :: IO ()
main = do
    signupRes <- runEitherT signup'
    case signupRes of
        Left err  -> putStrLn $ "Signup failed: " ++ err
        Right _   -> putStrLn "Signup success"