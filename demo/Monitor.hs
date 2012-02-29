{-# OPTIONS -fglasgow-exts #-}

--  ghc --make -ldbus-1 -o monitor Monitor.hs

import qualified DBus
import qualified DBus.Connection
import qualified DBus.Message
import Control.Exception(catchDyn)
import Data.Int
import Data.Word
import Data.Dynamic
import DBus.Message(Arg, variant)
import DBus.Connection(withConnection, readWriteDispatch, addFilter, addMatch, BusType(Session))


type Variant = DBus.Message.Variant
type Bus = DBus.Connection.Connection
type NotifyArgs = (String, Word32, String, String, String,
                   [String], DBus.Message.Dict String Variant, Int32)


main :: IO ()
main = do
  let loop bus = (readWriteDispatch bus (-1) >> loop bus)
  withConnection Session $ \bus -> do
    addFilter bus $ \msg -> do
      path <- DBus.Message.getPath msg
      iface <- DBus.Message.getInterface msg
      member <- DBus.Message.getMember msg
      src <- DBus.Message.getSender msg
      dst <- DBus.Message.getDestination msg
      errname <- DBus.Message.getErrorName msg
      sig <- DBus.Message.getSignature msg
      t <- DBus.Message.getType msg
      let cstr (Just s) = s
          cstr (Nothing) = "null"
      let s = "sender="++ cstr src ++ " -> dest="++cstr dst ++ " sig=" ++ sig
      let ms = s ++ " path=" ++ cstr path ++ "; interface="++ cstr iface
                 ++ "; member=" ++ cstr member ++ "\n"
      case t of
        DBus.Message.MethodCall -> putStr $ "Call " ++ ms
        DBus.Message.MethodReturn -> putStr $ "Return " ++ s ++ "\n"
        DBus.Message.Error -> putStr $ "Error " ++ s ++ " errname=" ++ cstr errname ++ "\n"
        DBus.Message.Signal -> putStr $ "Signal " ++ ms
        DBus.Message.Other x -> putStr $ "Other: " ++ (show x) ++ ms
      vars <- variantArgs msg
      mapM_ (\var -> do
               putStr "\t"
               print var)
            vars
      return True
    addMatch bus False "type='method_call'"
    addMatch bus False "type='method_return'"
    addMatch bus False "type='error'"
    addMatch bus False "type='signal'"
    loop bus

data Tagged = I32 Int32 | W32 Word32 | S String | A [Dynamic] | B Bool | E [Dynamic] [Dynamic] | Invalid

tag :: Dynamic -> Tagged
tag dyn =
  case fromDynamic dyn of
    Just (i :: Int32) -> I32 i
    Nothing ->
      case fromDynamic dyn of
        Just (word :: Word32) -> W32 word
        Nothing ->
          case fromDynamic dyn of
            Just (s :: String) -> S s 
            Nothing ->
              case fromDynamic dyn of
                Just (arr :: [Dynamic]) -> A arr
                Nothing ->
                  case fromDynamic dyn of
                    Just (b :: Bool) -> B b
                    Nothing ->
                      case fromDynamic dyn of
                        Just (a, b) -> E a b
                        Nothing -> Invalid

toVariant :: Dynamic -> Variant
toVariant dyn =
  case tag dyn of
    I32 i -> variant i
    W32 w -> variant w
    S   s -> variant s
    A arr -> variant (map toVariant arr)
    B   b -> variant (if b then (1 :: Int32) else 0)
    E a b -> variant (map toVariant a, map toVariant b)
    Invalid -> variant "<invalid>"

variantArgs msg = do
  (args :: [Dynamic]) <- DBus.Message.args msg
  return $ map toVariant args

{-
variantArgs msg =
  let load msg =
        do args <- DBus.Message.args msg;
           return args
      try cmp els =
        catch (do val <- cmp
                  return $ DBus.Message.variant val)
              (\err -> els)
  in try ((load msg) :: IO [Dynamic]) $
     try ((load msg) :: IO Int32) $
     try ((load msg) :: IO Word32) $
     try ((load msg) :: IO String) $
     try ((load msg) :: IO ()) $
     try ((load msg) :: IO [Int32]) $
     try ((load msg) :: IO [Word32]) $
     try ((load msg) :: IO [String]) $
     try ((load msg) :: IO [()]) $
     try ((load msg) :: IO NotifyArgs) $
     try ((load msg) :: IO Variant)
         (putStr "variantArgs failed\n" >> fail "Cannot figure out the type\n")
-}

{-
snoop :: Arg a => DBus.Message.Message -> IO a
snoop msg = do
  args <- DBus.Message.args msg
  print (DBus.Message.variant args)
  return args

eat :: IO a -> IO ()
eat cmp = cmp >> return ()

try :: IO a -> IO () -> IO ()
try cmp els =
  catch (eat cmp) (\err -> els)
-}
