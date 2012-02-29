{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}
-- HDBus -- Haskell bindings for DBus.
-- Copyright (C) 2006 Evan Martin <martine@danga.com>

import qualified DBus
import qualified DBus.Connection
import qualified DBus.Message
import Control.Exception
import Data.Int
import Data.Word

main :: IO ()
main = do
  DBus.Connection.withConnection DBus.Connection.Session $ \bus -> do
    (t1 :: [String]) <- testMethodCall bus DBus.serviceDBus DBus.pathDBus
                                       DBus.interfaceDBus "ListNames" ()
    (t2 :: String) <- testMethodCall bus DBus.serviceDBus DBus.pathDBus
                      DBus.interfaceIntrospectable "Introspect" ()

    --testGaim bus
    --testBmp bus
    testNotify bus

    signalTest bus

testGaim bus = do
  accounts <- gaimCall "GaimAccountsGetAll" ()
  let account :: Int32 = head accounts
  username <- gaimCall "GaimAccountGetUsername" account
  putStrLn $ "First account is " ++ username ++ "." where
    gaimCall :: (DBus.Message.Arg a, DBus.Message.Arg b) => String -> a -> IO b
    gaimCall = testMethodCall bus "net.sf.gaim.GaimService"
                              "/net/sf/gaim/GaimObject"
                              "net.sf.gaim.GaimInterface"

testBmp bus = do
  bmpCall "VolumeGet" []
  bmpCall "VolumeSet" [50 :: Int32] where
    bmpCall = testMethodCall bus "org.beepmediaplayer.bmp" "/SystemControl" "org.beepmediaplayer.bmp"

testNotify bus = do
  -- For details on this API, see the desktop notification spec at
  -- http://www.galago-project.org/specs/notification/
  let args = ("hdbus",
              0 :: Word32,   -- don't replace anybody
              "",  -- no icon
              "hdbus calling",
              "This message was sent via hdbus.",
              [] :: [String],
              [] :: DBus.Message.Dict String DBus.Message.Variant,
              3*1000 :: Int32  -- timeout
              )
  () <- notifyCall "Notify" args
  return () where
    notifyCall = testMethodCall bus "org.freedesktop.Notifications"
                                "/org/freedesktop/Notifications"
                                "org.freedesktop.Notifications"

showArgs :: DBus.Message.Arg a => a -> String
showArgs args = show args ++ " (sig: " ++ DBus.Message.signature args ++ ")"

testMethodCall :: (DBus.Message.Arg a, DBus.Message.Arg b)
               => DBus.Connection.Connection
               -> DBus.ServiceName -> DBus.PathName -> DBus.InterfaceName
               -> String -> a -> IO b
testMethodCall bus name path iface method args = do
  putStrLn ""
  putStrLn $ "Calling: " ++ show [name, path, iface, method]
  putStrLn $ "  with args:" ++ showArgs args
  do
    msg   <- DBus.Message.newMethodCall name path iface method
    DBus.Message.addArgs msg args
    reply <- DBus.Connection.sendWithReplyAndBlock bus msg 2000
    replyargs <- DBus.Message.args reply
    putStrLn $ "Result: " ++ showArgs replyargs
    return replyargs
  `catchDyn` (\(e :: DBus.Error) -> do print e; fail "fatal: dbus error")

signalTest bus = do
  msg <- DBus.Message.newSignal "/org/neugierig/HDBusTest" "org.neugierg.HDBus" "Test"
  ret <- DBus.Connection.send bus msg 0
  print ret

-- vim: set ts=2 sw=2 et ft=haskell :
