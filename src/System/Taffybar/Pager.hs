-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Pager
-- Copyright   : (c) José A. Romero L.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : José A. Romero L. <escherdragon@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Common support for pager widgets. This module does not provide itself
-- any widgets, but implements an event dispatcher on which widgets can
-- subscribe the desktop events they're interested in, as well as common
-- configuration facilities.
--
-- N.B. If you're just looking for a drop-in replacement for the
-- "System.Taffybar.XMonadLog" widget that is clickable and doesn't require
-- DBus, you may want to see first "System.Taffybar.TaffyPager".
--
-- You need only one Pager component to instantiate any number of pager
-- widgets:
--
-- > pager <- pagerNew defaultPagerConfig
-- >
-- > let wss = wspaceSwitcherNew pager  -- Workspace Switcher widget
-- >     los = layoutSwitcherNew pager  -- Layout Switcher widget
-- >     wnd = windowSwitcherNew pager  -- Window Switcher widget
--
-----------------------------------------------------------------------------

{-# LANGUAGE RankNTypes #-}

module System.Taffybar.Pager
  ( Pager (..)
  , PagerConfig (..)
  , PagerIO
  , Markup
  , WorkspaceInfo(..)
  , WSVisibility(..)
  , wsiEmpty
  , defaultPagerConfig
  , pagerNew
  , subscribe
  , colorize
  , liftPagerX11
  , liftPagerX11Def
  , runWithPager
  , shorten
  , wrap
  , escape
  ) where

import Control.Concurrent (forkIO)
import Control.Exception
import Control.Exception.Enclosed (catchAny)
import Control.Monad.Reader
import Data.IORef
import qualified Data.Map as M
import Graphics.UI.Gtk (escapeMarkup)
import Graphics.X11.Types
import Graphics.X11.Xlib.Extras
  hiding (rawGetWindowProperty, getWindowProperty8,
          getWindowProperty16, getWindowProperty32)
import System.Information.EWMHDesktopInfo
import System.Information.X11DesktopInfo
import Text.Printf (printf)

type Listener = Event -> IO ()
type Filter = Atom
type SubscriptionList = IORef [(Listener, Filter)]

type Markup = String

-- | Structure contanining functions to customize the pretty printing of
-- different widget elements.
data PagerConfig = PagerConfig
  { activeWindow            :: String -> String
  -- ^ the name of the active window.
  , activeLayout            :: String -> String
  -- ^ the currently active layout.
  , activeWorkspace         :: String -> String
  -- ^ the currently active workspace.
  , hiddenWorkspace         :: String -> String
  -- ^ inactive workspace with windows.
  , emptyWorkspace          :: String -> String
  -- ^ inactive workspace with no windows.
  , visibleWorkspace        :: String -> String
  -- ^ all other visible workspaces (Xinerama or XRandR).
  , urgentWorkspace         :: String -> String
  -- ^ workspaces containing windows with the urgency hint set.
  , markupWorkspaces :: forall f. Traversable f => f WorkspaceInfo -> f Markup -- ^ the currently active workspace.
  , widgetSep               :: String
  -- ^ separator to use between desktop widgets in 'TaffyPager'.
  , workspaceBorder         :: Bool
  -- ^ wrap workspace buttons in a frame
  , workspaceGap            :: Int
  -- ^ space in pixels between workspace buttons
  , workspacePad            :: Bool
  -- ^ pad workspace name in button
  , useImages               :: Bool
  -- ^ use images in the workspace switcher
  , imageSize               :: Int
  -- ^ image height and width in pixels
  , fillEmptyImages         :: Bool
  -- ^ fill empty images instead of clearing them
  , customIcon              :: Bool -> String -> String -> Maybe FilePath
  -- ^ get custom icon based on: has-EWMH-icon, window-title, window-class
  , windowSwitcherFormatter :: M.Map WorkspaceIdx String -> X11WindowHandle -> String
  -- ^ title windows for WindowSwitcher
  }

-- | Structure containing the state of the Pager.
data Pager = Pager
  { config  :: PagerConfig -- ^ the configuration settings.
  , clients :: SubscriptionList -- ^ functions to apply on incoming events depending on their types.
  , pagerX11ContextVar :: IORef X11Context
  }

-- | Is a workspace visible?
data WSVisibility = Active   -- ^ workspace is active
                  | Visible  -- ^ workspace is visible
                  | Hidden   -- ^ workspace is not visible
                  deriving (Show, Ord, Eq, Bounded, Enum)

-- | Information necessary to produce a formatted label for a workspace
data WorkspaceInfo = WSInfo { wsiName       :: !String -- ^ the name of the workspace
                            , wsiWindows    :: !Int    -- ^ how many windows are on the workspace?
                            , wsiVisibility :: !WSVisibility -- ^ is the workspace visible?
                            , wsiUrgent     :: !Bool   -- ^ do any of the windows have the urgent hint set?
                            }

-- | Does a workspace have any windows in it?
wsiEmpty :: WorkspaceInfo -> Bool
wsiEmpty = (==0) . wsiWindows

type PagerIO a = ReaderT Pager IO a

liftPagerX11 :: X11Property a -> PagerIO a
liftPagerX11 prop = ask >>= lift . flip runWithPager prop

liftPagerX11Def :: a -> X11Property a -> PagerIO a
liftPagerX11Def def prop = liftPagerX11 $ postX11RequestSyncProp prop def

runWithPager :: Pager -> X11Property a -> IO a
runWithPager pager prop = do
  x11Ctx <- readIORef $ pagerX11ContextVar pager
  -- runWithPager should probably changed so that it takes a default value
  runReaderT prop x11Ctx

-- | Default pretty printing options.
defaultPagerConfig :: PagerConfig
defaultPagerConfig = PagerConfig
  { activeWindow            = escape . shorten 40
  , activeLayout            = escape
  , activeWorkspace         = colorize "yellow" "" . wrap "[" "]" . escape
  , hiddenWorkspace         = escape
  , emptyWorkspace          = const ""
  , visibleWorkspace        = wrap "(" ")" . escape
  , urgentWorkspace         = colorize "red" "yellow" . escape
  , markupWorkspaces        = defaultMarkupWorkspaces
  , widgetSep               = " : "
  , workspaceBorder         = False
  , workspaceGap            = 0
  , workspacePad            = True
  , useImages               = False
  , imageSize               = 16
  , fillEmptyImages         = False
  , customIcon              = \_ _ _ -> Nothing
  , windowSwitcherFormatter = defaultFormatEntry
  }

-- | Build the name to display in the list of windows by prepending the name
-- of the workspace it is currently in to the name of the window itself
defaultFormatEntry
  :: M.Map WorkspaceIdx String -- ^ List $ names of all available workspaces
  -> X11WindowHandle -- ^ Handle of the window to name
  -> String
defaultFormatEntry wsNames ((ws, wtitle, _), _) =
  printf "%s: %s " wsName $ nonEmpty wtitle
  where
    wsName = M.findWithDefault ("WS#" ++ show wsN) ws wsNames
    WSIdx wsN = ws
    nonEmpty x =
      case x of
        [] -> "(nameless window)"
        _ -> x

-- | Default workspace markup
defaultMarkupWorkspaces :: Traversable f => f WorkspaceInfo -> f Markup
defaultMarkupWorkspaces = fmap f
  where
    f ws@(WSInfo {wsiName=name, wsiVisibility=vis})
      | wsiUrgent ws   = colorize "red" "yellow" $ escape name
      | Active <- vis  = colorize "yellow" "" $ wrap "[" "]" $ escape name
      | Visible <- vis = wrap "(" ")" $ escape name
      | wsiEmpty ws    = escape name
      | otherwise      = escape name

-- | Creates a new Pager component (wrapped in the IO Monad) that can be
-- used by widgets for subscribing X11 events.
pagerNew :: PagerConfig -> IO Pager
pagerNew cfg = do
  ref <- newIORef []
  ctx <- getDefaultCtx
  ctxVar <- newIORef ctx
  let pager = Pager cfg ref ctxVar
  _ <- forkIO $ withDefaultCtx (eventLoop $ handleEvent ref)
  return pager
    where handleEvent :: SubscriptionList -> Event -> IO ()
          handleEvent ref event = do
            listeners <- readIORef ref
            mapM_ (notify event) listeners

-- | Passes the given Event to the given Listener, but only if it was
-- registered for that type of events via 'subscribe'.
notify :: Event -> (Listener, Filter) -> IO ()
notify event (listener, eventFilter) =
  case event of
    PropertyEvent _ _ _ _ _ atom _ _ ->
      when (atom == eventFilter) $ catchAny (listener event) ignoreException
    _ -> return ()

-- | Registers the given Listener as a subscriber of events of the given
-- type: whenever a new event of the type with the given name arrives to
-- the Pager, it will execute Listener on it.
subscribe :: Pager -> Listener -> String -> IO ()
subscribe pager listener filterName = do
  eventFilter <- runWithPager pager $ getAtom filterName
  registered <- readIORef (clients pager)
  let next = (listener, eventFilter)
  writeIORef (clients pager) (next : registered)

ignoreException :: SomeException -> IO ()
ignoreException _ = return ()

-- | Creates markup with the given foreground and background colors and the
-- given contents.
colorize :: String -- ^ Foreground color.
         -> String -- ^ Background color.
         -> String -- ^ Contents.
         -> String
colorize fg bg = printf "<span%s%s>%s</span>" (attr "fg" fg) (attr "bg" bg)
  where attr name value
          | null value = ""
          | otherwise  = printf " %scolor=\"%s\"" name value

-- | Limit a string to a certain length, adding "..." if truncated.
shorten :: Int -> String -> String
shorten l s
  | length s <= l = s
  | l >= 3        = take (l - 3) s ++ "..."
  | otherwise     = "..."

-- | Wrap the given string in the given delimiters.
wrap :: String -- ^ Left delimiter.
     -> String -- ^ Right delimiter.
     -> String -- ^ Output string.
     -> String
wrap open close s = open ++ s ++ close

-- | Escape strings so that they can be safely displayed by Pango in the
-- bar widget
escape :: String -> String
escape = escapeMarkup
