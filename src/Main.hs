{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Main where

import Data.Maybe (fromJust)
import System.Environment

import Data.GI.Base
import Data.GI.Base.GValue
import qualified Data.Text as DataText
import qualified GI.Gtk as Gtk

import WorkingStack.Data

--------------------------------------------------------------------------------

dataFunc layout renderer model iter = do
  Just value <- Gtk.treeModelGetValue model iter 0 >>= fromGValue
  
  tr <- unsafeCastTo Gtk.CellRendererText renderer
  Gtk.setCellRendererTextText tr value

--------------------------------------------------------------------------------

addEntry model = do
  gv <- newGValue gtypeString
  set_string gv (Just $ DataText.pack "Test")

  iter <- Gtk.listStoreAppend model
  Gtk.setTreeIterUserData iter (Entry "a" "b")
  Gtk.listStoreSetValue model iter 0 gv

--------------------------------------------------------------------------------

main :: IO ()
main = do
  argv <- getArgs
  Gtk.init $ Just (map DataText.pack argv)

  builder <- Gtk.builderNewFromFile "gui.glade"

  -- Add and configure entriesColumn
  entriesColumn <- #getObject builder "entriesColumn" >>= unsafeCastTo Gtk.TreeViewColumn . fromJust
  entriesColumnRenderer <- Gtk.cellRendererTextNew
  Gtk.treeViewColumnPackStart entriesColumn entriesColumnRenderer True
  Gtk.cellLayoutSetCellDataFunc entriesColumn entriesColumnRenderer (Just dataFunc)

  -- Connect signals
  model <- #getObject builder "listStore" >>= unsafeCastTo Gtk.ListStore . fromJust
  addButton <- #getObject builder "addEntryButton" >>= unsafeCastTo Gtk.Button . fromJust
  on addButton #clicked (addEntry model)

  win <- #getObject builder "mainWindow" >>= unsafeCastTo Gtk.Window . fromJust
  on win #destroy Gtk.mainQuit

  quitMenu <- #getObject builder "fileMenuQuit" >>= unsafeCastTo Gtk.ImageMenuItem . fromJust
  on quitMenu #activate (Gtk.windowClose win)

  toolbarExit <- #getObject builder "toolbarExit" >>= unsafeCastTo Gtk.ToolButton . fromJust
  on toolbarExit #clicked (Gtk.windowClose win)
  
  #showAll win

  -- WorkingStack entriesList <- loadWorkingStackFromFile "workingstack.json"
  -- fromList entriesList

  -- initGui entriesList
  
  Gtk.main

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

{-
initGui entries = do
  -- main window and main layout
  win <- createWindow
  mainBox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical ]
  #add win mainBox

  -- add menubar
  menuBar <- createMenuBar win
  #add mainBox menuBar

  -- add toolbar
  toolBar <- createToolBar win
  #add mainBox toolBar

  -- central widget
  centralWidget <- createCentralWidget entries
  #add mainBox centralWidget
  
  #showAll win


createWindow = do
  win <- new Gtk.Window [ #title := "WorkingStack" ]
  on win #destroy Gtk.mainQuit

  return win

--------------------------------------------------------------------------------

createCentralWidget entries = do
  paned <- new Gtk.Paned [ #wideHandle := True ]
  
  -- left side
  leftBox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical ]
  Gtk.panedPack1 paned leftBox True False

  -- left tree
  ---- model
  -- listStore <- new Gtk.ListStore []
  listStore <- Gtk.listStoreNew [ (Entry "a" "b") ] -- entries
  
  elementList <- new Gtk.TreeView [ #model := listStore, #expand := True ]
  ---- column
  elementColumn <- new Gtk.TreeViewColumn [ #title := "Element" ]
  elementRenderer <- cellRendererTextNew
  Gtk.cellLayoutPackStart elementColumn elementRenderer True
  Gtk.cellLayoutAddAttribute elementColumn elementRenderer listStore $ \row -> [ cellText := description row ]
  

  Gtk.treeViewAppendColumn elementList elementColumn
  #add leftBox elementList

  -- left add button
  addButton <- new Gtk.Button [ #label := "gtk-add", #useStock := True ]
  on addButton #clicked (addEntry listStore)
  #add leftBox addButton

  -- left remote button
  removeButton <- new Gtk.Button [ #label := "gtk-remove", #useStock := True ]
  #add leftBox removeButton

  -- right TextView
  textView <- new Gtk.TextView [ #expand := True ]
  -- on textView #focusOutEvent 
  Gtk.panedPack2 paned textView True False

  return paned

--------------------------------------------------------------------------------

createToolBar win = do
  toolBar <- new Gtk.Toolbar []

  return toolBar

--------------------------------------------------------------------------------

createMenuBar win = do
  menuBar <- new Gtk.MenuBar []

  -- File - Menu
  fileMenu <- new Gtk.Menu []

  ---- File -> Save
  fileMenuSave <- new Gtk.ImageMenuItem [ #label := "gtk-save", #useStock := True ]
  Gtk.menuShellAppend fileMenu fileMenuSave

  ---- Separator
  sep <- new Gtk.SeparatorMenuItem []
  Gtk.menuShellAppend fileMenu sep

  ---- File -> Quit
  fileMenuQuit <- new Gtk.ImageMenuItem [ #label := "gtk-quit", #useStock := True ]
  on fileMenuQuit #activate (Gtk.windowClose win)
  Gtk.menuShellAppend fileMenu fileMenuQuit

  -- File - Menu
  file <- new Gtk.MenuItem [ #label := "File", #submenu := fileMenu ]
  #add menuBar file

  return menuBar
-}
