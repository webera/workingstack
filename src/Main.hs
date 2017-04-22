{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Main where

import Data.Maybe (fromJust)
import System.Environment

import Foreign.Ptr
import Foreign.StablePtr

import Data.Aeson

import Data.GI.Base
import Data.GI.Base.GValue
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as Text
import qualified GI.Gtk as Gtk

import WorkingStack.Data

--------------------------------------------------------------------------------

dataFunc layout renderer model iter = do
  tr <- unsafeCastTo Gtk.CellRendererText renderer

  value <- Gtk.treeModelGetValue model iter 0 >>= fromGValue
  entry <- deRefStablePtr $ castPtrToStablePtr value
  Gtk.setCellRendererTextText tr $ description entry

--------------------------------------------------------------------------------

addEntryToModel entry model = do
  iter <- Gtk.listStoreAppend model
  newEntryToGValue entry >>= Gtk.listStoreSetValue model iter 0
  
--------------------------------------------------------------------------------

newEntryToGValue entry = do
  sPtr <- newStablePtr entry
  gv <- toGValue $ castStablePtrToPtr sPtr
  return (gv)

--------------------------------------------------------------------------------

addEntry model = do
  addEntryToModel (Entry (Text.pack "New") (Text.pack "Notes")) model
  
--------------------------------------------------------------------------------

onListEntryEdited model path newText = do
  (av, iter) <- Gtk.treeModelGetIterFromString model path
  case av of
    True -> do
      ptr <- Gtk.treeModelGetValue model iter 0 >>= fromGValue
      entry <- deRefStablePtr $ castPtrToStablePtr ptr

      sPtr <- newStablePtr (Entry newText (notes entry))
      gv <- toGValue $ castStablePtrToPtr sPtr
      Gtk.listStoreSetValue model iter 0 gv

      freeStablePtr $ castPtrToStablePtr ptr
    False -> return ()
      
--------------------------------------------------------------------------------

notesLostFocus notesView treeView keyEvent  = do
  tvs <- Gtk.treeViewGetSelection treeView
  (valid, model, iter) <- Gtk.treeSelectionGetSelected tvs
  case valid of
    True -> do
      ptr <- Gtk.treeModelGetValue model iter 0 >>= fromGValue
      entry <- deRefStablePtr $ castPtrToStablePtr ptr

      textBuffer <- Gtk.textViewGetBuffer notesView
      (startIter, endIter) <- Gtk.textBufferGetBounds textBuffer
      notesText <- Gtk.textBufferGetText textBuffer startIter endIter False
      
      sPtr <- newStablePtr (Entry (description entry) notesText)
      gv <- toGValue $ castStablePtrToPtr sPtr
      listModel <- unsafeCastTo Gtk.ListStore model
      Gtk.listStoreSetValue listModel iter 0 gv

      freeStablePtr $ castPtrToStablePtr ptr
      return (False)
    False -> return (False)

--------------------------------------------------------------------------------

removeEntry treeView = do
  tvs <- Gtk.treeViewGetSelection treeView
  (valid, model, iter) <- Gtk.treeSelectionGetSelected tvs
  case valid of
    True -> do
      ptr <- Gtk.treeModelGetValue model iter 0 >>= fromGValue
      freeStablePtr $ castPtrToStablePtr ptr

      listModel <- unsafeCastTo Gtk.ListStore model
      Gtk.listStoreRemove listModel iter
      return ()
    False -> do
      return ()

--------------------------------------------------------------------------------

listSelectionChanged notesView treeView = do
  tvs <- Gtk.treeViewGetSelection treeView
  (valid, model, iter) <- Gtk.treeSelectionGetSelected tvs
  case valid of
    True -> do
      ptr <- Gtk.treeModelGetValue model iter 0 >>= fromGValue
      entry <- deRefStablePtr $ castPtrToStablePtr ptr

      textBuffer <- Gtk.textViewGetBuffer notesView
      Gtk.textBufferSetText textBuffer (notes entry) (-1)
      return ()
    False -> return ()

--------------------------------------------------------------------------------

{-
save treeView file = do
  model <- Gtk.treeViewGetModel treeView >>= fromJust

  (valid, sIter) <- Gtk.treeModelIterFirst model
  case valid of
    True -> do
      return ()
    False -> do
      saveWorkingStackToFile (WorkingStack []) "data.json"
      return ()

  where collect xs model = do
          treeModelIterNext model
-}          

{-
  where foreachFunc tm _ it = do
          ptr <- Gtk.treeModelGetValue model iter 0 >>= fromGValue
          entry <- deRefStablePtr $ castPtrToStablePtr ptr
          return (False)
-}

--------------------------------------------------------------------------------

main :: IO ()
main = do
  argv <- getArgs
  Gtk.init $ Just (map Text.pack argv)

  builder <- Gtk.builderNewFromFile "gui.glade"

  -- load data and setup model
  entries <- loadWorkingStackFromFile "data.json"

  model <- #getObject builder "listStore" >>= unsafeCastTo Gtk.ListStore . fromJust
  sequence_ [ addEntryToModel entry model | entry <- entries ] 

  -- Add and configure entriesColumn
  entriesColumn <- #getObject builder "entriesColumn" >>= unsafeCastTo Gtk.TreeViewColumn . fromJust
  entriesColumnRenderer <- new Gtk.CellRendererText [ #editable := True ]
  Gtk.afterCellRendererTextEdited entriesColumnRenderer (onListEntryEdited model)
  
  Gtk.treeViewColumnPackStart entriesColumn entriesColumnRenderer True
  Gtk.cellLayoutSetCellDataFunc entriesColumn entriesColumnRenderer (Just dataFunc)

  -- Connect signals
  entriesList <- #getObject builder "entriesList" >>= unsafeCastTo Gtk.TreeView . fromJust
  notesView <- #getObject builder "notesView" >>= unsafeCastTo Gtk.TextView . fromJust

  on notesView #keyReleaseEvent (notesLostFocus notesView entriesList)
  -- other events does not work in all cases for whatever reason
  -- on notesView #focusOutEvent (notesLostFocus notesView entriesList)
  -- on notesView #leaveNotifyEvent (notesLostFocus notesView entriesList)
  -- Gtk.onWidgetFocusOutEvent notesView (notesLostFocus notesView entriesList)

  on entriesList #cursorChanged (listSelectionChanged notesView entriesList)

  addButton <- #getObject builder "addEntryButton" >>= unsafeCastTo Gtk.Button . fromJust
  on addButton #clicked (addEntry model)

  removeButton <- #getObject builder "removeEntryButton" >>= unsafeCastTo Gtk.Button . fromJust
  on removeButton #clicked (removeEntry entriesList)

  fileMenuSave <- #getObject builder "fileMenuSave" >>= unsafeCastTo Gtk.ImageMenuItem . fromJust
  -- on quitMenu #activate (save entriesList "data.json")

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

