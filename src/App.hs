{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module App where

import Base.Create ( Create )
import Base.Filter ( Filter )
import Base.Update ( Update )
import Database.MongoDB qualified as Mongo
import Database.Types ( Entity(Entity), DBEntity, WithMongo )
import Entities.User as User ( User(..) )
import Entities.Article as Article ( Article(..) )
import Entities.Comment as Comment ( Comment(..) )
import Front ( Front )
import Data.Time ( getCurrentTime )
import Database.Utils ( queryEntity )
import Database.Handlers
    ( dbCreate, dbUpdate, dbSearch, dbLoadByID )
import Web.Scotty
    ( get, json, jsonData, post, put, scotty, text, ActionM, ScottyM )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Data.Generics.Labels ()
import Control.Lens ( (?~) )
import Data.Aeson.Types (FromJSON)
import qualified Data.Aeson.Types as J

type Handler = WithMongo => ActionM ()

app :: IO ()
app = do
  pipe <- Mongo.connect (Mongo.host "127.0.0.1")
  let ?pipe = pipe
  let ?database = "database"
  scotty 8000 do
    userHandlers
    articleHandlers
    commentHandlers
  
userHandlers :: WithMongo => ScottyM ()
userHandlers = do
  post "/user"        createUser
  get  "/user"        (getById @User)
  put  "/user"        updateUserByID
  get  "/user/search" (getByFilter @User)

createUser :: Handler
createUser = do
  User {..} <- jsonData @(User (Front Create))
  alreadyExists <- liftIO $ dbSearch $ queryEntity @User (#entity . #login ?~ [login]) 
  case alreadyExists of
    [] -> do
      userID <- liftIO do 
        now <- getCurrentTime 
        dbCreate $ User { registered = now, modified = Nothing,  .. }
      json userID
    _ -> text "User with such ID already exists"
    
updateUserByID :: Handler
updateUserByID = do
  Entity entityID User {..} <- jsonData @(Entity User (Front Update))
  liftIO do
    now <- getCurrentTime 
    dbUpdate $ Entity entityID User { registered = Nothing, modified = Just (Just now), .. }

articleHandlers :: WithMongo => ScottyM ()
articleHandlers = do
  post "/article"        createArticle
  get  "/article"        (getById @Article)
  put  "/article"        updateUserByID
  get  "/article/search" (getByFilter @Article)

createArticle :: Handler
createArticle = do
  Article {..} <- jsonData @(Article (Front Create))
  user <- liftIO $ dbLoadByID userID
  case user of
    Just _ -> do 
      articleID <- liftIO do 
        now <- getCurrentTime 
        dbCreate $ Article { created = now, modified = Nothing, .. }
      json articleID
    Nothing -> text "User with such ID does not exist"

updateArticle :: Handler
updateArticle = do
  Entity entityID Article {..} <- jsonData @(Entity Article (Front Update))
  liftIO do
    now <- getCurrentTime 
    dbUpdate $ Entity entityID Article { created = Nothing, modified = Just (Just now), .. }

commentHandlers :: WithMongo => ScottyM ()
commentHandlers = do
  post "/comment"        createComment
  get  "/comment"        (getById @Comment)
  put  "/comment"        updateCommentByID
  get  "/comment/search" (getByFilter @Comment)

createComment :: Handler
createComment = do
  Comment {..} <- jsonData @(Comment (Front Create))
  user <- liftIO $ dbLoadByID userID
  article <- liftIO $ dbLoadByID articleID
  case (user, article) of
    (Just _, Just _) -> do
      commentID <- liftIO do
        now <- getCurrentTime 
        dbCreate $ Comment { created = now, modified = Nothing, .. }
      json commentID
    (Nothing, _) -> text "User with such ID does not exist"
    (_, Nothing) -> text "Article with such ID does not exists"

updateCommentByID :: Handler
updateCommentByID = do
  Entity entityID Comment {..} <- jsonData @(Entity Comment (Front Update))
  liftIO do
    now <- getCurrentTime 
    dbUpdate $ Entity entityID Comment { created = Nothing, modified = Just (Just now), .. }

getById :: forall e. (WithMongo, DBEntity e, J.ToJSON (e Create)) => Handler
getById = do
  eID <- jsonData
  e <- liftIO do dbLoadByID @e eID
  json e

getByFilter :: forall e. (DBEntity e, J.FromJSON (e Filter), J.ToJSON (e Create)) => Handler
getByFilter = do
  eFilter <- jsonData @(Entity e Filter)
  liftIO (dbSearch eFilter) >>= json