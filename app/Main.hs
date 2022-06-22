module Main where

import           Control.Monad
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Text.ICU
import           Data.Text.ICU.Char
import           System.Environment
import           Telegram.Bot.API
import           Telegram.Bot.Simple

type Model = ()

data Action
  = DeleteEmoji ChatId MessageId

echoBot :: BotApp Model Action
echoBot = BotApp
  { botInitialModel = ()
  , botAction = updateToAction
  , botHandler = handleAction
  , botJobs = []
  }

updateToAction :: Update -> Model -> Maybe Action
updateToAction update _ = case updateMessage update of
  Just msg -> checkMsg msg
  Nothing  -> checkMsg =<< updateEditedMessage update

checkMsg :: Message -> Maybe Action
checkMsg msg = case messageText msg of
  Just text | hasEmoji text -> Just $ buildAction msg
  _                         -> Nothing

whitelist :: Text
whitelist = nfc (numbers <> apl <> asсii <> unicode)
  where
    numbers = "1234567890"
    apl = "↕️↩️"
    asсii = "*#©®"
    unicode = "™♀♂"

buildAction :: Message -> Action
buildAction msg = DeleteEmoji (chatId $ messageChat msg) (messageMessageId msg)

hasEmoji :: Text -> Bool
hasEmoji = T.any (\c -> property Emoji c && not (T.elem c whitelist)) . nfc

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  DeleteEmoji chat msg -> model <# do
    void . liftClientM $ deleteMessage chat msg

run :: Token -> IO ()
run token = do
  env <- defaultTelegramClientEnv token
  startBot_ echoBot env

main :: IO ()
main = do
  token <- Token . T.pack <$> getEnv "TG_TOKEN"
  run token
