module Main where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.ICU
import Data.Text.ICU.Char
import System.Environment
import Telegram.Bot.API
import Telegram.Bot.Simple

type Model = ()

type UserName = Maybe Text

data Action
  = DeleteEmojiWithResend
      { chatOfSender :: ChatId
      , senderMessage :: MessageId
      , nameOfUser :: UserName
      , originalReply :: Maybe MessageId
      , originalText :: Text
      }
  | DeleteEmojiWithDump
      { chatOfSender :: ChatId
      , senderMessage :: MessageId
      , nameOfUser :: UserName
      }
  | DeleteEmoji
      { chatOfSender :: ChatId
      , senderMessage :: MessageId
      }

echoBot :: BotApp Model Action
echoBot =
  BotApp
    { botInitialModel = ()
    , botAction = updateToAction
    , botHandler = handleAction
    , botJobs = []
    }

updateToAction :: Update -> Model -> Maybe Action
updateToAction update _ = case updateMessage update of
  Just msg -> checkMsg msg
  Nothing -> checkMsg =<< updateEditedMessage update

checkMsg :: Message -> Maybe Action
checkMsg msg = case messageText msg of
  Just text -> deduceAction text (emojiCount text) msg
  _ -> Nothing

deduceAction :: Text -> Int -> Message -> Maybe Action
deduceAction _ 0 _ = Nothing
deduceAction t c msg | c > T.length t `div` 3 = Just $ buildDumpAction msg
deduceAction t _ msg =
  Just $
    buildAction
      msg
      (messageMessageId <$> messageReplyToMessage msg)
      t

whitelist :: Text
whitelist = nfc (numbers <> apl <> asсii <> unicode)
 where
  numbers = "1234567890"
  apl = "↙↖↔↕️↩️"
  asсii = "*#©®"
  unicode = "™♀♂"

buildDumpAction :: Message -> Action
buildDumpAction msg =
  getChatId'n'MessageId'n'UserName msg DeleteEmojiWithDump

buildAction :: Message -> Maybe MessageId -> Text -> Action
buildAction msg =
  getChatId'n'MessageId'n'UserName msg DeleteEmojiWithResend

getChatId'n'MessageId'n'UserName :: Message -> (ChatId -> MessageId -> UserName -> a) -> a
getChatId'n'MessageId'n'UserName msg f =
  f
    (chatId $ messageChat msg)
    (messageMessageId msg)
    (userFirstName <$> messageFrom msg)

emojiCount :: Text -> Int
emojiCount = T.length . T.filter isEmoji . nfc

isEmoji :: Char -> Bool
isEmoji c = property Emoji c && not (T.elem c whitelist)

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  DeleteEmojiWithResend chat msg user r text ->
    model <# do
      let newMsg = bulidMsg user text
      sendMsgTo newMsg chat r
      pure (DeleteEmoji chat msg)
  DeleteEmojiWithDump chat msg user ->
    model <# do
      sendMsgTo (buildDump user) chat Nothing
      pure (DeleteEmoji chat msg)
  DeleteEmoji chat msg ->
    model <# do
      void . liftClientM $ deleteMessage chat msg

buildDump :: UserName -> Text
buildDump Nothing = "Я не понимаю, что происходит"
buildDump (Just user) =
  user <> " попытался отправить эмодзи, но доблестные силы контр-разведки его перехватили. Literally 1984!"

sendMsgTo :: Text -> ChatId -> Maybe MessageId -> BotM ()
sendMsgTo msg chat r =
  void . liftClientM $
    sendMessage
      SendMessageRequest
        { sendMessageChatId = SomeChatId chat
        , sendMessageText = msg
        , sendMessageParseMode = Nothing
        , sendMessageEntities = Nothing
        , sendMessageDisableWebPagePreview = Nothing
        , sendMessageDisableNotification = Just True
        , sendMessageProtectContent = Nothing
        , sendMessageReplyToMessageId = r
        , sendMessageAllowSendingWithoutReply = Nothing
        , sendMessageReplyMarkup = Nothing
        }

bulidMsg :: UserName -> Text -> Text
bulidMsg user t = maybe id (\name t' -> name <> " сказал, что:\n" <> t') user (T.filter (not . isEmoji) t)

run :: Token -> IO ()
run token = do
  env <- defaultTelegramClientEnv token
  startBot_ echoBot env

main :: IO ()
main = do
  token <- Token . T.pack <$> getEnv "TG_TOKEN"
  run token
