{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FUM.Types.SSHKey (
    SSHKeyFingerprint,
    sshKeyFingerprint,
    getSSHKeyFingerprint,
    SSHKey (..),
    mkSSHKey,
    ) where

import Control.Lens                  ((<&>))
import Data.Aeson                    (withText)
import Futurice.Generics
import Futurice.Lucid.Foundation     (ToHtml (..), code_)
import Futurice.Prelude
import Prelude ()
import System.Exit                   (ExitCode (..))
import System.IO.Unsafe              (unsafePerformIO)
import System.Process                (readProcessWithExitCode)
import Text.Regex.Applicative.Common (decimal)
import Text.Regex.Applicative.Text   (anySym, match, string)

newtype SSHKeyFingerprint = SSHKeyFingerprint Text
  deriving (Eq, Ord, Show)

getSSHKeyFingerprint :: SSHKeyFingerprint -> Text
getSSHKeyFingerprint = coerce

data SSHKey = SSHKey
    { _sshKeyFingerprint :: !SSHKeyFingerprint
    , sshKeyContext     :: !Text
    }
  deriving Show

sshKeyFingerprint :: Lens' SSHKey SSHKeyFingerprint
sshKeyFingerprint g (SSHKey f c) = g f <&> \f' -> SSHKey f' c

-------------------------------------------------------------------------------
-- SSHKeyFingerprint instances
-------------------------------------------------------------------------------

instance NFData SSHKeyFingerprint where
    rnf (SSHKeyFingerprint x) = rnf x

instance ToHtml SSHKeyFingerprint where
    toHtmlRaw = toHtml
    toHtml (SSHKeyFingerprint f) = code_ $ toHtml f

instance ToJSON SSHKeyFingerprint where
    toJSON = toJSON . getSSHKeyFingerprint

instance ToHttpApiData SSHKeyFingerprint where
    toQueryParam = getSSHKeyFingerprint

instance FromJSON SSHKeyFingerprint where
    parseJSON = withText "SSHKeyFingerprint" $ either fail pure . mkSSHKeyFingerprint

instance FromHttpApiData SSHKeyFingerprint where
    parseQueryParam = first (view packed) . mkSSHKeyFingerprint

mkSSHKeyFingerprint :: Text -> Either String SSHKeyFingerprint
mkSSHKeyFingerprint t = case matchFingerprint t of
    Nothing                    -> Left $ "Cannot match fingerprint: " ++ show t
    Just (bs, RSA) | bs < 2048 -> Left $ "RSA keys should have >= 2048 bits: " ++ show t
    Just _                     -> Right (SSHKeyFingerprint t)

-------------------------------------------------------------------------------
-- Fingerprint validation
-------------------------------------------------------------------------------

data SSHKeyType = RSA | ED25519
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Extract bits and type from fingerprint
matchFingerprint :: Text -> Maybe (Int, SSHKeyType)
matchFingerprint = match $ (,)
    <$> decimal
    <* string " "
    <* many anySym
    <* string "("
    <*> (RSA <$ string "RSA" <|> ED25519 <$ "ED25519")
    <* string ")"
    <* many anySym

-------------------------------------------------------------------------------
-- SSHKey Instances
-------------------------------------------------------------------------------

instance Eq SSHKey where
    x == y = _sshKeyFingerprint x == _sshKeyFingerprint y

instance Ord SSHKey where
    compare x y = compare (_sshKeyFingerprint x) (_sshKeyFingerprint y)

instance NFData SSHKey where
    rnf (SSHKey f c) = rnf f `seq` rnf c

instance ToHtml SSHKey where
    toHtmlRaw = toHtml
    toHtml (SSHKey f _) = code_ $ toHtml f

instance ToJSON SSHKey where
    toJSON = toJSON . sshKeyContext

instance ToHttpApiData SSHKey where
    toQueryParam = sshKeyContext

instance FromJSON SSHKey where
    parseJSON = withText "SSHKey" $ either fail pure . unsafeMkSSHKey

instance FromHttpApiData SSHKey where
    parseQueryParam = first (view packed) . unsafeMkSSHKey

-- Even we call @ssh-keygen@ the operation should be pure.
--
-- Note: we **don't** export this function.
unsafeMkSSHKey :: Text -> Either String SSHKey
unsafeMkSSHKey = unsafePerformIO . mkSSHKey

mkSSHKey :: Text -> IO (Either String SSHKey)
mkSSHKey contents = do
    (c, out, err) <- readProcessWithExitCode "ssh-keygen" ["-l", "-f", "-" ] (contents ^. unpacked)
    return $ case c of
        ExitFailure _ -> Left $ out <> err
        ExitSuccess   -> SSHKey <$> mkSSHKeyFingerprint (out ^. packed) <*> pure contents
