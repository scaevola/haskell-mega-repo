{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FUM.Types.SSHKey (SSHKey (..), mkSSHKey) where

import Data.Aeson                    (withText)
import Futurice.Generics
import Futurice.Lucid.Foundation     (ToHtml (..), code_)
import Futurice.Prelude
import Prelude ()
import System.Exit                   (ExitCode (..))
import System.Process                (readProcessWithExitCode)
import Text.Regex.Applicative        (anySym, match, string)
import Text.Regex.Applicative.Common (decimal)
import System.IO.Unsafe (unsafePerformIO)

data SSHKey = SSHKey
    { sshKeyFingerprint :: !Text
    , sshKeyContext     :: !Text
    }
  deriving Show

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance Eq SSHKey where
    x == y = sshKeyFingerprint x == sshKeyFingerprint y

instance Ord SSHKey where
    compare x y = compare (sshKeyFingerprint x) (sshKeyFingerprint y)

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

-------------------------------------------------------------------------------
-- Creation
-------------------------------------------------------------------------------

mkSSHKey :: Text -> IO (Either String SSHKey)
mkSSHKey contents = do
    (c, out, err) <- readProcessWithExitCode "ssh-keygen" ["-l", "-f", "-" ] (contents ^. unpacked)
    return $ case c of
        ExitFailure _ -> Left $ out <> err
        ExitSuccess   -> case matchFingerprint out of
            Nothing                    -> Left $ "Cannot match fingerprint: " ++ out
            Just (bs, RSA) | bs < 2048 -> Left $ "RSA keys should have >= 2048 bits: " ++ out
            Just _                     -> Right $ SSHKey (out ^. packed) contents

data SSHKeyType = RSA | ED25519
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Extract bits and type from fingerprint
matchFingerprint :: String -> Maybe (Int, SSHKeyType)
matchFingerprint = match $ (,)
    <$> decimal
    <* string " "
    <* many anySym
    <* string "("
    <*> (RSA <$ string "RSA" <|> ED25519 <$ "ED25519")
    <* string ")"
    <* many anySym
