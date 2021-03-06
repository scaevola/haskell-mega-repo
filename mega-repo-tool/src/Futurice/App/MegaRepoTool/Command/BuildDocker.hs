{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.MegaRepoTool.Command.BuildDocker (
    buildDocker,
    AppName,
    ) where

import Data.Aeson
       (object, (.=))
import Data.Yaml        (decodeFileEither)
import Futurice.Prelude
import Prelude ()
import System.Directory (copyFile)
import System.Exit      (exitFailure)
import System.FilePath  ((</>))
import System.IO        (hClose, hFlush)
import System.IO.Temp   (withTempFile)
import System.Process   (callProcess, readProcess)

import qualified Data.Map         as Map
import qualified Data.Text        as T
import qualified Data.Text.IO     as T
import qualified Text.Microstache as M

import Futurice.App.MegaRepoTool.Config

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------

buildCmd :: Text -> Text
buildCmd buildImage = T.unwords
    [ "docker run"
    , "--rm"
    , "-ti"
    , "--entrypoint /app/src/build-in-linux-cabal.sh"
    , "-e DOCKER=YES" -- tell script we are in docker
    , "-v $(pwd):/app/src"
    , "-v haskell-mega-repo-cabal:/home/root/.cabal"
    , "-v haskell-mega-repo-dist:/app/src/dist-newstyle-prod"
    , buildImage
    ]

-------------------------------------------------------------------------------
-- Command

buildDocker :: [AppName] -> IO ()
buildDocker appnames = do
    -- Read config
    cfg <- either throwM pure =<< decodeFileEither "mega-repo-tool.yaml"

    -- `some` verifies images aren't empty
    when (null appnames) $ do
        putStrLn "Image names are required"
        exitFailure

    -- What apps to build?
    apps <- fmap Map.fromList $ for appnames $ \appname -> do
        case cfg ^? mrtApps . ix appname of
            Nothing -> do
                putStrLn $ "Unknown app: " <> appname ^. unpacked
                exitFailure
            Just app -> pure (appname, app)

    -- Get the hash of current commit
    githash' <- readProcess "git" ["log", "--pretty=format:%h", "-n", "1"] ""
    _        <- readProcess "git" ["rev-parse", "--verify", githash'] ""
    let githash = githash' ^. packed
    T.putStrLn $ "Git hash aka tag for images: " <> githash

    -- Check that binaries are build with current hash
    githashBuild <- (T.strip <$> T.readFile "build/git-hash.txt") `catch` onIOError "<none>"

    when (githashBuild /= githash) $ do
        T.putStrLn $ "Git hash in build directory don't match: " <> githashBuild  <> " != " <> githash
        T.putStrLn $ "Make sure you have data volumes:"
        T.putStrLn $ "  docker volume create --name haskell-mega-repo-cabal"
        T.putStrLn $ "  docker volume create --name haskell-mega-repo-dist"
        T.putStrLn $ "Run following command to build image:"
        T.putStrLn $ "  " <> (buildCmd $ mrtDockerBaseImage cfg)
        exitFailure

    -- Build docker images
    images <- ifor apps $ \appname (ImageDefinition image exe) -> do
        -- Write Dockerfile
        dockerfile <- makeDockerfile exe cfg
        T.putStrLn dockerfile
        let directory = "build" </> exe ^. unpacked
        withTempFile directory "Dockerfile." $ \fp handle -> do
            -- Because of
            -- https://bugs.launchpad.net/ubuntu/+source/graphviz/+bug/1409280
            -- we have own sfdp
            --
            -- To compile own use sfdp
            -- /configure --prefix=/opt/graphviz --with-gtk=no --with-glade=no --with-glut=no --with-gts=yes --with-smyrna=no --with-pangocairo=yes --enable-static --disable-shared
            copyFile "vendor/sfdp" (directory </> "sfdp")

            -- write dockerfile
            T.hPutStrLn handle dockerfile
            hFlush handle
            hClose handle

            -- Build an image
            let fullimage = "futurice/" <> image <> ":" <> githash
            callProcess "docker" ["build", "-t", T.unpack fullimage, "-f", fp, directory]

            -- accumulate image names
            pure (appname, fullimage, "futurice/" <> image)

    T.putStrLn "Upload images by:"
    for_ images $ \(_, image, _) ->
        T.putStrLn $ "  docker push " <> image

    T.putStrLn "Deploy images by:"
    for_ images $ \(appname, _, image) ->
        T.putStrLn $ "  futuswarm app:deploy"
            <> " --name " <> appname
            <> " --image " <> image
            <> " --tag " <> githash

makeDockerfile :: Text -> MRTConfig -> IO Text
makeDockerfile exe cfg = do
    traverse_ (putStrLn . M.displayMustacheWarning) warns
    return (rendered ^. strict)
  where
    MustacheT tmpl = mrtDockerfileTmpl cfg
    debs =  T.intercalate " " (mtrDebs cfg)
    (warns, rendered) = M.renderMustacheW tmpl $ object
        [ "debs" .= debs
        , "exe" .= exe
        ]


onIOError :: Monad m => a -> IOError -> m a
onIOError v _ = return v
