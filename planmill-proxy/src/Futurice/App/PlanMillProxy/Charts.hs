{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Futurice.App.PlanMillProxy.Charts (
    timereportsAgeDistr
    ) where

import Control.Lens                   ((.=))
import Futurice.PostgresPool
import Futurice.Prelude
import Graphics.Rendering.Chart.Utils (isValidNumber)
import Prelude ()
import Servant.Chart                  (Chart (..))

import qualified Database.PostgreSQL.Simple    as Postgres
import qualified Graphics.Rendering.Chart.Easy as C
import qualified Futurice.Chart.Histogram      as FC

import Futurice.App.PlanMillProxy.Types (Ctx (..))

timereportsAgeDistr :: Ctx -> IO (Chart "cache-distr")
timereportsAgeDistr ctx = do
    res <- poolQuery_ ctx selectTimereportsQuery
    let xs = Postgres.fromOnly <$> res

    res' <- poolQuery_ ctx selectDataQuery
    let ys = Postgres.fromOnly <$> res'

    let r = (safeMinimum (xs ++ ys), safeMaximum (xs ++ ys))

    return $ Chart . C.toRenderable $ do
        -- Timereports
        C.plot $ return $ FC.histToPlot $ FC.defaultFloatPlotHist
              & FC.plot_hist_title  .~ "timereports, oldest " ++ show (safeMinimum xs)
              & FC.plot_hist_values .~ xs
              & FC.plot_hist_bins   .~ 100
              & FC.plot_hist_range  ?~ r
              & FC.plot_hist_line_style .~ mkLineStyle C.blue
              & FC.plot_hist_fill_style .~ mkFillStyle C.blue

        -- Other data
        C.plot $ return $ FC.histToPlot $ FC.defaultFloatPlotHist
              & FC.plot_hist_title  .~ "other data, oldest " ++ show (safeMinimum ys)
              & FC.plot_hist_values .~ ys
              & FC.plot_hist_bins   .~ 100
              & FC.plot_hist_range  ?~ r
              & FC.plot_hist_line_style .~ mkLineStyle C.red
              & FC.plot_hist_fill_style .~ mkFillStyle C.red

        -- Layout
        C.layout_title .= "Cache data age"
        C.layout_x_axis . C.laxis_title    .= "time"
        C.layout_y_axis . C.laxis_title    .= "records"
        C.layout_y_axis . C.laxis_generate .= zeroLogAxis C.def
  where
    safeMinimum [] = UTCTime (ModifiedJulianDay 0) 0
    safeMinimum xs = minimum xs

    safeMaximum [] = UTCTime (ModifiedJulianDay 0) 0
    safeMaximum xs = maximum xs

    selectTimereportsQuery :: Postgres.Query
    selectTimereportsQuery = fromString $ unwords
        [ "SELECT updated as a"
        , "FROM planmillproxy.timereports"
        , "WHERE age(day) < '3 years'"
        , ";"
        ]

    selectDataQuery :: Postgres.Query
    selectDataQuery = fromString $ unwords
        [ "SELECT updated as a"
        , "FROM planmillproxy.cache"
        , ";"
        ]

mkLineStyle :: C.Colour Double -> C.LineStyle
mkLineStyle c = (C.solidLine 1 $ C.opaque c)
     { C._line_cap  = C.LineCapButt
     , C._line_join = C.LineJoinMiter
     }

mkFillStyle :: C.Colour Double -> C.FillStyle
mkFillStyle c = C.solidFillStyle (C.withOpacity c 0.3)

-------------------------------------------------------------------------------
-- Our log function
-------------------------------------------------------------------------------

zeroLogAxis :: forall a. (Show a, RealFloat a) => C.LogAxisParams a -> C.AxisFn a
zeroLogAxis lap ps0 = C.makeAxis' forward backward
    (C._loga_labelf lap) (wrapped', wrapped', wrapped')
  where
    forward :: a -> Double
    forward x | x > 0     = 1 + log (realToFrac x / lowerBound)
              | otherwise = 0

    backward :: Double -> a
    backward x | x > 0     = realToFrac (lowerBound *  exp (x - 1))
               | otherwise = 0

    ps         = filter (\x -> isValidNumber x && x > 0) ps0

    minV, maxV :: Integer
    maxV       = truncate (maximum ps)
    minV       = truncate (minimum ps)

    lowerBound :: Double
    lowerBound = fromInteger $  go 1 where
        go e | e * 4 > minV  = e
             | e * 10 > minV = e * 4
             | otherwise     = go (e * 10)

    wrapped'  = map fromInteger logTicks'
    logTicks' = 0 : go 1 where
        go e | e > maxV      = [e]
             | 4 * e > maxV  = [e, 4 * e]
             | 4 * e > minV  = e : 4 * e : go (10 * e)
             | 10 * e > minV = 4 * e :  go (10 * e)
             | otherwise     = go (10 * e)
