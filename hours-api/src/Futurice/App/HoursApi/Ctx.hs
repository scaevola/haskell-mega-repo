{-# LANGUAGE TemplateHaskell #-}
module Futurice.App.HoursApi.Ctx where

import Prelude ()
import Futurice.Prelude
import Futurice.Cache (DynMapCache)
import Control.Concurrent.STM (TVar)

import qualified FUM
import qualified PlanMill as PM

type PlanmillUserLookupMap = HashMap FUM.UserName (FUM.User, PM.User)

-- | A quite stable data, which we update once in a while when service is run.
data PlanmillData = PlanmillData
    { _planmillUserLookup   :: !(PlanmillUserLookupMap)
    , _planmillProjects     :: !(HashMap PM.ProjectId (PM.Project, [PM.Task]))
    , _planmillTasks        :: !(HashMap PM.TaskId PM.Task)
    , _planmillCalendars    :: !(HashMap PM.CapacityCalendarId PM.CapacityCalendar)
    , _planmillTaskProjects :: !(HashMap PM.TaskId PM.Project)
    }
    -- TODO: add a field "reportable tasks per user"

-- TODO: mkPlanmillData from list of projects & tasks & calendars

mkPlanmillData
    :: PlanmillUserLookupMap
    -> [PM.Project]
    -> [PM.Task]
    -> [PM.CapacityCalendar]
    -> HashMap PM.UserId (Set PM.TaskId)  -- ^ reportable tasks per user
    -> PlanmillData
mkPlanmillData us ps ts cs _rts = undefined us ps ts cs

data Ctx = Ctx
    { ctxPlanmillData :: !(TVar PlanmillData)
    , ctxMockUser     :: !(Maybe FUM.UserName)
    , ctxCache        :: !DynMapCache
    , ctxLogger       :: !Logger
    , ctxPlanmillCfg  :: !PM.Cfg
    }

makeLenses ''PlanmillData