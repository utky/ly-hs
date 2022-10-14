{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Ilyaletre.Ly.Api where

import Ilyaletre.Ly.Core
import Servant.API

type TaskAPI =
  "tasks" :> Get '[JSON] [TaskView]
    :<|> "tasks" :> Capture "taskid" Id :> Get '[JSON] TaskView
    :<|> "tasks" :> Capture "taskid" Id :> DeleteNoContent
    :<|> "tasks" :> ReqBody '[JSON] AddTaskRequest :> Post '[JSON] TaskView
    :<|> "tasks" :> ReqBody '[JSON] UpdateTaskRequest :> Put '[JSON] TaskView
