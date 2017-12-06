module Prometheus.Push.Job where

import Data.Text

-- | A 'Job' is a single push to a push gateway, and will set the @job@ label.

newtype Job = Job
  { -- | A record accessor to get/set the name of a job.
    jobName :: Text
  }



-- | Create a new 'Job' with a specific name.

job :: Text -> Job
job = Job
