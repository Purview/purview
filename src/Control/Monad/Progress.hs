-- | The 'ProgressT' monad transformer, which allows an arbitrary
-- procedure to be monitored via progress events.
module Control.Monad.Progress
       ( -- * The Progress monad
         Progress
       , runProgress
         -- * The ProgressT monad transformer
       , ProgressT(..)
       , runProgressT
         -- * Progress metadata
       , TaskStack
       , Task(..)
         -- * Progress operations
       , task
       , step
       ) where

import Control.Monad
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.Identity
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import Data.Word

{-|
The parameterizable progress monad.

Computations are tagged with 'task's, and can yield progress 'step'
events, which are used to track the progress of the wrapped
procedure.

The 'return' function simply creates a taskless procedure with no
steps, while '(>>=)' adds a procedure to the currently active step.
-}
type Progress l = ProgressT l Identity

-- | Runs a pure progress procedure and, if the procedure completes a
-- step, returns the 'TaskStack' at that step and the continuation of
-- the procedure, or, if the procedure is complete, returns the
-- computed value.
runProgress :: Progress l a     -- ^ The progress procedure to run
               -> Either (Progress l a, TaskStack l) a
runProgress = runIdentity . runProgressT

{-|
The progress monad transformer, with an inner monad.

Computations are tagged with 'task's, and can yield progress 'step'
events, which are used to track the progress of the wrapped
procedure.

The 'return' function simply creates a taskless procedure with no
steps, while '(>>=)' adds a procedure to the currently active step.
-}
newtype ProgressT l m a =
  ProgressT
  { -- | The underlying 'Coroutine' describing the procedure in progress.
    procedure ::
       Coroutine
       (Yield (TaskStack l))
       (StateT (TaskStack l) m) a
  }

instance MonadTrans (ProgressT l) where
  lift = ProgressT . lift . lift

instance Monad m => Monad (ProgressT l m) where
  return = ProgressT . return
  p >>= f = ProgressT (procedure p >>= procedure . f)

instance MonadIO m => MonadIO (ProgressT l m) where
  liftIO = lift . liftIO

-- | Runs a progress procedure and, if the procedure completes a
-- step, returns the 'TaskStack' at that step and the continuation of
-- the procedure, or, if the procedure is complete, returns the
-- computed value.
runProgressT :: Monad m
                => ProgressT l m a  -- ^ The progress procedure to run
                -> m (Either (ProgressT l m a, TaskStack l) a)
runProgressT action = do
  result <- evalStateT (resume . procedure $ action) []
  return $ case result of
    Left (Yield stack cont) -> Left (ProgressT cont, stack)
    Right a -> Right a


-- | A stack with information about running 'Task's. The currently
-- running task is the first element in the stack; parent tasks
-- follow subsequently.
type TaskStack l = [Task l]

-- | A currently running task, with an user-defined label describing
-- the task
data Task l =
  Task
  { taskLabel :: l          -- ^ The task label
  , taskTotalSteps :: Word  -- ^ Total steps required to complete the task
  , taskStep :: Word        -- ^ The step at which the running task is
  } deriving (Show, Eq)

-- | Creates a new 'Task' to be tracked for progress. The task is given
-- a label that can be used to mark it with arbitrary metadata.
task :: Monad m
        => l                -- ^ The task label
        -> Word             -- ^ Total number of steps required to
                            -- complete the task
        -> ProgressT l m a  -- ^ The action describing the steps
                            -- necessary to complete the task
        -> ProgressT l m a
task label steps action = ProgressT $ do
  -- Add the task to the task stack
  lift . modify $ pushTask newTask

  -- Perform the procedure for the task
  result <- procedure action

  -- Insert an implicit step at the end of the task
  procedure step

  -- TODO Check if all the steps completed
  -- The task is completed, and is removed
  lift . modify $ popTask

  return result
  where
    newTask = Task label steps 0
    pushTask = (:)
    popTask = tail

-- | Marks one step of the current task as completed. If the task
-- already is completed, meaning that all the steps have been
-- performed, does nothing.
step :: Monad m => ProgressT l m ()
step = ProgressT $ do
  (current : tasks) <- lift get
  let currentStep = taskStep current
      nextStep =
        if currentStep < taskTotalSteps current
        then currentStep + 1
        else currentStep
      updatedTask = current { taskStep = nextStep }
      updatedTasks = updatedTask : tasks
  when (currentStep /= nextStep) $ yield updatedTasks
  lift . put $ updatedTasks