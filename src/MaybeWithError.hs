module MaybeWithError
    ( MaybeWithError(..)
    , ErrorLoggedPromise

    , error_logged_promise
    , to_maybe
    ) where

import qualified Message (ToDiagnostic)

data ErrorLoggedPromise e = ErrorLoggedPromise
data MaybeWithError r e
    = JustWithError r
    | NothingWithError (ErrorLoggedPromise e)

error_logged_promise :: Message.ToDiagnostic e => e -> ErrorLoggedPromise e
error_logged_promise _ = ErrorLoggedPromise

to_maybe :: MaybeWithError r e -> Maybe r
to_maybe (JustWithError r) = Just r
to_maybe (NothingWithError _) = Nothing
