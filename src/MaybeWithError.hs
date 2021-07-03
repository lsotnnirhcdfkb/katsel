module MaybeWithError
    ( MaybeWithError
    , ErrorLoggedPromise

    , error_logged_promise

    , to_maybe
    , just_with_error
    , nothing_with_error
    ) where

import qualified Message (ToDiagnostic)

data ErrorLoggedPromise e = ErrorLoggedPromise
newtype MaybeWithError r e = MaybeWithError { to_maybe :: Maybe r }

error_logged_promise :: Message.ToDiagnostic e => e -> ErrorLoggedPromise e
error_logged_promise _ = ErrorLoggedPromise

just_with_error :: r -> MaybeWithError r e
just_with_error r = MaybeWithError (Just r)
nothing_with_error :: ErrorLoggedPromise e -> MaybeWithError r e
nothing_with_error _ = MaybeWithError Nothing
