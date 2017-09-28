# elm-rethink-http

Rethinking an http request in elm. This is an example app for a Front End Orlando talk on elm.

### 3 branches to look over
Start with the originalRequest branch, then maybeRefactor and finally unionTypesRefactor to see the progression.

##### originalRequest
This is how I originally began to write elm code. I set the repos model to an empty List Repo. It comes from working with react / redux. Have to set loading and request received flags to check what to display to the screen.

##### maybeRefactor
I changed the repos model to a Maybe (List Repo) in order to eliminate the request received flags. This made the view function more readable, but we were still checking for error messages and loading states.

##### unionTypesRefactor
This is the final version. I changed repos to take a union type of ExternalRequest error data with values of NotRequested | Loading | Failure error | Success data. I no longer had any flags to set and moved the errors into the ExternalRequest union type. The model shrank in half and the view function became a pattern matching case expression, making it much more readable.
