`monitor-stumble` is the initial work we did -- just stumble along without a
plan until we get something working.

The problems with our "stumble" version:

- Not fault tolerant
- Not canonical Erlang "app"

`monitor-app` is an app created with the help of [e2](http://e2project.org),
which is basically equivalent to our stumble version, but is run correctly as
an OTP app. We get fault tolerance (process supervision) and a proper framework
for adding new features.

In particular, we added:

- Support for externally configuring our app
- Support for multiple concurrent checks
