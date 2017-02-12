# my-fx-apps
JavaFX apps from Clojure

This is an experimental project. What follows will just be random:
-----------

Q1
----
I know that the :root of a scene can be changed in a state-driven, declarative way:

:root (case replaced-mode
        :counter (test-control)
        :nothing (empty-control))

However is it possible to change the :root without using state? If the stage, which is a fn-fx.diff.Component
was instead a map then this would be possible:

(swap! (atom {:scene {:root nil}}) assoc-in [:scene :root] (test-control))

A fn-fx.diff.Component does not have a :scene, so this, equivalent to the above but where my-stage
is an atom that holds the stage, fails:

(swap! my-stage assoc-in [:scene :root] (test-control))

With: "ClassCastException fn_fx.diff.Component cannot be cast to clojure.lang.Associative"

Is there a `setScene` method on a stage component, or a `setRoot` method on a scene component?

Q2
----
Are the three threads a JavaFX thing or fn-fx thing?

Q3
----
Are the three threads the reason why can't get to the underlying node, so that can set transitions etc on it?
Original JavaFX Script was like Javascript in that it was single threaded.






