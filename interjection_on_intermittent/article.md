Concurrency is hard
====================
 
"That's why (almost) everyone is getting out of that game" (one way or the other)


There are 4 regimes

* Fails hard and reliably under concurrency
* Never observed to fail under concurrency
* fails intermittently under concurrency
* actually correct under concurrency


Why it's hard
----------------



### difficult to reason about
#### pop quiz: how does your debugger work?




Why it's hard II
----------------


overlapping critical sections
mark space ratio
 Never -> 50% -> Thundering herd
 
 
 slightly before 50% the software becomes *no use to man nor beast*


Why it's hard in real life
--------------------------


And this assumes the critical sections are random.
Imagine that they are being serialised in and of themselves by some other process (LIKE YOUR DEBUG LOGGING):   
disturb this and you are in an unexplored world of hurt



