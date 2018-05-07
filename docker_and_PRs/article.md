Docker is fun and exciting for many reasons, but mainly because it imposes less coffee breaks at random times.



However, if you want to be a Good Engineer (TM), then the issue is that the workflow (IMHO) should be a DAG.

source -> base runtime -> base libraries -> application

and this needs to 
* stay true
* still be fast




Locally
-----------------

You only have one pair of hands, so not so bad


On a CI server
--------------



Need some unique tuple of (PR, build, [state of source])
