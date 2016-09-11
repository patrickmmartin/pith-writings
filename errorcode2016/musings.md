Musings
===

Follow-up to the roaring success of 

errorcode2015 (https://github.com/patrickmmartin/pith-writings/tree/master/errorcode2015)


TODO CHECK
---

### Complexity arguments

https://en.wikipedia.org/wiki/Cyclomatic_complexity


### Performance arguments for simple pointer


### Simplicity arguments for simple pointer


### GODDAMN I'M GOING TO CAST IT TO AN INT


### Data structures for tacking metadata onto error_id
Ideally, for C land we need something that does not need to be freed (or supplies its deleter?)
Obviously we will be led to an `error_info` like structure which will have all the data


``` C
typedef struct TAGerror_info 
{
  param_id parameter;
  value_id value;
} error_info, error_info_p
```


#### ORA-00942 table or view does not exist.
WAT - this can be 
  * BY DESIGN
  * wrong user
  * wrong grants
  * WRONG DATABASE

#### File does not exist
  * important file?
  * race condition?
  * create on demand?


#### I could go on


### C land printf(NULL)
Is this a problem (in most sensible systems)?


### C++ land print_error(NULL)
Make this work?

