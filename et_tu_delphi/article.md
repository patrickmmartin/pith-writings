`I come here not to bury Delphi, but to praise it.`
----

Good things:

RTTI that *worked*

- the language, well you either love it or you don't  
- cite recent rant on state of affairs in C++
- NO_MACROS!
- blindingly fast compilation
- almost perfect dependency management for units (IMHO)
- incremental, smart linker
- simple type system (well until generics, anyway)
 - records (heap, stack)
 - classes (heap *only*)
  - classes had langauge support for inheritance
 - well and also objects, so weird class / record hybrid everyone forgot about
- PROPERTIES
- READ ONLY PROPERTIES
- PROPERTIES
 - indexed properties
- a really good string type right out of the box: "c and c++ - don't think I can't see you in the corner - don't even start, ok?"
- it's just modules all the way down, baby
 - circular dependencies are a _compiler error_, not a "way of life" as some people seem to treat it
 - NO LIBS
 - proper compiler supported initialisation and finalisation
  - NO STATIC INITIALIZATION FIASCO
  - unless you want to write your own
- dynamic packages - again something c/ c++ are still yearning for
- An IDE "that was really good, once"
- well thought through streaming system 
 - feeding into a well through through form design system
-  truly amazing commitment to compatibility (for this day and age) demo apps from VCL version 1(?) _would load and run upon pressing F9 in VCL version 17_
- no RAII :P
- easy FFI interface for "C", COM, c++ with some effort
- language supported reference counted interfaces - so RAII if you want it :)
- IDispatch support (freaky, but whatever floats your boat)
- entire application could be statically linked (with very minimal dependencies) or not, at the "press of a button"
- Exception handling that "just worked"
  - related to the singly rooted object hierarchy
   - no `throw 99` - you know who you are!
 - _finally_ blocks - sometimes cleanup is needed
 - there were some very successful templates which were exploited to produce solutions 

#TODO: break it down

## singly rooted class hierarchy

## RTTI


## Property syntax
### read-write - with getters /setters | direct access | a mixture
### read only properties discoverable at runtime using RTTI
### indexed properties 
### write only properties


## more compile-time gurantees 
- initialization
  
- finalization

## pretty decent set of application classes


## pretty decent set of windowing clases
- handled ownership and parenting and all that good stuff in a manageable manner

## strong component design methodology and support
- interesting, component model from the days of yore, which was part of the then thriving component market 



