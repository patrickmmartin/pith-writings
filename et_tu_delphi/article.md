`I come here not to bury Delphi, but to praise it.`
----

Key  things:

- built in concept of _interface_ and _implementation_ 

- hey, that super trendy _"no copy constructor"_ concept again!

RTTI that *worked*

- the language, well you either love it or you don't  
```pascal
procedure Proc
var
   i : Integer;
begin
   // we can have comments
   (*
    in both styles
   *)
end;
```

- cite recent rant on state of affairs in C++
- NO_MACROS!
```pascal
$IFDEF _ 
$ENDIF
```
  - yup, no ```#define private public``` ...

- blindingly fast compilation
  You'll have to just take my word for it

- a really good string type right out of the box: "c and c++ - don't think I can't see you in the corner - don't even start, ok?"
 language supports concatentation, reference counted, string length stored as part of the string, fully compatible with C char arrays 
  - note the modernised version of Delphi cunningly stores the encoding 

- Char and Byte are not the same type

- integer types and subrange stypes
  
- proper enforced enumerations ( compile time and even runtime for your sneaky sneaky type casters)

- almost perfect dependency management for units (IMHO)
  TODO(PMM) research
- incremental, smart linker
  TODO(PMM) research
- simple type system (well until generics, anyway)
  TODO(PMM) research
 - records (heap, stack)
```pascal
type TRecord = record
// fields
end;
```
 - classes (heap *only*)
```pascal
type TSomeClass = class(TSomeBase, ISomeInterface)
// fields, methods, properties
end;
```
  - classes had langauge support for inheritance
 - well and also objects, so weird class / record hybrid everyone forgot about
```pascal
type TSomeClass = object
// fields, methods NO constructor / destructor
end;
```
 
- PROPERTIES
```pascal
  property SomeProp read FSomeProp write SetSomeProp;
```
- READ ONLY PROPERTIES
```
  property SomeProp read FSomeProp;
```pascal
- PROPERTIES
 - indexed properties
```pascal
  property SomeProp index TODO(PMM) read FSomeProp write SetSomeProp;
```

- pointer to class method types
  - pain-free syntax for class callbacks 
  - also, entirely trivial to stitch together methods to attach behaviour to compose classes
  - this was used (and sometimes abused) for UI development
  
  
```pascal
  type TCallBack = procedure (i : Integer) of object;
  
  recipient.Callback = handler.CallbackProc;
  
  // or even more abstracted....
  
  recipient.Callback = handlerRegistry.Lookup(key);
  
  
```

- compilation units: it's just modules all the way down, baby
 - TODO(PMM) research this
 - circular dependencies are a _compiler error_,
  - not a "way of life" as some people seem to treat it
 - NO LIBS
 - although there were "packages"
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


# code samples

#  writing a registry
- factoring out components
- adding elements into the registry
 - explicitly
 - implicitly
 - finally (ha!) singleton cleanup can be done using _finalization_
TODO(PMM) code samples

```pascal
  
  unit SomeRegistry
  
  interface ..
  ...
  
  type
     // one might make this object interfaced for additional complexity
     TSomeRegistry = class
     end;

   // how to expose?
   // one way that works fine is a getter
   function GetSomeRegistry : TSomeRegistry;
   
   type TSomeRegistryManager
   private:
      FSomeRegistry : TSomeRegistry;
   public    
      class property SomeRegistry : TSomeRegistry read fSomeRegistry; 
   end;
   
   TODO(PMM)      
      
  implementation
  ...
  mSomeRegistry : TSomeRegistry = nil;
  
  initialization
    mSomeRegistry := TSomeRegistry.Create();
    
  finalization
    mSomeRegistry.Free;     
  
   unit SomeProcessor
   
   initialization
   GetSomeRegistry().AddProcessorClass(TSomeProcessor);
   
   unit SomeProcessor2
   
   initialization
   GetSomeRegistry().AddProcessor(TSomeProcessor2.Create);
   

  
```

  - note the initialisation follows the lexical ordering in the program unit (but see later) 
  - note the de-init occurs perfectly
  - also note that adding in a code level dependency re-jogs the initialisation order correctly 




# interfaces are the only multiple inheritance route (i.e. Java, there isn't one)
 - which if we're honest with ourselves if probably what we really wanted all along
TODO(PMM) code samples

# NewInstance, CreateInstance if you really want to go that route
TODO(PMM) code samples



## singly rooted class hierarchy

## RTTI
Almost as rich and dynamic as say, Java, allowing the composition of UI (and other) classes, driven effectively by data  

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



