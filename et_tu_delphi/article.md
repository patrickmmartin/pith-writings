`I come here not to bury Delphi, but to praise it.`
====



`Making Hyperbole Great Again` (Thesis)
--- 

TODOs
---



Object Pascal as implemented by Borland had reference types for objects
This meant unlike old style Pascal struct / objects which had to be `New(obj)` to be in a defined state.

Invoking the constructor explicitly in code RAII OOTB  EVERYWHERE
This is in exchange for giving up scope managed object lifetime
  Except ... "with" 
The natural corrolary is it is now possible for external systems to manage scope
Therefore dependencyinjection blah blah blah all that good stuff

But inception of these ideas larger predates all those concepts as many of the systems they develop plan did not really exist

Now the real killer app for the scope management was the _fully synchronised visual designer_

Let's have a look at some actual code to plug together some hypothetical framework objects
TODO

```pascal
var
  frmServers: TfrmServers;
//  time passes ...
begin

  frmServers := TfrmServers.Create(Application);
  tmrLocate := TTimer.Create;
  tmrLocate.onTimer = tmrLocateTimer;
  frmServers.AddChild(trmLocate);
  
end;  
```

And let's have a look at some hypothetical DSL code to describe the moral equivalent of that code
TODO


```dfm
object frmServers: TfrmServers
// some properties
  object tmrLocate: TTimer
    OnTimer = tmrLocateTimer
```    

Full disclosure: of course it's actual real DSL

* AND THE IDE WOULD GENERATE ALL OF THAT FOR YOU * 


_Invented in 1994_

Full disclosure there are some caveats

* Number 1 A base level object type is used everywhere in constructor arguments and blue code arguments. so raising the very legitimate concerns about whether one should apply the rule of using the most derived possible virtual type in arguments
* Objection number 2 of course let the object hierarchy holy wars begin
  * Callbacks to objects were supported again cplusplus misses this feature and therefore does not have this problem but the considerations of typing arguments equally apply
  * So as the phrase goes and if it doesn't let me invent it here "things were wild in the 90s" which brings me to 
 * objection number 3 the whole thing was stitched together by wiring up Windows DLLs.
  *  for those too little to remember the period of time of the *great DLL hell apocalypse* or in fact current times with our massive problems with malware: in many ways this is now frowned upon. Practically of course the issue that you are forcing unwitting users to become expert in writing these DLLs, what creating a system in which deliberate use and misuse of the underlying framework is permitted.
  * As a sidebar: practically the whole thing worked amazingly well. But. Things could drift into going wrong and then it was very hard to figure out.
 * Objection number 4 _they had to modify the definition of c plus plus and produced their own compiler_
  * Have I mentioned the 90s were wild yet?
* Objection number 5: it's Pascal. Apparently in some disciplines it is acceptable, may profitable to be a zealot around the choice of language. 
* Although, *all the preceding considerations* are language independent concepts






* Can't create abstract classes in c++/
* Agenda - can modules be this good ever again?
* TComponent owner, notify were pretty clever
* Federation of languages comment
* Dangling pointers still possible without a coding style to avoid
* Crashes tend to be more reproducible (why?)



Disclaimer  TODO(PMM)

If you want a much funnier, longer lived, less overtly technical summary - try this on for size

http://www.theregister.co.uk/2016/05/20/verity_sons_of_khan_witch_of_wookey/


``` pascal
type Foo = type Bar;
```

registration examples

Key things:

- built in concept of _interface_ and _implementation_ 

- hey, that super trendy _"no copy constructor"_ concept again! TODO(PMM) reference Rust

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


`Making modules great again.` 
---

- compilation units: it's just modules all the way down, baby
 - TODO(PMM) research this
 - circular dependencies are a _compiler error_,
  - not a "way of life" as some people seem to treat it
 - NO LIBS
 - although there were "packages"
   - TODO(PMM) HOW COULD I HAVE FORGOTTEN ABOUT PACKAGES 
   - although there were some "horror stories" like key variables in some packages initialised *upon load* by other packages, which foxed the IDE's dependency management (which was otherwise pretty cool)
   - THE GREAT DELPHI PACKAGE DRIVE-BY FIASCO
 - proper compiler supported initialisation and finalisation
  - NO STATIC INITIALIZATION FIASCO
   - although see package abuse if you want to learn how to *re-introduce* this
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
  
program registration;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  SomeRegistry in 'SomeRegistry.pas',
  SomeProcessor in 'SomeProcessor.pas',
  AnotherProcessor in 'AnotherProcessor.pas';

begin

end.
  
  unit SomeRegistry
  
  interface ..
  ...
  
  type
     // one might make this object interfaced for additional complexity
     TSomeRegistry = class
        procedure RegisterClass(AClass: TClass);
        procedure DeregisterClass(AClass: TClass);
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
  
  (* implement  RegisterClass,  DeregisterClass *)

  
  
  initialization
    mSomeRegistry := TSomeRegistry.Create();
    
  finalization
    mSomeRegistry.Free;     
  
  
  ...
  
   unit SomeProcessor
   
   type TSomeProcessor = class
   ...
   end;
   
   initialization
   GetSomeRegistry.RegisterClass(TSomeProcessor);
   
   unit AnotherProcessor
   
   type TAnotherProcessor = class
   ...
   end;
   
   implementation
   
   initialization
   GetSomeRegistry.RegisterClass(TAnotherProcessor);
   
```

output:

```
Adding: TSomeProcessor
Adding: TAnotherProcessor
starting
Registration complete
exiting
Removing: TAnotherProcessor
Removing: TSomeProcessor
```

  - note the initialisation follows the lexical ordering in the program unit _in this case_ (but see later) 
  - note the de-init occurs perfectly in the inverse order
  - 
  
  - also note that adding in a code level dependency re-jigs the initialisation order correctly 
  
Add this uses directive into SomeProcessor, adding a source level dependency to AnotherProcessor from the SomeProcessor _implementation_

```pascal

unit SomeProcessor

uses
  AnotherProcessor,
  SomeRegistry;

```

output:

```
Adding: TAnotherProcessor
Adding: TSomeProcessor
starting
Registration complete
exiting
Removing: TSomeProcessor
Removing: TAnotherProcessor
```

Note this happens when updating the unit, not the ```program``` code, which remains blissfully agnostic of the changes. 

so, 
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


`Building a wall against old tools` (Antithesis)
--- 




`I've said things` (Synthesis)
--- 






