The Tao of Scratch
====

Abstract
---

When I look at Scratch, I see something different. 

I'd like persuade you to my viewpoint by going through what I see are the `Good Points`.

I'll be up-front here and state my target audience is the "noble corporate toiler".
It has hopefully been a while since you were introduced / subjected to a computer based teaching tool.
Scratch is a tool for implementing computing based upon many years of thought, and as such is worth some study.   

# Just What is Scratch?

The best place to start is their web site: the description of the project is at https://llk.media.mit.edu/projects/783/ and the main portal is http://scatch.mit.edu

The Wikipedia page is good on this https://en.wikipedia.org/wiki/Scratch_(programming_language)

It's a visual programming language, available as a web application online, that operates on a visual stage with some simple but effective visual components that allows the programmer to construct a range of different visual experiences.  
There is an offline version now which seems to entirely match the basic capabilities of the web version.

# Why Should I be interested in Scratch?

## It's coming a "classroom near you" 

Good Thing @@@@: “This Thing is Happening”" Note also that Scratch at this moment on the way to being taught in the UK in primary and secondary schools - that's years 3-8!. See the announcement here: https://www.gov.uk/government/news/harmful-ict-curriculum-set-to-be-dropped-to-make-way-for-rigorous-computer-science

## The Scratch Heritage 

The Scratch environment is inspired by the work of Seymour Papert http://www.papert.org/ epitomised in the book Mindstorms  http://www.goodreads.com/book/show/703532.Mindstorms 
Listing the whole corpus of the whole back story is a massive endeavour for a brief story, so this is going to be left for the interested reader.

## Some Examples

You can explore for yourself by visiting http://scratch.mit.edu, making use of `Good Point 1: it has a single entry search box`.
Having found a project of interest, you can dive straight in and take a look using `Good Point 2: Scratch has a "See Inside" button`.
If you like what you find you can then just fork it using `Good Point 3: Scratch has a "Remix" button`
- that new project is now available for you to modify, debug and run in any way you see fit.

Now, although it ruins the nice 3-part list rhetorical flourish, let me just mention
`Good Point 4 here: Scratch lets you edit before login *and then* allows you to login and save if you wish`.

Question: how many _paid for_ services that we use for generating wealth can say the same?
For the classroom environment, when dealing with the early years where it is a stretch to require detailed forward planning, this is a Good Thing.

## What can be done in Scratch?
I won't promote individual projects, and indeed I will assert that I don’t need to. Instead let’s rely upon `Good Point 1: it has a single entry search box`

 * Yorkshire to English dictionary
  * https://scratch.mit.edu/search/google_results/?q=Yorkshire+Dictionary 
 * innumerable RPG type things about cat clans
  * https://scratch.mit.edu/search/google_results/?q=cat+clans
  * no, me neither
 * Space Invaders
  * aww, yeah...
  * https://scratch.mit.edu/search/google_results/?q=space+invaders
 * Pacman 
  *  https://scratch.mit.edu/search/google_results/?q=pacman

## Who are the users?

Well, it seems to be primarily educationalists and UK scholastic years 3-8 (at least); that's around 7 – 12, we’ll see why in a bit, though Good Point ~~~~ figures.

# A Whistle Stop tour
 
 Scratch operates on a visual stage of 480x360 virtual pixels, and can be viewed at varying dpi -  `Good Point UUUU: fixed stage extent.` The stage is a sprite (https://en.wikipedia.org/wiki/Sprite_(computer_graphics)) and can be given script code, and any number of sprites can be placed on the stage to compose a visual scene.
 
The sprites own code blocks which can interact with stage changes and manipulate the sprites' properties.
The sprites have a concept of their direction and can be made to orient in any direction, move and "bounce" within the stage area automatically.
 
 ![A minimal Scratch project](scratch-large.png)
 
## Visualisation 
The entire (basic) environment is visual, with editors for all aspects of the sprites and code blocks. The stage is manipulated at both "run time" and "design time" in the same way, which is Good Point VVVV: persistence of stage state. This allows fine positioning by eye, avoiding too many demands on arithmetic for the initial learning stages.

## Running the project

There is a Green button to start running, and a Red button to stop running.
What running means can be deceptively simple - the simplest event is "When Flag Clicked", which can have some code blocks attached underneath - this could perform initialisation or start a processing loop of some kind. You can have as many as you like.

## Debugging

Debugging is an interesting proposition for the target audience.
* Adults - example: @garybernhardt "I'm in a super good mood. Everything is still broken, but now it's funny instead of making me mad."
* Years 3-8 - I've seen their projects - for some, let me assure you "Everything *is* broken", but they're not fazed.

There is a learning curve on the road to learning to debug your project, and there are some useful built-ins 
The coolest of which is: Good point TTTT: code blocks can be modified at runtime in the designer all versions allow blocks to be dragged in, then dragged out while the project is running. Can your tool do that?

`Good point TTTT: code blocks can be modified at runtime in the designer` all versions allow blocks to be dragged in, then dragged out _while the project is running_.
Can _your_ tool do that?

This is a grab of the code blocks being inspected _while the code is running_
![The code blocks being run](in-large.png)

This is a grab of a chunk of code blocks after being pulled out of their container block _while the code is running_
![The code blocks not being run!](out-large.png)

Did I mention you can do this _while the code is running_?

You can do this while just using the mouse - the selection extends naturally from your selected block to the end of the enclosing container block - another `Good point SSSS: selection of blocks has some subtle but powerful affordances`.
Making use of another `Good point UUUU: the positioning of code blocks in the scripts area can be arbitrary.`, so you can park a small chunk of logic still within the visual context of where it was running to reduce the cognitive load.

This works great on an interactive whiteboard to show the effect of a chunk of code blocks.

Even more impressively: the Old Scratch 1.x desktop version, has a similar _single step_, which illuminates the currently running block!
Clearly this was too mind-blowing and is not available in the current version.

## A List of Additional  Good Points

There's a raft of features: `Good Points AAAA-OOOO:`

Logo like Sprite Primitives, Code Blocks, Message Passing, Composable Mathematical Operations, Sprite Cloning, Entry Prompts, Text Messages, Pen operations, Sound. The latter are in my personal order of appreciation.
A feature worth mentioning is _Message Passing_  , which allows broadcasts of messages to all items on a stage.

# Supported Development Styles
So, the possibilities are limitless, but there is a definite set of approaches that practitioners will work through.

## Basic: Sprites, Backdrops, Costumes
 Coupled with setting some properties, the  bounce / direction / touching colour capabilities allow treating the visual stage as a mini engine with some rudimentary support for using the stage as a dressed "set".

## Advanced: Explicit Positioning / Drawing
To move onto to more dynamic generation of content, it is possible to use explicit positioning, and the use of pens to craft abritrary shapes. 

## Immediacy
This goes to the kind of benefits mentioned in the kind of thing Brett Victor talks about 
in http://worrydream.com/#!/InventingOnPrinciple
I still think about the impression that talk had on me. It’s not quite possible to reproduce that talk right now in Scratch, but the immediacy of the run – change – run loop is still very impressive.  

## Accessibility
Why haven't I mentioned _entirely free to use_ yet? If you care about diversity and granting access to self improvement to all then this is huge.
And no ads...

## Sharing
From `Good Point 3: Scratch has a "Remix" button`
*	It exists and works – if you like something and want to have a try at improving it, you just press the button
* Is this because it of `Good Point 15: Scratch has No Merge Action` (for you corporate warriors)

## Deployment 
From `Good Point 16: Scratch has a "public" checkbox`
* pwaahahaha - you just press "Share"!

## Politeness
TODO(PMM) From Good Point 17: Scratch seems to be incredibly good natured
*	Is it because they're all under 12? ( or over 30 ?)
 *	what element of self selection of the group, and self policing is going on here?
 *	is this in fact a very interesting outcome we might want to think about more closely?

# It's a constrained environment
 * `Good Point MMM: there are  "Only a few ways to do certain things"`
# Except when it's not
 * `Good Point NNN: counterpoint to MMM  'Since when did that stop anyone"`

## Don't worry: it's not like "coding"
http://worrydream.com/MeanwhileAtCodeOrg/

# Suitability for classroom and distributed teaching
  `Good Point PPPP: Scratch supports multiple sessions on the same account and concurrent work`
  Given each project has a thumbnail: `Good Point QQQQ: all projects have a thumbnail` it's very easy for a supervisor to see what they're up to: even in the corners.

## Teachable Moments 1:  "Big chunks of blocks" vs. Message Passing
When did you, dear reader, love to learn message passing as a style? In the Scratch environment, it seems to be a few months from a standing start. Very quickly, the users seem to grasp that giant chunks of nested blocks and variables can be replaced by sending the appropriate message.


## Teachable Moments 2: Space Invaders vs Pacman
This seems to be one of challenges that sorts the population. Caveat: in my purely personal sampling: the Space Invaders clones tend to be very high quality, whereas for the "deceptively simple” Pacman there tends to be a raft of issues that challenge the users Good Point RRRR: some tasks lend themselves to "teachable moments". My suspicion is that there are a couple of required complex concepts in a maze game that Scratch does not deliver in its default toolbox, or that are easily synthesised. This might be a fruitful  area to extend the sprite’s capabilities

*	confession time: for the second time (approx 30+ years after the first attempt) I am re-implementing Pacman	and I'm finding that talking about the incomplete (broken?) versions I "Saved As" on the way can be used for Good Point RRRR: some tasks lend themselves to "teachable moments"
 *	for example, I have various baby step projects that will show the stages of
 *	move smoothly betweens points on a grid
 *	choose from N, E, W, S to head to Pacman
 *	now turn left instead of reversing on the next choice -> delivers the ghost patrolling in circles behaviour for free
   

# Scratch has some "Magic" features which are well loved 
 * `Good Point OOO: Vector -> bitmap -> Vector!`
 * Defining concurrent operations “just works” (Multiple “When_Flag_Clicked” can be used)
 

# User Straw Poll
So, I decided to get an assessment from the horse's mouth, as it were

The results are for "best thing about Scratch" are:
 
| Thing  | Votes |
| ------------- | ------------- |
| You can draw costumes            | 1 |
| You can use your imagination     | 2 |
| You can make games               | 4 |
| It's simple                      | 1 |
| It's easy to share               | 1 |
| It's satisfying to finish things | 1 |

So, the games have it, which is fine- what is not immediately clear is the sheer breadth and depth of games created – perfect Space Invaders clone to Fantasy Football style games

# Can it really all be Lovely In The Garden?

*	Uses ActionScript
* Security concerns will always be with us
*  TODO(PMM) Is there a dark scratch?
* Is it addictive enough to prevent people progressing?
 * - to HTML (? barf)
 * to python (arguably more serious)
 
# Lessons for "Grown ups"

Here we come to the punchline: TODO(PMM) - think about some "contrast and compares" here.
I have joked that some companies should figure out how to deploy their business apps using Scratch of something very like it.
Though, come to think of it, what would so wrong?

Acknowledgments
---------

TODO(PMM) - check the wording here carefully
Bloomberg LP (http://www.bloomberg.net) very generously supports my volunteering of time to mentor at an after school club, under the auspices of Code Club (http://codeclub.org.uk) 



