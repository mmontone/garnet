@device(postscript)
@Make(manual)
@disable(figurecontents)
@LibraryFile(Stable)
@LibraryFile(Garnet)
@String(TitleString = "Opal")
@Use(Bibliography = "garnet.bib")
@begin(TitlePage)
@begin(TitleBox)
@blankspace(0.6 inch)
@Bg(Opal Reference Manual
The Garnet Graphical Object System)

@b(Andrew Mickish
Brad A. Myers
David Kosbie
Richard McDaniel
Edward Pervin
Matthew Goldberg)
@BlankSpace(0.3 line)
@value[date]
@end(TitleBox)
@BlankSpace(0.5 inch)
@center[@b(Abstract)]
@begin(Text, spacing=1.1)
This document is a reference manual for the graphical object system used by
the Garnet project, which is called Opal.  ``Opal'' stands for the Object
Programming Aggregate Layer.  Opal makes it very simple to create and
manipulate graphical objects.  In particular, Opal automatically handles
object redrawing when properties of objects are changed.

@blankspace(0.5 inch)
@include(creditetc.mss)
@End(Text)
@end(TitlePage)


@include(pagenumbers.mss)
@set(Page = opal-first-page)

@Chapter(Introduction)

This document is the reference manual for the Opal graphical object system.
Opal, which stands for the @u(O)bject @u(P)rogramming @u(A)ggregate @u(L)ayer,
is being developed as part of the Garnet project @cite(garnet).  The
goal of Opal is to make it easy to create and edit graphical objects.  To
this end, Opal provides default values for all of the properties of
objects, so simple objects can be drawn by setting only a few parameters.
If an object is changed, Opal automatically handles refreshing the screen
and redrawing that object and any other objects that may overlap it.  The
algorithm used to handle the automatic update is documented in
@Cite(OpalUpdate).  Objects in Opal can be connected together using
@i(constraints), which are relations among objects that are declared once
and automatically maintained by the system.  An example of a constraint is
that a line must stay attached to a rectangle.  Constraints are discussed in
the Tutorial and the KR Manual.

Opal is built on top of the Gem module, which is the Graphics and Events
Module that refers to machine-specific functions.  Gem provides an
interface to both X windows and the Macintosh QuickDraw system, so
applications implemented with Opal objects and functions will run on
either platform without modification.

Opal is known to work in virtually any Common Lisp environment on many
different machines (see the Overview section of this manual).
Opal will also work with any window manager on top of X/11, such as
uwm, twm, awm, etc.  Additionally, Opal provides support for color and
gray-scale displays.

Within the Garnet toolkit, Opal forms an intermediary layer.  It uses
facilities provided by the KR object and constraint system
@Cite(KRTR2), and provides graphical objects that comprise the higher
level gadgets.  To use Opal, the programmer should be familiar with
the ideas of objects and constraints presented in the Tour and Tutorial.
Opal does not handle any input from the keyboard or mouse.
That is handled by the separate @i(Interactors) package.  On top of Opal is
also the @i(Aggregadgets) module
which makes it significantly easier to create groups of objects.
A collection of pre-defined interaction techniques, such as menus, scroll
bars, buttons, and sliders, is provided in the Garnet Gadget set which, of
course, use Opal, Interactors, and Aggregadgets.

The highest level of Garnet, built using the toolkit, contains the graphical 
construction tools that allow significant parts of application graphics to be
created without programming.  The most sophisticated tool is Lapidary.
When Lapidary is used, the programmer should rarely need to write code that
calls Opal or any other part of the toolkit.



@Chapter(Overview of Opal)

@Section(Basic Concepts)

The important concepts in Opal are @i(windows), @i(objects), and
@i(aggregates).

X/11 and Macintosh QuickDraw both allow you to create windows on the
screen.  In X they are called "drawables", and in QuickDraw they are
called "views".  An Opal @u(window) is a schema that contains
pointers to these machine-@|specific structures.  Like in X/11 and
QuickDraw, Opal windows can be nested inside other windows (to form
``sub-windows'').  
Windows clip all graphics so they do not extend outside the window's borders.
Also, each window forms a new coordinate system with (0,0) in the upper
left corner.  The coordinate system is one-to-one with the pixels on the
screen (each pixel is one unit of the coordinate system).  Garnet windows are
discussed fully in section @ref(windows).

The basics of object-oriented programming are beyond the scope of this
manual.  The @u(objects) in Opal use the KR object system @cite(KRTR2), and
therefore operate as a prototype-instance model.  This means that each
object can serve as a prototype (like a class) for any further instances;
there is (almost) no distinction between classes and instances.  Each
graphic primitive in Opal is implemented as an object.  When the programmer
wants to cause something to be displayed in Opal, it is necessary to create
instances of these graphical objects.  Each instance remembers its
properties so it can be redrawn automatically if the window needs to
be refreshed or if objects change.

@Index(components)
@Index(parent)
An @u(aggregate) is a special kind of Opal object that holds a collection
of other objects.  Aggregates can hold any kind
of graphic object including other aggregates, but an object can only be in
one aggregate at a time.  Therefore, aggregates form a pure hierarchy.
The objects that are in an aggregate are called @i(components) of that
aggregate, and the aggregate is called the @i(parent) of each of the components.
Each window has associated with it a top-level aggregate.  All objects that
are displayed in the window must be reachable by going through the
components of this aggregate (recursively for any number of levels, in case
any of the components are aggregates themselves).

The prototype inheritance hierarchy for all graphical objects in Opal is
shown in Figure @ref(ObjectSchemata).

@Begin(Figure)
@center[@graphic(Postscript="opal/opal-objects.ps",boundingbox=File, magnify=.8)]
@bar()
@Caption{The objects in Opal and their slots.  Each object also inherits slots
from its prototype (the object to its left).  The default
values for the slots are shown.  Those with values like @pr(#k<F21>) have
formulas in them (refer to the Tutorial and the KR Manual).}
@Tag(ObjectSchemata)
@End(Figure)


@Section(The Opal Package)
@Index(Opal Package)
@Index(use-package)
Once Garnet is loaded, all the graphical objects reside in the @pr[opal]
package.  We recommend that programmers explicitly reference names
from the @pr(opal) package, for example: @pr(opal:rectangle), but you
can also get complete access to all exported symbols by doing a
@w[@pr[(use-package "OPAL")]].  All of the symbols referenced in this
document are exported from @pr(opal), unless otherwise stated.


@Section(Simple Displays)

An important goal of Opal is to make it significantly easier to create
pictures, hiding most of the complexity of the X/11 and QuickDraw
graphics models.
Therefore, there are appropriate defaults for all properties of objects
(such as the color, line-thickness, etc.).  These only need to be set if
the user desires to.  All of the complexity of the X/11 and QuickDraw
graphics packages is available to the Opal user, but it is hidden so
that you do not need to deal with it unless it is necessary to your task.

@Begin(group)
To get the string "Hello world"
displayed on the screen (and refreshed automatically if the window is
covered and uncovered), you only need the following simple program:

@blankspace(.5 line)
@Index(Hello World)
@Begin(ProgramExample)
(use-package "KR")

;;@i{Create a small window at the upper left corner of the screen}
(create-instance 'WIN inter:interactor-window
  (:left 10)(:top 10)
  (:width 200)(:height 50))

;;@I{create an aggregate for the window}
(s-value WIN :aggregate (create-instance 'AGG opal:aggregate))

;;@i{create the string}
(create-instance 'HELLO opal:text
  (:left 10)(:top 20)
  (:string "Hello World"))

(opal:add-component AGG HELLO) ;@i{add the string to the aggregate}

(opal:update WIN) ;@i{cause the window and string to be displayed}
@End(ProgramExample)
@end(group)
@blankspace(.5 line)

Opal also strives to make it easy to change the picture.  To change the
@i(x) position of the rectangle only requires setting the value of the
@pr(:left) slot;  Opal handles the refresh:

@blankspace(.2 line)
@Begin(ProgramExample)
(s-value HELLO :left 50)  ;@i{change the position}

(opal:update WIN) ;@i{cause the change to be visible}
@End(ProgramExample)
@blankspace(.2 line)

Note that the programmer never calls ``draw'' or ``erase'' methods on
objects.  This is a significant difference from other graphical object
systems.  Opal causes the objects to be drawn and erased at the appropriate
times automatically.

Chapter @ref(specificobs) and figure @ref(allobjsfig) present all the kinds
of objects available in Opal.


@Section(Object Visibility)

@Index(visibility)
Objects are visible if and only if their @pr(:visible) slot is non-@c(nil) and
they are a component of a visible aggregate that (recursively) is attached
to a window.  (Aggregates are discussed in Chapter @ref(aggregates).)
Therefore, to make a single object @u(in)visible, its
@pr(:visible) slot can be set to NIL.  To make it visible
again, it is only necessary to set the @pr(:visible) slot to T.
Alternatively, the object can be removed from its aggregate to make it
invisible.

Of course, an object with a non-@c(nil) @pr(:visible) slot in a visible
aggregate hierarchy might be completely obscured behind another object
so it cannot be seen.

Every object has a default formula in its @pr[:visible] slot that depends
on the visibility of the its parent (the ``parent'' is the aggregate that
it is in).  Therefore, to make an entire aggregate and all its components
invisible, it is only
necessary to set the @pr(:visible) slot of the aggregate.  All the
components will become invisible (in this case, it is important that the
components have the default formula in their @pr(:visible) slot).

If you provide a specific value or formula for the @pr(:visible) slot to
override the default formula, it is important that this value be @c(nil)
if the object's parent aggregate is not visible.  Otherwise, routines such as
@pr(point-in-gob) may report that a point is inside the object, even though
the object is invisible.

For example, if you want the @pr(:visible) slot of an object to depend
on its own @pr(:selected) slot, you should additionally constrain it
to depend on the visibility of its parent:

@begin(programexample)
(s-value OBJ :visible (o-formula (if (gvl :parent :visible)
                                     (gvl :selected))))
@end(programexample)


@begin(group)
@Section(View Objects)
@Index(view-object)
At the top of the class hierarchy is the class @pr[opal:view-object].

@index(left) @index(top) @index(width) @index(height) @index(visible)
@begin{programexample}
(create-instance 'opal:View-Object NIL
  (:left 0)
  (:top 0)
  (:width 0)
  (:height 0)
  (:visible (o-formula ...))
  ...)
@end{programexample}
@end(group)
@blankspace(1 line)

Each view object has a bounding box as defined by the left, top corner and
a width and height.  The @pr[:left], @pr[:top], @pr[:width], and @pr[:height]
slots describe the bounding box for the object.  Coordinates are given as
non-negative fixnums, so any formulas must apply @pr[floor] or @pr[round]
to all values that could generate floating point or ratio values.  In
particular, be
careful using "@pr(/)" for division, because that generates ratios or floats
which are not legal values.

With the exception of windows, coordinates of objects are relative to the
window in which the object appears.  (If the window in which an object
appears has borders, then the coordinates of the object are relative to the
@i[inner] edges of the borders.)  Windows coordinates are given in
the coordinate system of the parent of the window, or in the case of top level
windows, given in screen coordinates.

@Section(Read-Only Slots)
@index[parent (slot)]
@index[window (slot)]

There are many slots in graphical objects, windows, and interactors that are
set internally by Garnet and should never be set by users.  For example,
the @pr(:parent), @pr(:window), and @pr(:components) slots of graphical objects
are set automatically whenever the objects are added to an aggregate using
@pr(opal:add-@|component), and should not be set manually.

All public slots that are intended to be
read-only are labeled as such in their object's definitions.  Internal slots
of an object (used for data or calculations) that are not documented should
be considered read-only.  Setting these
slots "temporarily" or during initialization can lead to insidious errors
at run-time.


@section(Different Common Lisps)
Running Opal under different implementations of Common Lisp should be
almost the same.  The differences in the locations of files, such the
Opal binary files, and the cursors, bitmaps and fonts, are all handled in
the top level @pr(garnet-loader) file, which defines variables for the
locations of the files.  

@index(Allegro CommonLisp)
@index(Lucid CommonLisp)
@index(CMU CommonLisp)
@index(lispworks)
@index(Main-Event-Loop)
@index(Update)
An important difference among Lisp interpreters is the @pr(main-event-loop).
In CMU CommonLisp, there is a process running in the background that
allows interactors to always run with automatic refresh of Garnet
windows.@foot[Automatic refresh while an interactor is running is
different from updating a window after you manually make a change with
@pr(s-value).  Unless changes are made by the interactors, you will
still have to call @pr(opal:update) to see the graphics change.]
In Allegro, Lucid, and LispWorks, Garnet creates its own @pr(main-event-loop)
process in the background that does the same thing.  Some Lisp
interpreters have problems running this process in the background,
and you may have to call @pr(inter:main-event-loop) by hand in order
to run the interactors.  Consult the Interactors manual for directions
on how to control the @pr(main-event-loop) process.


@Chapter(Slots of All Graphical objects)
This chapter discusses properties shared by all graphical objects.

@begin(ProgramExample)

@Index(graphical-object)

(create-instance 'opal:Graphical-Object opal:view-object
  (:left 0)   (:top 0)
  (:width 20) (:height 20)
  (:line-style opal:default-line-style)
  (:filling-style NIL)
  (:draw-function :copy)
  (:select-outline-only NIL)
  (:hit-threshold 0)
  ...)
@end(ProgramExample)

@Section(Left, top, width and height)
Graphical objects are objects with graphical properties that can be
displayed in Garnet windows.  They inherit the @pr(:left, :top, :width) and
@pr(:height) slots from @pr(view-objects), of course.

@Section(Line style and filling style)
@index(line-style)
@index(filling-style)
The @pr[:line-style] and @pr[:filling-style] slots hold instances of the
@pr[opal:line-style] prototype and the @pr[opal:filling-style] prototype,
respectively.
These objects parameterize the drawing of graphical objects.  Graphical objects
with a @pr[:line-style] of @c[nil] will not have an outline.  Those with a
@pr[:filling-style] of @c[nil] will have no filling.  Otherwise, the
@pr[:line-style] and @pr[:filling-style] control various parameters of the
outline and filling when the object is drawn.
Appropriate values for the @pr[:line-style] and @pr[:filling-style] slots are
described below in Chapter @Ref(GraphicQualities).

@Section(Drawing function)
@index(draw-function)
The value of the @pr(:draw-function) slot determines how the object
being drawn will affect the graphics already in the window.  For example,
even though a line may be "black", it could cause objects that it covers
to be "whited-out" if it is drawn with a @pr(:clear) draw-function.
A list of all allowed values for the
@pr(:draw-function) slot is included in Figure @ref{draw-fn-fig}.

Every time an object is displayed in a window, its drawn bits interact with
the bits of the pixels already in the window.  The way the object's bits
(the source bits) interact with the window's current bits (the destination
bits) depends on the draw function.
The @pr[:draw-function] is the bitwise function to use in calculating the
resulting bits.  Opal insures that black pixels pretend to be ``1'' and
white pixels pretend to be ``0'' for the purposes of the drawing functions
(independent of the values of how the actual display works).
Therefore, when using the colors black and white, you can rely on @pr(:or)
to always add to the picture and make it more black, and @pr(:and) to take
things away from the picture and make it more white.

Results of draw-functions on colors other than black and white tend to be
random.  This is because X/11 and Mac QuickDraw initialize the colormap with
colors stored in
an arbitrary order, and a color's index is unlikely to be the same between
Garnet sessions.  So performing a logical operation on two particular colors
will yield a different resulting color in different Garnet sessions.

One of the most useful draw functions is @pr(:xor), which occurs frequently
in feedback objects.  If a black rectangle is XOR'ed over another object,
the region under the rectangle will appear in inverse video.  This technique
is used in the @pr(gg:text-button), and many other standard Garnet gadgets.

A fundamental limitation of the PostScript language prevents it from
rendering draw functions properly.  If @pr(opal:make-ps-file) (see Chapter
@ref(printing)) is used to generate a PostScript file from a Garnet window,
the draw functions used in the window will be ignored in the printed image.
Usually the graphics in the window can be reimplemented without using
draw-functions to get the same effect, so that the picture generated by
@pr(opal:make-ps-file) matches the window exactly.


@begin(figure)
@index(clear) @index(copy) @index(no-op) @index(copy-inverted)
@index(invert) @index(or) @index(and) @index(xor)
@index(equiv) @index(nand) @index(nor) @index(and-inverted)
@index(and-reverse) @index(or-inverted) @index(or-reverse)
@StandardTable(Name DrawFunction, Columns 2, HeadingBoxed)
@Begin(DrawFunction)
@TableID(DrawFunction)
@TableHeading(Immediate, RowFormat DrawFunctionColumnHeadings,
Line [@pr(Draw-Function)@\Function])

@pr[:clear]@\0

@pr[:set]@\1

@pr[:copy]@\src

@pr[:no-op]@\dst

@pr[:copy-inverted]@\(NOT src)

@pr[:invert]@\(NOT dst)

@pr[:or]@\src OR dst

@pr[:and]@\src AND dst

@pr[:xor]@\src XOR dst

@pr[:equiv]@\(NOT src) XOR dst

@pr[:nand]@\(NOT src) OR (NOT dst)

@pr[:nor]@\(NOT src) AND (NOT dst)

@pr[:and-inverted]@\(NOT src) and dst

@pr[:and-reverse]@\src AND (NOT dst)

@pr[:or-inverted]@\(NOT src) OR dst

@pr[:or-reverse]@\src OR (NOT dst)

@end(DrawFunction)
@Caption{Allowed values for the @pr(:draw-function) slot and their logical
counterparts.}
@tag(draw-fn-fig)
@bar()
@end(figure)

@begin(group)
@Section(Select-Outline-Only, Hit-Threshold, and Pretend-To-Be-Leaf)
@index(select-outline-only)
@index(hit-threshold)
@index(pretend-to-be-leaf)
The @pr[:select-outline-only], @pr[:hit-threshold], @pr(:pretend-to-be-leaf),
and @pr[:visible] slots are used by functions which search for objects
given a rectangular region or an (x,y) coordinate (see sections
@ref[rect-regions] and @ref[querying-children]).  If the
@pr[:select-outline-only] slot is non-@c{NIL} then @pr[point-in-gob] will
only report hits only on or near the outline of the object.
Otherwise, the object will be sensitive over the entire region (inside
and on the outline).  The @pr[:select-outline-only] slot defaults to @c(nil).
@end(group)
@blankspace(1 line)

The @pr(:hit-threshold) slot controls the sensitivity of the internal Opal
@pr(point-in-gob) methods that decide whether an event (like a mouse click)
occurred "inside" an object.  If the @pr(:hit-threshold) is 3, for example,
then an event 3 pixels away from the object will still be interpreted as being
"inside" the object.  When @pr(:select-outline-only) is T, then any event
directly on the outline of the object, or within 3 pixels of the outline,
will be interpreted as a hit on the object.  The default value of
@pr(:hit-threshold) is 0.  @b(Note:) it is often necessary to set the
@pr(:hit-threshold) slot of all aggregates @i(above) a target object;
if an event occurs "outside" of an aggregate, then the @pr(point-in-gob)
methods will not check the components of that aggregate.  The function
@pr(opal:set-@|aggregate-@|hit-@|threshold) (see section @ref[agg-class])
can simplify this procedure.

When an aggregate's @pr(:pretend-to-be-leaf) slot contains the value
@c(t), then the functions @pr(point-to-component) and
@pr(leaf-objects-in-rectangle) will treat that aggregate as a leaf
object (even though the aggregate has components).  This might be
useful in searching for a button aggregate in an aggrelist of buttons.



@Chapter(Methods on All View-Objects)
@Label(GobMethods)
There are a number of methods defined on all subclasses of
@pr[opal:view-object].  This section describes these methods and other
accessors defined for all graphical objects.

@section(Standard Functions)

@Label(stdfuncs)
The various slots in objects, like @pr(:left, :top, :width, :height,
:visible), etc. can be set and accessed using the standard
@pr(s-value) and @pr(gv) functions and macros.  Some additional
functions are provided for convenience in accessing and setting the size
and position slots.  Some slots of objects should not be set (although they can
be accessed).  This includes the @pr(:left, :top, :width,) and @pr(:height)
of lines and polylines (since they are computed from the end points), and
the components of aggregates (use the @pr(add-component) and
@pr(remove-component) functions).

@blankspace(1 line)
@index(point-in-gob)
@begin(ProgramExample)
opal:Point-In-Gob @i(graphical-object x y)@value(method)
@end(ProgramExample)

This routine determines whether the point @pr[(x,y)] is inside the
graphical object ("gob" stands for graphical object).  This uses an
object-specific method, and is dependent on the setting of the
@pr(:select-outline-only) and @pr(:hit-threshold) slots in the object
as described above.

The @pr[:point-in-gob] methods for @pr(opal:polyline) and @pr(opal:arrowhead)
actually check whether the point is inside the polygon, rather than just
inside the polygon's bounding box.  Additionally, the @pr(:hit-full-interior-p)
slot of a polygon controls which algorithm is used to determine if a point
is inside it (see section @ref[polyline]).
If an object's @pr[:visible] slot is @c(nil), then @pr[point-in-gob]
will always return @c(nil) for that object.

@blankspace(1 line)
@index(destroy)
@begin(ProgramExample)
opal:Destroy @i(graphical-object) &optional @i(erase)@value(method)
@end(ProgramExample)

This causes the object to be removed from an aggregate (if it is in one),
and the storage for the object is deallocated.  
You can @pr(destroy) any kind of object, including windows.  If you destroy
a window, all objects inside of it are automatically destroyed.  Similarly,
if you destroy an aggregate, all objects in it are destroyed (recursively).
When you destroy an object, it is automatically removed from any aggregates
it might be in and erased from the screen.  If destroying the object causes
you to go into the debugger (usually due to illegal values in some slots),
you might try passing in the @pr(erase) parameter as @c(nil) to cause Opal to
not erase the object from the window.  The default for @pr(erase) is T.

Often, it is not necessary to destroy individual objects because they
are destroyed automatically when the window they are in is destroyed.

@blankspace(1 line)
@index(rotate)
@Begin[ProgramExample]
opal:Rotate @i(graphical-object angle) &optional @i(center-x center-y)@value(method)
@End[ProgramExample]

The @pr[rotate] method rotates @i[graphical-object] around
@w[(@i[center-x], @i[center-y])] by @i[angle] radians.  It does this by
changing the values of the controlling points (using @pr(s-value)) for the
object (e.g., the
values for @pr(:x1, :y1, :x2,) and @pr(:y2) for lines).  Therefore, it is a
bad idea to call @pr(rotate) when there are formulas in these slots.
If @i[center-x] or
@i[center-y] are not specified, then the geometric center of the object (as
calculated by using the center of its bounding box) is used.  Certain
objects can't be rotated, namely Ovals, Arcs, Roundtangles, and Text.  A
rectangle that is rotated becomes a polygon and remains one even if it is
rotated back into its original position.


@begin(group)
@section(Extended Accessor Functions)
@Label(Extended-accessors)

The following macros, functions and @pr[setf] methods are defined to make it
easier to access the slots of graphical objects. 

When set, the first set of functions below only change the position of
the graphical object; the width
and height remain the same.  The following are both accessors and valid
place arguments for @pr[setf].  These use @pr(s-value) and @pr(g-value) so
they should not be used inside of formulas, use the @pr(gv-xxx) forms below
instead inside of formulas.

@index(bottom) @index(right) @index(center-x) @index(center-y)
@begin(ProgramExample)
opal:Bottom @i(graphical-object)@value(function)

opal:Right @i(graphical-object)@value(function)

opal:Center-X @i(graphical-object)@value(function)

opal:Center-Y @i(graphical-object)@value(function)
@end(ProgramExample)
@end(group)

@begin(group)
To use one of these in a @pr(setf), the form is
@begin(ProgramExample)
(setf (opal:bottom obj) new-value)
@end(ProgramExample)
@end(group)

@begin(group)
In contrast to the above accessors, the four below when set change the
size of the object.  For example, changing the top-side of an object
changes the top and height of the object; the bottom does not change.

@index(top-side) @index(left-side)@index(bottom-side)@index(right-side)
@begin(ProgramExample)
opal:Top-Side @i(graphical-object value)@value(macro)

opal:Left-Side @i(graphical-object value)@value(macro)

opal:Bottom-Side @i(graphical-object value)@value(macro)

opal:Right-Side @i(graphical-object value)@value(macro)
@end(ProgramExample)
@end(group)

@begin(group)
Opal also provides the following accessor functions which set up
dependencies and should only be used inside of formulas.  For more
information on using formulas, see the example section and the KR document.
These should not be used outside of formulas.

@index(gv-bottom) @index(gv-right) @index(gv-center-x) @index(gv-center-y)
@begin(ProgramExample)
opal:Gv-Bottom @i(graphical-object)@value(function)

opal:Gv-Right @i(graphical-object)@value(function)

opal:Gv-Center-X @i(graphical-object)@value(function)

opal:Gv-Center-Y @i(graphical-object)@value(function)
@end(ProgramExample)
@end(group)

@begin(group)
The following functions should be used in the @pr(:left) and @pr(:top) slots
of objects, respectively.  The first returns the value for @pr[:left] such
that @i[(gv-center-x :self)] equals @i[(gv-center-x object)].

@index(gv-center-x-is-center-of)
@index(gv-center-y-is-center-of)
@Begin(ProgramExample)
opal:Gv-Center-X-Is-Center-Of @i[object]@value(function)

opal:Gv-Center-Y-Is-Center-Of @i[object]@value(function)
@End(ProgramExample)

In more concrete terms, if you had two objects OBJ1 and OBJ2, and
you wanted to constrain the @pr[:left] of OBJ1 so that the centers of
OBJ1 and OBJ2 were the same, you would say:

@Begin(ProgramExample)
(s-value OBJ1 :left (o-formula (opal:gv-center-x-is-center-of OBJ2)))
@End(ProgramExample)
@end(group)

@begin(group)
The next group of functions are for accessing multiple slots
simultaneously.  These are not @pr[setf]'able.

@index(center) @index(set-center) @index(bounding-box) @index(set-bounding-box)
@index(set-position)@index(set-size)
@begin(ProgramExample)
opal:Center @i(graphical-object)@value(function)
  (declare (values center-x center-y))

opal:Set-Center @i(graphical-object center-x center-y)@value(function)

opal:Bounding-Box @i(graphical-object)@value(function)
  (declare (values left top width height))

opal:Set-Bounding-Box @i(graphical-object left top width height)@value(function)

opal:Set-Position @i(graphical-object left top)@value(function)

opal:Set-Size @i(graphical-object width height)@value(function)
@end(ProgramExample)
@end(group)



@Chapter(Graphic Qualities)
@Label(GraphicQualities)

@Index(Graphic-quality)


Objects that are instances of class @pr[opal:graphic-quality] are used to
specify a number of related drawing qualities at one time.  The
@pr[:line-style] and @pr[:filling-style] slots present in all graphical
objects hold instances of @pr[opal:line-style] and @pr[opal:filling-style]
objects.  The @pr[opal:line-style] object controls many parameters about how
a graphical object's outline is displayed.  Likewise, the
@pr[opal:filling-style] object controls how the filling of
objects are displayed.  Figure @ref(GraphicQualsFig) shows the graphic
qualities provided by Opal.

@Begin(Figure)
@bar()
@center[@graphic(Postscript="opal/gquality.ps",boundingbox=File, magnify=.85)]
@Caption{The graphic qualities that can be applied to objects.}
@Tag(GraphicQualsFig)
@bar()
@End(Figure)

The properties controlled by the @pr[opal:line-style],
@pr[opal:filling-style], and @pr[opal:font] objects are similar to
PostScript's graphics state (described in section 4.3 in the
PostScript Language Reference Manual) or the XLIB graphics context
(described in the X Window System 
Protocol Manual).  The Opal design is simpler since there are
appropriate defaults for all values and you only have to set the ones you
are interested in.
The @pr[:line-style] slot in graphical objects holds an object that
contains all relevant information to parameterize the drawing of lines and
outlines.  Similarly, the @pr[:filling-style] controls the insides of
objects.  The @pr[:font] slot appears only in text and related objects, and
controls the font used in drawing the string.

@b(Note): Although the properties of these graphic qualities can be changed
after they are created, for example to make a font change to be italic,
Garnet will not notice the change because the font object itself is
still the same (i.e., the value of the @pr(:font) slot has not changed).
Therefore, line-styles, filling-styles and fonts should be considered
read-only after they are created.  You can make as many as you 
want and put them in objects, but if you want to change the property of an
object, insert a @i(new) line-style, filling-style, or font object rather
than changing the slots of the style or font itself.  If a set of objects
should share a changeable graphics quality, then put a formula into each
object that calculates which graphic quality to use, so they will all change
references together, rather than sharing a pointer to a single graphic
quality object that is changed.



@Section(Color)

@index(color)
A graphical quality called @pr[opal:color] exists which
is defined as:

@blankspace(.5 line)
@index(red) @index(green) @index(blue)
@begin(programexample)
(create-instance 'opal:Color opal:graphic-quality
   (:constant '(:color-p))
   (:color-p ...)  @i[; Set during initialization according to the display - T if color, NIL otherwise]
   (:red 1.0)
   (:green 1.0)
   (:blue 1.0)
   (:color-name NIL))
@end(programexample)

The following colors are exported from Opal.  They are instances of
@pr(opal:color) with the appropriate values for their @pr(:red), @pr(:green),
and @pr(:blue) slots as shown:

@index(white) @index(black) @index(yellow) @index(purple)
@index(cyan) @index(orange)
@index(motif-gray) @index(motif-blue) @index(motif-orange)
@index(motif-green)
@blankspace(.5 line)
@Begin(example, size=10)
opal:Red     @dash  (:red 1.0)  (:green 0.0)  (:blue 0.0)
opal:Green   @dash  (:red 0.0)  (:green 1.0)  (:blue 0.0)
opal:Blue    @dash  (:red 0.0)  (:green 0.0)  (:blue 1.0)
opal:Yellow  @dash  (:red 1.0)  (:green 1.0)  (:blue 0.0)
opal:Purple  @dash  (:red 1.0)  (:green 0.0)  (:blue 1.0)
opal:Cyan    @dash  (:red 0.0)  (:green 1.0)  (:blue 1.0)
opal:Orange  @dash  (:red 0.75) (:green 0.25) (:blue 0.0)
opal:White   @dash  (:red 1.0)  (:green 1.0)  (:blue 1.0)
opal:Black   @dash  (:red 0.0)  (:green 0.0)  (:blue 0.0)
@end(example)

The following objects are also instances of @pr(opal:color), with RGB values
chosen to correspond to standard Motif colors:

@blankspace(.5 line)
@Begin(Text, columns = 2, columnmargin = 0.33 in, linewidth = 3.33 in,
       boxed, columnbalance=on, size=10, spread=-1, indent=4, FaceCode T)
opal:Motif-Gray

opal:Motif-Blue

opal:Motif-Green

opal:Motif-Orange

opal:Motif-Light-Gray

opal:Motif-Light-Blue

opal:Motif-Light-Green

opal:Motif-Light-Orange
@end(Text)
@blankspace(1 line)

Users can create any color they want by creating an
object of type @pr[opal:color], and setting the @pr[:red], @pr[:green] and
@pr[:blue] slots to be any real number between 0.0 and 1.0.

@Index[color-name]
An @pr(opal:color) can also be created using the @pr(:color-name) slot
instead of the @pr(:red, :green,) and @Pr(:blue) slots.  The
@pr(:color-name) slot takes a
string such as @i("pink") or atom such as @i('pink).  These names are
looked up by the Xserver, and the appropriate color will be returned.
Usually the list of allowed color names is stored in the
file @pr(/usr/misc/lib/rgb.txt) or
@pr(/usr/misc/.X11/lib/rgb.txt) or @pr(/usr/lib/X11/rgb.txt).
However, if the Xserver does not find the color, an error will be
raised.  There is apparently no way to ask X whether it understands a
color name.  Thus, code that uses the @pr(:color-name) slot may
not be portable across machines.  Note that the @pr(:red, :green,) and
@Pr(:blue) slots of the color are set automatically in color objects
defined with names.

For example:
@Begin(ProgramExample)
(create-instance 'FUN-COLOR opal:color (:color-name "papaya whip"))
@End(ProgramExample)

@Index(color-p)
The @pr[:color-p] slot of @pr[opal:color] is automatically set to @c(t) or
@c(nil) depending on whether or not your screen is color or
black-and-white (it is also @c(t) if the screen is gray-scale).
This should not be set by hand.  The Motif widget set contains formulas
that change their display mode based on the value of @pr[:color-p].


@begin(group)
@section(Line-Style Class)
@index(line-style)

@blankspace(.5 line)
@begin(ProgramExample)
@Index(line-style)
(create-instance 'opal:Line-Style opal:graphic-quality
  (:maybe-constant '(:line-thickness :cap-style :join-style :line-style :dash-pattern
		     :foreground-color :background-color :stipple))
  (:line-thickness 0)
  (:cap-style :butt)
  (:join-style :miter)
  (:line-style :solid)
  (:foreground-color opal:black)
  (:background-color opal:white)
  (:dash-pattern NIL)
  (:stipple NIL))

(create-instance 'opal:Default-Line-Style opal:line-style
   (:constant T))
@end(ProgramExample)
@end(group)
@blankspace(.5 line)

Before you read the sordid details below about what all these slots
mean, be aware that most applications will just use the default line
styles provided.

@blankspace(.5 line)
@begin(group)
The following line-styles (except @pr(opal:no-line)) are all instances of
@pr(opal:line-style), with particular values for their @pr(:line-thickness),
@pr(:line-style), or @pr(:dash-pattern) slots.  Except as noted, they are
identical to @pr(opal:default-@|line-@|style).  All of them are black.

@Index(no-line) @Index(default-line-style) @Index(thin-line) @Index(line-0)
@Index(line-1)
@Index(line-2) @Index(line-4) @Index(line-8) @Index(dotted-line)
@Index(dashed-line)
@begin(description, indent=-2)
@pr(opal:No-Line) @dash @c(NIL)

@pr(opal:Thin-Line) @dash Same as @pr[opal:default-line-style]

@pr(opal:Line-0) @dash Same as @pr[opal:default-line-style]

@pr(opal:Line-1) @dash @pr[:line-thickness] = 1

@pr(opal:Line-2) @dash @pr[:line-thickness] = 2

@pr(opal:Line-4) @dash @pr[:line-thickness] = 4

@pr(opal:Line-8) @dash @pr[:line-thickness] = 8

@pr(opal:Dotted-Line) @dash @pr[:line-style] = @pr[:dash], and @pr[:dash-pattern] = '(1 1)

@pr(opal:Dashed-Line) @dash @pr[:line-style] = @pr[:dash], and @pr[:dash-pattern] = '(4 4)
@end(description)
@end(group)
@blankspace(.5 line)

@begin(group)
The following line-styles are all identical to @pr[opal:default-line-style],
except that their @pr(:foreground-@|color) slot is set with the appropriate
instance of @pr(opal:color).  For example, the @pr(:foreground-@|color) slot
of @pr(opal:red-line) is set to @pr(opal:red).

@blankspace(.5 line)
@Begin(Text, columns = 2, columnmargin = 0.33 in, linewidth = 3.33 in,
       boxed, columnbalance=on, size=10, spread=-1, indent=4, FaceCode T)
opal:Red-Line

opal:Green-Line

opal:Blue-Line

opal:Yellow-Line

opal:Purple-Line

opal:Cyan-Line

opal:Orange-Line

opal:White-Line
@end(text)
@end(group)
@blankspace(.5 line)

For each of the predefined line-styles above, you may not customize
any of the normal parameters described below.  These line-styles have
been created with their @pr(:constant) slot set to T for
efficiency, which prohibits the overriding of the default values.
You may use these line-styles as values of any @pr(:line-style) slot,
but you may not create customized instances of them.  Instead, to create
a thick red line-style, for example, you should create your own
instance of @pr(opal:line-style) with appropriate values for
@pr(:line-thickness), @pr(:foreground-color), etc.  See the examples at
the end of this section.

@blankspace(1 line)
@begin(group)
@index(line-thickness)
The @pr[:line-thickness] slot holds the integer line thickness in pixels.
There may be a subtle difference between lines with thickness zero and
lines with thickness one.  Zero thickness lines are free to use a device
dependent line drawing algorithm, and therefore may be less aesthetically
pleasing.  They are also probably drawn much more efficiently.  Lines with
thickness one are drawn using the same algorithm with which all the thick lines
are drawn.  For this reason, a thickness zero line parallel to a thick line
may not be as aesthetically pleasing as a line with thickness one.
@end(group)
@blankspace(1 line)

For objects of the types @pr[opal:rectangle], @pr[opal:roundtangle],
@pr[opal:circle] and @pr[opal:oval], increasing the @pr[:line-thickness] of
the @pr[:line-style] will not increase the @pr[:width] or @pr[:height] of
the object; the object will stay the same size, but the solid black
boundary of the object will extend @i[inwards] to occupy more of the
object.  On the other hand, increasing the @pr[:line-thickness] of the
@pr[:line-style] of objects of the types @pr[opal:line],
@pr[opal:polyline] and @pr[opal:arrowhead] will increase the objects'
@pr[:width] and @pr[:height]; for these objects the thickness will extend
@i[outward] on @i(both sides) of the line or arc.

@blankspace(1 line)
@begin(group)
@index(cap-style)

The @pr[:cap-style] slot (which is ignored by the Mac) describes how the
endpoints of line segments
are drawn: 
@index(butt) @index(not-last)@index(round)@index(projecting)
@StandardTable(Name CapStyle, Columns 2, HeadingBoxed)
@Begin(CapStyle)
@TableID(CapStyle)
@TableHeading(Immediate, RowFormat CapStyleColumnHeadings,
Line [@pr{:cap-style}@\Result])

@pr[:butt]@\Square at the endpoint (perpendicular to the slope of the line)
with no projection beyond. 

@pr[:not-last]@\Equivalent to @pr[:butt], except that for
@pr[:line-thickness] 0 or 1 the final endpoint is not drawn. 

@pr[:round]@\A circular arc with the diameter equal to the @pr[:line-thickness]
centered on the endpoint. 

@pr[:projecting]@\Square at the end, but the path continues beyond the
endpoint for a distance equal to half of the @pr[:line-thickness.]

@end(CapStyle)
@end(group)
@blankspace(1 line)

@begin(group)
@index(join-style)
The @pr[:join-style] slot (which is ignored by the Mac) describes how corners
(where multiple lines come
together) are drawn for thick lines as part of poly-line, polygon, or
rectangle kinds of objects.  This does not affect individual lines
(instances of @pr(opal:line)) that are part of an aggregate, even if they
happen to have the same endpoints.

@blankspace(.5 line)
@index(miter)@index(round)@index(bevel)
@StandardTable(Name JoinStyle, Columns 2, HeadingBoxed)
@Begin(JoinStyle)
@TableID(JoinStyle)
@TableHeading(Immediate, RowFormat JoinStyleColumnHeadings,
Line [@pr{:join-style}@\Result])
	
@pr[:miter@ @ @ @ @ @ ]@\The outer edges of the two lines extend to meet at an angle. 

@pr[:round]@\A circular arc with a diameter equal to the @pr[:line-thickness]
is drawn centered on the join point. 

@pr[:bevel]@\@pr[:butt] endpoint styles, with the triangular notch filled.

@end(JoinStyle)
@end(group)

@blankspace(1 line)
@begin(group)
@index(foreground-color)@index(background-color)
The @pr[:foreground-color] slot contains an object of type
@pr[opal:color] which specifies the color in which the line will appear
on a color screen.  The default value is @pr[opal:black].

The @pr[:background-color] slot contains an object of type
@pr[opal:color] which specifies the color of the "off" dashes of
double-dash lines will appear on a color screen (see below).
The default value is @pr[opal:white].  It also specifies the color
of the bounding box of a text object whose @pr[:fill-background-p]
slot is set to T.
@end(group)
@blankspace(1 line)
@begin(group)
@Index[line-style (slot)]@index(double-dash)
The contents of the @pr[:line-style] slot declare whether the line is solid
or dashed. Valid values are @pr[:solid], @pr[:dash] or @pr[:double-dash.] With
@pr[:dash] only the on dashes are drawn, and nothing is drawn in the off
dashes.  With @pr[:double-dash], both on and off dashes are drawn; the
on dashes are drawn with the foreground color (usually black) and the
off dashes are drawn with the background color (usually white).
@blankspace(1 line)
@index(dash-pattern)
The @pr[:dash-pattern] slot holds an (optionally empty) list of numbers
corresponding to the pattern used when drawing dashes.  Each pair of
elements in the list refers to an on and an off dash.  The numbers are
pixel lengths for each dash.  Thus a @pr[:dash-pattern] of @w[(1 1 1 1 3 1)] is
a typical dot-dot-dash line.  A list with an odd number of elements is
equivalent to the list being appended to itself.  Thus, the dash pattern
@w[(3 2 1)] is equivalent to @w[(3 2 1 3 2 1)].
@end(group)
@blankspace(1 line)

Since Mac QuickDraw does not support drawing real dashed lines, Garnet
simulates dashed lines on the Mac by drawing lines with a stippled pattern.
There is only one stipple pattern available for this simulation, so lines
whose @pr(:line-style) is @pr(:dash) or @pr(:double-dash) have the same
gray stipple.  The @pr(:dash-pattern) slot is ignored on the Mac.
You can supply your own stipple for this simulation in the @pr(:stipple)
slot of the @pr(line-style) object (see below).

@index(stipple)
The @pr[:stipple] slot holds either @c(nil) or a @pr[opal:bitmap]
object with which the line is to be stippled.  The
@pr(:foreground-color) of the line-style will be used for the "dark"
pixels in the stipple pattern, and the @pr(:background-color) will be
used for the "light" pixels.

@blankspace(1 line)
@begin(group)
Some examples:
@begin(ProgramExample)
; black line of thickness 2 pixels
opal:line-2

; black line of thickness 30 pixels
(create-instance 'THICKLINE opal:line-style (:line-thickness 30))

; gray line of thickness 5 pixels
(create-instance 'GRAYLINE opal:line-style
  (:line-thickness 5)
  (:stipple (create-instance NIL opal:bitmap
              (:image (opal:halftone-image 50))))) ; 50% gray

; dot-dot-dash line, thickness 1
(create-instance 'DOTDOTDASHLINE opal:line-style
  (:line-style :dash)
  (:dash-pattern '(1 1 1 1 3 1)))
@end(programexample)
@end(group)




@section(Filling-Styles)

@blankspace(.5 line)
@begin(ProgramExample)
@Index(filling-style)
(create-instance 'opal:Filling-Style opal:graphic-quality
  (:foreground-color opal:black)
  (:background-color opal:white)
  (:fill-style :solid)    @i[;; Transparent or opaque.  See section @ref(other-stipple-slots)].
  (:fill-rule :even-odd)  @i[;; For self-intersecting polygons.  See section @ref(other-stipple-slots)].
  (:stipple NIL))         @i[;; The pattern.  See section @ref(custom-stipple)].

(create-instance 'opal:Default-Filling-Style opal:filling-style)
@end(ProgramExample)

Before you read all the sordid details below about what all these slots
mean, be aware that most applications will just use the default filling styles
provided.  There are two basic types of filling-styles: those that rely on
stipple patterns to control their shades of gray, and those that are solid
colors.

@blankspace(1 line)
@begin(group)
@b(Stippled Filling-Styles)

Stippled filling-styles rely on their patterns to control their color shades.
The @pr(:stipple) slot controls the mixing of the @pr(:foreground-color) and
@pr(:background-color) colors, which default to @pr(opal:black) and
@pr(opal:white), respectively.  Thus, the default stippled filling-styles
are shades of gray, but other colors may be used as well.
Here is a list of pre-defined stippled filling-styles:

@begin(description, leftmargin=10, indent=-6)
@Index(default-filling-style) @Index(no-fill) @Index(black-fill)
@Index(white-fill) @Index(gray-fill)
@Index(light-gray-fill) @Index(dark-gray-fill)

@pr(opal:No-Fill) @dash NIL

@pr(opal:Black-Fill) @dash Same as @pr[opal:default-filling-style]

@pr(opal:Gray-Fill) @dash Same as @pr[(opal:halftone 50)]

@pr(opal:Light-Gray-Fill) @dash Same as @pr[(opal:halftone 25)]

@pr(opal:Dark-Gray-Fill) @dash Same as @pr[(opal:halftone 75)]

@pr(opal:Diamond-Fill) @dash A special pattern, defined with
@pr(opal:make-filling-style).  See section @ref(fancy-stipple).
@end(description)
@end(group)

@blankspace(1 line)
@begin(group)
@b(Solid Filling-Styles)

The second set of filling-styles are solid colors, and do not rely on stipples.
For these filling-styles, the @pr(:foreground-color) slot of the object is
set with the corresponding instance of @pr(opal:color).  For example, the
@pr(:foreground-color) slot of @pr(opal:red-fill) is set with @pr(opal:red).
Otherwise, these filling-styles are all identical to
@pr(opal:default-@|filling-@|style).

@blankspace(1 line)
@index(red-fill) @index(green-fill) @index(blue-fill) @index(yellow-fill)
@index(purple-fill) @index(cyan-fill) @index(orange-fill)
@Begin(Text, columns = 2, columnmargin = 0.33 in, linewidth = 3.33 in,
       boxed, columnbalance=on, size=10, spread=-1, indent=4, FaceCode T,)
opal:White-Fill

opal:Red-Fill

opal:Green-Fill

opal:Blue-Fill

opal:Yellow-Fill

opal:Purple-Fill

opal:Cyan-Fill

opal:Orange-Fill
@end(text)

@blankspace(1 line)
@Index[motif-(light-)gray-fill] @Index[motif-(light-)blue-fill]
@Index[motif-(light-)green-fill] @Index[motif-(light-)orange-fill]
@Begin(Text, columns = 2, columnmargin = 0.33 in, linewidth = 3.33 in,
       boxed, columnbalance=on, size=10, spread=-1, indent=4, FaceCode T)
opal:Motif-Gray-Fill

opal:Motif-Blue-Fill

opal:Motif-Green-Fill

opal:Motif-Orange-Fill

opal:Motif-Light-Gray-Fill

opal:Motif-Light-Blue-Fill

opal:Motif-Light-Green-Fill

opal:Motif-Light-Orange-Fill
@end(text)
@end(group)


@Subsection(Creating Your Own Stippled Filling-Styles)
@label(custom-stipple)

@index(stipple)
The @pr[:stipple] slot of a @pr(filling-style) object is used to specify
patterns for mixing the foreground and background colors.  The @pr[:stipple]
slot is either NIL or an @pr[opal:bitmap] object, whose image can be generated
from the @pr(/usr/misc/.X11/bin/bitmap) Unix program (see section
@ref[bitmap-sec]).  Alternatively, there is a Garnet
function supplied for generating halftone bitmaps to get various gray
shades.

@blankspace(.5 line)
@index(halftone)
@Begin[ProgramExample]
opal:Halftone @i(percentage)@value(function)
@End[ProgramExample]

@label(halftone)
The @pr[halftone] function returns an @pr[opal:filling-style] object.  The
@i[percentage] argument is used to specify the shade of the halftone (0 is
white and 100 is black).  Its halftone is as close as possible to the
@i[percentage] halftone value as can be generated.  Since a range of
@i[percentage] values map onto each halftone shade, two additional
functions are provided to get halftones that are guaranteed to be one
shade darker or one shade lighter than a specified value.

@blankspace(.5 line)
@index(halftone-darker)@index(halftone-lighter)
@Begin[ProgramExample]
opal:Halftone-Darker @i[percentage]@value(function)

opal:Halftone-Lighter @i[percentage]@value(function)
@End[ProgramExample]

The @pr[halftone-darker] and @pr[halftone-lighter] functions return a stippled
@pr(opal:filling-style) object that is guaranteed to be exactly one shade
different than the halftone object with the specified
@i[percentage]. With these functions you are guaranteed to get a different
darker (or lighter) @pr[filling-style] object.  Currently, there are 17
different halftone shades.

@blankspace(1 line)
@begin(group)
Examples of creating rectangles that are: black, 25% gray, and 33% gray are:

@begin(programexample)
(create-instance 'BLACKRECT opal:rectangle
   (:left 10)(:top 20)(:width 50)(:height 70)
   (:filling-style opal:black-fill))
(create-instance 'LIGHTGRAYRECT opal:rectangle
   (:left 10)(:top 20)(:width 50)(:height 70)
   (:filling-style opal:light-gray-fill))
(create-instance 'ANOTHERGRAYRECT opal:rectangle
   (:left 10)(:top 20)(:width 50)(:height 70)
   (:filling-style (opal:halftone 33)))
@end(Programexample)
@end(group)


@Subsection(Fancy Stipple Patterns)
@label(fancy-stipple)

@index(make-filling-style)@index(diamond-fill)
Another way to create your own customized filling styles is to use
the function @pr[opal:make-filling-style]:

@blankspace(.5 line)
@begin(programexample)
opal:Make-Filling-Style @i[description] &key @i[from-file-p] @value[function]
                        (@i[foreground-color] opal:black) (@i[background-color] opal:white)
@end(programexample)

The @i[description] can be a list of lists which represent the bit-mask of the
filling style, or may be the name of a file that contains a bitmap.  The
@i[from-file-p] parameter should be T if a filename is being supplied as the
@i[description].

As an example, the filling-style @pr[opal:diamond-fill] is defined by:

@begin(programexample)
(setq opal:diamond-fill
      (opal:make-filling-style
       '((1 1 1 1 1 1 1 1 1)
	 (1 1 1 1 0 1 1 1 1)
	 (1 1 1 0 0 0 1 1 1)
	 (1 1 0 0 0 0 0 1 1)
	 (1 0 0 0 0 0 0 0 1)
	 (1 1 0 0 0 0 0 1 1)
	 (1 1 1 0 0 0 1 1 1)
	 (1 1 1 1 0 1 1 1 1)
	 (1 1 1 1 1 1 1 1 1))))
@end(programexample)


@begin(group)
@Subsection(Other Slots Affecting Stipple Patterns)
@label(other-stipple-slots)

@index(fill-style)
The @pr[:fill-style] slot specifies the colors used for drawing the
"off" pixels in the stippled pattern of filling-styles.  The "on"
pixels are always drawn with the @pr(:foreground-color) of the filling-style.

@index(solid)@index(stippled)@index(opaque-stippled)
@Comment(*** The @ is to leave enough room for the titles)
@StandardTable(Name FillStyle, Columns 2, HeadingBoxed)
@Begin(FillStyle)
@TableID(FillStyle)
@TableHeading(Immediate, RowFormat FillStyleColumnHeadings,
Line [@pr{:fill-style}@\Color used for "off" pixels])
	
@pr[:solid@ @ @ @ @ ]@\Color in @pr(:foreground-color)

@pr[:stippled]@\Transparent

@pr[:opaque-stippled]@\Color in @pr(:background-color)
@end(FillStyle)

@index(even-odd) @index(winding)

@index(fill-rule)
The @pr[:fill-rule] is either @pr[:even-odd] or @pr[:winding].  These are used
to control the filling for self-intersecting polygons.  For a better
description of these see any reasonable graphics textbook, or the X/11
Protocol Manual.
@end(group)



@Section(Fast Redraw Objects)
@label(fast-redraw-objects)
@index(fast-redraw-p)
@index(fast redraw objects)
When an interface contains one or more objects that must be redrawn
frequently, the designer may choose to define these objects as fast
redraw objects.  Such objects could be feedback rectangles
that indicate the current selection, or text strings which are updated
after any character is typed.  Fast redraw objects are redrawn with an
algorithm that is much faster than the standard update procedure for
refreshing Garnet windows.

However, because of certain requirements
that the algorithm makes on fast redraw objects, most objects in an
interface are not candidates for this procedure.  Primarily, fast
redraw objects cannot be covered by other objects, and they must be
either drawn with XOR, or else are guaranteed to be over only a solid
background.  Additionally, aggregates cannot be fast-redraw objects;
only instances of @pr(opal:graphical-object) (those with their own @pr(:draw)
methods) can be fast-redraw objects.

To define an object as a fast redraw object, the @pr(:fast-redraw-p)
slot of the object must be set to one of three allowed values -- 
@pr(:redraw), @pr(:rectangle), or T.  These values determine how the
object should be erased from the window (so that it can be redrawn at
its new position or with its new graphic qualities).  The following
paragraphs describe the functions and requirements of each of these values.

@begin(description)

@pr(:redraw) -- The object will be erased by drawing it a second time
with the line style and filling style defined in the slots
@pr(:fast-redraw-line-style) and @pr(:fast-redraw-filling-style).
These styles should be defined to 
have the same color as the background behind the object.
Additionally, these styles should have the same structure as the line
and filling styles of the object.  For example, if the object has a
line thickness of 8, then the fast redraw line style must have a
thickness of 8 also.  This value may be used for objects on color screens
where there is a uniform color behind the object.

@blankspace(1 line)
@begin(group)
@pr(:rectangle) -- The object will be erased by drawing a rectangle over
it with the filling style defined in the slot @pr(:fast-redraw-filling-style).
This filling style should have the same color as the
background behind the object.  Like @pr(:redraw), this value assumes
that there is a uniform color behind the object.  However, @pr(:rectangle)
is particularly useful for complicated objects like bitmaps and text,
since drawing a rectangle takes less time than drawing these intricate
objects. 
@end(group)
@blankspace(1 line)

@begin(group)
@pr(T) -- In this case, the object must additionally have its
@pr(:draw-function) slot set to @pr(:xor).  This will cause the object
to be XOR'ed on top of its background.  To erase the object, the
object is just drawn again, which will cause the two images to cancel out.
This value is most useful when the background is white and the objects
are black (e.g., on a monochrome screen), and can be used with a
feedback object that shows selection by inverse video.
@end(group)

@end(description)



@chapter(Specific Graphical Objects)
@label(specificobs)

This chapter describes a number of specific subclasses of the
@pr[opal:graphical-object] prototype that implement all of the graphic
primitives that can be displayed, such as rectangles, lines, text strings, etc.

For all graphical objects, coordinates are specified as fixnum quantities
from the top, left corner of the window.  All coordinates and distances are
specified in pixels.

Most of these objects can be filled with a filling style, have a border
with a line-style or both.
The default for closed objects is that @pr(:filling-style) is NIL
(not filled) and the @pr(:line-style) is @pr(opal:default-line-style).

Note that only the slots that are not inherited from view objects and
graphic objects are shown below.  In addition, of course, all of the
objects shown below have the following slots (described in the previous sections):
@index(left)@index(top)@index(width)@index(height)@index(visible)@index(line-style)
@index(filling-style)@index(draw-function)
@index(draw-function)@index(select-outline-only)@index(hit-threshold)
@begin(programexample)
  (:left 0)
  (:top 0)
  (:width 0)
  (:height 0)
  (:visible (o-formula ...))
  (:line-style opal:default-line-style)
  (:filling-style NIL)
  (:draw-function :copy)
  (:select-outline-only NIL)
  (:hit-threshold 0)
@end(programexample)

@index(maybe-constant)@index(constant slots)
Most of the prototypes in this section have a list of slots in their
@pr(:maybe-constant) slot, which generally correspond to the
customizable slots of the object.  This is part of the @i(constant
slots) feature of Garnet which allows advanced users to optimize their
Garnet objects by reusing storage space.  Consult the KR manual for
documentation about how to take advantage of constant slots.

HINT: If you want a
black-filled object, set the line-style to be @c(nil) or else the object will
take twice as long to draw (since it draws both the border and the inside).

Figure @ref(allobjsfig) shows examples of the basic object types in Opal.

@Begin(Figure)
@bar()
@Center[@graphic(Postscript="opal/allobjspic.PS",boundingbox=File,magnify=.75)]
@Caption{Examples of the types of objects supported by Opal: lines,
rectangles, rounded rectangles, text, multipoints, polylines,
arrowheads, ovals, circles, arcs, and bitmaps, with a
variety of line and filling styles.}
@Tag(allobjsfig)
@bar()
@End(Figure)


@section(Line)

@begin(ProgramExample)

@Index(line)

(create-instance 'opal:Line opal:graphical-object
  (:maybe-constant '(:x1 :y1 :x2 :y2 :line-style :visible))
  (:x1 0)
  (:y1 0)
  (:x2 0)
  (:y2 0))
@end(ProgramExample)

The @pr[opal:line] class describes an object that displays a line from
(@pr[:x1@r[,] :y1]) to (@pr[:x2@r[,] :y2]).  The @pr[:left@r[,] :top@r[,]
:width@r[, and] :height] reflect the correct bounding box for the line, but
cannot be used to change the line (i.e., @b(do not set the) @pr[:left],
@pr[:top], @pr[:width], @b(or) @pr[:height] @b[slots]).  Lines ignore
their @pr(:filling-style) slot.

@section(Rectangles)

@begin(ProgramExample)

@Index(rectangle)

(create-instance 'opal:Rectangle opal:graphical-object
  (:maybe-constant '(:left :top :width :height :line-style :filling-style
		     :draw-function :visible)))
@end(ProgramExample)

The @pr[opal:rectangle] class describes an object that displays a rectangle with
top, left corner at (@pr[:left], @pr[:top]), width of @pr[:width], and height
of @pr[:height]. 


@subsection(Rounded-corner Rectangles)

@begin(ProgramExample)

@Index(roundtangle) @index(radius)

(create-instance 'opal:Roundtangle opal:rectangle
  (:maybe-constant '(:left :top :width :height :radius :line-style
		     :filling-style :draw-function :visible))
  (:radius 5))
@end(ProgramExample)

Instances of the @pr[opal:roundtangle] class are rectangles with rounded
corners.  Objects of this class are similar to rectangles,
but contain an additional slot, @pr[:radius], which specifies the
curvature of the corners.  The values for this slot can be either
@pr[:small], @pr[:medium], @pr[:large], or a numeric value interpreted as the
number of pixels to be used.  The keyword values do not correspond directly
to pixels values, but rather compute a pixel value as a fraction of the
length of the shortest side of the bounding box.

@index(small)@index(medium)@index(large)
@StandardTable(Name RoundtangleKeywords, Columns 2, HeadingBoxed)
@Begin(RoundtangleKeywords)
@TableID(RoundtangleKeywords)
@TableHeading(Immediate, RowFormat RoundtangleKeywordsColumnHeadings,
Line [@pr{:radius}@\Fraction])
	
@pr[:small]@\1/5@ @ @ @ @ @ @ @ @ @ @ @ @ @ @ 

@pr[:medium]@\1/4

@pr[:large]@\1/3

@end(RoundtangleKeywords)

Figure @ref(Roundtanglefig) demonstrates the meanings of the slots of
roundtangles.  If the value of @pr[:radius] is 0, the roundtangle looks
just like a rectangle.  If the value of @pr[:radius] is more than half
of the minimum of @pr[:width] or @pr[:height], the roundtangle is drawn as
if the value of @pr[:radius] were half the minimum of @pr[:width] and @pr[:height].

@Begin(Figure)
@bar()
@Comment(l, bot, l->r,b->t)
@Center[@graphic(Postscript="opal/roundtangle.PS",boundingbox=File,magnify=.75)]
@Caption{The parameters of a roundtangle.}
@Tag(Roundtanglefig)
@bar()
@End(Figure)

@section(Polyline and Multipoint)
@label(polyline)
@Begin[ProgramExample]

@Index(polyline)@index(multipoint)

(create-instance 'opal:MultiPoint opal:graphical-object
  (:maybe-constant '(:point-list :line-style :filling-style :draw-function :visible))
  (:point-list NIL))

(create-instance 'opal:PolyLine opal:multipoint
  (:hit-full-interior-p NIL))
@End[ProgramExample]

The @pr[opal:polyline] prototype provides for multi-segmented lines.  Polygons
can be specified by creating a polyline with the same first and last points.
The point list is a flat list of values (@i{x@-[1] y@-[1] x@-[2] y@-[2] ...
x@-[n] y@-[n]}).  If a polyline object has a filling-style, and if the last
point is not the same as the first point, then an invisible
line is drawn between them, and the resulting polygon is filled.

The @pr[:point-in-gob] method for the @pr(opal:polyline)
actually checks whether the point is inside the polygon, rather than just
inside the polygon's bounding box.  If the @pr(:hit-full-interior-p) slot
of a @pr(polyline) is NIL (the default), then the @pr(:point-in-gob) method
will use the "even-odd" rule to determine if a point is inside it.  If the
value of @pr(:hit-full-interior-p) is T, the method will use the "winding"
rule.  The slot @pr(:hit-threshold) has its usual functionality.

The @pr[:left@r[,] :top@r[,]
:width@r[, and] :height] slots reflect the correct bounding box for the
polyline, but cannot be used to change the polyline (i.e., @b(do not
set the) @pr[:left], @pr[:top], @pr[:width], @b(or) @pr[:height] @b(slots)).

@blankspace(1 line)
@begin(Group)
For example:
@begin(programexample)
@Center(@graphic(Postscript="opal/polyline.PS",boundingbox=File,magnify=.75))
(create-instance NIL opal:polyline
   (:point-list '(10 50 50 10 90 10 130 50))
   (:filling-style opal:light-gray-fill)
   (:line-style opal:line-4))
@end(programexample)
@end(Group)

A multipoint is like a polyline, but only appears on the screen as
a collection of disconnected points.  The line-style and filling-style
are ignored.

@begin(group)
@section(Arrowheads)

@Index(arrowhead)
@begin(programexample)
(create-instance 'opal:ArrowHead opal:polyline
  (:maybe-constant '(:line-style :filling-style :length :diameter :open-p
		     :head-x :head-y :from-x :from-y :visible))
  (:head-x 0) (:head-y 0)
  (:from-x 0) (:from-y 0)
  (:connect-x (o-formula ...))  @i(; Read-only slot)
  (:connect-y (o-formula ...))  @i(; Read-only slot)
  (:length 10)
  (:diameter 10)
  (:open-p T)
  ...)
@end(programexaple)
@end(group)

@index(head-x) @index(head-y) @index(from-x) @index(from-y) @index(connect-x)
@index(connect-y) @index(length) @index(diameter) @index(open-p)
The @pr[opal:arrowhead] class provides arrowheads.  Figure @ref(arrowfig)
shows the meaning of the slots for arrowheads. The arrowhead is
oriented with the point at (@pr[:head-x], @pr[:head-y]) and will point away
from (@pr[:from-x], @pr[:from-y]).  (@b(Note:) no line is
drawn from (@pr[:from-x], @pr[:from-y]) to (@pr[:head-x], @pr[:head-y]); the
@pr(:from-) point is just used for reference.)  The @pr[:length] slot
determines the distance (in pixels) from the point of the arrow to the base
of the triangle.  The @pr[:diameter] is the distance across the base.  The
@pr[:open-p] slot determines if a line is drawn across the base.

The arrowhead can have both a filling and an outline (by using the standard
@pr(:filling-style) and @pr(:line-style) slots).  Arrowhead objects
also have 2 slots that describe the point at the center of the base to
which one should attach other lines.  This point is (@pr[:connect-x,
:connect-y]) and is set automatically by Opal; do not set these slots.
These slots are useful if the arrow is closed (see Figure
@ref(arrowfig) below).

If you want an arrowhead connected to a line, you might want to use the
@pr(arrow-line) object (with one arrowhead) or @pr(double-arrow-line) (with
arrow-heads optionally at either or both ends) supplied in the Garnet
Gadget Set @cite(GarnetGadgetsManual).

@Begin(Figure)
@bar()
@Comment(l, bot, l->r,b->t)
@Center[@graphic(Postscript="opal/arrow-illus.PS", boundingbox=File)]
@begin(format)
@TabSet(2in, 2.5in, 3in, 3.5in, 4in)
@pr(:open-p:@\T@\NIL@\T@\NIL@\T)
@pr(:filling-style:@\NIL@\NIL@\opal:light-gray-fill)
@pr(:line-style:@\.... opal:line-0 ....@\NIL)
@Tabclear
@blankspace (1 line)
@end(Format)
@Caption{The slots that define an arrowhead. At the bottom are various
arrowheads with different styles.  Note that a shaft for the arrow must be
drawn by the user.}
@Tag(arrowfig)
@bar()
@End(Figure)


@section(Arcs)

@begin(ProgramExample)

@Index(arc)

(create-instance 'opal:Arc opal:graphical-object
  (:maybe-constant '(:left :top :width :height :line-style :filling-style
		     :draw-function :angle1 :angle2 :visible))
  (:angle1 0)
  (:angle2 0))
@end(ProgramExample)

The @pr[opal:arc] class provides objects that are arcs, which are pieces of
ovals.  The arc
segment is parameterized by the values of the following slots:
@pr[:left], @pr[:top], @pr[:width], @pr[:height], @pr[:angle1], and
@pr[:angle2].  

The arc is a section of an oval centered about the point
<(@pr(center-x @i[arc]), @pr(center-y @i{arc}))> calculated from the arc's
@pr(:left, :top, :width) and @pr(:height), with
width @pr[:width] and height @pr[:height].  The arc runs from @pr[:angle1]
counterclockwise for a distance of @pr[:angle2] radians.  That is,
@pr(:angle1) is measured
from 0 at the center right of the oval, and @pr(:angle2) is measured from
@pr(:angle1) (@pr(:angle2) is relative to @pr(:angle1)).

Arcs are filled as pie pieces to the center of the oval.

@begin(Group)
For example:
@begin(programexample)
@Center(@graphic(Postscript="opal/arcexample.PS",boundingbox=File,magnify=.75))
;; the rectangle is just for reference
(create-instance 'MYRECT opal:rectangle
  (:left 10)(:top 10)(:width 100)(:height 50)) 
(create-instance 'MYARC opal:arc
  (:left 10)(:top 10)
  (:width 100)(:height 50)
  (:angle1 (/ PI 4))
  (:angle2 (/ PI 2))
  (:line-style opal:line-2)
  (:filling-style opal:light-gray-fill))
@end(programexample)
@end(Group)



@section(Ovals)

@begin(ProgramExample)

@Index(oval)

(create-instance 'opal:Oval opal:arc)
@end(ProgramExample)

Instances of the @pr[:oval] class are closed arcs parameterized by the slots
@pr[:left@r[,] :top@r[,] :width@r[, and] :height].



@section(Circles)

@begin(ProgramExample)

@Index(circle)

(create-instance 'opal:Circle opal:arc)
@end(ProgramExample)

The circle is positioned at the top, leftmost part of the bounding box
described with the @pr[:left]. @pr[:top], @pr[:width], and @pr[:height] slots.
The circle drawn has diameter equal to the @u(minimum) of the width
and height, though the effective bounding box (used by
@pr[point-in-gob], for example) will still be defined by the actual
values in @pr(:width) and @pr(:height).  Both @pr(:width) and
@pr(:height) need to be specified.



@section(Fonts and Text)

@subsection(Fonts)
There are two different ways to get fonts from Garnet.  One way is to
explicitly create your own font object, and supply the object with a
description of the desired font, either with family, face, and size
descriptions, or with a font pathname.  The other way is to use the function
@pr(get-standard-font) which will create a new font object for you if
necessary, or return a previously created font object that you can use again.

There are two different types of font objects -- one which handles the
standard Garnet fonts (described by family, face, and size parameters), and
one which handles fonts specified by a filename.  The @pr(get-standard-font)
function only returns font objects that can be described with the three
standard parameters.  Either kind of font object may be used anywhere a
"font" is called for.

@Paragraph(Built in Fonts)

@indexsecondary[Primary="fonts", Secondary="prototype"]
@begin(ProgramExample)
(create-instance 'opal:Font opal:graphic-quality
  (:maybe-constant '(:family :face :size))
  (:family :fixed)
  (:face :roman)
  (:size :medium)
   ...)

@index(default-font)
(create-instance 'opal:Default-Font opal:font
   (:constant T))
@end(ProgramExample)

To use the standard Garnet fonts, create an instance of @pr(opal:font) with
your desired values for the @pr(:family), @pr(:face), and @pr(:size) slots.
Opal will automatically find the corresponding font for your display.  The
allowed values for each slot are as follows:

@index(family)@index(fixed) @index(serif) @index(sans-serif)
@indexsecondary[Primary="fonts", Secondary="family"]

Values for @pr[:family] can be:
@begin(itemize, spread 0)
@pr[:fixed] - a fixed width font, such as Courier.  All characters are the
same width.

@pr[:serif] - a variable-width font, with ``serifs'' on the characters,
such as Times.

@pr[:sans-serif] - a variable-width font, with no serifs on the characters,
such as Helvetica.
@end(Itemize)

@index(face)@index(roman) @index(italic) @index(bold) @index(bold-italic)
@index(plain) @index(condense) @index(extend) @index(outline) @index(shadow)
@index(underline)
@indexsecondary[Primary="fonts", Secondary="face"]
Values for @pr[:face] can be a single keword or a list of the following:
@begin(description, leftmargin=10)
Faces available for both X windows and the Mac:
@begin(itemize, spread 0, columns 2, boxed)
@pr[:roman]

@pr[:italic]

@NewColumn()

@pr[:bold]

@pr[:bold-italic]
@end(itemize)

@begin(group)
Faces available for the Mac only:
@begin(itemize, spread 0, columns 2, boxed)
@pr[:plain]

@pr[:condense]

@pr[:extend]

@NewColumn()

@pr[:outline]

@pr[:shadow]

@pr[:underline]
@end(itemize)
@end(group)
@end(description)

@begin(group)
@index(size) @index(small) @index(medium) @index(large)@index(very-large)
@indexsecondary[Primary="fonts", Secondary="size"]
Values for @pr[:size] can be:
@begin(itemize, spread 0)
@pr[:small] - a small size, such as 10 points.

@pr[:medium] - a normal size, such as 12 points.

@pr[:large] - a large size, such as 18 points.

@pr[:very-large] - a larger size, such as 24 points.

@end(itemize)
@end(group)

The exported @pr(opal:default-font) object contains the font described by
@pr(:fixed), @pr(:roman), and @pr(:medium).  This object should be
used when a font is required and you want to use the default values.
However, since this object's slots have been made constant for efficiency,
do not create instances of the @pr(opal:default-font) object.
Instead, create instances of the @pr(opal:font) objects with customized
values for the parameters, or use @pr(get-standard-font) (explained below).


@Paragraph(Reusing Fonts)
Instead of creating a new font object every time one is needed, you may
use the same font object in multiple applications.  The function
@pr(get-standard-font) remembers what fonts have been created, and will
return a previously created font object if a new font is needed that has
a matching description.  Otherwise, @pr(get-standard-font) will allocate a
new font object and return it, remembering it for later.

@blankspace(1 line)
@index(get-standard-font)
@begin(programexample)
opal:Get-Standard-Font @i[family face size]@value(function)
@end(programexample)
The parameters are all the keywords that are allowed for standard fonts.
For example: @pr[(opal:get-standard-font :fixed :italic :medium)].  In
addition, any of the parameters can be @c(nil), which means to use the
defaults (@pr[:fixed :roman :medium]).  It is more efficient to use
this procedure than to repeatedly allocate new font objects.

Since all the font objects returned by @pr(get-standard-font) have
been declared constant for efficiency, you may not change the font
descriptions after the objects have been created.

Note:  @pr(get-standard-font) only remembers those fonts that were allocated
by using @pr(get-@|standard-@|font).  If a requested font matches an
independently-generated font, @pr(get-@|standard-@|font) will not know about it
and will allocate a new font.



@Paragraph(Fonts from Files)

@indexsecondary[Primary="fonts", Secondary="font-from-file"]
@Index(font-from-file)
@begin(ProgramExample)
(create-instance 'opal:Font-From-File opal:graphic-quality
  (:font-path NIL)
  (:font-name "")
   ...)
@end(ProgramExample)

This allows you to specify a file name to load a font from.

@Index(font-path)
@Index(font-name)
@indexsecondary[Primary="fonts", Secondary="font directories"]
@Index(xset)
X/11 keeps a set of font directories, called the current "Font Path".
You can see what directories are on the font path by typing @pr(xset q)
to the Unix shell, and you can add and remove directories from the
font path by using the @pr(xset fp+) and @pr(xset fp-) commands.

If the @pr(:font-path) slot of a @pr(:font-from-file) is a string
which is a directory, Opal pushes that directory onto
the X font path and then looks up the font.  If the font name is
somewhere on the path already, you can let the @pr(:font-path) slot be @c(nil).
You can usually access fonts in the standard system font area (often
@pr(/usr/misc/.X11/lib/fonts/)) without specifying a path name.

For example, for the font @pr(vgi-25.snf) in the default
directory, use:
@begin(programexample)
(create-instance NIL opal:font-from-file
   (:font-name "vgi-25"))
@end(programexample)
If the font was not in the default font path, then use something like:
@begin(programexample)
(create-instance NIL opal:font-from-file
   (:font-path "/usr/misc/.X11/lib/fonts/75dpi/")
   (:font-name "vgi-25"))
@end(programexample)

@Index(fonts.dir)
The font name @pr("vgi-25") is looked up in a special file in the font
directory called @pr(fonts.dir).  This file contains a long list of fonts
with the file name of the font on the left and the name for the server to
use on the right.  For example, the entry corresponding to
@pr(opal:default-font) may look like this:

@begin(programexample)
courier12.pcf           -adobe-courier-medium-r-normal--17-120-100-100-m-100-iso8859-1
@end(programexample)

On some displays, this font lookup may not proceed smoothly, and you may have
to supply the long @pr("-adobe-...") name as the value of @pr(:font-name)
instead of the more convenient @pr("courier12").  Garnet internally builds
these names for the standard fonts, so font name lookup should never be a
problem for them.


@begin(group)
@paragraph(Opal:Cursor-Font)
@index(cursor-font)

@begin(programexample)
(create-instance 'opal:Cursor-Font opal:font-from-file
  (:constant T)
  (:font-name "cursor"))
@end(programexample)

The @pr(opal:cursor-font) object accesses the
font used by your window manager to display cursors.  This object is an
instance of @pr(opal:font-from-file), and may not be fully portable on
different machines.  Regular text strings may be printed in this font, but
it is specifically intended for use when changing the cursor of Garnet
windows (see section @ref[the-cursor-slot]).
@end(group)



@paragraph(Functions on Fonts)

@index(string-width)
@index(string-height)
@Begin[ProgramExample]
opal:String-Width @i[font-obj string] &key (@i[start] 0) @i[end]@value(function)

opal:String-Height @i[font-obj string] &key (@i[actual-heightp] NIL)@value(function)
@end[ProgramExample]
The function @pr[string-width] takes a font object (which can be a
@pr(font) or a @pr(font-from-file)) and a Lisp string, and
returns the width in pixels of that string written in that font.
The @i(start) and @i(end) parameters allow you to specify the beginning and
ending indices of the portion of @i(string) that you want to measure.

The function @pr[string-height] takes a font (or font-from-file) and a Lisp
string, and returns the height in pixels of that string written in that
font.  There is an optional keyword parameter @i[actual-heightp] which
defaults to @c(nil), and has exactly the same effect on the return value of
@pr[string-height] that the @pr[:actual-heightp] slot of an @pr(opal:text)
object has on the value of the @pr[:height] slot of that @pr(opal:text)
object (see section @ref(actualheightp)).

@subsection(Text)
@label(text)
@begin(ProgramExample)

@Index(text)
@Index(string)
@Index(actual-heightp)

(create-instance 'opal:Text opal:graphical-object
  (:maybe-constant '(:left :top :string :font :actual-heightp :line-style :visible))
  (:string "")
  (:font opal:default-font)
  (:actual-heightp NIL)
  (:justification :left)
  (:fill-background-p NIL)
  (:line-style opal:default-line-style)
  (:cursor-index NIL))
@end(ProgramExample)

Instances of the @pr[opal:text] class appear as a horizontal string of glyphs
in a certain font.  The @pr(:string) slot holds the string to be displayed,
and can contain multiple lines.  The @pr[:font] slot specifies a font object
as described in the previous section (an instance of @pr(opal:font) or
@pr(opal:font-@|from-@|file)).

@index(line-style)@index(fill-background-p)
The @pr(:line-style) slot can control the color of the object, and can hold
any instance of @pr(opal:line-style), such as @pr(opal:red-line).  The
@pr(:foreground-color) slot of the @pr(line-style) object determines the
color of the text.  When the @pr(:fill-background-p) slot is T,
then the background of each glyph of the text is drawn with the color in the
@pr(:background-color) slot of the @pr(line-style).  If the
@pr(:fill-background-p) slot is @c(nil), then the background is unaffected.

@Index(justification)
@Index[left (justification)]@Index[center (justification)]
@index[right (justification)]
The @pr[:justification] slot can
take one of the three values @pr[:left], @pr[:center], or @pr[:right],
and tells whether the multiple-line string is left-, center-, or
right-justified.  The default value is @pr[:left].

A vertical bar cursor before the @pr[:cursor-index]th
character.  If @pr(:cursor-index) is 0, the cursor is at the left of the
string, and if it is >= the length of the string, then it is at the right
of the string.  If @pr(:cursor-index) is @c(nil), then the cursor is turned
off.  The @pr(:cursor-index) slot is set by the @pr(inter:text-interactor)
during text editing.

@blankspace(1 line)
@index(get-cursor-index)
@Begin[ProgramExample]
opal:Get-Cursor-Index @i[string-obj x y]@value(function)
@end[ProgramExample]
This function returns the appropriate cursor-index for the (x,y) location
in the string.  It assumes that the string is displayed on the screen.
This is useful for getting the position in the string when the user presses
over it with the mouse.

@label(actualheightp)
The slot @pr(:actual-heightp) determines whether the height of the string
is the actual height of the characters used, or the maximum height of the
font.  This will make a difference in variable size fonts if you have boxes
around the characters or if you are using a cursor (see section
@ref(text)).  The
default (@c(nil)) means that the height of the font is used so all strings
that are drawn with the same font will have the same height.

The @pr[:width] and @pr[:height] slots reflect the correct width and height
for the string, but cannot be used to change the size (i.e., @b(do not
set the) @pr[:width] @b(or) @pr[:height] @b(slots)).



@subsection(Scrolling Text Objects)
@label(auto-scroll)

@indexsecondary[Primary="auto scroll", Secondary="opal:text"]
When an @pr(opal:text) or @pr(opal:multifont-text) object is used inside a
scrolling-window, there is an option that allows the window to scroll
automatically whenever the cursor is moved out of the top or bottom of
the visible region.  To use this feature, two things need to be done:

@begin[enumerate]
The @pr[:scrolling-window] slot of the text object must contain the scrolling
window object.
@index(scrolling-window slot)
@indexsecondary[Primary="auto scroll", Secondary="scrolling-window slot"]

The text object must also have its @pr[:auto-scroll-p] slot set to T.
@indexsecondary[Primary="auto scroll", Secondary="auto-scroll-p slot"]
@end[enumerate]

@indexsecondary[Primary="auto scroll", Secondary="vs. word wrap"]
@index[word wrap (in multifont-text)]
NOTE: Auto scroll is NOT the same as word wrap.  If the cursor is
moved out of the right edge of the window, auto-scroll will not do
anything.  

@index(demo-multifont)
For an example of how the auto-scroll feature works, look at the code
for Demo-Multifont.  Try the demo with the
@pr[:auto-scroll-p] slot of the object @pr(demo-multifont::text1) set to
both T and NIL.

Auto scroll does not keep track of changes in family, font, size, or
when a segment is cut or pasted.  The @pr(:auto-scroll) method has to be
invoked explicitly in such cases, using the following method:

@programexample(gg:Auto-Scroll @i(text-obj)@value[method])

For examples of calling @pr(gg:auto-scroll) explicitly, look at the menu
functions in Demo-Multifont.


@section(Bitmaps)
@label(bitmap-sec)

@begin(programexample)
@Index(bitmap) @index(image)
(create-instance 'opal:Bitmap opal:graphical-object
  (:maybe-constant '(:left :top :image :filling-style :visible))
  (:image NIL)
  (:filling-style opal:default-filling-style)
  ...)
@end(programexample)
On the Mac, and in the
usual case with X/11, the @pr(:image) slot contains a machine-dependent
structure generated by the function @pr[opal:read-image] (see below).
Under X/11, there are a variety of other CLX image objects that can be
stored in this slot (consult your CLX manual for details on images).  

Bitmaps can be any size.  Opal provides a function to read in a bitmap
image from a file:
@index(read-image)
@begin(programexample)
opal:Read-Image @i{file-name}@value(function)
@end(programexample)
The @pr[read-image] function reads a bitmap image from @i[file-name] which
is stored in the default X/11 ".bm" file format.  Files of this format may
be generated by using the Unix program @pr(/usr/misc/.X11/bin/bitmap).

The @pr(:filling-style) slot can contain any instance of
@pr(opal:filling-style).  If the @pr[:fill-style] of the bitmap's
@pr[:filling-style] is @pr[:solid] or @pr[:opaque-stippled], then the bitmap
will appear with that filling-style's foreground-color and background-color.
If, however, the @pr[:fill-style] of the filling-style is @pr[:stippled],
then the bitmap will appear with the filling-style's @pr[:foreground-color],
but its background will be transparent.  For example, the following
code creates a bitmap which will be drawn with a red and white stipple
(because white is the default @pr(:background-color) of
@pr(opal:filling-style)):  @index(stippled)

@Begin(ProgramExample)
(create-instance 'RED-ARROW opal:arrow-cursor
   (:filling-style (create-instance NIL opal:filling-style
                      (:foreground-color opal:red)
                      (:fill-style :stippled))))
@End(ProgramExample)


There are several functions supplied for generating halftone images, which
can then be supplied to the @pr[:image] slot of a bitmap object.  These
functions are used to create the filling styles returned by the
@pr(halftone) function (section @ref(halftone)).
@blankspace(1 line)
@index(halftone-image)
@Begin[ProgramExample]
opal:Halftone-Image @i(percentage)@value(function)
@End[ProgramExample]
The @pr[halftone-image] function returns a image for use in the
@pr[:image] slot of a bitmap object. The @i[percentage] argument is
used to specify the 
shade of the halftone (0 is white and 100 black). This image is as close
as possible to the @i[percentage] halftone value as can be generated.
Since a range of @i[percentage] values map onto each halftone image, two
additional functions are provided to get images that are guaranteed to be
one shade different or one shade lighter than a specified value.

@blankspace(1 line)

@index(halftone-image-darker)
@index(halftone-image-lighter)
@Begin[ProgramExample]
opal:Halftone-Image-Darker @i[percentage]@value(function)

opal:Halftone-Image-Lighter @i[percentage]@value(function)
@End[ProgramExample]

The @pr[halftone-image-darker] and @pr[halftone-image-lighter]
functions return a halftone that is guaranteed to be exactly one shade
darker than the halftone with the specified @i[percentage]. With these
functions you are guaranteed to get a different darker (or lighter)
image.  Currently, there are 17 different halftone shades.

The @pr[:width@r[, and] :height] slots reflect the correct width and
height for the bitmap, but cannot be used to change the size (i.e.,
@b(do not set the) @pr[:width] @b(or) @pr[:height] slots)).



@section(Pixmaps)

@begin(programexample)
@index(pixmap) @index(image)
(create-instance 'opal:pixmap opal:bitmap
  (:image NIL)
  (:line-style opal:default-line-style)
  (:pixarray (o-formula (if (gvl :image)
			    (gem:image-to-array (gv-local :self :window)
						(gvl :image))))))
  ...)

@end(programexample)

This object is similar to the @pr(opal:bitmap) object, except that it handles
images which use more than one bit per pixel.

@index(image)The @pr(:image) slot works exactly like that of
@pr(opal:bitmap), in conjunction with the function @pr[opal:read-xpm-file]
(see below).

@index(pixarray)
The @pr(:pixarray) slot contains an array of colormap indices.
This is useful if you want to manipulate a pixmap directly,
as in the demo "demo-pixmap".

The @pr[:width@r[, and] :height] slots reflect the correct width and
height for the pixmap, but cannot be used to change the size (i.e.,
@b(do not set the) @pr[:width] @b(or) @pr[:height] @b[slots]).


@subsection(Creating a pixmap)
@index(creating pixmaps)

The following routine can be used to create an image for a pixmap.

@index(read-xpm-file)
@begin(ProgramExample)
opal:Read-XPM-File @i[pathname] @value[function]
@end(ProgramExample)

The argument @i[pathname] should be the name of a file containing a C
pixmap image.  @pr[Read-xpm-file] returns an X-specific or Mac-specific
object, which then should be put in the @pr[:image] slot of an
@pr[opal:pixmap].  The file @i[pathname] containing the C pixmap image
should be in the @i[xpm] format.  Please refer to the X Window System
documentation for more details about that format.

The function @pr[read-xpm-file] will read pixmaps in the XPM1 or XPM2 format.
Files in these formats are produced by the program @pr[ppmtoxpm] and the
OpenLook @pr[IconEditor] utility.  The @pr[ppm] collection of
utilities are useful for converting one format into another.  If you
do not have them, you can @c[ftp] them from one of the standard sites that
store Unix utilities.

In Unix, to convert the contents of a color window into an @i[xpm] format
file, you can use programs such as @pr(xwd), @pr(xwdtopnm), @pr(ppmtoxpm),
etc.  For example, inside a Unix shell, type:

@index(xwd)
@begin(ProgramExample)
xwd > foo.xwd
@end(ProgramExample)

When the cursor changes to a plus, click on the window you want to dump.
Then type:

@index(xwdtopnm) @index(ppmtoxpm) @index[ppm]
@begin(ProgramExample)
xwdtopnm foo.xwd > foo.ppm
ppmtoxpm foo.ppm > foo.xpm
@end(ProgramExample)

This will create a file named "foo.xpm". @comment[in the ??? format]
Finally, in Garnet, type:

@begin(ProgramExample)
(create-instance 'FOO opal:pixmap
   (:image (opal:read-xpm-file "foo.xpm")))
@end(ProgramExample)


Here are two more routines that can be used to create images for pixmaps.

@index(create-pixmap-image)
@begin(programexample)
opal:Create-Pixmap-Image @i(width height) &optional @i(color) @value(Function)
@end(programexample)
This creates a solid color pixmap image.  If you wanted to create a
pixmap whose image was, say, a 20x30 blue rectangle, you
would say:

@begin(programexample)
(create-instance 'BLUE-PIXMAP opal:pixmap
   (:image (opal:create-pixmap-image 20 30 opal:blue)))
@end(programexample)

If no color is given, the color defaults to white.

@index[creating the image of a window]
@index(window-to-pixmap-image)
@begin(programexample)
opal:Window-To-Pixmap-Image @i(window) &key @i(left top width height) @value(Function)
@end(programexample)
This creates an image containing the contents of a Garnet window,
within a rectangular region specified by the values @i(left),
@i(top), @i(width), and @i(height).  Left and top default to 0.
@i(Width) and @i(height) default to the values of the @pr(:width) and
@pr(:height) slots of the window, respectively.


@begin(group)
@subsection(Storing a pixmap)

@blankspace(1 line)
@index[saving pixmaps]
@index(write-xpm-file)
@begin(programexample)
opal:Write-XPM-File @i(pixmap pathname) &key @i[(xpm-format :xpm1)] @value(Function)
@end(programexample)

This function writes the @pr[:image] of a pixmap object into a C pixmap file
whose name is @i[pathname].  @pr(Write-xpm-file) will write pixmap files in
either XPM1 or XPM2 format, depending on the value of the @i(xpm-format) key,
which may be either @pr(:xpm1) or @pr(:xpm2).  By default, the function
generates files in XPM1 format, which can be read by the @pr[xpmtoppm]
utility.
@end(group)



@begin(group)
@Chapter(Multifont)
@Index(multifont-text)

@begin(ProgramExample)
(create-instance 'opal:Multifont-Text opal:aggregate
   (:left 0)
   (:top 0)
   (:initial-text ...)
   (:word-wrap-p NIL)
   (:text-width 300)
   (:current-font ...)
   (:current-fcolor ...)
   (:current-bcolor ...)
   (:fill-background-p T)
   (:draw-function :copy)
   (:show-marks NIL))
@end(ProgramExample)

The @pr(multifont-text) object is loaded by default, since it is used
by the new @pr(garnet-debug:Inspector).  If you are not already
loading the @pr(Inspector), you can load @pr(multifont-text) and all
of its interactors with @w{@pr[(garnet-load "opal:multifont-loader")]}.
@end(group)
@blankspace(1 line)

@Index[word wrap (in multifont-text)]
The @pr[opal:multifont-text] object is designed to allow users to create more
complicated editing applications.  The object is similar to the 
@pr[opal:text] object with many added abilities.  As the name implies,
the @pr[opal:multifont-text] object can accept text input in multiple fonts.
Also, the object has a word wrap mode to permit word-processor-like editing as
well as the ability to highlight text for selection.

Positioning the object is
performed with @pr[:left] and @pr[:top] as with most Garnet objects.  The slots
@pr[:width] and @pr[:height] are read-only and can be used to
see the size of the object, 
but should not be changed by the user.  The @pr[:initial-text] slot is used to
initialize the contents of the @pr[multifont-text].  The format of the
@pr[:initial-text] slot is complicated enough that the next section is devoted
to discussing it.  If the user is not particular about the font of the initial
contents, a simple string is sufficient for the 
@pr[:initial-text] slot.  The slots
@pr[:word-wrap-p] and @pr[:text-width] control the word wrap mode.
If @pr[:word-wrap-p] is T, the text will wrap at the pixel width given in the
@pr[:text-width] slot.  If @pr[:word-wrap-p] is @c(NIL), word wrap mode will
not be activated and no wrapping will occur.  In this case, your
string should contain @pr(#\newline)s wherever required.
Both @pr[:word-wrap-p] and @pr[:text-width] can be modified at run time.

The @pr[:current-font] slot can
be used to control what font newly added characters will appear as.  Also, the
@pr[:current-font] slot can be polled to determine the last font of the
character the cursor most recently passed over.  The slots @pr[:current-fcolor]
and @pr[:current-bcolor] act similarly for the foreground and background colors
of the text.  The slot
@pr[:fill-background-p] controls the background of the characters.  If
@pr[:fill-background-p] is T, the background of the character will be drawn in
the @pr[:current-bcolor].  If @pr[:fill-background-p] is @c(NIL),
the background of the glyphs will not be drawn at all (allowing
whatever is behind the multifont text object to show through).  The slot 
@pr[:show-marks] turns on and off the visibility of text marks.  If
@pr[:show-marks] is T, text-marks will be visible, appearing as little carats
pointing to the character to which they are stuck.  When @pr[:show-marks] is
NIL, the marks will be invisible.

Along with the multi-font text object are a pair of special
interactors that make them editable (see section
@ref(multifontinters)).  The font 
object and the two interactors are combined into the
@pr(multifont-gadget) gadget for convenience (section
@ref(multifontgadgetsec)).

There are two demos that show off multifont capabilities.  @pr(Demo-text)
shows how to use the @pr(multifont-text) object with the
@pr(multifont-text-interactor).  @pr(Demo-multifont) shows how to use
multiple text fields in a single window with the
@pr(focus-multifont-textinter) and @pr(selection-interactor), and
demonstrates the indentation and paren-matching features of lisp mode.

@section(Format of the :initial-text Slot)

The format used in the @pr[:initial-text] slot of @pr[multifont-text]
is also used by many of the procedures and
functions that can be called using the multifont object.

In its simplest form, the @pr[:initial-text] format can be a single
string.  In this form, the default font and colors are used.
The simplest values for @pr[:initial-text] are:

@begin(ProgramExample)
"Here is my example string."

"An example string
with multiple lines."
@end(programexample)

All other formats require a list structure.  The outermost list is the list of
lines:  @pr[(list line1 line2 ... )].  A line can either be a string in which
case the default font and colors are used, or 
a line can be a list of fragments:
@pr[(list frag1 frag2 ... )].  Each line acts as though it ends with a newline
character.  If the @pr[multifont-text] has word wrap activated, each line will
also be broken at places where the length of the text exceeds the
@pr[:text-width], thus the user need not compute how to break up the text to
be placed in the window.  A fragment is the unit that allows the user to enter
font data into the @pr[:initial-text] format.  A fragment can be 
one of the following:
@begin(itemize, spread 0)
a string,  in which case the defaults are used.

a "cons"ing of a string with a Garnet font:  @pr[(cons "string" garnet-font)].

a list of a string, font, foreground color, and background color:
@pr[(list "string" font f-color b-color)].  If @i(font) or
@i(color) is NIL, the default will be used.

a @pr(view-object) (see @ref(objects)).

a mark, in the form @pr[(list :mark sticky-left name info)] (see @ref(marks)).
@end(Itemize)

Note that only the fragment level contains font or color information.
For instance, a single line in bold font may look like this:


@begin(ProgramExample)
 `((("Here is my example string" . ,(opal:get-standard-font :fixed :bold :medium))))
@end(ProgramExample)

@begin(group)
Here is a set of sample values for the @pr(:initial-text) slot.  Each
of these examples are pictured in Figure @ref[multifont-pix].  Details
on using fonts, colors, marks, and graphical objects are given in
section @ref[multifont-fns].

@begin(ProgramExample)
@i[; Define some fonts for brevity, and a circle to use in a string.]
(setf ITALIC (opal:get-standard-font :fixed :italic :medium))
(setf BOLD   (opal:get-standard-font :fixed :bold :medium))
(create-instance 'MY-CIRCLE opal:circle)

@i[; A pair of lines.  Both lines are strings.]
'("An example string" "with multiple lines")

@i[; Same pair of lines in italics.]
`((("An example string" . ,ITALIC))
  (("with multiple lines" . ,ITALIC)))

@i[; A single line with multiple fragments.  Note fragments can be strings]
@i[; when default font is desired.]
`(("Here " ("is" . ,ITALIC) " my " ("example" . ,BOLD) " string."))

@i[; A single line containing a graphical object]
`(("Here is a circle:" ,MY-CIRCLE))

@i[; A single line with colored fragments]
`(("Here is " ("yellow" ,BOLD ,opal:yellow) " and " ("red" ,BOLD ,opal:red) " text"))

@i[; A single line with marks.  Note: make marks visible by setting] :show-marks @i[to] T.
`(("The " (:mark NIL) "(parentheses)" (:mark T) " are marked")))
@end(programexample)
@end(group)

@begin(figure)
@center[@graphic(Postscript="opal/multifont-pix.ps",boundingbox=file,magnify=.75)]
@caption[Examples of the multifont-text object]
@tag[multifont-pix]
@end(figure)



@section(Functions on Multifont Text)
@label[multifont-fns]

The @pr[opal:multifont-text] differs from most objects in that it has a great
number of functions that operate on it.  The functions range
from mundane cursor movement to complicated operations upon selected
text.  Very few operations can be performed by manipulating the slots of a
multifont object.

@subsection(Functions that Manipulate the Cursor)

@index(set-cursor-visible)
@Begin[ProgramExample]
opal:Set-Cursor-Visible @i[text-obj vis]@value(function)
@end[ProgramExample]

This makes the cursor of a @pr[multifont-text] visible or invisible, depending
on whether @i[vis] is @c(t) or @c(NIL).  Having a visible cursor is
not required for entering text, but is recommended for situations
requiring user feedback.  This function does not return any useful value.

@index(set-cursor-to-x-y-position)
@index(set-cursor-to-line-char-position)
@Begin[ProgramExample]
opal:Set-Cursor-To-X-Y-Position @i[text-obj x y]@value(function)

opal:Set-Cursor-To-Line-Char-Position @i[text-obj line# char#]@value(function)
@end[ProgramExample]

These move the cursor to a specific location in the @pr[multifont-text].  The
function @pr[set-cursor-to-x-y-position] sets the cursor to the position
nearest the <x, y> pixel location.  The function
@pr[set-cursor-to-line-char-position] tries to place the cursor at the
position indicated (zero-based).  If the line or character
position is not legal, it will
try to find a reasonable approximation of the location given.  Neither
function returns any useful value.

@index(go-to-next-char)
@index(go-to-prev-char)
@index(go-to-next-word)
@index(go-to-prev-word)
@index(go-to-next-line)
@index(go-to-prev-line)
@Begin[ProgramExample]
opal:Go-To-Next-Char @i[text-obj]@value(function)

opal:Go-To-Prev-Char @i[text-obj]@value(function)

opal:Go-To-Next-Word @i[text-obj]@value(function)

opal:Go-To-Prev-Word @i[text-obj]@value(function)

opal:Go-To-Next-Line @i[text-obj]@value(function)

opal:Go-To-Prev-Line @i[text-obj]@value(function)
@end[ProgramExample]

These functions move the cursor relative to where it is currently located.
The functions @pr[go-to-next-char] and @pr[go-to-prev-char] move the cursor one
character at a time.  The functions @pr[go-to-next-word] and @pr[go-to-prev-word]
move the cursor one word at a time.  In this case, a word is defined by
non-whitespace characters separated by whitespace.  A whitespace character is
either a space or a newline.  These functions will skip over all
non-whitespace until they reach a whitespace character.  They will then skip
over the whitespace until they find the next non-white character.  The
functions @pr[go-to-next-line] and @pr[go-to-prev-line] moves down and up one
line at a time.  The horizontal position of the cursor will be maintained as
close as possible to its position on the original line.  The functions
@pr[go-to-next-char], @pr[go-to-prev-char], @pr[go-to-next-word], and
@pr[go-to-prev-word] all return the characters that were passed over including
newlines as a simple string.  @c(NIL) will be returned if the cursor
does not move as a
consequence of being at the beginning or end of the text.  The
functions @pr[go-to-next-line] and @pr[go-to-prev-line] do not return
useful values. 

@index(go-to-beginning-of-line)
@index(go-to-end-of-line)
@index(go-to-beginning-of-text)
@index(go-to-end-of-text)
@Begin[ProgramExample]
opal:Go-To-Beginning-Of-Line @i[text-obj]@value(function)

opal:Go-To-End-Of-Line @i[text-obj]@value(function)

opal:Go-To-Beginning-Of-Text @i[text-obj]@value(function)

opal:Go-To-End-Of-Text @i[text-obj]@value(function)
@end[ProgramExample]

These functions move the cursor to a position at the beginning or end of
something.  The functions @pr[go-to-beginning-of-line] and
@pr[go-to-end-of-line] move the cursor to the beginning or end of its current
line.  The functions @pr[go-to-beginning-of-text] and @pr[go-to-end-of-text]
move the cursor to the beginning or end of the entire document.  None of these
functions return a useful value.


@subsection(Functions for Text Selection)

@index(toggle-selection)
@Begin[ProgramExample]
opal:Toggle-Selection @i[text-obj mode]@value(function)
@end[ProgramExample]

This will turn off and on the selection mode.  When selection mode is on,
moving the cursor will drag the selection highlight to include characters
that it passes over.  Moving the cursor back over selected text will unselect
and unhighlight the text.  Setting @i[mode] to @c(t) turns on
selection mode, and setting it to @c(NIL) turns off selection mode.
Turning off selection mode will unhighlight all highlighted text.

@index(set-selection-to-x-y-position)
@index(set-selection-to-line-char-position)
@Begin[ProgramExample]
opal:Set-Selection-To-X-Y-Position @i[text-obj  x y]@value(function)

opal:Set-Selection-To-Line-Char-Position @i[text-obj  line# char#]@value(function)
@end[ProgramExample]

These functions are similar to the functions @pr[set-cursor-to-x-y-position]
and @pr[set-cursor-to-line-char-position].  The selection highlight has two
ends.  One end is bound by the cursor; here, the other end is called the
selection end.  To move the cursor end of the highlight, use the cursor
functions.  To move the selection end, use these two functions.  The function
@pr[set-selection-to-x-y-position] sets the selection end based on pixel
position.  The function @pr[set-selection-to-line-char-position] is based on
line and character position.  Neither function returns a useful value.

@index(copy-selected-text)
@index(delete-selection)
@Begin[ProgramExample]
opal:Copy-Selected-Text @i[text-obj]@value(function)

opal:Delete-Selection @i[text-obj] &optional @i[lisp-mode-p] @value(function)
@end[ProgramExample]

These functions are used to manipulate the selected text.  The
@pr[copy-selected-text] function just returns the selected text without affecting
the multifont object.  The function @pr[delete-selection] removes all selected
text from the multifont object and returns it.  Both functions return the text
in the @pr[text] format described above.  The function @pr[delete-selection]
will also automatically turn off selection mode.  Since special bookkeeping
is done to keep track of parentheses and function names in lisp-mode, you must
supply a value of T for @i(lisp-mode-p) when the interactors currently working
on the @i(text-obj) are in lisp-mode.

@index(change-font-of-selection)
@Begin[ProgramExample]
opal:Change-Font-Of-Selection @i[text-obj  font] &key @i[family size italic bold]@value(function)
@end[ProgramExample]

The font of selected text can be updated using this function.  There are two
options.  The new font can be given explicitly using the @i[font] parameter,
or it can be updated by setting @i[font] to @c(NIL) and using the key
parameters.  

Valid values for @i[family] are:
@begin(itemize, spread 0)
@pr[:fixed] - makes font fixed width

@pr[:serif] - makes font variable-width with "serifs" on the characters

@pr[:sans-serif] - makes font variable-width with no serifs on the characters
@end(Itemize)

Values for @i[size] are:
@begin(itemize, spread 0)
@pr[:small] - makes font smallest size

@pr[:medium] - makes font medium size

@pr[:large] - makes font large size

@pr[:very-large] - makes font the largest size

@pr[:bigger] - makes font one size larger than it is

@pr[:smaller] - makes font one size smaller than it is
@end(Itemize)

Values for @i[italic] and @i[bold] are:
@begin(itemize, spread 0)
@pr[T] - makes font italic or bold

@pr[NIL] - undoes italic or bold

@pr[:toggle] - toggles italic or bold throughout the selected region.


@pr[:toggle-first] - looks at the first character of the selection,
and changes the entire region by toggling based on the bold or italic
of that character
@end(Itemize)

The function @pr[change-font-of-selection] is also used to change the value
of the slot @pr[:current-font] even if there is no text selected.


@index(change-color-of-selection)
@Begin[ProgramExample]
opal:Change-Color-Of-Selection @i[text-obj  foreground-color background-color]@value(function)
@end[ProgramExample]

This function will change the color of the selected text.  If only one of
foreground-color and background-color needs to be changed, the other should
be sent as NIL.  This function also changes the values of the slots @pr[:current-fcolor] and @pr[:current-bcolor].


@subsection(Functions that Access the Text or Cursor)

@index(get-string)
@index(get-text)
@Begin[ProgramExample]
opal:Get-String @i[text-obj]@value(function)

opal:Get-Text @i[text-obj]@value(function)
@end[ProgramExample]

These functions return the entire contents of the @pr[multifont-text] object.
The function @pr[get-string] returns the contents as a single string with
@pr[#\newline]s separating lines.  The function @pr[get-text] returns the
contents in the @pr[:initial-text] slot format.

@index(get-cursor-line-char-position)
@index(get-selection-line-char-position)
@Begin[ProgramExample]
opal:Get-Cursor-Line-Char-Position @i[text-obj]@value(function)

opal:Get-Selection-Line-Char-Position @i[text-obj]@value(function)
@end[ProgramExample]

These return the position of the cursor or the selection end of a highlight.
The values are returned using multiple return values: (@i[values line char]).

@index(fetch-next-char)
@index(fetch-prev-char)
@Begin[ProgramExample]
opal:Fetch-Next-Char @i[text-obj]@value(function)

opal:Fetch-Prev-Char @i[text-obj]@value(function)
@end[ProgramExample]

These return the character before or after the cursor.  The function
@pr[fetch-next-char] returns the character after the cursor, and
@pr[fetch-prev-char] returns the character before the cursor.  Neither function
affects the text of the object.  The functions will return @C(NIL) if the cursor
is at the beginning or end of the text where there is no character before or
after the cursor.


@subsection(Adding and Editing Text)

@index(add-char)
@index(insert-string)
@index(insert-text)
@Begin[ProgramExample]
opal:Add-Char @i[text-obj  char] &optional @i[font  foreground-color  background-color lisp-mode-p]@value(function)

opal:Insert-String @i[text-obj  string] &optional @i[font  foreground-color  background-color]@value(function)

opal:Insert-Text @i[text-obj  text]@value(function)
@end[ProgramExample]

These functions are used to add text to a multifont object.  The function
@pr[add-char] adds a single character, the function @pr[insert-string] adds
a whole string possibly including newline, and @pr[insert-text] adds
text that is in @pr[:initial-text] slot format.

The optional @i(font) and @i(color) parameters indicate the font and color
of the new text.  If any of these parameters are NIL, the newly added text
will use the value of the @pr[:current-font], @pr[:current-fcolor], and/or
@pr[:current-bcolor] slots, which can be set manually or allowed to take on
the font and colors of the character over which the cursor last passed.

The optional @i(lisp-mode-p) argument indicates whether the interactors
currently working on the multifont object are in lisp-mode.  Extra operations
are performed on the string to keep track of parentheses and function names
when in lisp-mode, and this parameter is required to keep the bookkeeping
straight.


@index(delete-char)
@index(delete-prev-char)
@index(delete-word)
@index(delete-prev-word)
@Begin[ProgramExample]
opal:Delete-Char @i[text-obj]@value(function)

opal:Delete-Prev-Char @i[text-obj]@value(function)

opal:Delete-Word @i[text-obj]@value(function)

opal:Delete-Prev-Word @i[text-obj]@value(function)
@end[ProgramExample]

These functions are used to delete text from a multifont object.  The functions
@pr[delete-char] and @pr[delete-prev-char] delete a single character after
or before the cursor.  The functions @pr[delete-word] and @pr[delete-prev-word]
delete a single word.  A word is defined the same way as in the functions
@pr[go-to-next-word] and @pr[go-to-prev-word].  The word will be deleted by
deleting whitespace characters up to the first non-whitespace character and
then deleting all non-whitespace up to the next whitespace character.  The
value returned by these functions is the characters deleted.  @C(NIL) is returned
if no characters are deleted.

@index(delete-substring)
@index(kill-rest-of-line)
@Begin[ProgramExample]
opal:Delete-Substring @i[text-obj start-line# start-char# end-line# end-char#]@value(function)

opal:Kill-Rest-Of-Line @i[text-obj]@value(function)
@end[ProgramExample]

These functions are used to delete larger portions of text.  The function
@pr[delete-substring] removes all characters within the given range.  If the
start position is after the end position, nothing will happen.  The function
@pr[kill-rest-of-line] deletes all characters from the cursor to the end
of the current line.  When word wrap is on, the end of a wrapped line is where
the wrap occurs.  Both functions return the deleted text as a string.

@index(set-text)
@Begin[ProgramExample]
opal:Set-Text @i[text-obj  text]@value(function)
@end[ProgramExample]

This function is used to reset everything in the multifont object.  All
previous text is deleted and the new @i[text] is put in its place.  The
@i[text] parameter uses the @pr[:initial-text] slot format.  The new cursor
position will be at the beginning of the text.  This function does not return
a useful value.


@subsection(Operations on :initial-text Format Lists)

@index(text-to-pure-list)
@index(pure-list-to-text)
@Begin[ProgramExample]
opal:Text-To-Pure-List @i[text]@value(function)

opal:Pure-List-To-Text @i[list]@value(function)
@end[ProgramExample]

These functions converts text in the @pr[:initial-text] slot format into a format
that is similar but uses a list representation for fonts, colors,
marks, and view-objects.  Converting the
fonts from Garnet objects to lists makes operations such as reading or writing
text objects to files easier.  To convert from @pr[:initial-text]
format to list use @pr[text-to-pure-list] and to convert back use
@pr[pure-list-to-text]. 

@index(text-to-string)
@Begin[ProgramExample]
opal:Text-To-String @i[text]@value(function)
@end[ProgramExample]

This function converts text in the @pr[:initial-text] format into a regular
character string, losing all font, color, and mark information.

@index(concatenate-text)
@Begin[ProgramExample]
opal:Concatenate-Text @i[text1 text2]@value(function)
@end[ProgramExample]

This function is like the lisp function @pr[concatenate] for arrays.  The
function will return the concatenation of @i[text2] onto the end of
@i[text1].  The function will not affect @i[text1] or @i[text2].


@begin(group)
@subsection(Using View-Objects as Text)
@label(objects)
@index(add-object)
@index(get-objects)
@index(notice-resize-object)
@Begin[ProgramExample]
opal:Add-Object @i[gob object]@value(function)

opal:Get-Objects @i[gob]@value(function)

opal:Notice-Resize-Object @i[object]@value(function)
@end[programexample]
@end(group)

These functions are useful when you want to include a shape or other
view-object in the multifont text.  The function @pr[add-object] will insert
a view-object at the cursor.  The object will act just like a character;  the 
cursor can move over it, and it can be selected, deleted, etc.  The function 
@pr[get-objects] will return a list of all the objects currently in the text.  
When the size of an object which is in the text changes, the function 
@pr[notice-resize-objects] should be used to notify multifont of the change.


@subsection(Using Marks)
@label(marks)
@index(marks)
@index(show-marks)

Another feature of the multifont object is the ability to use text-marks.  The
function @pr[insert-mark] will insert a mark at the cursor.  Marks are
invisible to the cursor as you are typing, and are primarily used as
place-holders in the text.  The lisp-mode feature uses marks to keep
track of parentheses when it is paren-matching.  To make all of the
marks in a multifont object visible (so you can see them), set the
@pr(:show-marks) slot to @pr(T).

@index(insert-mark)
@Begin[ProgramExample]
opal:Insert-Mark @i[gob sticky-left] &key @i[name info]@value(function)
@end(programexample)

The @i[sticky-left] parameter should be T if the mark should stick to
the character
on its left, and NIL if it should stick to the one on its right.  When
a mark "sticks" to a character, the cursor cannot be inserted between
the character and the mark.  This makes the position of the mark
equivalent to the position of the character, so it is easy to
determine whether the cursor is on the left or right side of the mark.

One implication of "stickiness" is that a mark moves through the
string along with the character that it is stuck to (i.e., if you are
typing with the cursor in front of the mark, the mark will be pushed forward
along with the character in front of it).  Another implication is
that when a character is deleted, the mark(s) stuck to it will be
deleted as well.

The @i[name] parameter is a useful way to differentiate between marks,
and @i[info] can be used to let the mark carry any additional
information that might be useful.

@index(search-for-mark)
@index(search-backwards-for-mark)
@index(between-marks-p)
@Begin[ProgramExample]
opal:Search-For-Mark @i[gob] &key @i[name info]@value(function)

opal:Search-Backwards-For-Mark @i[gob] &key @i[name info]@value(function)

opal:Between-Marks-P @i[gob] &key @i[name info]@value(function)
@end(programexample)

The functions @pr[search-for-mark] and 
@pr[search-backwards-for-mark] will return the mark which is nearest to the
cursor.  Leaving out the keywords will search for any mark, or include a
@i[name] or @i[info] to search for a specific type of mark.
The function @pr[between-marks-p] can help to use marks as a type of region.
It will search right and left, and  will return T if the mark found to the 
left is sticky-left and the one on the right is sticky-right.  


@section(Interactors for Multifont Text)
@label(multifontinters)

It may seem strange to find a section about interactors in the Opal chapter,
Since the interactors mentioned here are integral to using the
@pr[opal:multifont-text] object, it was decided to include their description
here, near the description of the @pr[multifont-text].  If you are not
familiar with the basic principles of interactors, you will be best served
if you read the interactors manual first, particularly the parts about the
@pr[inter:text-interactor] and the slots of all interactors.

There are three interactors for multifont-text objects.  The
@pr(multifont-text-interactor) is similar to the standard
@pr(text-interactor), and is used in much the same way.  Two other
interactors, the @pr(focus-multifont-textinter) and
@pr(selection-interactor) are designed to work together in more
complicated situations, like when there are two or more multifont
objects being edited in the same window.

The convenient @pr(multifont-gadget) (section @ref(multifontgadgetsec))
combines the @pr(focus-multifont-textinter) and @pr(selection-interactor) 
with a @pr(multifont-text) object, so you might be able to use it rather
than explicitly creating the interactors below.   However, the gadget
is only useable when you have exactly one @pr(multifont-text) object in
a window.  If you want more than one text object, then you should
create the interactors explicitly because there should still be only
one pair of @i(interactors) in each window, and the interactors should
be set up so the @pr(:start-where) will return one of the multifont
objects.  So, it could be an @pr(:element-of...) type
specification or a @pr(:list-of...) or whatever that will return
multifonts, just so long that it doesn't return other types of objects.

@subsection(Multifont Text Interactor)
@label(multifontkeyboardcmds)

@begin(ProgramExample)
@Index(multifont-text-interactor)

(create-instance 'inter:Multifont-Text-Interactor inter:text-interactor
   (:window NIL)
   (:edit-func #'inter::MultiFont-Text-Edit-String)
   (:lisp-mode-p NIL)             @i[; See section @ref{lisp-mode}]
   (:match-parens-p NIL)          @i[;  "        "          "] 
   (:match-obj ...)               @i[;  "        "          "]
   (:drag-through-selection? T)   @i[; See below]
   (:button-outside-stop?    T)   @i[; See the] text-interactor @i[section of the Interactors Manual]
   (:stop-action #'inter::MultiFont-Text-Int-Stop-Action)
   (:after-cursor-moves-func NIL) @i[; (lambda (inter text-obj))]
   )
@end(ProgramExample)

This interactor was designed to appeal to people familiar with the
@pr[inter:text-interactor].  The interactor is started when you click
the mouse on a text object, and it stops when you type the stop-event,
like #\RETURN.  The editing commands (listed below) are similar to
@pr[inter:text-interactors]'s commands, with many additional ones.

@index(drag-through-selection?)
The new slot @pr(:drag-through-selection?) controls whether dragging
through the string with the mouse will cause the indicated region to
become selected.  You can apply all the standard multifont commands to
a region that is selected this way.  Note: since we use "pending-delete"
like the Macintosh, if you type anything when something is selected,
the selected text is deleted.  

@Index(font changing keys)  @Index(Editing commands for multifont)
The words in upper case are labelings of the keys
(on the Sun keyboard).  If your keyboard has keys labeled differently,
let us know and we will insert them into the code.
@index(key bindings)
@begin(format)
    @pr(^f ^b ^d ^h) = forward, backwards, delete forwards, delete backwards char
    @pr(leftarrow), @pr(rightarrow) =  backwards, forwards
    @pr(META-f, META-b, META-d, META-h)  = same but by words
    @pr(^p) = previous line, @pr(^n) = next line
    @pr(uparrow), @pr(downarrow) = previous line, next line
    @pr(^,) or @pr(HOME) = beginning of document
    @pr(^.) or @pr(END) = end of document
    @pr(^a) = beginning of line
    @pr(^e) = end of line

    @pr(^k) = kill line, @pr(^u) = delete entire string, @pr(^w), @pr(CUT) = delete selection
    @pr(META-w), @pr(COPY) = copy selection to interactor cut buffer
    @pr(^c) = copy entire string to X cut buffer
    @pr(^y), @pr(PASTE) = yank interactor cut buffer or X cut buffer into string
    @pr(^Y), @pr(^PASTE) = yank X buffer
    @pr(META-y), @pr(META-PASTE) = yank interactor cut buffer

@begin(group)
   The following ones extend the selection while moving:
       @pr(^leftarrow), @pr(^rightarrow) = prev, next char selecting
       @pr(META-leftarrow), @pr(META-rightarrow) = prev, next word selecting
       @pr(^uparrow), @pr(^downarrow) = up-line, down-line selecting
       @pr(^HOME), @pr(^END) = beginning, end of string selecting
       @pr(^*) = select all
@end(group)

@begin(group)
   @pr(CONTROL-META) is Lisp stuff if you have lisp mode on (see below):
       @pr(^-META-b), @pr(^-META-leftarrow) = prev lisp expression
       @pr(^-META-f), @pr(^-META-rightarrow) =  next lisp expression
       @pr(^-META-h), @pr(^-META-backspace), @pr(^-META-delete) = delete prev s-expr
       @pr(^-META-d)  = delete next s-expr
@end(group)

   @pr(^-shift-) is for font stuff:
       @pr(^-shift-B) = toggle bold
       @pr(^-shift-I) = toggle italic
       @pr(^-shift-F) = fixed font (courier)
       @pr(^-shift-T) = times font (serif)
       @pr(^-shift-H) = helvetica font (sans-serif)
       @pr(^-shift-<) = smaller font
       @pr(^-shift->) = bigger font
       @pr(^1 ^2 ^3 ^4)  = small, medium, large, and very-large fonts
@end(format)

Of course, you can change the mapping of all these functions, using
the standard @pr(inter:bind-key) mechanism described with the regular
@pr(text-interactor).

@subsection(Focus Multifont Text Interactor)

@begin(ProgramExample)
@Index(focus-multifont-textinter)

(create-instance 'inter:Focus-Multifont-Textinter inter:interactor
   (:window NIL)
   (:obj-to-change NIL)
   (:stop-event NIL)
   (:lisp-mode-p NIL)
   (:match-parens-p NIL)
   (:match-obj ...)
   (:final-function NIL)           @i[; (lambda (inter obj final-event final-string x y))]
   (:after-cursor-moves-func NIL)  @i[; (lambda (inter text-obj))]
   )
@end(ProgramExample)

For applications where one wants the user to be able to type text into a
multifont text object without first having to click on the object, the
@pr[focus-multifont-textinter] was created.  This interactor provides
a feel more like a text editor.  The demo @pr(demo-text) shows how to
use the @pr(focus-multifont-textinter) to create and edit @pr(multifont-text)
objects.  The @pr(demo-multifont) text editor shows how to use this
interactor along with the @pr(selection-interactor) described in the
next section.

Unlike other interactors, this interactor never goes into the "running" state.
The interactor can only "start."  This means that aborting this interactor,
or setting the @pr[:continuous] slot to non-@c(NIL) is meaningless.  The only
way to stop the interactor is either to deactivate it (set the @pr[:active-p]
slot to @c(NIL)) or to destroy it.  If two or more of these interactors are in
the same window, all of the interactors will fetch the keyboard events and
send them to their corresponding multifont text objects.  Extreme caution
is urged when having two or more focus interactors in the same window to avoid
having keystrokes go to multiple objects.  Ways to avoid having keystrokes go
to multiple destinations are to have non-overlapping @pr[:start-where]
positions for all the interactors or to make certain that all idle interactors
have their @pr[:obj-to-change] slot set to @c(NIL).

Usually this interactor will continue running until it is destroyed, but you may
want to execute a final function whenever a particular key is pressed.
Whenever the user issues the event specified in the @pr(:stop-event) slot
(like #\RETURN), the function in @pr(:final-function) is executed.
The parameters to the final-function are the same as for the standard
@pr(text-interactor):
@programexample[(lambda (an-interactor obj-being-edited final-event final-string x y))]

When a @pr[focus-multifont-textinter]
is in a window, all keyboard input will be fed directly into the multifont
text object that is in its @pr[:obj-to-change] slot.  If the
@pr[:obj-to-change] slot is @C(NIL), then no multifont text object has
the focus.

The @pr[inter:focus-multifont-textinter] has the same key bindings as the 
@pr[inter:multifont-@|text-@|interactor].

The @pr[inter:focus-multifont-textinter] also has several functions that can
be used on it.  These functions are used mainly to manipulate the multifont
text that the interactor is focused upon.

@index(set-focus)
@Begin[ProgramExample]
inter:Set-Focus @i[interactor multifont-text]@value(function)
@end[ProgramExample]

This function changes the focus of a @pr[focus-multifont-textinter] from one
text object to another.  The cursor of the newly activately text object will
become visible indicating that it is ready to accept text.  The cursor of the
previous text object will become invisible and any selected text will become
unselected.  If the @i(multifont-text) parameter is NIL, then the currently
selected text object will become unselected and no object will have the
focus.  This function does not return any useful value.

@index(copy-selection)
@index(cut-selection)
@index(paste-selection)
@Begin[ProgramExample]
inter:Copy-Selection @i[interactor]@value(function)

inter:Cut-Selection @i[interactor]@value(function)

inter:Paste-Selection @i[interactor]@value(function)
@end[ProgramExample]

These functions perform cut, copy, and paste operations upon the text object
that currently has the focus.  The @pr[cut-selection] and @pr[copy-selection]
operations copy the selected text into the cut-buffer.  @pr[Cut-selection]
will delete the selected text, but @pr[copy-selection] will leave it
unaffected.  @pr[Paste-selection] inserts the cut buffer at the position of
the cursor.

@subsection(Selection Interactor)

@begin(ProgramExample)
@Index(selection-interactor)

(create-instance 'inter:Selection-Interactor inter:interactor
   (:focus-interactor ...)
   (:match-parens-p NIL)
   (:match-obj ...))
@end(ProgramExample)

The @pr[selection-interactor] is a complementary interactor to the
@pr[focus-@|multifont-@|textinter].  The @pr[selection-interactor] controls
mouse input so that the user may click and drag the mouse in order to select
text and choose a new multifont object to edit.  The @pr[:focus-interactor]
slot must be filled with a valid @pr[inter:focus-multifont-textinter]
interactor.  It is the interactor in that slot that will be used to reset the
focus if a new multifont object is clicked upon.  The @pr[:start-where] slot
must include all possible multifont objects that the @pr[selection-interactor]
operates upon.  If a new multifont object is clicked upon the
@pr[selection-interactor] will reset the focus to the new object and place the
cursor at the point where the mouse was clicked.  If the mouse is clicked in
the multifont object that contains the cursor, the cursor will be moved to
position of the click.  Dragging the mouse across a multifont object will
select the text that was passed over by the mouse.  Clicking the mouse while
holding the shift key (or clicking the mouse with the right button instead
of the left) causes the selection highlight to extend to the newly clicked
position.

The @pr[selection-interactor] uses a key translation table to decode different
types of clicking operations.  The current table translates @pr[:leftdown]
to @pr[:start-selection] and @pr[:shift-leftdown] and @pr[:rightdown] to
@pr[:start-selection-continue].  These combinations can be changed and other
combinations added by using the @pr[inter:bind-key] function.


@subsection(Lisp Mode)
@label(lisp-mode)
@index(lisp mode in multifont)

Multifont supports a special text-entry mode which is useful for typing 
Lisp functions or programs.  This mode can be used by setting the 
@pr[:lisp-mode-p] slot of the @pr[multifont-text-interactor] or 
@pr[focus-multifont-textinter] to T.  When in lisp mode, lines of text will 
tab to the appropriate spot, and semicolon comments will appear in italics. 
It is important that the fonts of the text are not changed during lisp-mode,
since certain fonts hold special meaning for tabs and parenthesis-matching.  

@blankspace(1 line)
@index(indent in lisp-mode)
@Begin[ProgramExample]
inter:Indent @i[string how-many how-far]@value(function)
@end[ProgramExample]

This function can be used to define a special indent amount for your
own function.  The  
argument @i[string] is the name of the function, @i[how-many] is the number  
of arguments (starting with the first) that should be indented the special 
amount, and @i[how-far] is an integer signifying how many spaces from the 
start of the function name these special arguments should be placed.  If 
@i[how-far] is -1, then the indent will line up with the first argument on the 
line above it.  The argument following the last special argument will be 
placed one space in from the start of the function name, and all following 
arguments will line up with the first argument on the line above it.
Here are some examples of the default indentations:

@blankspace(1 line)
@Begin(Text, columns = 2, columnmargin = 0.33 in, linewidth = 3.33 in,
       boxed, columnbalance=on, size=8, spread=0, indent=2, FaceCode T,
       leftmargin=4)
(indent "defun" 2 4)

(indent "create-instance" 2 4)

(indent "let" 1 4)

(indent "do" 2 -1)

(indent "cond" 0)

(indent "define-method" 3 4)
@end(text)
@blankspace(1 line)

There are several keys which are bound specially during lisp mode:

@begin(description, indent=-2)
@Index(key descriptions)
@pr(^-META-f), @pr(^-META-rightarrow) = skip forward lisp expression

@pr(^-META-b), @pr(^-META-leftarrow) = skip backward lisp expression

@pr(^-META-d) = delete lisp expression

@pr(^-META-h), @pr(^-META-backspace) = delete previous lisp expression
@end(description)

@index(match-parens-p) @index(parenthesis matching)
Also helpful in lisp mode is setting the @pr[:match-parens-p] of the 
interactors to T.  When the cursor is next to a close parenthesis, the
corresponding open parenthesis will be highlighted in boldface.  Also, if 
the interactors' @pr[:match-obj] is set to another multifont object, that 
object's text will be set to the text of the line that the matching 
open parenthesis is on.

@index(turn-off-match)
@Begin[ProgramExample]
inter:Turn-Off-Match @i[interactor]@value(function)
@end[ProgramExample]

This function can be used to externally turn off a matched parenthesis, since 
it will only be automatically turned off when the cursor is moved away from the
close parenthesis.


@index(add-lisp-char)
@index(delete-lisp-region)
@Begin[ProgramExample]
inter:Add-Lisp-Char @i[text-obj char] &optional @i[new-font new-foreground-color new-background-color]@value(function)

inter:Delete-Lisp-Region @i[text-obj]@value(function)
@end[ProgramExample]

Because lisp mode does some extra things during addition and deleting of text, 
these special functions should be used when in lisp mode in the place of 
@pr[opal:add-char] and @pr[opal:delete-selection].  If changes are made 
externally without using these functions, future tabs and parenthesis-matching 
may not work properly.  Note: you can also use the @i(lisp-mode-p) parameter
of @pr(opal:add-char) and @pr(opal:delete-selection) to indicate that the
operation is taking place while lisp-mode is active.

@blankspace(1 line)
@index(lispify)
@Begin[ProgramExample]
inter:Lispify @i[string]@value(function)
@end[programexample]

This function takes a plain string and will return text which will work in 
lisp mode.  The returned text is in @pr[:initial-text] format, and can be used
with functions such as @pr[set-text].  The text will already be indented and 
italicized properly.



@section(Auto-Scrolling Multifont Text Objects)
@indexsecondary(Primary="Auto scroll", Secondary="opal:multifont-text")

A companion to the word-wrap feature is the vertical auto scroll feature.
The auto scroll option can be utilized when a multifont-text object is
used inside a scrolling-window along with a focus-multifont-textinter,
multifont-text-interactor, or selection-interactor.

The interface for auto-scrolling @pr(opal:multifont-text) is the same as for
@pr(opal:text), which is described in section @ref(auto-scroll)



@begin(group)
@section(After Cursor Moves)
@index(after-cursor-moves-func)
To support lisp-mode, there is a slot of the three multifont
interactors (@pr[multifont-textinter], @pr[focus-multifont-textinter],
@pr[selection-interactor]) called @pr(:after-cursor-moves-func).  If
non-NIL, it should be a function called as @pr[(lambda (inter text-obj))]
and will be called whenever the cursor moves, or the
text to the left of the cursor changes.

If the function in this slot is overridden with a user-supplied function,
the new function should do a @pr[(call-prototype-method ...)] to ensure
that the default lisp-mode indentation function is executed, also.
@end(group)



@section(A Multifont Text Gadget)
@label(multifontgadgetsec)

Putting a gadget description into the Opal section is fairly
strange.  Just as the interactors section above, it was decided that the
@pr[multifont-gadget] should be described in the
@pr[multifont-text] section.

@begin(ProgramExample)
@Index(multifont-gadget)

(create-instance 'gg:Multifont-Gadget opal:aggregadget
   (:left 0)
   (:top 0)
   (:initial-text (list ""))
   (:fill-background-p NIL)
   (:word-wrap-p NIL)
   (:text-width 300)
   (:stop-event NIL)
   (:selection-function NIL))
@end(ProgramExample)

This gadget is @u(not) automatically loaded by the
@pr(multifont-loader).  Instead, you should load
@pr(multifont-@|gadget-@|loader) from the gadgets directory to load the
gadget and all of the required multifont files.

The @pr[multifont-gadget] is a conglomeration of a @pr[multifont-text],
a @pr[focus-@|multifont-@|textinter], and a @pr[selection-interactor].
These are all put together to take some of the trouble out of assembling the
pieces by hand.  The slots of the gadget are the same as the
@pr[multifont-text].  To use the gadget just create it and go.  The keyboard
and mouse handling are built in.  The trouble with this gadget is that you
cannot have more than one @pr[multifont-gadget] per window.  If you have more
than one, all the gadgets will receive the same keystrokes; thus, all the
gadgets will respond to the keyboard at the same time.

Usually the gadget will continue running until it is destroyed, but you may
want to execute a selection function whenever a particular key is pressed.
Whenever the user issues the event specified in the @pr(:stop-event) slot
(like #\RETURN), the function in @pr(:selection-function) is executed.  The
selection function takes the usual parameters (the gadget and its value), where
the value is the pure text representation of the gadget's current string.

There is a small demo of how to use the multifont text gadget in the
gadget file.  To run it, execute @w[@pr{(garnet-gadgets:multifont-gadget-go)}].



@Chapter(Aggregate objects)

@label(aggregates)
Aggregate objects hold a collection of
other graphical objects (possibly including other aggregates).  The objects
in an aggregate are called its @i(components) and the aggregate is the
@i(parent) of each component.  An aggregate itself has no filling or
border, although it does have a left, top, width and height.

Note: When you create an aggregate and add components to it, creating an
instance of that aggregate afterwards does @i(not) create instances of the
children.  If you use Aggregadgets instead, then you @i(do) get copies
of all the components.  Aggregadgets also provide a convenient syntax for
defining the components.  Therefore, it is often more appropriate to use
Aggregadgets than aggregates.  See the Aggregadgets manual
@cite(AggregadgetsManual).

@section(Class Description)
@label(agg-class)
@begin(ProgramExample)

@Index(aggregate)

(create-instance 'opal:Aggregate opal:view-object
  (:components NIL)
  (:hit-threshold 0)
  (:overlapping T))
@end(ProgramExample)

@Index(components)
The @pr[:components] slot holds a list of the graphical objects that are
components of the aggregate.  @i{This slot should
not be set directly but rather changed using @pr[add-component] and
@pr[remove-component] (section @ref(addremsection)).}
The covering (which is the ordering among children) in the
aggregate is determined by the order of components in the @pr[:components]
slot.  @u(The list of components is stored from bottommost to topmost.)  This
slot cannot be set directly.

@blankspace(.5 line)
@Index(hit-threshold)
@Index(set-aggregate-hit-threshold)
@Begin[ProgramExample]
opal:Set-Aggregate-Hit-Threshold @i[agg]@value(function)
@end[ProgramExample]
@blankspace(.5 line)
As is the case with graphical objects, the @pr[:hit-threshold] slot of an
aggregate controls the sensitivity of the @pr[point-in-gob] methods to hits
that are near to that aggregate.  The value of the @pr[:hit-threshold] slot
defaults to 0, but calling @pr[set-aggregate-hit-threshold]
sets the @pr[:hit-threshold] of an aggregate to be the maximum of all
its components.

@Index(overlapping)
The @pr[:overlapping] slot is used as a hint to the aggregate as to whether
its components overlap.  This property allows the aggregate to redraw it's
components more efficiently.  You can set the @pr(:overlapping) slot to @c(nil)
when you know that the first level children of this aggregate will never
overlap each other on the screen.  @i(Currently, this slot is not used, but
it may be in the future.)

Aggregates have a bounding box, which, by default, is calculated from the
sizes and positions of all its children.  If you want to have the position
or size of the children depend on that of the parent, it is important to
provide an explicit value for the position or size of the aggregate, and
then provide formulas in the components that depend on the aggregate's
values.  Be careful to avoid circularities: either the aggregate should
depend on the sizes and positions of the children (which is the default)
@b(or) the children should depend on the parent.  These cannot be easily
mixed in a single aggregate.  It is important that the size and position of
the aggregate correctly reflect the bounding box of all its components, or
else the redisplay and selection routines will not work correctly.


@begin(group)
@section(Insertion and Removal of Graphical Objects)
@label(addremsection)

@blankspace(1 line)
@Index(add-component)
@begin(ProgramExample)
opal:Add-Component @i[aggregate graphical-object] [[:where] @i[position][@i{locator}]]@value(method)
@end(ProgramExample)

The method @pr[add-component] adds @i[graphical-object] to @i[aggregate].  The
@i[position] and @i[locator] arguments can be used to adjust the
placement/covering of @i[graphical-object] with respect to the rest of the
components of @i[aggregate].
@end(group)
@blankspace(1 line)

@Index(position) @Index(front) @Index(back) @Index(behind) @Index(in-front)
@Index(at)
There are five legal values for @i[position]; these are: @pr[:front],
@pr[:back], @pr[:behind], @pr[:in-front], and @pr[:at].  Putting an object
at the @pr(:front) means that it is not covered by any other objects in
this aggregate, and
at the @pr(:back), it is covered by all other objects in this aggregate.
Positioning @i[graphical-object] at either @pr[:front] or @pr[:back]
requires no value
for @i[locator], as these are unique locations.  If position is either
@pr[:behind] or @pr[:in-front] then the value of @i[locator] should be a
graphical object already in the component list of the aggregate, in which
case @i[graphical-object] is placed with respect to @i[locator].  In the
final case, with @i[position] being @pr[:at], @i[graphical-object] is placed
at the @i[locator]th position in the component list, where 0 means at
the head of the list (the back of the screen).

If none are supplied, then the new object is
in front of all previous objects.  The @pr(:where) keyword is optional
before the locators, so all of the following are legal calls:
@begin(ProgramExample)
(opal:add-component agg newobj :where :back)
(opal:add-component agg newobj :back)
(opal:add-component agg newobj)       @i(; adds newobj at the :front)
(opal:add-component agg newobj :behind otherobj)
(opal:add-component agg newobj :at 4)
@end(ProgramExample)

Objects cannot belong to more than one aggregate.  Attempting to add a
component of one aggregate to a second aggregate will cause Opal to signal
an error.  If the @i[locator] for @pr[:behind] or @pr[:in-front] is not a
component of the aggregate Opal will also signal an error.

@blankspace(1 line)
@Index(add-components)
@begin(ProgramExample)
opal:Add-Components @i{aggregate} &rest @i{graphical-objects}@value(function)
@end(ProgramExample)

This function adds multiple components to an aggregate.  Calling
this function is equivalent to:

@begin(ProgramExample)
(dolist (gob (list {@i[graphical-object]}@+[*]))
   (add-component @i{aggregate} gob))
@end(ProgramExample)

An example of using @pr(add-components) is:
@begin(ProgramExample)
(opal:add-components agg obj1 obj2 myrect myarc)
@end(ProgramExample)

Note that this has the effect of placing the list of graphical objects from
back to front in @i{aggregate} since it inserts each new object with the
default @pr(:where :front).

@Index(remove-component)
@begin(ProgramExample)
opal:Remove-Component @i[aggregate graphical-object]@value(method)
@end(ProgramExample)

The @pr[remove-component] method removes the
@i[graphical-object] from @i[aggregate].  If @i{aggregate} is connected to
a window, then @i{graphical-object} will be erased when the window next has
an update message (section @ref(windowfuncs)) sent to it.

@Index(remove-components)
@begin(ProgramExample)
opal:Remove-Components @i[aggregate] &rest @i[graphical-object]@value(function)
@end(ProgramExample)

Removes all the listed components from @i{aggregate}.

@Index(move-component)
@begin(ProgramExample)
opal:Move-Component @i[aggregate graphical-object] [[:where] @i[position][@i{locator}]]@value(method)
@end(ProgramExample)

@pr(Move-component) is used to change the drawing order of objects in an
aggregate, and therefore change their covering (since the order of objects
in an aggregate determines their drawing order).  For example, this function
can be used to move an object to the front or back.  The object should
already be in the aggregate, and it is moved to be at the position specified.
It is like a @pr(remove-component) followed by an @pr(add-component) except
that it is more efficient.  The parameters are the same as @pr(add-component).


@section(Application of functions to components)

There are two methods defined on aggregates to apply functions to some
subset of the aggregate's components.  The methods work on either the
direct components of the aggregate or all objects that are either direct or
indirect components of the aggregate.

@Index(do-components)
@begin(ProgramExample)
opal:Do-Components @i[aggregate function] &key @i[type self]@value(method)
@end(ProgramExample)

The @pr[do-components] method applies @i[function] to all components of
@i[aggregate] in back-to-front order.  The @i[function] should take one
argument which will be the
component. If a type is specified, the function is only applied to
components that are of that type.  If the call specifies @pr(:self) to be @c[t]
(the default is @c(nil)), and the aggregate is of the specified type,
then the function is applied to @i[aggregate] after being applied to
all of the components.

The @i[function] must be non-destructive, since it will be applied to the
components list of @i[aggregate], not to a copy of the components
list.  For instance, @i[function] cannot call @i[remove-component] on
the components.  If you want to use a @i[function] that is
destructive, you must make a copy of the components list and call
dolist yourself.

@blankspace(1 line)
@Index(do-all-components)
@begin(ProgramExample)
opal:Do-All-Components @i[aggregate function] &key @i[type self]@value(method)
@end(ProgramExample)

The @pr[do-all-components] method works similarly to @pr[do-components],
except that in the case that a component is an aggregate,
@pr(do-all-components) is first called recursively on the component
aggregate and then applied to the component aggregate itself.
@pr(Self) determines whether to call the function on the top level
aggregate (default=@c(nil)) after all components.

@section(Finding objects under a given point)
@label(querying-children)

@Index(point-to-component)
@blankspace(1 line)
@begin(ProgramExample)
opal:Point-To-Component @i[aggregate x y] &key @i[type]@value(method)

@Index(point-to-leaf)
opal:Point-To-Leaf @i[aggregate x y] &key @i[type]@value(method)
@end(ProgramExample)

@pr[Point-to-component] queries the aggregate for the first generation
children at point (@i[x],@i[y]).  The value of @i[type] can limit the search to
graphical objects of a specific type.  This function returns the topmost
object at the specified point (@i[x],@i[y]).

@pr[Point-to-leaf] is similar except that the query continues to the deepest
children in the aggregate hierarchy (the leaves of the tree).  Sometimes you
will want an aggregate to be treated as a leaf in this search, like a button
aggregate in a collection of button aggregates.  In this case, you should set
the @index(pretend-to-be-leaf) @pr(:pretend-to-be-leaf) slot of each
aggregate that should be treated like a leaf.  The search will not
proceed through the components of such an aggregate, but will return
the aggregate itself.

The @i[type] slot can be either @c(t) (the default), a type, or a list
of types. 
If @i[type] is specified as an atom, only objects that are of that
@i[type] will be tested.  If @i[type] is specified as a list, only
objects whose type belongs to that list will be tested.
The value @c(t) for @i[type] will match all objects.  If the @i[type] is
specified for a @pr(point-to-leaf) call, and the @pr(type) is a kind of
aggregate, then the search will stop when an aggregate of that type (or
types) is found at the specified (x,y) location, rather than going all
the way to the leaves.  For example:
@begin(programexample)
(create-instance 'MYAGGTYPE opal:aggregate)
(create-instance 'MYAGG MYAGGTYPE)
(create-instance TOP-AGG opal:aggregate)
(opal:add-component TOP-AGG MYAGG)

(create-instance OBJ1 ...)
(create-instance OBJ2 ...)
(opal:add-components MYAGG OBJ1 OBJ2)

(opal:point-to-leaf TOP-AGG x y) ;@i{will return obj1, obj2, or NIL}
(opal:point-to-leaf TOP-AGG x y :type MYAGGTYPE) ;@i{will return MYAGG or NIL}
@end(programexample)

@index(point-in-gob)
@pr(Point-to-leaf) and @pr(point-to-component) always use the function
@pr(point-in-gob) on the components.



@begin(group)
@section(Finding objects inside rectangular regions)
@label(rect-regions)
@Index(components-in-rectangle)

@blankspace(1 line)
@begin(ProgramExample)
opal:Components-In-Rectangle @i[aggregate  top  left  bottom  right] &key @i[type  intersect]@value(function)
@end(ProgramExample)

@Index(leaf-objects-in-rectangle)
@begin(ProgramExample)
opal:Leaf-Objects-In-Rectangle @i[aggregate  top  left  bottom  right] &key @i[type  intersect]@value(function)
@end(ProgramExample)

@Index(obj-in-rectangle)
@begin(ProgramExample)
opal:Obj-In-Rectangle @i[object  top  left  bottom  right] &key @i[type intersect]@value(function)
@end(ProgramExample)

The routine @pr[components-in-rectangle] queries the aggregate for the first
generation children that intersect the rectangle bounded by @i(top), @i(left),
@i(bottom), and @i(right).  If @i[intersect] is @c(nil), then the
components which 
are returned must be completely inside the rectangle, whereas if
@i[intersect] is non-@c(nil) (the default), then the components need only
intersect the rectangle.  The value of @i[type] can limit the search
to graphical objects of a specific type. 
@end(group)
@blankspace(1 line)


@pr[Leaf-objects-in-rectangle] is similar except that the query
continues to the deepest children in the aggregate hierarchy (the
leaves of the tree).  Sometimes you will want an aggregate to be
treated as a leaf in this search, like a button aggregate in an
aggregate of buttons.  In this case, you should set the
@index(pretend-to-be-leaf) @pr(:pretend-to-be-leaf) slot of each
aggregate that should be treated like a leaf.  The search will not
proceed through the components of such an aggregate, but will return
the aggregate itself.

@pr[Obj-in-rectangle] tells whether the bounding box of @i[object]
intersects the rectangle bounded by @i[top], @i[left], @i[width] and
@i[height].  If @i[intersect] is non-@c(nil) (the default) 
then @i[object] need only intersect the rectangle, whereas if
@i[intersect] is @c(nil) then @i[object] must lie completely inside the
rectangle.  If @i[type] is not @c(t) (the default) then @i[object] must be
of type @i[type]. 



@Chapter(Virtual-Aggregates)
@Index(virtual-aggregates)

@i(Virtual-aggregates) are used when you are going to create a very large
number of objects (e.g., 300 to 50,000) all of which are fairly similar.
For example, they are useful for points in a scatter plot, squares in
a "fat-bits" bitmap editor, line segments in a map, etc.  The virtual
aggregate @i(pretends) to provide an object for each element, but actually
doesn't.  This can save an enormous amount of memory and time, while
still providing an interface consistent with the rest of Garnet.

The primary restriction is that there cannot be references or
constraints from external objects @i(to) or @i(from) any of the
elements of the virtual-aggregate.  Typically, all the constraints
will be internal to each object displayed, and all the properties will
be determined by the values in the @pr(:items) array.

The interface is similar to @i(aggrelists).  The programmer provides an
item-prototype, used for all the elements, and an (optional) items
list to form the initial value.  To be more efficient, the items list
is actually an array for virtual-aggregates.  The item-prototype can
be an arbitrary object or aggregadget structure, and can use whatever
formulas are desired to calculate the appropriate display based on the
corresponding value of the items list and the object's rank in the
item's list.

We have implemented two styles of virtual-aggregates, with
a third style in planning.
The first style is for arbitrary overlapping objects, and is described
below.  The second style is for non-overlapping 2-D arrays of objects,
such as bitmap-editor tiles.  

The third style is like the first, for arbitrary overlapping objects.
However, unlike the first style, it would use more sophisticated
techniques for computing the overlapping of objects, rather than using
linear search like the first style.  For example, it might use
quad trees or whatever.

So far, we have implemented the first and second style only.  Examples
of using these virtual-aggregates are in demo-circle for the first
style and demo-array for the second.

@Section(Virtual-Aggregates Slots)

A virtual-aggregate is a graphical object, with its own @pr[:draw],
@pr[:point-to-component], @pr[:add-item], and @pr[:remove-item] methods.
It is defined as:
@begin(programexample)
(create-instance 'opal:virtual-aggregate opal:graphical-object
    ...
   (:item-prototype ...)  @i(;; you must provide this)
   (:point-in-item ...)   @i(;; you must provide this)
   (:item-array ...)      @i(;; you may provide this)
   (:dummy-item ...)
   )
@end(programexample)

For example, in demo-circle the virtual-aggregate is:

@begin(programexample)
    (create-instance NIL opal:virtual-aggregate
       (:item-prototype MY-CIRCLE)
       (:point-in-item #'My-Point-In-Circle))
@end(programexample)

Here are the slots you must provide for a virtual-aggregate.

@b(:ITEM-PROTOTYPE)@*
In the :item-prototype slot, you put the Garnet object of your choice
(primitive object or aggregadget).  You must, however, have formulas
in your :item-prototype object that depend on its
@pr(:item-values) and/or @pr(:rank) slot.  The @pr(:rank) is set with
the object's rank in the @pr(:items) array.  The @pr(:item-values) is
set with the appropriate data from the @pr(:item-array).  For instance, in
demo-circle, the item-prototype is:

@begin(programexample)
(create-instance 'MY-CIRCLE opal:circle
   (:filling-style (o-formula (fourth (gvl :item-values))))
   (:radius (o-formula (third (gvl :item-values))))
   (:left (o-formula (- (first (gvl :item-values)) (gvl :radius))))
   (:top (o-formula (- (second (gvl :item-values)) (gvl :radius))))
   (:width (o-formula (* 2 (gvl :radius))))
   (:height (o-formula (gvl :width))))
@end(programexample)

In this case the @pr[:item-values] slot contains a list of four numbers:
the x and y coordinates of the center of the circle, the radius of the
circle, and an Opal color.  
For your item-prototype, the format for
the item-values data can be anything you like, and you don't have to
set the @pr[:item-values] slot yourself: Opal will do that for you.

@blankspace(1 line)
@begin(group)
@b(:POINT-IN-ITEM)@*
This slot contains a function of the form
@begin(programexample)
(lambda (virtual-aggregate item-values x y) ...)
@end(programexample)
which returns @c(t) or @c(nil) depending on whether the point <x,y> lies within
an @pr[:item-prototype] object with @pr[:item-values] item-values.
Typically, you will be able to compute this function efficiently based
on your knowledge of the how the objects will look.
For instance, in demo-circle, the @pr[:point-in-item] slots contains:
@end(group)

@begin(programexample)
(lambda (virtual-aggregate item-values x y)
  (<= (+ (expt (- x (first item-values)) 2)
         (expt (- y (second item-values)) 2))
      (expt (third item-values) 2)))
@end(programexample)

@b(:ITEM-ARRAY)@*
This is a slot you @i(may), but need not provide.  If you don't provide
one, then all of the items will be added using the add-item function, below.
@pr(:item-array) contains either a 1-dimensional array of item-values, ordered
from back to front on your display, or a 2-dimensional array.  So for the
demo-circle example, it will look something like:
@begin(programexample)
#((304 212 12 #k<RED-FILL>)
  (88 64 11 #k<GREEN-FILL>)
  ...)
@end(programexample)
The array may have @c(nil)s in it.  Each @c(nil) represents a gap in this items
list.

@section(Two-dimensional virtual-aggregates)
You can create a virtual-aggregate whose @pr(:item-array) is a @i(two)
dimensional array.  The formulas in the @pr(:dummy-item) of the aggregate
must depend on two slots @pr(:rank1) and @pr(:rank2) instead of the single
slot @pr(:rank).  This is useful for non-overlapping tables, such as
bitmap editors (fat-bits), spreadsheets, etc.  See the example in
demo-array.


@section(Manipulating the Virtual-Aggregate)
These are the routines exported by Opal that you can use to manipulate
the item array:

@index(add-item)
@begin(programexample)
opal:Add-Item @i(a-virtual-aggregate  item-values)@value(method)
@end(programexample)

This adds a new item to the @pr(:item-array) of @i(a-virtual-aggregate).
@i(Item-values) is a list containing the values for an @pr(:item-values) slot
of the item-prototype.  @pr(Add-item) returns the rank into the
@pr(:item-array) where the new item was inserted.  The
@pr(:item-array) must be one-dimensional. 

@index(remove-item)
@begin(programexample)
opal:Remove-Item @i(a-virtual-aggregate  rank)@value(method)
@end(programexample)
  This removes an item from the @pr(:item-array) of @i(a-virtual-aggregate).
Actually, it puts a @c(nil) in the @pr(:item-array) (it does not compress the
array).  The @pr(:item-array) must be one-dimensional.

@index(change-item)
@begin(programexample)
opal:Change-Item @i(a-virtual-aggregate  new-item  rank) &optional @i(rank2)@value(method)
@end(programexample)
This changes the @i[rank]'th entry of the @pr[:item-array] of the
virtual-aggregate to be @i[new-item].  (It also marks that item to be
redrawn at the next update).  To manipulate a two-dimensional array, use
@i[rank] and @i[rank2] as the two indices.  Note: you have to use
this function and cannot directly modify the items array after the
virtual-aggregate has been displayed.

@index(point-to-rank)
@begin(programexample)
opal:Point-To-Rank @i(a-virtual-aggregate  x  y)@value(method)
@end(programexample)

Returns the rank of the front-most item in the virtual-aggregate
that contains point <x,y>.  (This is why you had to supply @pr[:point-in-item].)
The virtual-aggregate must be one-dimensional.

@index(point-to-component)
@begin(programexample)
opal:Point-To-Component @i(a-virtual-aggregate  x  y)@value(method)
@end(programexample)
  This is like @pr[point-to-rank], but it returns an actual Opal object.
However, the object is actually a dummy object with the appropriate
value placed in its @pr[:item-values] and @pr[:rank] slots.  So you cannot call
Point-to-component twice and hope to hold on the first value.
(The virtual-aggregate must be one-dimensional.)

@index(recalculate-virtual-aggregate-bboxes)
@begin(programexample)
opal:Recalulate-Virtual-Aggregate-Bboxes @i(a-virtual-aggregate)@value(function)
@end(programexample)
  The purpose of this routine is to re-initialize all the bounding
boxes of the items of the virtual-aggregate.  This would come in
handy if, for instance, you created a virtual-aggregate whose items
depended for their position on the position of the virtual-aggregate
itself.  After you changed the @pr[:left] or @pr[:top] of the
virtual-aggregate,
you would call @pr(recalculate-virtual-aggregate-bboxes) to re-calculate
the bounding boxes of the items.


There is a macro for performing operations iteratively on
elements of a 2-dimensional virtual-aggregate:
@index(do-in-clip-rect)
@begin(programexample)
opal:Do-In-Clip-Rect (@i[var1  var2  a-virtual-aggregate  clip-rect]) &body @i[body]@value(macro)
@end(programexample)
The variables @i(var1) and @i(var2) take on all values for which the item with
@pr(:rank1) = @i(var1) and @pr(:rank2) = @i(var2) intersect the
clip-rectangle @i(clip-rect).
The @i(clip-rect) is a list of left, top, width, and height -- the kind of
argument that is returned from a two-point-interactor.

As an example, consider the following code borrowed from demo-array:
@begin(programexample)
(defun Whiten-Rectangle (dum clip-rect)
  (declare (ignore dum))
  (do-in-clip-rect (index-1 index-2 the-array clip-rect)
    (change-item the-array 1 index-1 index-2)))

(create-instance 'WHITER inter:two-point-interactor
   (:start-event :leftdown)
   (:continuous T)
   (:start-where `(:in ,The-Array))
   (:window w)
   (:feedback-obj FEED-RECT)
   (:final-function #'Whiten-Rectangle))
@end(programexample)
The-array is a 2-dimensional virtual-aggregate.  The routine
@pr(Whiten-Rectangle)
performs @pr(opal:change-item) on every element of the-array that is
inside the clip-rect (the second argument to the @pr(:final-function) of
a two-point interactor is always a rectangle).



This is a macro for performing operations iteratively on
elements of a 2-dimensional virtual-aggregate.  The variables @i(var1) and
@i(var2) take on all values for which the item with @pr[:rank1] = @i(var1) and
@pr[:rank2] = @i(var2) intersect the clip-rectangle clip-rect.  The clip-rect
is a list of left, top, width, and height -- the kind of argument
that is returned from a two-point-interactor.





@Chapter(Windows)
@label(windows)
@index(window) 

Graphical objects can only display themselves in a @i(window).  

@Begin[ProgramExample]
(create-instance 'inter:Interactor-Window opal::window
  (:maybe-constant '(:left :top :width :height :visible))
  (:left 0)
  (:top 0)
  (:width 355)
  (:height 277)
  (:border-width 2)
  (:left-border-width ...) (:top-border-width ...)     @i(;; Read-only slots -- Do not set!)
  (:right-border-width ...) (:bottom-border-width ...) @i(;; See section @ref[border-widths].)
  (:max-width NIL) (:max-height NIL)
  (:min-width NIL) (:min-height NIL)
  (:cursor opal:Arrow-Pair)    @i[;; Shape of the pointer in this window.  (See section @ref[window-cursors]).]
  (:position-by-hand NIL)
  (:title "Opal @i[N]")
  (:omit-title-bar-p NIL)
  (:icon-title "Opal @i[N]")
  (:icon-bitmap NIL)
  (:draw-on-children NIL)
  (:background-color NIL)
  (:double-buffered-p NIL)
  (:save-under NIL)
  (:aggregate NIL)
  (:parent NIL)
  (:visible ...)
  (:modal-p NIL)               @i[;; Whether to suspend input while visible.  See the Interactors Manual.]
  (:in-progress NIL)           @i[;; Read by ]opal:update-all@i[.  See section @ref[quarantine-slot].]
  ...)
@End[ProgramExample]

@b(Caveats:)
@begin(itemize)
Garnet windows will not appear on the screen until they are
updated, by calling the functions @pr(opal:update) or @pr(opal:update-all).
These functions will also cause all of the graphics in the window to be
brought up-to-date.

Windows are not usually used as prototypes for other windows.  If a window
is created with its @pr(:visible) slot set to T, then it should be expected
to appear on the screen (even if @pr(opal:update) is not explicitly called
on it).  When similar windows need to be generated, it is recommended that
a function be written (like at the end of the Tutorial) that will return
the window instances.
@end(itemize)


@index(top)@index(left)@index(width)@index(height)
The @pr[:left], @pr[:top], @pr[:width], and @pr[:height] slots of the
window control its position and dimensions.
These slots can be set using @pr(s-value) to change the window's
size and position (which will take affect after the next @pr(update) call).
If the user changes the size or position of a window using the window manager
(e.g., using the mouse), this will @i(usually) be reflected in the values
for these slots.@foot(There are bugs in some window managers that make
this difficult or impossible.)  Some special issues involving the position and
dimensions of Garnet windows when adorned with window manager title bars are
discussed in section @ref(border-widths).

@Index[min-width] @Index[max-width]@Index[min-height] @Index[max-height]
If you create a window with values in its @pr(:max-width), @pr(:max-height),
@pr(:min-width), and @pr(:min-height), then the window manager will make sure
the user doesn't change the
window's size to be outside of those ranges.  However, you can
still @pr[s-value] the @pr[:width] and @pr[:height] of @pr[win] to be
any value.  The slots @pr[:max-width] and @pr[:max-height] can only be
set at creation time.  Furthermore, due to peculiarities in X windows, you
must set @i(both) @pr[:max-width] and @pr[:max-height] 
to be non-@c(nil) at creation time to have any effect.
The slots @pr[:min-width] and @pr[:min-height] behave in the analogous
manner. 

@index(title)
The @pr[:title] slot contains a string specifying the title of the Garnet
window.  The default title is "Opal @i[N]", where @i[N] starts at 1, and
increments each time a new window is created in that Lisp.

@index(omit-title-bar-p)
The @pr[:omit-title-bar-p] slot tells whether or not the Garnet window should
have a title bar.  If the slot has value @c(nil) (the default), and the window
manager permits it, then the window will have a title bar; otherwise the
window will not have a title bar.

@index(icon-title)
The @pr[:icon-title] slot contains a string specifying the icon title
of the window.  The default icon title is the same as the
@pr[:title.]  This is the string that gets displayed when a window is
iconified. 

@index(icon-bitmap)
You may set the icon of a window to be an arbitrary bitmap by setting
its @pr[:icon-bitmap] slot.  The value should be a filename which
specifies the location of a bitmap file.

@index(draw-on-children)
In the rare case when you want to have graphics drawn on a parent
window appear over the enclosed (child) windows, you can set the
@pr[:draw-on-children] of the parent to be non-@c(nil).
Then any objects that belong to that window will appear on top of
the window's subwindows (rather than being hidden by the subwindows).
Note:  Because of the inability to redraw the graphics in the window and the
subwindows simultaneously, objects that will appear over the subwindows must
be fast-redraw objects drawn with @pr(:xor)
(see section @ref[fast-redraw-objects]).

@index(background-color)
The @pr[:background-color] slot of an @pr(inter:interactor-window) can be set
to be any @pr(opal:color).  The window will then appear with that as its
background color.  This is more efficient than putting a rectangle
behind all the objects.

@index(double-buffered-p)
When the @pr(:double-buffered-p) slot is T, then an exact copy of the window
will be maintained internally by Garnet.  Then, when the graphics in the window
change, the change occurs first in the copy, and then the changed region is
transferred as a pixmap to the original window.  This has the potential to
reduce flicker in the redrawing of the window.  By default, windows do not use
this feature because of the extra memory required by the internal buffer.

@index(save-under)
When the @pr(:save-under) slot is T, then Garnet internally stores the
contents of the screen under the window.  If the window is made invisible,
then Garnet does not have to redraw any Garnet windows under it, because the
image can simply be redrawn from the saved contents.  This option is used in
the @pr(menubar) and @pr(option-button) gadgets.

@index(aggregate)
The @pr[:aggregate] slot specifies an aggregate object to hold all the
objects to be displayed in the
window.  Each window must contain exactly one aggregate in this slot, and
all objects in the window should be put into this aggregate.  This
slot should be set after the window is created, not during the
@pr(create-instance) call.  This will ensure that the proper demons
are running when the slot is set.
@b(Performance hint: specify the top, left, width and height of this
aggregate to be formulas depending on the window, rather than using the
default formulas, which depend on all of the objects in the aggregate).

@index(visible)
The @pr[:visible] slot specifies if the window is currently visible on the
screen or not.  In X terminology, this
determines if the window is mapped or not.  You can set the @pr(:visible)
slot at any time to change the visibility (which will take effect after an
@pr(update) call).

@index(position-by-hand)
If you create a window and set the @pr[:position-by-hand] slot to be
T, then when you call @Pr(opal:update) the first time, the cursor on your
screen will change to a
prompt asking you where to position the window, and the initial values of
@pr[:left] and @pr[:top] will be ignored.

@index(parent)
If a window is created with a window object in its @pr(:parent) slot, then
the new window will be a sub-window of the parent window.
Each window sets up its own coordinate system, so the @pr(:left) and
@pr(:top) of the subwindow will be with respect to the parent window.
@b(The parent window must be updated before the subwindow is created.)
Using @c(nil) for the @pr(:parent) makes the window be at the top level.  Only
top-level windows can be manipulated by the window manager (i.e, by using
the mouse).



@Section(Window Positioning)

When top-level windows first become visible, their @pr(:left) and @pr(:top)
slots may change values slightly to accomodate the title bars added by the
window manager.  When you create a
regular top-level window with a @pr(:top) of 100, for example, the inside
edge of the window will appear at 100.  The window manager frame of
the window (the outside edge) will appear a little higher, depending on
the window manager, but somewhere around 25 pixels higher.
The window manager then notifies Garnet that this frame has been added by
changing the @pr(:top) of the window to 75.  The drawable region of the window
remains at 100.

When the @pr(:top) of the window is changed (via @pr(s-value)) after it is
visible, then it is the outside edge of the window that is being changed,
which is the top of the frame.
You can always determine the height of the window's title bar in the
@pr(:top-@|border-@|width) slot (see section @ref[border-widths]).
There are corresponding slots for @pr(:left-), @pr(:right-), and
@pr(:bottom-@|border-@|width).  All of these slots are read-only,
and are set by Garnet according to your window manager.

When stacking windows in a cascading arrangement, it is
sufficient to be consistent in setting their positions either before or
after updating them.  If the two kinds of position-setting strategies need to
be mixed, then the @pr(:top-@|border-@|width) of the windows that have already
been made visible should be taken into account, versus those that have
never been updated.


@begin(group)
@Section(Border Widths)
@label[border-widths]

There are two different meanings of "border widths" in windows.  One involves
the user-settable thickness of subwindows, and the other kind involves
@i(read-only) widths that are determined by the window manager:

@begin(itemize)
@index(border-width)
@b(Subwindow Border Width) - The @pr(:border-width) slot affects the width
of the border on a subwindow.
Setting the @pr(:border-width) slot of a subwindow to 0 during its
@pr(create-instance) call will cause the window to have no border at all,
but setting it to a value larger than the default usually has no effect.
Currently, the border width cannot be changed after the window is created.

@index(left-border-width)@index(right-border-width)
@index(top-border-width)@index(bottom-border-width)
@b(Window Manager Frame Widths) - After a window has been created, the
@pr[:left-@|border-@|width], @pr[:right-@|border-@|width],
@pr[:top-@|border-@|width], and
@pr[:bottom-@|border-@|width] slots tell what thicknesses the left, right, top,
and bottom borders of the windows actually have.  These slots are set
by the window manager, and should @u(not) be set by Garnet users.
@end(itemize)
@end(group)


@Section(Window Cursors)
@label[window-cursors]
@index[cursor (pointer)]  @index(pointer)

The default cursor shape for Garnet windows is an arrow pointing to the
upper left.  However, it would be nice to change this shape sometimes,
particularly when an application is performing a long computation and you
would like to display an hourglass cursor.  Several functions and objects
make it easy to change the cursors of Garnet windows.

The following sections discuss how to change window cursors, starting with
some background at the lowest level of the cursor interface.  The later
sections, particularly @ref(with-hourglass-sec), describe the high-level
functions that allow you to change the cursor with a single function call.


@begin(group)
@Subsection(The :cursor Slot)
@label[the-cursor-slot]
@index(cursor slot syntax)

At the lowest level, the cursor of a Garnet window is governed by the value
of its @pr(:cursor) slot.  The default value for an
@pr(inter:interactor-window)'s @pr(:cursor) slot is a list of two objects,
@pr[(#k<OPAL:ARROW-CURSOR> . #k<OPAL:ARROW-CURSOR-MASK>)], which are
pre-defined bitmaps whose images are read from the @pr(garnet/lib/bitmaps/)
directory.  The @pr(opal:arrow-cursor) object is the black part of the
pointer, and the @pr(opal:arrow-cursor-mask) is the underlying white part.
@foot[Whenever you change the cursor of a window, it is a good idea to have a
contrasting mask beneath the primary image.  This will keep the cursor
visible even when it is over an object of the same color.]
@end(group)

@blankspace(1 line)
The @pr(:cursor) slot permits three different syntaxes which all describe
a cursor/mask pair for the window.  The most basic syntax is used for the
default value:

@programexample{(list @i[bitmap-1 bitmap-2])}

The second syntax allows you to use a font as the source for your cursor,
with the primary image and mask specified by indices into the font:

@programexample{(list @i[my-font  index-1  index-2])}

Most machines come with a font specifically for the window manager cursors,
and this font can be accessed with the @pr(opal:cursor-font) object.
So you could try the syntax above with the @pr(opal:cursor-font) object
and two consecutive indices, like this:

@programexample[(s-value WIN :cursor (list opal:cursor-font 50 51))]

You have to update the window to make the cursor change take effect.
It appears that sequential pairs, like 50 and 51,
reliably yield primary cursors and their masks.
It is easy to experiment to find a nice cursor.

Since so many cursors are created from the cursor font, a third syntax is
provided that is analogous to the previous one:

@programexample{(list :cursor @i[index-1 index-2])}

Any of these three syntaxes can be used to @pr(s-value) the @pr(:cursor)
slot of a window.  Changing the @pr(:cursor) slot of a window changes
it permanently, until you @pr(s-value) the @pr(:cursor) slot again.


@begin(group)
@Subsection(Garnet Cursor Objects)
@index(arrow-cursor) @index(hourglass-cursor)

@begin(programexample)
(create-instance 'opal:ARROW-CURSOR opal:bitmap
  (:image (opal:Get-Garnet-Bitmap "garnet.cursor")))

(create-instance 'opal:ARROW-CURSOR-MASK opal:bitmap
  (:image (opal:Get-Garnet-Bitmap "garnet.mask")))

(defparameter opal:Arrow-Pair
              (cons opal:ARROW-CURSOR opal:ARROW-CURSOR-MASK))



(create-instance 'opal:HOURGLASS-CURSOR opal:bitmap
  (:image (opal:Get-Garnet-Bitmap "hourglass.cursor")))

(create-instance 'opal:HOURGLASS-CURSOR-MASK opal:bitmap
  (:image (opal:Get-Garnet-Bitmap "hourglass.mask")))

(defparameter opal:HourGlass-Pair
              (cons opal:HOURGLASS-CURSOR opal:HOURGLASS-CURSOR-MASK))
@end(programexample)
@end(group)
@blankspace(1 line)

The arrow-cursors are used for the default value of the @pr(:cursor) slot
in Garnet windows.  The Gilt interface builder and the @pr(save-gadget) use
the hourglass-cursors when they are busy with file I/O and performing long
calculations.  Users are free to use these objects in their own applications.

The variables @pr(opal:Arrow-Pair) and @pr(opal:HourGlass-Pair) are provided
so that users can avoid
cons'ing up the same list repeatedly.  Setting the @pr(:cursor) slot of
a window to be @pr(opal:HourGlass-Pair) and then updating the window
will change the cursor in the window.



@Subsection(Temporarily Changing the Cursor)
@label(with-hourglass-sec)

Often when the cursor needs to be changed, we will be changing it back to
the default very soon (e.g., when the application has finished its
computation).
Also, usually we want to change all of the windows in an application, rather
than just one window.  For this situation, the functions
@pr(opal:change-cursors) and
@pr(opal:restore-cursors) were written to change the cursors of multiple
windows @u(without) changing the @pr(:cursor) slots.

@index(change-cursors)
@programexample{opal:Change-Cursors @i(cursor-list) &optional @i(window-list) @value(function)}

The @i(cursor-list) argument is a pair or triplet that adheres to the syntax
for the @pr(:cursor) slot, discussed in the previous section.  When
@i(window-list) is supplied, the cursor of each window is temporarily set
with a cursor constructed out of the
@i(cursor-list) spec.  When @i(window-list) is NIL (the default), then @u(all)
Garnet windows are set with the temporary cursor.  The value of the
@pr(:cursor) slot of
each window remains unchanged, allowing the window's normal cursor to be
restored with @pr(opal:restore-cursors).

@index(restore-cursors)
@programexample{opal:Restore-Cursors &optional @i(window-list) @value(function)}

This function undoes the work of @pr(opal:change-cursors).  Each window is set
with the cursor described by the value of its @pr(:cursor) slot (which was
not changed by @pr[opal:change-cursors]).


Even the work of calling @pr(opal:change-cursors) and @pr(opal:restore-cursors)
can be abbreviated, by using the following macros instead:

@index(with-cursor) @index(with-hourglass-cursor)
@programexample{opal:With-Cursor @i(cursor) &body @i(body) @value(macro)}
@programexample{opal:With-HourGlass-Cursor &body @i(body) @value(macro)}

The @i(cursor) parameter must be a pair or triplet adhering to the @pr(:cursor)
syntax.  These macros change the cursor of all Garnet windows while executing
the @i(body), and then restore the old cursors.  These are the highest level
functions for changing window cursors.
To test the @pr(opal:with-hourglass-cursor) macro, bring up any Garnet window
(demos are fine) and execute the following instruction:

@programexample[(opal:with-hourglass-cursor (sleep 5))]

While lisp is sleeping, the cursors of all the Garnet windows will change
to hourglass cursors, and then they will change back to normal.



@Section(Update Quarantine Slot)
@label(quarantine-slot)
@index(quarantine slot) @index(in-progress)

A "quarantine slot" named @pr(:in-progress) exists in all Garnet windows.
If there was a crash during the last update of the window, then the window
will stop being updated automatically along with the other Garnet windows,
until you can fix the problem and update the window successfully.

Usually when there is an update failure, it is while the main-event-loop
process is running and it is repeatedly calling @pr(opal:update-all).
Without a quarantine slot, these repeated updates
would keep throwing Garnet into the debugger, even as you tried to figure
out what the problem was with the offending window.  With the quarantine slot,
@pr(opal:update-all) first checks to see if the @pr(:in-progress) slot of
the next window is T.  If so, then the last update to that window must not
have terminated successfully, and the window is skipped.  After you fix
the problem in the window, a successful call to @pr(opal:update) will clear
the slot, and it will resume being updated automatically.

Here is an example of a typical interaction involving the quarantine slot.

@begin(enumerate)
Execute @pr[(garnet-load "demos:demo-multiwin")] and
 @pr[(demo-multiwin:do-go)].

Artificially create an error situation by executing
@begin(programexample)
(kr:with-types-disabled
 (kr:s-value demo-multiwin::OBJ1 :left 'x))
@end(programexample)

Try to move an object in the demo by clicking on it and dragging with the
mouse.  Even if you did not click on OBJ1 (the rectangle), the main-event-loop
called @pr(opal:update-all), which caused OBJ1's window to update.  This
caused a crash into the debugger when @pr('x) was found in the @pr(:left)
slot.  Get out of the debugger with @pr(:reset) or @pr(q) or whatever your
lisp requires.

Now move objects again.  As long as your first mouse click is not in the same
window as OBJ1, you will not get the crash again.  You can even drag
objects into and through OBJ1's window, but that window will not be
updated.

After you give OBJ1's @pr(:left) slot a reasonable value and do a @u(total)
update on its window -- @pr[(opal:update demo-multiwin::WIN1 T)] -- the window
will be treated normally again.  Note: the total update is sometimes required
because the bad @pr(:left) value can get stored in an internal Opal data
structure.  A total update clears these data structures.
@end(enumerate)

We have found that this feature makes it much easier to find the source
of a problem in a window that cannot update successfully.  Without this
feature, useful tools like the @pr(Inspector) would not be able to run
while there was one broken window, since interacting with the @pr(Inspector)
requires repeated calls to @pr(opal:update-all).



@section(Windows on other Displays)
@index(windows on other displays)
@index(machines) @index(displays)

An important feature of the X window manager is that it allows you to run a
process on one machine and have its window appear on another machine.  Opal
provides a simple way to do this, although many commands have to be given
to the Unix Shell.

Let's suppose that you want to run Opal on a machine named 
@pr(OpalMachine.cs.edu) and you want the windows to appear on a machine
named @pr(WindowMachine.cs.edu) (of course you will substitute your own
full machine names).  Assuming you are sitting at
@pr(WindowMachine.cs.edu), perform the following steps before starting Garnet:

@begin(itemize)
Create an extra Xterm (shell) window and use it to telnet to
@pr(OpalMachine.cs.edu) and then log in.

@begin(Multiple)
Type the following to @pr(OpalMachine.cs.edu) to tell Opal where the
windows should go:
@begin(programexample)
setenv DISPLAY WindowMachine.cs.edu:0.0
@end(programexample)
@end(Multiple)

@begin(Multiple)
Now go to another Xterm (shell) window on @pr(WindowMachine.cs.edu)
and type the following to allow @pr(OpalMachine.cs.edu) to talk to X:
@begin(programexample)
xhost + OpalMachine.cs.edu
@end(programexample)
@end(Multiple)

Now go back to the telnet window, and start Lisp and load Garnet and any
programs.  All windows will now appear on @pr(WindowMachine.cs.edu).
@end(Itemize)

@index(*screen-width*) @index(*screen-height*)
The exported variables @pr(opal:*screen-width*) and @pr(opal:*screen-height*)
contain the width and height of the screen of the machine you are using.  Do
not set these variables yourself.

@section(Methods and Functions on Window objects)
@label(windowfuncs)

There are a number of functions that work on window objects, in addition to
the methods described in this section.  All of the extended accessor
functions (@pr[bottom], @pr[left-side], @pr[set-center], etc.) described in
section @Ref(Extended-accessors) also work on windows.

@index(update)

@Begin[ProgramExample]
opal:Update @i[window] &optional @i[total-p]@value(method)
@End[ProgramExample]

The @pr[update] method updates the image in @i[window] to reflect
changes to the objects contained inside its aggregate.  If @i(total-p) is a
non-@c(nil) value, then the window is erased, and all the components of the
window's aggregate are redrawn.  This is useful for when the window is
exposed or when something is messed up in the window (e.g., after a bug).
The default for @i(total-p) is @c(nil), so the window only redraws the
changed portions.  @pr(Update) must be called on a newly-created window
before it will be visible.  Updating a window also causes its subwindows to
be updated.

@index(fix-up-window)
If @pr(update) crashes into the debugger, this is usually because there is
an object with an illegal value attached to the window.  In this case, the
debugging function @pr(garnet-debug:fix-up-window) is very useful@dash@;see
the Debugging Manual.

@index(destroy)
@Begin[ProgramExample]
opal:Destroy @i[window]@value(method)
@End[ProgramExample]

The @pr[destroy] method unmaps and destroys the X
window, destroys the @i[window] object, and calls destroy on the
window's aggregate and the window's subwindows.

@index(update-all)
@Begin[ProgramExample]
opal:Update-All &optional @i(total-p)@value(function)
@End[ProgramExample]

been created but never @pr(update)d (so they are not yet visible).
When @i(total-p) is T, then @pr(opal:update-all) will redraw the entire
contents of all existing Garnet windows.  Since this procedure is expensive,
it should only be used in special situations, like during debugging.


@index(clean-up)
@Begin[ProgramExample]
opal:Clean-Up [@i[how-to]]@value(function)
@End[ProgramExample]

This function is useful when debugging for deleting the windows created using
Opal.  It can delete windows in various ways:

@index(orphans-only)@index(clx)
@StandardTable(Name CleanUp, Columns 2, HeadingBoxed)
@Begin(CleanUp)
@TableID(CleanUp)
@TableHeading(Immediate, RowFormat CleanUpColumnHeadings,
Line [@pr(How-to)@\Result])

@pr{:orphans-only}@\Destroy all orphaned garnet windows.  Orphans are
described below.

@pr{:opal}@\Destroy all garnet windows by calling @pr[xlib:destroy-window]
or @pr[ccl:window-close] on orphaned CLX "drawables" and
Mac "views", and @pr[opal:destroy] on non-orphaned windows.

@pr{:opal-set-agg-to-nil}@\Same as above, but before calling @pr[opal:destroy],
set the aggregate to @c(nil) so it won't get destroyed as well.

@pr{:clx}@\Destroy all Garnet windows by calling @pr[xlib:destroy-window]
or @pr[ccl:window-close].  Does not call the @pr(:destroy) method on
the window or its aggregate.


@end(CleanUp)

A window is "orphaned" when the Opal name is no longer attached to the
CLX drawable or Mac view.
This can happen, for example, if you create an instance of a window
object, update it, then create another instance of a window with the same
name, and update it as well.  Then the first window will not be
erased and will be orphaned.

@index(fix-up-window)
The default is @pr(orphans-only).  Another useful value is @pr(:opal).  The
other options are mainly useful when attempts to use these fail due to
bugs.  See also the function @pr(Fix-Up-Window) in the Garnet Debugging
Manual @cite(Garnetdebugmanual).

@index(convert-coordinates)
@Begin[ProgramExample]
opal:Convert-Coordinates @i[win1 x1 y1] &optional @i[win2]@value(function)
  (declare (values @i(x2 y2)))
@End[ProgramExample]
This function converts the coordinates @pr(x1) and @pr(y1) which are in
window @pr(win1)'s
coordinate system to be in @pr(win2)'s.  Either window can be @c(nil), in which
case the screen is used.

@index(get-x-cut-buffer)@index(set-x-cut-buffer)@index(cut buffer)
@Begin[ProgramExample]
opal:Get-X-Cut-Buffer @i[window]@value(function)
opal:Set-X-Cut-Buffer @i[window newstring]@value(function)
@End[ProgramExample]
These manipulate the window manager's cut buffer.  @pr(get-x-cut-buffer)
returns the string that is in the X cut buffer, and @pr(set-x-cut-buffer)
sets the string in the X cut buffer.

@Index(Raise-Window)@Index(Lower-Window)@Index(Iconify-Window)
@Begin[ProgramExample]
opal:Raise-Window @i[window]@value(function)
opal:Lower-Window @i[window]@value(function)
opal:Iconify-Window @i[window]@value(function)
opal:Deiconify-Window @i[window]@value(function)
@End[ProgramExample]
@pr(Raise-window) moves a window to the front of the screen, so that
it is not covered by any other window.  @pr(Lower-window) moves a
window to the back of the screen.  @pr(Iconify-window) changes the
window into an icon, and @pr(deiconify-window) changes it back to a window.



@Chapter(Printing Garnet Windows)
@label(printing)
@index(make-ps-file) @index(PostScript) @index(printing)

The function @pr(make-ps-file) is used to generate a PostScript file for
Garnet windows.  This file can then be sent directly to any PostScript
printer.  The file is in "Encapsulated PostScript"
format, so that it can also be included in other documents, such as Scribe,
LaTeX and FrameMaker on Unix, and Pagemaker on Macintoshes.

The PostScript files generated by this function will produce pictures that
are prettier, have much smaller file sizes, and work better in color than
those produced by the window utilities like @pr(xwd) and @pr(xpr).  However,
a limitation of PostScript is that it is not possible to print with XOR.
It is usually possible to change the implementation of Garnet objects or
hand-edit the generated PostScript file to simulate the XOR draw function.

By default, the contents of the window and all subwindows are
reproduced exactly as on the screen, with the image scaled and
centered on the output page.  Other options (see the @pr(clip-p)
parameter) allow this function to be
used to output the entire contents of a window (not just what is on
the screen), so it can be used to do the printing for application data
that might be in a scrolling-window, for example.  This is used in the
demo @pr(demo-arith).

@blankspace(1 line)
@begin(programexample)
opal:Make-PS-File @i(window-or-window-list  filename)@value(function)
                  &key @i(position-x  position-y  left-margin  right-margin  top-margin  bottom-margin  left  top)
                  @i(scale-x  scale-y  landscape-p  borders-p  clip-p  subwindows-p  color-p  background-color)
                  @i(paper-size  title  creator  for  comment)
@end(programexample)


The only two required parameters to @pr(make-ps-file) are the Garnet window
to be printed and the name of the file in which to store the PostScript
output.  The @i(window-or-window-list) parameter may be either a single window
or a list of
windows.  When multiple windows are printed, the space between the windows is
filled with the color specified by @i(background-color).

The optional arguments affect the position and appearance of the
picture:

@begin(description)
@i(position-x) - Either @pr(:left), @pr(:center), or @pr(:right).
Determines the position of the picture on the page horizontally.
Ignored if a value is supplied for @i(left).  Default is @pr(:center).

@i(position-y) - Either @pr(:top), @pr(:center), or @pr(:bottom).
Determines the position of the picture on the page vertically.
Ignored if a value is supplied for @i(top).  Default is @pr(:center).

@i(left-margin, right-margin, top-margin, bottom-margin) - These parameters
specify the minimum distance (in points) from the corresponding edge of the
page to the rendered image.  All four values default to 72,
which is one inch in PostScript.

@i(left, top) - The distance (in points) from the left and top margins
(offsets from @i(left-margin) and @i(top-margin)) to
the rendered image.  The defaults are @c(nil), in which
case the values of @i(position-x) and @i(position-y) are used instead.

@i(scale-x, scale-y) - Horizontal and vertical scaling for the image.  The
default is @c(nil), which will ensure that the image fits within the specified
margins (the scaling will be the same for vertical and horizontal).

@i(landscape-p) - If @c(nil) (the default) then the top of the picture will be
parallel to the short side of the page (portrait).
If T, then the picture will be rotated 90 degrees, with the top of the picture
parallel to the long side of the page.

@i(subwindows-p) - Whether to include the subwindows of the specified window
in the image.  Default is T.

@i(borders-p) - Whether to draw the outline of the window (and subwindows,
if any).  The allowed values are @pr(T), @pr(NIL),
@pr(:generic), and @pr(:motif).  The default value of @pr(:motif) gives your
image a simulated Motif window manager frame, like the picutres in the Gilt
Reference Manual.  The value of @pr(:generic) puts a plain black frame around
your printed image, with the title of the window centered in the title bar.
The value @pr(T) gives the image a thin black border, and @pr(NIL) yields no
border at all.

@i(clip-p) - How to clip the objects in the window.  Allowed values are:

@begin(itemize)
@pr(T) - This is the default, which means that the printed picture will look
like the screen image.  If the graphics inside the window extend outside the
borders of the window, then they will be clipped in the printed image.

@pr(NIL) - This value causes the window in the printed image to be the same
size as the top-level aggregate, whether it is larger or smaller than the
actual window.  That is, if the window is too small to show all of the objects
in its aggregate, then the printed window will be enlarged to show all of the
objects.  Conversely, if the top-level aggregate is smaller than the dimensions
of the window on the screen, then the printed window will be "shrink wrapped"
around the objects.

@pr{(@i[left top width height])} - A list of screen-relative coordinates that
describe absolute pixel positions for the printed window.  This makes it
possible to clip to a region when you are printing @i(multiple) windows.
Clip regions can be used to make multiple-page PostScript
files -- you have to manually divide the image into its component regions,
and generate one PostScript file for each region.  In the future, we may
attempt to automate the process of multiple-page printing.
@end(itemize)

@i(color-p) - Whether to generate a file that will print out the real colors
of the window's objects (T), or pretend that all the colors are
black (@c[nil]).  Default is T.  (Many PostScript printers will
automatically produce half-tones for colors, so usually T will work
even for color pictures printed on black and white printers.)  @b(Note:)
@index(pixmap) @index[colorimage]
Pixmaps print in full color when they are being displayed on a color
screen and the @i(color-p) parameter is T.  However, older
printers may not know the PostScript command @pr(colorimage) which is
required to render a color pixmap.  This command is only defined on Level 2
printers. If your printer cannot print your pixmap (it crashes with a
"colorimage undefined" error), then try using a @i(color-p) argument of
@c(NIL).

@i(background-color) - When @i(window-or-window-list) is a list of windows,
the space between the windows will be filled with this color.  The value of
this parameter may be any Opal color.  The default is @pr[opal:white].

@i(paper-size) - This parameter is provided mainly for users in the
United Kingdom.  Allowed values are @pr(:letter), @pr(:a4), or a list
of (@i[width height]).  The default value of @pr(:letter) generates a
PostScript image for 612x792 point size paper.  The @pr(:a4) value generates
an image for 594x842 point size paper, which is commonly used in the UK.

@i(title, creator, for) - These parameters should take strings to be printed
in the header comments of the PostScript file.  These comments are sometimes
used to print user information on the header sheets of printer output.
The default @i(title) is based on the window's title.  The default @i(creator)
is Garnet, and the default @i(for) is "".

@i(comment) - This parameter 
allows you to put a single line of text at the top of your PostScript file.
In the generated file, the characters @pr("%%") are concatenated to the front
of your comment, telling PostScript to ignore the text in the line.  If you
wish to use multiple lines in the comment, you will have to add the @pr("%%")
to the second line of the string and every line thereafter.


@end(description)


@chapter(Saving and Restoring)
@index(lisp image)
@index(core image)
@index(saving lisp images)
Opal includes the ability to save and restore Garnet core images.
The function @pr(opal:make-image), described below, can be used to automate
the process of closing the connection to the display server and generating
a core file.  Low-level details are provided below also, in case you need
more control over the saving process.


@Section(Saving Lisp Images)
@index(make-image)

@blankspace(1 line)
@programexample{opal:Make-Image @i(filename) &key @i(quit) (@i[verbose] T) (@i[gc] T) &rest @i[other-args]@value(function)}

The function @pr(opal:make-image) is used to save an image of your current
lisp session.  Without @pr(make-image), you would have to call
@pr(opal:disconnect-garnet), use your implementation-@|dependent function
to save your lisp image, and then call @pr(opal:reconnect-garnet) if you
wanted to continue the session.  @pr(Opal:make-image) does all of this for
you, and also does a total garbage collection before the save if the @i(gc)
parameter is T.  If the @i(quit) parameter is T, then your lisp image will
automatically exit after saving itself.  The @i(verbose) parameter controls
whether the function should announce when it is in the stages of garbage
collection, disconnection, saving, and reconnection.

The @i[other-args] parameter is supplied to accomodate the miscellaneous
parameters of each lisp vendor's image-saving function.  For example, with
Allegro's @pr(dumplisp) command, you can supply the keywords @pr(:libfile)
and @pr(:flush-source-info?).  Since @pr(opal:make-image) calls @pr(dumplisp)
for Allegro, you can supply the extra parameters to @pr(opal:make-image) and
they will be passed on to @pr(dumplisp).  Therefore, it is not necessary to
call your lisp's image-saving function manually; you can always pass the
additional desired parameters to @pr(opal:make-image).

When you restart the saved image, it will print a banner indicating the time
at which the image was saved, and will automatically call
@pr(opal:reconnect-@|garnet).  Some lisps (like Allegro) allow you to restart
the saved image just by executing the binary file, while others (like CMUCL)
require that the binary file is passed as an argument when the standard lisp
image is executed.  Consult your lisp's reference manual for instructions on
restarting your saved image.


@Section(Saving Lisp Images Manually in X/11)

It recommended that you use @pr(opal:make-image) whenever possible to save
images of lisp.  In particular, restarted images of MCL containing Garnet that
were created by other means will probably not work right, due to the skipping
of initialization steps that would have been performed automatically if the
image had been saved with @pr(opal:make-image).

When you do not want to use the function @pr(opal:make-image) to generate
an executable lisp image, and instead want to perform the saving procedure
manually, you can use the functions @pr(opal:disconnect-garnet) and
@pr(opal:reconnect-garnet), along with your implementation-@|dependent
function for saving lisp images.

@blankspace(.5 line)
@begin(ProgramExample)
@index(disconnect-garnet)
opal:Disconnect-Garnet@value(function)

opal:Reconnect-Garnet &optional @i[display-name] @value(function)
@end(ProgramExample)

Before saving a core image of Garnet, you must first close all connections
to the X server by calling @pr(opal:disconnect-garnet).  All windows which are
currently visible will disappear (but will reappear when
@pr(opal:reconnect-@|garnet) is executed).

While the connection to the X server is closed, you
may save a core image of Garnet by calling the appropriate Lisp
command.  In Lucid Lisp the command is @pr[(disksave)], in
Allegro Lisp it is @pr[(excl:dumplisp)], and in CMU Common Lisp
it is @pr[(ext:save-lisp)].  Consult your Common Lisp manual to
find the disk save command for your version of Common Lisp, as
well as how to start up a saved Lisp core.

It is usually convenient to specify @pr(opal:reconnect-garnet) as the
@i(restart-function) during your save of lisp.  For example, the following
instruction will cause @pr(opal:reconnect-garnet) to be invoked in
Allegro lisp whenever the saved lisp is restarted:

@begin(programexample)
(excl:dumplisp :name "garnet-image" :restart-function #'opal:reconnect-garnet)
@end(programexample)

Otherwise, you will need to call @pr(opal:reconnect-garnet) manually when
the lisp image is restarted in order to restore the connection to the server
and make all Garnet windows visible again.

If the @i[display-name] parameter to @pr(opal:reconnect-garnet) is specified,
it should be the name of a machine (e.g., "ecp.garnet.cs.cmu.edu").
If not specified, @i[display-name] defaults to the current machine.



@chapter(Utility Functions)

@begin(group)
@Section(Executing Unix Commands)
@index(unix) @index(shell-exec)

@programexample{opal:Shell-Exec @i(command) @value(function)}

The function @pr(opal:shell-exec) is used to spawn a Unix shell and
execute Unix commands.  The @i(command) parameter should be a string of the
Unix command to be executed.  The spawned shell does not read the @pr(.cshrc)
file, in order to save time.  The function returns a string of the output
from the shell.

In Lucid, CMUCL, and LispWorks, the shell spawned by @pr(opal:shell-exec)
is @pr(/bin/sh).  In Allegro and CLISP, the shell is the user's default.
Executing this function in other lisps, including MCL, causes an error
(please let the Garnet group know how to enhance this function to run in
your lisp).
@end(group)


@begin(group)
@Section(Testing Operating System Directories)
@index(directory-p)

This function is used to determine whether a string describes an
existing directory or not.

@programexample(opal:Directory-P @i(string)@value{function})

The @i(string) should name a potential directory, like @pr("/usr/garnet/").
If your lisp is running on a Unix system, this function spawns a shell and
executes a Unix command to test the directory.
There is no other standard way to test directories on different
lisps and operating systems.  On the Mac, a lisp-specific directory command
is executed.
@end(group)




@chapter(Aggregadgets and Interactors)

@index(aggregadgets)
The @i(Aggregadgets) module makes it much easier to create instances of an
aggregate and all its components.  With an aggregadget, you only have to
define the aggregate and
its components once, and then when you create an instance, it creates all
of the components automatically.  Aggregadgets also allow lists of items to
be created by simply giving a single prototype for all the list elements,
and a controlling value that the list iterates through.  Aggregadgets are
described in their own manual @Cite(AggregadgetsManual). 

@Index(Interactors)
@i(Interactors) are used to handle all input from the user.  Interactor
objects control input and perform actions on Opal graphical objects.
There are high-level interactor objects to
handle all the common forms of mouse and keyboard input.  Interactors are
described in their own manual @Cite(InterManual).

Together Opal and Interactors should hide all details of X and QuickDraw
from the programmer.  There should never be a need to reference any symbols in
@pr(xlib) or @pr(ccl).


@Chapter(Creating New Graphical Objects)

An interesting feature of object-oriented programming in Garnet is that
users are expected to create new objects only by combining existing
objects, not by writing new methods.  Therefore, you should only need to
use Aggregadgets to create new kinds of graphical objects.  It should never
be necessary to create a new @pr(:draw) method, for example.

If for some reason, a new kind of primitive object is desired (for example,
a spline or some other primitive not currently supplied by X/11), then
contact the Garnet group for information about how this can be done.  Due
to the complexities of X/11, Mac QuickDraw, and automatic update and
redrawing of objects in Opal, it is not particularly easy to create
new primitives.

@Begin(Comment)
*************************************************************
@B(BAM: I am not sure this section is correct.)

Most users of Opal will only use the pre-defined graphical objects, and
will combine them into aggregates and use formulas to attach them together.
It will be rare to create new kinds of graphical objects.  This should only
be needed when new primitives are available, such as splines.  

This chapter discusses how to create new types of graphical objects, should
that be necessary.

@section(Internal slots in graphical objects)

There are numerous extra slots in all graphical objects that are used
internally by Opal.  This section will attempt to describe these slots and
their potential uses when designing new graphical objects.

@subsection(:update-slots)
@label(opaldemons)

@index(update-slots)
The @pr(:update-slots) slot contains an association list of all slots in
the object that affect the output picture from the object.
For example:

@begin(programexample)
* (gv opal:arc :update-slots)
((:VISIBLE) (:LINE-STYLE) (:FILLING-STYLE) (:DRAW-FUNCTION) (:LEFT)
 (:TOP) (:WIDTH) (:HEIGHT) (:ANGLE1) (:ANGLE2))
@end(programexample)

If any of the values of these objects slots in an instance of an
@pr[opal:arc] object change, the instance will need to be redrawn at the
next window update.  

@index(demons)
Anytime a slot on the @pr[:update-slots] list is changed (either with
@pr[s-value] or by a formula being invalidated) the KR's invalidate demon
is called with the object, the slot, and the slot's value on the association
list.

Opal doesn't use the second value of the association pair, so it should be
left as @c(nil).

When creating an object that is a specialized instance of a prototype
object, one should inherit all the slots on the @pr[:update-slots]
list, and then add any others as necessary.  Commonly this is done by
something of the form:

@begin[programexample]
(create-instance 'opal:arc opal:graphical-object
  ...
  (:update-slots
   (append (gv opal:graphical-object :update-slots)
	   '((:left) (:top) (:width) (:height)
	     (:angle1) (:angle2))))
  ...)
@end[programexample]

By doing this, you insure that all the necessary slots are inherited, and
add any new slots as necessary.

@subsection(:drawable)

@index(drawable)
The @pr(:drawable) slot contains a structure that is the CLX drawable
object that the
object is to display itself into when it is sent a @pr[draw] message.  This
object may not be the physical window that the object is to be displayed
into, it may be a pixmap that is double buffered onto the screen somewhere
in the update algorithm.  All objects should trust the value in this slot,
even though is may not correspond to the drawable of their window.  This
slot may not contain a value until the object (or one of its parents) is
placed in a Garnet window.

@subsection(:display-info)
@index(display-info)

The @pr(:display-info) slot holds information used by many of the CLX
primitives for
computation, and drawing.  Once an object is placed in a window, this slot
contains an opal structure:

@begin(programexample)

(defstruct (display-info 
            (:print-function display-info-printer))
  display
  screen
  root-window
  default-gcontext)

@end(programexample)

The form @pr[(display-info-@i[xxx] (gv @i[object] :display-info))]
returns the @i[xxx] structure from @i[object]'s @pr[:display-info] slot.
These fields are useful as follows:

@index(display-info-display)@index(display)
@index(display-info-screen)@index(screen)
@index(display-info-root-window)@index(root-window)
@index(display-info-default-gcontext)@index(default-gcontext)
@begin[itemize]

@pr[display] is the CLX structure corresponding to the current display
connection to the X server.  This is used in calls that affect or query the
server directly, such as @t<xlib:open-font>, @pr[xlib:display-force-output],
and @t<xlib:global-pointer-position>.

@pr[screen] is the CLX structure containing information about the window's
screen.  This is used most often with structure accessors to get
information on values for the screen's white and black pixels, width and
height in pixels or millimeters.

@pr[root-window] is the CLX window that corresponds to @pr[@w{(xlib:screen-root @i[screen])}] for use in calls to @pr[xlib:create-window].

@pr[default-context] is a CLX graphical context structure used all drawing
requests.  Opal maintains a cache on this object, so it should not be
changed.  It is acceptable to use this structure in an
@pr[xlib:with-gcontext] form when it is necessary to modify a gcontext
outside the bounds of @pr[with-filling-styles @r[and ]with-line-styles].

@end[itemize]

@subsection(:x-tiles)

@index(x-tiles)
The @pr(x-tiles) slot contains a formula that computes a pixmap for use in drawing
tiled lines, or pattern filled regions.  The formula evaluates to a cons
cell the car of which is the pixmap to use for tiling lines, and the cdr of
which is a pixmap to use when drawing fillings.  These are computed from
values in the object's @pr[:line-style @r[and] :filling-style] slots.

@subsection(:x-draw-function)
@index(x-draw-function)

This slot contains a formula that is used to compute the CLX drawing
function from the @pr[:draw-function] slot.  It probably won't ever be
necessary to change the formula in this slot.

@section(Methods on all graphical objects)

The following methods are defined on all graphical objects and may be
specialized upon when creating new classes of graphical objects.

@index(draw)
@Begin[ProgramExample]
opal:Draw @i(graphical-object)@value(method)
@End[ProgramExample]

The @pr[draw] method on a graphical object causes the object to display
itself in the window of its aggregate.  This is only called by the update
methods, never directly by users of Opal.

@index(initialize)
@begin(ProgramExample)
initialize @i(graphical-object)@value(method)
@end(ProgramExample)

This method is called immediately after an instance of an object is
created.  It is passed the new object as its only argument.  

@begin(ProgramExample)
opal:Point-In-Gob @i(graphical-object x y)@value(method)
@end(ProgramExample)

This method should be provided for all new objects.  It is used by
@pr[point-to-component] and @pr[point-to-leaf] to query an object for a hit.
The method should return @c[t] if the object is under the point
@w{(@i[x,y])}.  This function should also take into account the values in the
@pr[:hit-threshold], @pr[:select-outline-only], and @pr[:visible] slots of
the object, as described in section @ref(stdfuncs).  Objects that are not
visible should return @c(nil).

@index(fix-properties)
@begin(ProgramExample)
opal::Fix-Properties @i(graphical-object changed-slots)@value(method)
@end(ProgramExample)

This method is called on aggregates and windows at the time when the update
algorithm passes them during an update.  The method is called with an
object, and a list of slots that have changed since it was last called.
This function is often useful for calling functions that cannot
easily be put into formulas.

Currently @pr[fix-properties] is not called on graphical objects, but this
functionality can be added to Opal by talking to the maintainer.

@section(Draw Methods)

There are several things that are worthy of note when working on @pr[draw]
methods for new objects.  

Objects @i[must] draw entirely within their bounding box.  The redisplay
algorithm will not work properly if things are drawn outside of their
bounding boxes.

There are two macros for use in writing draw methods that prepare a
gcontext from the gcontext cache that is appropriate for drawing outlines
or fillings as described by the values in the @pr[:line-style] and
@pr[:filling-style] slots of the object.

@index(with-filling-styles)
@begin(programexample)
with-filling-styles (@i[variable graphical-object@r[)] body ]@value(macro)
@end(programexample)

This form executes the forms inside @i[body] with @i[variable] bound to a
CLX gcontext structure suitable for drawing the filling of an object with
respect to @i[graphical-object]'s filling style object in the slot
@pr[:filling-style].

@index(with-line-styles)
@begin(programexample)
with-line-styles (@i[variable graphical-object@r[)] body ]@value(macro)
@end(programexample)

This form executes the forms inside @i[body] with @i[variable] bound to a
CLX gcontext structure suitable for drawing the outline of an object with
respect to @i[graphical-object]'s line style object in the slot
@pr[:line-style].

These forms are commonly used like this:

@begin(programexample)
(define-method :draw opal:polyline (polyline)
  (let ((point-list (gv polyline :point-list))
	(drawable (gv polyline :window :drawable)))      
    (with-filling-styles (gcontext polyline)
      (xlib:draw-lines drawable gcontext
	               point-list :fill-p t))
    (with-line-styles (gcontext polyline)
      (xlib:draw-lines drawable gcontext
	               point-list))))
@end(programexample)

@label(creating-new-gobs)
*************************************************************
@end(Comment)

@UnNumbered(References)
@bibliography


